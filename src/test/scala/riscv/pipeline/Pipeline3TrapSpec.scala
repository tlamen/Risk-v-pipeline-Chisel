package riscv.pipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Pipeline3TrapSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3 (Trap Handling)"

  // ============================================================
  // HELPERS PARA CODIFICAR INSTRUÇÕES
  // ============================================================

  private def addi(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x13L
  }

  private def add(rd: Int, rs1: Int, rs2: Int): Long = {
    (0x00L << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x33L
  }

  private def csrrw(rd: Int, csr: Int, rs1: Int): Long = {
    ((csr & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x1L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x73L
  }

  private def csrrs(rd: Int, csr: Int, rs1: Int): Long = {
    ((csr & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x73L
  }

  private def sw(rs2: Int, rs1: Int, imm: Int): Long = {
    val imm11_5 = (imm >> 5) & 0x7f
    val imm4_0  = imm & 0x1f
    (imm11_5.toLong << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      (imm4_0.toLong << 7) |
      0x23L
  }

  private def lw(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x03L
  }

  private def jal(rd: Int, imm: Int): Long = {
    val e = imm & 0x1fffff
    (((e >> 20) & 0x1).toLong << 31) |
      (((e >> 1) & 0x3ff).toLong << 21) |
      (((e >> 11) & 0x1).toLong << 20) |
      (((e >> 12) & 0xff).toLong << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x6fL
  }

  // Instruções de sistema
  private val ECALL  = 0x00000073L
  private val EBREAK = 0x00100073L
  private val MRET   = 0x30200073L
  private val NOP    = 0x00000013L

  // ============================================================
  // ESTRUTURA DE CAPTURA DE WRITEBACK
  // ============================================================
  class WritebackTracker {
    var valor: BigInt = 0
    var capturado: Boolean = false
    def capturar(data: BigInt): Unit = {
      valor = data
      capturado = true
    }
  }

  // ============================================================
  // TESTE 1: CSR write/read e regra Zicsr (csrrs com rs1=x0 não escreve)
  // ============================================================
  it should "ler e escrever CSRs corretamente, sem escrita em csrrs com rs1=x0" in {
    val program = Seq(
      addi(1, 0, 0x100),     // 0x00
      csrrw(2, 0x305, 1),    // 0x04
      csrrs(3, 0x305, 0),    // 0x08
      csrrs(4, 0x305, 0),    // 0x0C
      jal(0, 0)              // 0x10: loop infinito
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      val x1 = new WritebackTracker
      val x2 = new WritebackTracker
      val x3 = new WritebackTracker
      val x4 = new WritebackTracker

      for (_ <- 0 until 30) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd   = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()
          rd match {
            case 1 => x1.capturar(data)
            case 2 => x2.capturar(data)
            case 3 => x3.capturar(data)
            case 4 => x4.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(x1.capturado, "x1 não foi escrito")
      assert(x1.valor == BigInt(0x100),
        s"x1 esperado 0x100, obtido 0x${x1.valor.toString(16)}")

      assert(x2.capturado, "x2 não foi escrito")
      assert(x2.valor == BigInt(0),
        s"x2 esperado 0 (mtvec antigo), obtido 0x${x2.valor.toString(16)}")

      assert(x3.capturado, "x3 não foi escrito")
      assert(x3.valor == BigInt(0x100),
        s"x3 esperado 0x100 (mtvec), obtido 0x${x3.valor.toString(16)}")

      assert(x4.capturado, "x4 não foi escrito")
      assert(x4.valor == BigInt(0x100),
        s"x4 esperado 0x100 (mtvec não foi alterado pelo csrrs), obtido 0x${x4.valor.toString(16)}")
    }
  }

  // ============================================================
  // TESTE 2: semântica de mstatus no trap e no mret
  //   - antes do ecall: MIE = 1
  //   - durante o handler: MIE = 0, MPIE = 1
  //   - após o mret: MIE = 1, MPIE = 1
  // ============================================================
  it should "preservar a semântica de mstatus no trap e no mret" in {
    val program = Seq(
      addi(1, 0, 8),          // 0x00
      csrrs(0, 0x300, 1),     // 0x04: mstatus.MIE = 1
      addi(2, 0, 0x20),       // 0x08
      csrrw(0, 0x305, 2),     // 0x0C: mtvec = 0x20
      ECALL,                  // 0x10
      csrrs(7, 0x300, 0),     // 0x14: após mret: x7 = mstatus
      jal(0, 0),              // 0x18: loop infinito
      NOP,                    // 0x1C
      csrrs(6, 0x300, 0),     // 0x20: handler: x6 = mstatus
      csrrs(5, 0x341, 0),     // 0x24: x5 = mepc
      addi(5, 5, 4),          // 0x28: x5 = mepc + 4
      csrrw(0, 0x341, 5),     // 0x2C: mepc = mepc + 4
      MRET,                   // 0x30
      NOP                     // 0x34
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x6 = new WritebackTracker
      val x7 = new WritebackTracker

      for (_ <- 0 until 80) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd   = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()
          rd match {
            case 6 => x6.capturar(data)
            case 7 => x7.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      // Durante o handler: MIE=0, MPIE=1
      assert(x6.capturado, "x6 não foi escrito (leitura de mstatus no handler)")
      val mieHandler  = (x6.valor >> 3) & 1
      val mpieHandler = (x6.valor >> 7) & 1
      assert(mieHandler == 0,
        s"MIE esperado 0 durante handler, obtido $mieHandler (mstatus=0x${x6.valor.toString(16)})")
      assert(mpieHandler == 1,
        s"MPIE esperado 1 durante handler, obtido $mpieHandler (mstatus=0x${x6.valor.toString(16)})")

      // Após o mret: MIE=1, MPIE=1
      assert(x7.capturado, "x7 não foi escrito (leitura de mstatus após mret)")
      val mieApos  = (x7.valor >> 3) & 1
      val mpieApos = (x7.valor >> 7) & 1
      assert(mieApos == 1,
        s"MIE esperado 1 após mret, obtido $mieApos (mstatus=0x${x7.valor.toString(16)})")
      assert(mpieApos == 1,
        s"MPIE esperado 1 após mret, obtido $mpieApos (mstatus=0x${x7.valor.toString(16)})")
    }
  }

  // ============================================================
  // TESTE 3: EBREAK gera trap, handler executa, mret retoma
  // ============================================================
  it should "gerar trap em EBREAK e retomar via handler + mret" in {
    val program = Seq(
      addi(2, 0, 0x20),       // 0x00
      csrrw(0, 0x305, 2),     // 0x04: mtvec = 0x20
      addi(1, 0, 55),         // 0x08
      EBREAK,                 // 0x0C
      addi(3, 0, 1),          // 0x10: retomada
      jal(0, 0),              // 0x14: loop infinito
      NOP,                    // 0x18
      NOP,                    // 0x1C
      csrrs(6, 0x341, 0),     // 0x20: x6 = mepc
      addi(6, 6, 4),          // 0x24
      csrrw(0, 0x341, 6),     // 0x28: mepc = mepc + 4
      csrrs(7, 0x342, 0),     // 0x2C: x7 = mcause
      MRET,                   // 0x30
      NOP                     // 0x34
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x1 = new WritebackTracker
      val x3 = new WritebackTracker
      val x6 = new WritebackTracker
      val x7 = new WritebackTracker

      for (_ <- 0 until 80) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd   = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()
          rd match {
            case 1 => x1.capturar(data)
            case 3 => x3.capturar(data)
            case 6 => x6.capturar(data)
            case 7 => x7.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(x1.capturado, "x1 não foi escrito")
      assert(x1.valor == BigInt(55), s"x1 esperado 55, obtido ${x1.valor}")

      assert(x6.capturado, "x6 não foi escrito (mepc lido no handler)")
      assert(x6.valor == BigInt(0x10),
        s"x6 esperado 0x10 (mepc+4), obtido 0x${x6.valor.toString(16)}")

      assert(x7.capturado, "x7 não foi escrito (mcause lido no handler)")
      assert(x7.valor == BigInt(3),
        s"x7 esperado 3 (mcause do ebreak), obtido ${x7.valor}")

      assert(x3.capturado, "x3 não foi escrito (retomada após mret)")
      assert(x3.valor == BigInt(1), s"x3 esperado 1, obtido ${x3.valor}")
    }
  }

  // ============================================================
  // TESTE 4: PC avança normalmente sem traps
  // ============================================================
  it should "avançar PC normalmente sem traps" in {
    val program = Seq(
      addi(1, 0, 1),          // 0x00
      addi(2, 0, 2),          // 0x04
      addi(3, 0, 3),          // 0x08
      addi(4, 0, 4),          // 0x0C
      jal(0, 0)               // 0x10: loop infinito (evita instruções ilegais)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      for (_ <- 0 until 15) { dut.clock.step(1) }
      val pcFinal = dut.io.pc.peekInt()
      // Após 5 instruções (0x00-0x10) e o loop em 0x10, o PC deve estar
      // em 0x10 ou 0x14. A asserção verifica que o pipeline avançou além
      // da quarta instrução (0x0C), o que prova que o PC não está travado.
      assert(pcFinal >= BigInt(0x10),
        s"PC final esperado >= 0x10, obtido 0x${pcFinal.toString(16)}")
    }
  }

  // ============================================================
  // TESTE 5: load-use hazard gera stall e forwarding correto
  // ============================================================
  it should "gerar stall em load-use hazard e produzir o valor correto" in {
    val program = Seq(
      addi(1, 0, 0x100),      // 0x00
      addi(2, 0, 42),         // 0x04
      sw(2, 1, 0),            // 0x08
      lw(3, 1, 0),            // 0x0C
      add(4, 3, 2),           // 0x10
      jal(0, 0)               // 0x14: loop infinito
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x4 = new WritebackTracker
      var stallObservado = false

      for (_ <- 0 until 40) {
        if (dut.io.stalled.peekBoolean()) { stallObservado = true }
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd   = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()
          if (rd == 4) x4.capturar(data)
        }
        dut.clock.step(1)
      }

      assert(stallObservado, "stall não foi observado durante load-use hazard")
      assert(x4.capturado, "x4 não foi escrito")
      assert(x4.valor == BigInt(84), s"x4 esperado 84, obtido ${x4.valor}")
    }
  }
}