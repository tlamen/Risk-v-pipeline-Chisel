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
    val imm4_0 = imm & 0x1f
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

  // Instruções de sistema (32 bits)
  private val ECALL  = 0x00000073L
  private val EBREAK = 0x00100073L
  private val MRET   = 0x30200073L
  private val SRET   = 0x10200073L
  private val NOP    = 0x00000013L

  // ============================================================
  // ESTRUTURA DE CAPTURA
  // ============================================================

  /**
    * Estrutura que armazena o último writeback observado para cada registrador.
    */
  class WritebackTracker {
    var valor: BigInt = 0
    var capturado: Boolean = false

    def capturar(data: BigInt): Unit = {
      if (!capturado) {
        valor = data
        capturado = true
      }
    }
  }

  // ============================================================
  // TESTE 1: CSR Write/Read básico
  // ============================================================
  it should "ler e escrever CSRs corretamente" in {
    val program = Seq(
      addi(1, 0, 0x100),      // 0x00: x1 = 0x100
      csrrw(2, 0x305, 1),     // 0x04: mtvec = x1, x2 = mtvec antigo (0)
      csrrs(3, 0x305, 0),     // 0x08: x3 = mtvec (leitura = 0x100)
      NOP,                    // 0x0C
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      println("COMECA O TESTE 1")
      val x1 = new WritebackTracker
      val x2 = new WritebackTracker
      val x3 = new WritebackTracker

      for (_ <- 0 until 20) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()

          rd match {
            case 1 => x1.capturar(data)
            case 2 => x2.capturar(data)
            case 3 => x3.capturar(data)
            case _ => // x0 ou outro registrador
          }
        }
        dut.clock.step(1)
      }
      
      println("TERMINA O TESTE 1")
      assert(x1.capturado, "x1 não foi escrito")
      assert(x1.valor == BigInt(0x100),
        s"x1 esperado 0x100, obtido 0x${x1.valor.toString(16)}")

      assert(x2.capturado, "x2 não foi escrito")
      assert(x2.valor == BigInt(0),
        s"x2 esperado 0 (mtvec antigo), obtido 0x${x2.valor.toString(16)}")

      assert(x3.capturado, "x3 não foi escrito")
      assert(x3.valor == BigInt(0x100),
        s"x3 esperado 0x100 (mtvec novo), obtido 0x${x3.valor.toString(16)}")
    }
  }

  // ============================================================
  // TESTE 2: ECALL + Handler + MRET
  // ============================================================
  it should "executar ECALL, handler e MRET corretamente" in {
    val program = Seq(
      addi(2, 0, 0x20),       // 0x00: x2 = 0x20
      csrrw(0, 0x305, 2),     // 0x04: mtvec = 0x20
      addi(1, 0, 100),        // 0x08: x1 = 100
      ECALL,                  // 0x0C: ECALL
      addi(3, 0, 1),          // 0x10: x3 = 1 (após MRET)
      NOP,                    // 0x14
      NOP,                    // 0x18
      NOP,                    // 0x1C
      addi(5, 0, 5),          // 0x20: handler: x5 = 5
      MRET,                   // 0x24: retorna para 0x10
      NOP,                    // 0x28
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x1 = new WritebackTracker
      val x3 = new WritebackTracker
      val x5 = new WritebackTracker

      for (_ <- 0 until 50) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()

          rd match {
            case 1 => x1.capturar(data)
            case 3 => x3.capturar(data)
            case 5 => x5.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(x1.capturado, "x1 não foi escrito (antes do ECALL)")
      assert(x1.valor == BigInt(100),
        s"x1 esperado 100, obtido $x1.valor")

      assert(x5.capturado, "x5 não foi escrito (handler)")
      assert(x5.valor == BigInt(5),
        s"x5 esperado 5, obtido $x5.valor")

      assert(x3.capturado, "x3 não foi escrito (após MRET)")
      assert(x3.valor == BigInt(1),
        s"x3 esperado 1, obtido $x3.valor")
    }
  }

  // ============================================================
  // TESTE 3: mstatus.MIE desabilitado durante handler
  // ============================================================
  it should "desabilitar MIE durante handler e restaurar após MRET" in {
    val program = Seq(
      addi(1, 0, 8),          // 0x00: x1 = 8 (bit MIE)
      csrrs(0, 0x300, 1),     // 0x04: mstatus |= 8 (MIE=1)
      addi(2, 0, 0x20),       // 0x08
      csrrw(0, 0x305, 2),     // 0x0C: mtvec = 0x20
      ECALL,                  // 0x10
      addi(3, 0, 1),          // 0x14
      NOP,                    // 0x18
      NOP,                    // 0x1C
      addi(5, 0, 5),          // 0x20: handler
      csrrs(6, 0x300, 0),     // 0x24: x6 = mstatus
      MRET,                   // 0x28
      NOP,                    // 0x2C
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x3 = new WritebackTracker
      val x5 = new WritebackTracker
      val x6 = new WritebackTracker

      for (_ <- 0 until 60) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()

          rd match {
            case 3 => x3.capturar(data)
            case 5 => x5.capturar(data)
            case 6 => x6.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(x5.capturado, "x5 não foi escrito (handler)")
      assert(x5.valor == BigInt(5),
        s"x5 esperado 5, obtido $x5.valor")

      assert(x6.capturado, "x6 não foi escrito (leitura de mstatus)")
      val mie_durante_handler = (x6.valor >> 3) & 1
      assert(mie_durante_handler == 0,
        s"MIE esperado 0 durante handler, obtido $mie_durante_handler (mstatus=0x${x6.valor.toString(16)})")

      assert(x3.capturado, "x3 não foi escrito (após MRET)")
      assert(x3.valor == BigInt(1),
        s"x3 esperado 1, obtido $x3.valor")
    }
  }

  // ============================================================
  // TESTE 4: mstatus.MPIE salva MIE antes do trap
  // ============================================================
  it should "salvar MIE em MPIE no trap entry" in {
    val program = Seq(
      addi(1, 0, 8),          // 0x00
      csrrs(0, 0x300, 1),     // 0x04: mstatus |= 8 (MIE=1)
      addi(2, 0, 0x20),       // 0x08
      csrrw(0, 0x305, 2),     // 0x0C: mtvec = 0x20
      ECALL,                  // 0x10
      addi(3, 0, 1),          // 0x14
      NOP,                    // 0x18
      NOP,                    // 0x1C
      addi(5, 0, 5),          // 0x20
      csrrs(6, 0x300, 0),     // 0x24: x6 = mstatus
      MRET,                   // 0x28
      NOP,                    // 0x2C
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x6 = new WritebackTracker

      for (_ <- 0 until 60) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()

          println(s"[CICLO] rd=$rd, data=0x${data.toString(16)}")

          rd match {
            case 6 => x6.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(x6.capturado, "x6 não foi escrito (leitura de mstatus)")
      val mpie = (x6.valor >> 7) & 1
      assert(mpie == 1,
        s"MPIE esperado 1 durante handler, obtido $mpie (mstatus=0x${x6.valor.toString(16)})")
    }
  }

  // ============================================================
  // TESTE 5: Leitura de CSR após escrita
  // ============================================================
  it should "ler mtvec após escrever" in {
    val program = Seq(
      addi(1, 0, 0xABCD),     // 0x00
      csrrw(0, 0x305, 1),     // 0x04: mtvec = 0xABCD
      csrrs(2, 0x305, 0),     // 0x08: x2 = mtvec (0xABCD)
      NOP,                    // 0x0C
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      val x2 = new WritebackTracker

      for (_ <- 0 until 20) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()

          println(s"[CICLO] rd=$rd, data=0x${data.toString(16)}")

          rd match {
            case 2 => x2.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(x2.capturado, "x2 não foi escrito")
      assert(x2.valor == BigInt(0xABCD),
        s"x2 esperado 0xABCD, obtido 0x${x2.valor.toString(16)}")
    }
  }

  // ============================================================
  // TESTE 6: EBREAK gera trap
  // ============================================================
  it should "gerar trap em EBREAK" in {
    val program = Seq(
      addi(2, 0, 0x20),       // 0x00
      csrrw(0, 0x305, 2),     // 0x04: mtvec = 0x20
      addi(1, 0, 55),         // 0x08
      EBREAK,                 // 0x0C
      addi(3, 0, 1),          // 0x10
      NOP,                    // 0x14
      NOP,                    // 0x18
      NOP,                    // 0x1C
      addi(5, 0, 5),          // 0x20: handler
      MRET,                   // 0x24
      NOP,                    // 0x28
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x3 = new WritebackTracker
      val x5 = new WritebackTracker

      for (_ <- 0 until 50) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()

          rd match {
            case 3 => x3.capturar(data)
            case 5 => x5.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(x5.capturado, "x5 não foi escrito (handler)")
      assert(x5.valor == BigInt(5),
        s"x5 esperado 5, obtido $x5.valor")

      assert(x3.capturado, "x3 não foi escrito (após MRET)")
      assert(x3.valor == BigInt(1),
        s"x3 esperado 1, obtido $x3.valor")
    }
  }

  // ============================================================
  // TESTE 7: PC avança normalmente sem trap
  // ============================================================
  it should "avançar PC normalmente sem traps" in {
    val program = Seq(
      addi(1, 0, 1),          // 0x00
      addi(2, 0, 2),          // 0x04
      addi(3, 0, 3),          // 0x08
      addi(4, 0, 4),          // 0x0C
      NOP,                    // 0x10
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      for (_ <- 0 until 10) {
        dut.clock.step(1)
      }

      val pcFinal = dut.io.pc.peekInt()
      assert(pcFinal >= BigInt(0x10),
        s"PC final esperado >= 0x10, obtido 0x${pcFinal.toString(16)}")
    }
  }

  // ============================================================
  // TESTE 8: Load-use hazard gera stall
  // ============================================================
  it should "gerar stall em load-use hazard" in {
    val program = Seq(
      addi(1, 0, 0x100),      // 0x00: x1 = 0x100 (endereço)
      addi(2, 0, 42),         // 0x04: x2 = 42
      sw(2, 1, 0),            // 0x08: mem[0x100] = 42
      lw(3, 1, 0),            // 0x0C: x3 = mem[0x100] (LOAD)
      add(4, 3, 2),           // 0x10: x4 = x3 + x2 (load-use hazard!)
      NOP,                    // 0x14
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val x4 = new WritebackTracker
      var stallObservado = false

      for (_ <- 0 until 30) {
        if (dut.io.stalled.peekBoolean()) {
          stallObservado = true
        }
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()

          println(s"[CICLO] rd=$rd, data=0x${data.toString(16)}")

          rd match {
            case 4 => x4.capturar(data)
            case _ =>
          }
        }
        dut.clock.step(1)
      }

      assert(stallObservado, "Stall não foi observado durante load-use hazard")
      assert(x4.capturado, "x4 não foi escrito")
      assert(x4.valor == BigInt(84),
        s"x4 esperado 84 (42+42), obtido $x4.valor")
    }
  }
}