package riscv.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.RV32I

class Pipeline5Spec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline5"

  // ============================================================
  // ENCODERS (idênticos ao Pipeline3Spec)
  // ============================================================
  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x13L

  private def add(rd: Int, rs1: Int, rs2: Int): Long =
    (0x00L << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x33L

  private def lui(rd: Int, imm20: Int): Long =
    ((imm20 & 0xfffff).toLong << 12) | ((rd & 0x1f).toLong << 7) | 0x37L

  private def beq(rs1: Int, rs2: Int, imm: Int): Long = {
    val e = imm & 0x1fff
    (((e >> 12) & 0x1).toLong << 31) |
      (((e >> 5) & 0x3f).toLong << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      (((e >> 1) & 0xf).toLong << 8) |
      (((e >> 11) & 0x1).toLong << 7) |
      0x63L
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

  private val ECALL  = 0x00000073L
  private val EBREAK = 0x00100073L
  private val MRET   = 0x30200073L

  private def csrrw(rd: Int, csr: Int, rs1: Int): Long =
    ((csr & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x1L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x73L

  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    ((csr & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x73L

  /** Store word: sw rs2, imm(rs1)  — opcode 0x23, funct3 010 */
  private def sw(rs2: Int, rs1: Int, imm: Int): Long = {
    val e = imm & 0xfff
    val imm11_5 = (e >> 5) & 0x7f
    val imm4_0  = e & 0x1f
    (imm11_5.toLong << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      (imm4_0.toLong << 7) |
      0x23L
  }

  /** Load word: lw rd, imm(rs1)  — opcode 0x03, funct3 010 */
  private def lw(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x03L
  }

  // ============================================================
  // TESTE 1: sequência aritmética com forwarding
  // ============================================================
  it should "executar uma sequencia aritmetica pequena com forwarding basico" in {
    val program = Seq(
      addi(1, 0, 5),
      addi(2, 1, 3),
      add(3, 1, 2),
      addi(0, 0, 0)
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 32)) { dut =>
      // 5 estágios: add x3 em WB no ciclo 6 (0-indexado: após 6 steps)
      for (_ <- 0 until 6) { dut.clock.step(1) }
      dut.io.writebackRd.expect(3)
      dut.io.writebackData.expect(13)
      dut.io.writebackEnable.expect(true)
    }
  }

  // ============================================================
  // TESTE 2: flush no branch tomado
  // ============================================================
  it should "fazer flush da instrucao seguinte quando um branch e tomado" in {
    val program = Seq(
        addi(1, 0, 5),  // 0x00
        addi(2, 0, 5),  // 0x04
        beq(1, 2, 8),   // 0x08 → alvo = 0x10
        addi(3, 0, 99), // 0x0C (descartada)
        addi(3, 0, 42), // 0x10 (alvo)
        addi(0, 0, 0)
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 32)) { dut =>
        var x3Vistos = scala.collection.mutable.Set[BigInt]()

        for (_ <- 0 until 20) {
        if (dut.io.writebackEnable.peekBoolean() &&
            dut.io.writebackRd.peekInt().toInt == 3) {
            x3Vistos += dut.io.writebackData.peekInt()
        }
        dut.clock.step(1)
        }

        assert(!x3Vistos.contains(BigInt(99)),
        s"x3=99 foi escrito; flush falhou (vistos: $x3Vistos)")
        assert(x3Vistos.contains(BigInt(42)),
        s"x3=42 não foi escrito (vistos: $x3Vistos)")
    }
    }

  // ============================================================
  // TESTE 3: ECALL → mtvec
  // ============================================================
  it should "redirecionar para mtvec em ECALL e salvar mepc/mcause" in {
    val program = Seq(
      addi(5, 0, 0x40),    // 0x00
      csrrw(0, 0x305, 5),  // 0x04: mtvec = 0x40
      addi(1, 0, 7),       // 0x08
      ECALL,               // 0x0C: trap
      addi(1, 0, 99),      // 0x10: NÃO executa
      addi(0, 0, 0)
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 64)) { dut =>
      var ciclos = 0
      while (dut.io.debug_mcause.peek().litValue == 0 && ciclos < 200) {
        dut.clock.step(1); ciclos += 1
      }
      assert(ciclos < 200, "timeout: mcause não foi escrito")
      dut.io.debug_mcause.expect(11.U)
      dut.io.debug_mepc.expect(0x0C.U)
      dut.io.debug_mtvec.expect(0x40.U)
      dut.io.debug_priv.expect(3.U)
    }
  }

  // ============================================================
  // TESTE 4: ECALL + handler + MRET + retomada
  // ============================================================
  it should "executar ECALL + handler + MRET e retomar na instrucao seguinte" in {
  val program = Seq(
    addi(5, 0, 0x20),   // 0x00
    csrrw(0, 0x305, 5), // 0x04: mtvec = 0x20
    addi(1, 0, 7),      // 0x08
    ECALL,              // 0x0C: trap -> 0x20
    addi(1, 1, 1),      // 0x10: retomada
    jal(0, 0x24),       // 0x14: pula handler -> 0x38
    0x00000013L,        // 0x18
    0x00000013L,        // 0x1C
    csrrs(6, 0x341, 0), // 0x20: x6 = mepc
    addi(6, 6, 4),      // 0x24: x6 = mepc + 4
    csrrw(0, 0x341, 6), // 0x28: mepc = 0x10
    csrrs(7, 0x342, 0), // 0x2C: x7 = mcause
    MRET,               // 0x30: PC = mepc
    0x00000013L,        // 0x34
    jal(0, 0)           // 0x38: loop infinito
  )

  test(new Pipeline5(initialProgram = program, memoryWords = 64)) { dut =>
    // Passo 1: mcause=11, mtvec=0x20
    var ciclos = 0
    while (dut.io.debug_mcause.peek().litValue == 0 && ciclos < 200) {
      dut.clock.step(1); ciclos += 1
    }
    assert(ciclos < 200, "timeout: mcause não foi escrito pelo ecall")
    dut.io.debug_mcause.expect(11.U)
    dut.io.debug_mtvec.expect(0x20.U)

    // Passo 2: mepc=0x10
    ciclos = 0
    while (dut.io.debug_mepc.peek().litValue != 0x10 && ciclos < 200) {
      dut.clock.step(1); ciclos += 1
    }
    assert(ciclos < 200, "timeout: handler não avançou mepc para 0x10")
    dut.io.debug_mepc.expect(0x10.U)

    // Passo 3: PC passa por 0x10 após o mret
    ciclos = 0
    var viuRetomada = false
    while (!viuRetomada && ciclos < 200) {
      dut.clock.step(1); ciclos += 1
      if (dut.io.pc.peek().litValue == 0x10) viuRetomada = true
    }
    assert(viuRetomada, "PC não passou por 0x10 após o mret")

    // Passo 4: mepc e mcause preservados após 50 ciclos
    for (_ <- 0 until 50) { dut.clock.step(1) }
    dut.io.debug_mepc.expect(0x10.U)
    dut.io.debug_mcause.expect(11.U)

    // Passo 5: PC não voltou ao handler
    val pcFinal = dut.io.pc.peek().litValue.toInt
    assert(pcFinal != 0x20 && pcFinal != 0x28,
      s"PC final 0x${pcFinal.toHexString} sugere reexecução do handler")
  }
}


  // ============================================================
  // TESTE 5: EBREAK → mtvec
  // ============================================================
  it should "redirecionar para mtvec em EBREAK" in {
    val program = Seq(
      addi(5, 0, 0x20),   // 0x00
      csrrw(0, 0x305, 5), // 0x04: mtvec = 0x20
      EBREAK,             // 0x08
      0x00000013L,        // 0x0C
      0x00000013L,        // 0x10
      0x00000013L,        // 0x14
      0x00000013L,        // 0x18
      0x00000013L,        // 0x1C
      csrrs(7, 0x342, 0), // 0x20: x7 = mcause
      0x00000013L         // 0x24
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 64)) { dut =>
      var ciclos = 0
      while (dut.io.debug_mcause.peek().litValue == 0 && ciclos < 200) {
        dut.clock.step(1); ciclos += 1
      }
      assert(ciclos < 200, "timeout: mcause não foi escrito pelo ebreak")
      dut.io.debug_mcause.expect(3.U)
      dut.io.debug_mepc.expect(0x08.U)
      dut.io.debug_mtvec.expect(0x20.U)
      dut.io.debug_priv.expect(3.U)
    }
  }

  // ============================================================
  // TESTE 6: instrução ilegal → trap
  // ============================================================
  it should "tratar .word 0x00000000 como instrucao ilegal e redirecionar" in {
    val program = Seq(
      addi(5, 0, 0x20),   // 0x00
      csrrw(0, 0x305, 5), // 0x04: mtvec = 0x20
      0x00000000L,        // 0x08: ilegal
      0x00000013L,        // 0x0C
      0x00000013L,        // 0x10
      0x00000013L,        // 0x14
      0x00000013L,        // 0x18
      0x00000013L,        // 0x1C
      csrrs(7, 0x342, 0), // 0x20: x7 = mcause
      0x00000013L         // 0x24
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 64)) { dut =>
      var ciclos = 0
      while (dut.io.debug_mcause.peek().litValue == 0 && ciclos < 200) {
        dut.clock.step(1); ciclos += 1
      }
      assert(ciclos < 200, "timeout: mcause não foi escrito pela instrução ilegal")
      dut.io.debug_mcause.expect(2.U)
      dut.io.debug_mepc.expect(0x08.U)
      dut.io.debug_mtvec.expect(0x20.U)
      dut.io.debug_priv.expect(3.U)
    }
  }

  // ============================================================
  // TESTE 7: csrrs com rs1=x0
  // ============================================================
  it should "não escrever em CSR quando csrrs tem rs1=x0" in {
    val program = Seq(
      addi(1, 0, 0x100),      // x1 = 0x100
      csrrw(0, 0x305, 1),     // mtvec = 0x100
      csrrs(2, 0x305, 0),     // x2 = mtvec; mtvec NÃO deve mudar
      csrrs(3, 0x305, 0),     // x3 = mtvec; confirma
      jal(0, 0)               // loop infinito
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 32)) { dut =>
      var x2: BigInt = 0; var x2Ok = false
      var x3: BigInt = 0; var x3Ok = false
      for (_ <- 0 until 20) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val d  = dut.io.writebackData.peekInt()
          if (rd == 2 && !x2Ok) { x2 = d; x2Ok = true }
          if (rd == 3 && !x3Ok) { x3 = d; x3Ok = true }
        }
        dut.clock.step(1)
      }
      assert(x2Ok, "x2 não foi escrito")
      assert(x2 == BigInt(0x100), s"x2 esperado 0x100, obtido 0x${x2.toString(16)}")
      assert(x3Ok, "x3 não foi escrito")
      assert(x3 == BigInt(0x100), s"x3 esperado 0x100 (mtvec não mudou), obtido 0x${x3.toString(16)}")
    }
  }

  it should "gerar stall em load-use hazard" in {
    val program = Seq(
      addi(1, 0, 0x100),      // x1 = 0x100
      addi(2, 0, 42),         // x2 = 42
      sw(2, 1, 0),            // mem[0x100] = 42
      lw(3, 1, 0),            // x3 = mem[0x100]
      add(4, 3, 2),           // x4 = x3 + x2 = 84 (load-use hazard)
      jal(0, 0)               // loop infinito
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 64)) { dut =>
      var x4: BigInt = 0; var x4Ok = false
      var stallOk = false
      for (_ <- 0 until 40) {
        if (dut.io.stalled.peekBoolean()) stallOk = true
        if (dut.io.writebackEnable.peekBoolean() &&
            dut.io.writebackRd.peekInt().toInt == 4) {
          x4 = dut.io.writebackData.peekInt(); x4Ok = true
        }
        dut.clock.step(1)
      }
      assert(stallOk, "stall não foi observado")
      assert(x4Ok, "x4 não foi escrito")
      assert(x4 == BigInt(84), s"x4 esperado 84, obtido $x4")
    }
  }

  it should "executar mret isolado e redirecionar para mepc" in {
    val program = Seq(
      addi(1, 0, 0x20),       // x1 = 0x20
      csrrw(0, 0x341, 1),     // mepc = 0x20
      MRET,                   // PC = mepc = 0x20
      0x00000013L,            // 0x0C
      0x00000013L,            // 0x10
      0x00000013L,            // 0x14
      0x00000013L,            // 0x18
      0x00000013L,            // 0x1C
      jal(0, 0)               // 0x20: loop infinito
    )

    test(new Pipeline5(initialProgram = program, memoryWords = 32)) { dut =>
      var viuMepc = false
      for (_ <- 0 until 30) {
        dut.clock.step(1)
        if (dut.io.pc.peek().litValue == 0x20) viuMepc = true
      }
      assert(viuMepc, "PC não passou por 0x20 após o mret")
    }
  }
}