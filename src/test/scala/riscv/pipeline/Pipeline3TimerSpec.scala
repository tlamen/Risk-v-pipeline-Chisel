package riscv.pipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Pipeline3TimerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3 (Timer Integration)"

  // ============================================================
  // HELPERS
  // ============================================================

  private def addi(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x13L
  }

  private def lui(rd: Int, imm: Int): Long = {
    ((imm & 0xfffff).toLong << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x37L
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

  private def lw(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x03L
  }

  private val ECALL = 0x00000073L
  private val MRET  = 0x30200073L
  private val NOP   = 0x00000013L

  // ============================================================
  // TESTE: Timer gera trap
  // ============================================================
  it should "gerar trap quando o timer atinge o limite" in {
    // Programa:
    // 0x00: lui  x5, 0x80000     (x5 = 0x80000000 = TIMER_BASE)
    // 0x04: addi x4, x0, 5       (x4 = 5 = limit)
    // 0x08: sw   x4, 0x4(x5)     (timer.limit = 5)
    // 0x0C: addi x1, x0, 0x20    (x1 = 0x20 = handler)
    // 0x10: csrrw x0, mtvec, x1  (mtvec = 0x20)
    // 0x14: addi x2, x0, 8       (x2 = 8 = MIE)
    // 0x18: csrrs x0, mstatus, x2 (mstatus.MIE = 1)
    // 0x1C: addi x3, x0, 0x80    (x3 = 0x80 = MTIE)
    // 0x20: csrrs x0, mie, x3    (mie.MTIE = 1)
    // 0x24: nop
    // 0x28: nop
    // 0x2C: nop
    // 0x30: addi x6, x0, 5       (handler: x6 = 5)
    // 0x34: mret
    val program = Seq(
      lui(5, 0x80000),        // 0x00: x5 = 0x80000000
      addi(4, 0, 5),          // 0x04: x4 = 5
      sw(4, 5, 0x4),          // 0x08: timer.limit = 5
      addi(1, 0, 0x30),       // 0x0C: x1 = 0x30
      csrrw(0, 0x305, 1),     // 0x10: mtvec = 0x30
      addi(2, 0, 8),          // 0x14: x2 = 8
      csrrs(0, 0x300, 2),     // 0x18: mstatus.MIE = 1
      addi(3, 0, 0x80),       // 0x1C: x3 = 0x80
      csrrs(0, 0x304, 3),     // 0x20: mie.MTIE = 1
      NOP,                    // 0x24
      NOP,                    // 0x28
      NOP,                    // 0x2C
      addi(6, 0, 5),          // 0x30: handler: x6 = 5
      MRET,                   // 0x34: retorna
      NOP,                    // 0x38
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 256)) { dut =>
      var x6Capturado = false
      var x6Valor: BigInt = 0

      // Roda ciclos suficientes para o timer disparar
      for (_ <- 0 until 200) {
        if (dut.io.writebackEnable.peekBoolean() &&
            dut.io.writebackRd.peekInt().toInt == 6) {
          x6Valor = dut.io.writebackData.peekInt()
          x6Capturado = true
        }
        dut.clock.step(1)
      }

      assert(x6Capturado, "x6 não foi escrito (handler não executou)")
      assert(x6Valor == BigInt(5),
        s"x6 esperado 5, obtido $x6Valor")
    }
  }

  // ============================================================
  // TESTE: Leitura do Timer via MMIO
  // ============================================================
  it should "ler o limit do timer via MMIO" in {
    val program = Seq(
      lui(5, 0x80000),        // 0x00: x5 = 0x80000000
      addi(4, 0, 42),         // 0x04: x4 = 42
      sw(4, 5, 0x4),          // 0x08: timer.limit = 42
      lw(6, 5, 0x4),          // 0x0C: x6 = timer.limit
      NOP,                    // 0x10
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 256)) { dut =>
      var x6Capturado = false
      var x6Valor: BigInt = 0

      for (_ <- 0 until 30) {
        if (dut.io.writebackEnable.peekBoolean() &&
            dut.io.writebackRd.peekInt().toInt == 6) {
          x6Valor = dut.io.writebackData.peekInt()
          x6Capturado = true
        }
        dut.clock.step(1)
      }

      assert(x6Capturado, "x6 não foi escrito")
      assert(x6Valor == BigInt(42),
        s"x6 esperado 42, obtido $x6Valor")
    }
  }
}