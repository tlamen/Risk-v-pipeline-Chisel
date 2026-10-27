package riscv.pipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Pipeline3Spec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3"

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

  private def beq(rs1: Int, rs2: Int, imm: Int): Long = {
    val encodedImm = imm & 0x1fff
    (((encodedImm >> 12) & 0x1).toLong << 31) |
      (((encodedImm >> 5) & 0x3f).toLong << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      (((encodedImm >> 1) & 0xf).toLong << 8) |
      (((encodedImm >> 11) & 0x1).toLong << 7) |
      0x63L
  }

  it should "executar uma sequencia aritmetica pequena com forwarding basico" in {
    val program = Seq(
      addi(1, 0, 5), // x1 = 5
      addi(2, 1, 3), // x2 = x1 + 3
      add(3, 1, 2), // x3 = x1 + x2
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 256)) { dut =>
      for (_ <- 0 until 4) {
        dut.clock.step(1)
      }

      dut.io.writebackRd.expect(3)
      dut.io.writebackData.expect(13)
      dut.io.writebackEnable.expect(true)
    }
  }

  it should "fazer flush da instrucao seguinte quando um branch e tomado" in {
    val program = Seq(
      addi(1, 0, 5), // x1 = 5
      addi(2, 0, 5), // x2 = 5
      beq(1, 2, 8), // pula a proxima instrucao
      addi(3, 0, 99), // deve ser descartada pelo flush
      addi(3, 0, 42), // alvo do branch
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 256)) { dut =>
      for (_ <- 0 until 7) {
        dut.clock.step(1)
      }

      dut.io.writebackRd.expect(3)
      dut.io.writebackData.expect(42)
      dut.io.writebackEnable.expect(true)
    }
  }
}
