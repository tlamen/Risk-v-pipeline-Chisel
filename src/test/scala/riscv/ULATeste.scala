package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.ULA
import riscv.elementosbasicos.ALUOp._

// O teste estende AnyFlatSpec (do ScalaTest) e ChiselScalatestTester
class ULATest extends AnyFlatSpec with ChiselScalatestTester {

  // O bloco 'behavior of' descreve qual componente estamos testando
  behavior of "Unidade Lógica e Aritmética (ULA) do RISC-V"

  // Máscara para forçar representação de 32 bits em valores Scala/Long.
  private def u32(x: Long): BigInt = BigInt(x & 0xffffffffL)

  private def runCase(
      dut: ULA,
      opName: String,
      opCode: UInt,
      a: Long,
      b: Long,
      expected: Long
  ): Unit = {
    val inA = u32(a)
    val inB = u32(b)
    val exp = u32(expected)

    println(
      s"[$opName] A=$a (0x${inA.toString(16)}), B=$b (0x${inB.toString(16)})"
    )

    dut.io.a.poke(inA.U(32.W))
    dut.io.b.poke(inB.U(32.W))
    dut.io.op.poke(opCode)
    dut.clock.step(1)

    val got = dut.io.result.peekInt()
    println(
      s"[$opName] Resultado: $got (0x${got.toString(16)}), esperado: $exp (0x${exp.toString(16)})"
    )
    dut.io.result.expect(exp.U(32.W))
  }

  it should "cobrir todas as operacoes da ULA com casos basicos e de borda" in {
    test(new ULA) { dut =>
      println("==== INICIO DO TESTE COMPLETO DA ULA ====\n")

      // ADD
      runCase(dut, "ADD", ADD, 10, 5, 15)
      runCase(dut, "ADD", ADD, 0xffffffffL, 1, 0)

      // SUB
      runCase(dut, "SUB", SUB, 20, 8, 12)
      runCase(dut, "SUB", SUB, 0, 1, 0xffffffffL)

      // AND
      runCase(dut, "AND", AND, 0xf0f0f0f0L, 0x0ff00ff0L, 0x00f000f0L)
      runCase(dut, "AND", AND, 0xaaaaaaaa, 0x55555555, 0)

      // OR
      runCase(dut, "OR", OR, 0xf0f00000L, 0x00000ff0L, 0xf0f00ff0L)
      runCase(dut, "OR", OR, 0, 0, 0)

      // XOR
      runCase(dut, "XOR", XOR, 0xaaaaaaaa, 0x55555555, 0xffffffffL)
      runCase(dut, "XOR", XOR, 0x12345678L, 0x12345678L, 0)

      // SLL (usa apenas b(4,0))
      runCase(dut, "SLL", SLL, 1, 4, 16)
      runCase(dut, "SLL", SLL, 1, 40, 256) // 40 -> 8 (5 LSBs)

      // SRL (usa apenas b(4,0))
      runCase(dut, "SRL", SRL, 0x80000000L, 4, 0x08000000L)
      runCase(dut, "SRL", SRL, 0x80000000L, 32, 0x80000000L) // 32 -> 0

      // SRA (shift aritmetico preserva sinal)
      runCase(dut, "SRA", SRA, 0xfffffff0L, 2, 0xfffffffcL) // -16 >> 2 = -4
      runCase(dut, "SRA", SRA, 0x7ffffff0L, 3, 0x0ffffffeL)

      // SLT (comparacao com sinal)
      runCase(dut, "SLT", SLT, -5, 2, 1)
      runCase(dut, "SLT", SLT, 5, -2, 0)

      // SLTU (comparacao sem sinal)
      runCase(dut, "SLTU", SLTU, 1, 2, 1)
      runCase(dut, "SLTU", SLTU, -1, 1, 0) // 0xFFFFFFFF unsigned > 1

      // MUL (multiplicação baixa)
      runCase(dut, "MUL", MUL, 2, 2, 4)
      runCase(dut, "MUL", MUL, 0x00010000L, 0x00010000L, 0)

      // MULH (multiplicação alta com sinal)
      runCase(dut, "MULH", MULH, 0x00010000L, 0x00010000L, 1)
      runCase(dut, "MULH", MULH, -2, 2, 0XffffffffL)

      // MULHU (mulitplicação alta sem sinal)
      runCase(dut, "MULHU", MULHU, 0x00010000L, 0x00010000L, 1)
      runCase(dut, "MULHU", MULHU, 0xffffffffL, 0xffffffffL, 0xfffffffeL)

      // MULHSU
      runCase(dut, "MULHSU", MULHSU, -2, 2, 0xffffffffL)
      runCase(dut, "MULHSU", MULHSU, 0x80000000L, 2, 0xffffffffL)

      println("\n==== TODOS OS TESTES DA ULA PASSARAM ====")
    }
  }
}
