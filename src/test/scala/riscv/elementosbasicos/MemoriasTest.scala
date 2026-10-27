package riscv.elementosbasicos

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MemoriasTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "DataMemory"

  it should "escrever e ler uma palavra de 32 bits" in {
    test(new DataMemory(depthWords = 256)) { dut =>
      dut.io.address.poke(8.U(32.W)) // palavra de indice 2
      dut.io.writeData.poke("hDEADBEEF".U(32.W))
      dut.io.memSize.poke(RV32I.MemorySize.WORD)
      dut.io.unsignedLoad.poke(false.B)
      dut.io.writeEnable.poke(true.B)
      dut.clock.step(1)

      dut.io.writeEnable.poke(false.B)
      dut.io.address.poke(8.U(32.W))
      dut.clock.step(1)

      dut.io.readData.expect("hDEADBEEF".U(32.W))
    }
  }

  it should "aplicar tamanho e extensao de sinal em byte e halfword" in {
    test(new DataMemory(depthWords = 256)) { dut =>
      dut.io.address.poke(0.U(32.W))
      dut.io.writeData.poke("h00000080".U(32.W))
      dut.io.memSize.poke(RV32I.MemorySize.BYTE)
      dut.io.unsignedLoad.poke(false.B)
      dut.io.writeEnable.poke(true.B)
      dut.clock.step(1)

      dut.io.writeEnable.poke(false.B)
      dut.io.readData.expect("hFFFFFF80".U(32.W))

      dut.io.unsignedLoad.poke(true.B)
      dut.io.readData.expect("h00000080".U(32.W))

      dut.io.address.poke(4.U(32.W))
      dut.io.writeData.poke("h00008001".U(32.W))
      dut.io.memSize.poke(RV32I.MemorySize.HALF)
      dut.io.unsignedLoad.poke(false.B)
      dut.io.writeEnable.poke(true.B)
      dut.clock.step(1)

      dut.io.writeEnable.poke(false.B)
      dut.io.readData.expect("hFFFF8001".U(32.W))

      dut.io.unsignedLoad.poke(true.B)
      dut.io.readData.expect("h00008001".U(32.W))
    }
  }

  behavior of "InstructionMemory"

  it should "ler instrucoes iniciais pelo endereco em bytes" in {
    test(
      new InstructionMemory(
        depthWords = 16,
        initialData = Seq(0x13L, 0x00100093L, 0x00200113L)
      )
    ) { dut =>
      dut.io.address.poke(0.U(32.W))
      dut.clock.step(1)
      dut.io.readData.expect("h00000013".U(32.W))

      dut.io.address.poke(4.U(32.W))
      dut.clock.step(1)
      dut.io.readData.expect("h00100093".U(32.W))

      dut.io.address.poke(8.U(32.W))
      dut.clock.step(1)
      dut.io.readData.expect("h00200113".U(32.W))

      dut.io.address.poke(12.U(32.W)) // palavra de indice 3
      dut.clock.step(1)
      dut.io.readData.expect(0.U(32.W))
    }
  }
}
