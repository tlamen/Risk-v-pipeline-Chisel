package riscv.elementosbasicos

import chisel3._
import chisel3.util._

/** Banco de registradores RV32I (32 registradores x 32 bits).
  *
  * Regras RISC-V atendidas:
  *   - Duas portas de leitura combinacionais (rs1, rs2).
  *   - Uma porta de escrita síncrona (rd, writeData) controlada por regWrite.
  *   - Registrador x0 é imutável e sempre retorna zero.
  */
class RegisterFile extends Module {
  val io = IO(new Bundle {
    val rs1 = Input(UInt(5.W))
    val rs2 = Input(UInt(5.W))
    val rd = Input(UInt(5.W))
    val writeData = Input(UInt(32.W))
    val regWrite = Input(Bool())

    val readData1 = Output(UInt(32.W))
    val readData2 = Output(UInt(32.W))

    val debug_regs = Output(Vec(32, UInt(32.W)))
  })

  // 32 registradores de 32 bits inicializados em zero.
  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  // Escrita síncrona: só escreve se habilitado e rd != x0.
  when(io.regWrite && io.rd =/= 0.U) {
    regs(io.rd) := io.writeData
  }

  // Leitura combinacional com regra de x0 fixo em zero.
  io.readData1 := Mux(io.rs1 === 0.U, 0.U, regs(io.rs1))
  io.readData2 := Mux(io.rs2 === 0.U, 0.U, regs(io.rs2))

  for (i <- 0 until 32) {
    io.debug_regs(i) := regs(i)
  }
}
