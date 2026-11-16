package riscv.elementosbasicos

import chisel3._
import chisel3.util._

/** Gerador de imediatos para instruções RV32I.
  *
  * Recebe a instrução de 32 bits e devolve o imediato de 32 bits no formato
  * correto (com extensão de sinal quando aplicável).
  *
  */
class ImmGen extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W))
    val imm   = Output(UInt(32.W))
  })

  import RV32I.Opcode

  // Opcode: 7 bits menos significativos da instrução.
  val opcode = io.instr(6, 0)

  // Extração dos imediatos brutos.
  val immI = io.instr(31, 20)                                    // I
  val immS = Cat(io.instr(31, 25), io.instr(11, 7))              // S
  val immB = Cat(io.instr(31), io.instr(7),                      // B
                 io.instr(30, 25), io.instr(11, 8), 0.U(1.W))
  val immU = Cat(io.instr(31, 12), 0.U(12.W))                    // U
  val immJ = Cat(io.instr(31),                                   // J
                 io.instr(19, 12), io.instr(20),
                 io.instr(30, 21), 0.U(1.W))

  val signExtI = Cat(Fill(20, immI(11)), immI)
  val signExtS = Cat(Fill(20, immS(11)), immS)
  val signExtB = Cat(Fill(19, immB(12)), immB)
  val signExtJ = Cat(Fill(11, immJ(20)), immJ)

  io.imm := 0.U(32.W)

  switch(opcode) {
    // Tipo I
    is(Opcode.LOAD)   { io.imm := signExtI }
    is(Opcode.OP_IMM) { io.imm := signExtI }
    is(Opcode.JALR)   { io.imm := signExtI }

    // Tipo S
    is(Opcode.STORE)  { io.imm := signExtS }

    // Tipo B
    is(Opcode.BRANCH) { io.imm := signExtB }

    // Tipo U
    is(Opcode.LUI)    { io.imm := immU }
    is(Opcode.AUIPC)  { io.imm := immU }

    // Tipo J
    is(Opcode.JAL)    { io.imm := signExtJ }
  }
}