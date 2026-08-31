package riscv.elementosbasicos

import chisel3._

object RV32I {
  object Opcode {
    val LOAD = "b0000011".U(7.W)
    val OP_IMM = "b0010011".U(7.W)
    val AUIPC = "b0010111".U(7.W)
    val STORE = "b0100011".U(7.W)
    val OP = "b0110011".U(7.W)
    val LUI = "b0110111".U(7.W)
    val BRANCH = "b1100011".U(7.W)
    val JALR = "b1100111".U(7.W)
    val JAL = "b1101111".U(7.W)
    val ATOMIC = "b0101111".U(7.W)
  }

  object BranchType {
    val NONE = 0.U(3.W)
    val BEQ = 1.U(3.W)
    val BNE = 2.U(3.W)
    val BLT = 3.U(3.W)
    val BGE = 4.U(3.W)
    val BLTU = 5.U(3.W)
    val BGEU = 6.U(3.W)
  }

  object MemorySize {
    val BYTE = 0.U(2.W)
    val HALF = 1.U(2.W)
    val WORD = 2.U(2.W)
  }

  object OperandASel {
    val RS1 = 0.U(2.W)
    val PC = 1.U(2.W)
    val ZERO = 2.U(2.W)
  }

  object OperandBSel {
    val RS2 = 0.U(1.W)
    val IMM = 1.U(1.W)
  }

  object WritebackSel {
    val ALU = 0.U(2.W)
    val MEM = 1.U(2.W)
    val PC4 = 2.U(2.W)
    val IMM = 3.U(2.W)
  }

  object AMOOp {
    val SWAP  = 0.U(4.W)
    val ADD   = 1.U(4.W)
    val AND   = 2.U(4.W)
    val OR    = 3.U(4.W)
    val XOR   = 4.U(4.W)
    val MAX   = 5.U(4.W)
    val MIN   = 6.U(4.W)
    val MAXU  = 7.U(4.W)
    val MINU  = 8.U(4.W)
  }

  // Funct7 para AMOs (5 bits superiores)
  object AMOFunct5 {
    val LR     = "b00010".U(5.W)   // 0x02
    val SC     = "b00011".U(5.W)   // 0x03
    val SWAP   = "b00001".U(5.W)   // 0x01
    val ADD    = "b00000".U(5.W)   // 0x00
    val AND    = "b01100".U(5.W)   // 0x0C
    val OR     = "b01000".U(5.W)   // 0x08
    val XOR    = "b00100".U(5.W)   // 0x04
    val MAX    = "b10100".U(5.W)   // 0x14
    val MIN    = "b10000".U(5.W)   // 0x10
    val MAXU   = "b10110".U(5.W)   // 0x16
    val MINU   = "b10010".U(5.W)   // 0x12
  }
}
