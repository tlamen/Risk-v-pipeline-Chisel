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
    val SYSTEM  = "b1110011".U(7.W)
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
    val ALU = 0.U(3.W)
    val MEM = 1.U(3.W)
    val PC4 = 2.U(3.W)
    val IMM = 3.U(3.W)
    val CSR = 4.U(3.W)
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

  object PrivilegeMode {
    val U = 0.U(2.W)  // User
    val S = 1.U(2.W)  // Supervisor
    val M = 3.U(2.W)  // Machine
  }

  object CSRAddress {
    // ============================================================
    // M-MODE CSRs
    // ============================================================
    val mstatus  = 0x300.U(12.W)
    val misa     = 0x301.U(12.W)
    val medeleg  = 0x302.U(12.W)  // NOVO: Exception Delegation
    val mideleg  = 0x303.U(12.W)  // NOVO: Interrupt Delegation
    val mie      = 0x304.U(12.W)
    val mtvec    = 0x305.U(12.W)
    val mscratch = 0x340.U(12.W)
    val mepc     = 0x341.U(12.W)
    val mcause   = 0x342.U(12.W)
    val mtval    = 0x343.U(12.W)
    val mip      = 0x344.U(12.W)
    
    // ============================================================
    // S-MODE CSRs (NOVOS)
    // ============================================================
    val sstatus  = 0x100.U(12.W)  // Supervisor Status
    val sie      = 0x104.U(12.W)  // Supervisor Interrupt Enable
    val stvec    = 0x105.U(12.W)  // Supervisor Trap Vector
    val sscratch = 0x140.U(12.W)  // Supervisor Scratch
    val sepc     = 0x141.U(12.W)  // Supervisor Exception PC
    val scause   = 0x142.U(12.W)  // Supervisor Cause
    val stval    = 0x143.U(12.W)  // Supervisor Trap Value
    val sip      = 0x144.U(12.W)  // Supervisor Interrupt Pending
    val satp     = 0x180.U(12.W)  // Supervisor Address Translation (MMU!)
    
    // ============================================================
    // M-MODE INFORMATION
    // ============================================================
    val mvendorid = 0xF11.U(12.W)
    val marchid   = 0xF12.U(12.W)
    val mimpid    = 0xF13.U(12.W)
    val mhartid   = 0xF14.U(12.W)
  }

  // ECALL: 0x00000073
  //   funct7=0x00, rs2=0x00, rs1=0x00, funct3=0x0, rd=0x00, opcode=0x73
  val ECALL  = "h00000073".U(32.W)
  
  // EBREAK: 0x00100073
  //   funct7=0x00, rs2=0x01, rs1=0x00, funct3=0x0, rd=0x00, opcode=0x73
  val EBREAK = "h00100073".U(32.W)
  
  // MRET: 0x30200073
  //   funct7=0x18, rs2=0x02, rs1=0x00, funct3=0x0, rd=0x00, opcode=0x73
  val MRET   = "h30200073".U(32.W)
  
  // SRET: 0x10200073
  //   funct7=0x08, rs2=0x02, rs1=0x00, funct3=0x0, rd=0x00, opcode=0x73
  val SRET   = "h10200073".U(32.W)
  
  // WFI: 0x10500073
  val WFI    = "h10500073".U(32.W)
  
  // Funct3 para instruções CSR
  object CSRFunct3 {
    val CSRRW  = "b001".U(3.W)
    val CSRRS  = "b010".U(3.W)
    val CSRRC  = "b011".U(3.W)
    val CSRRWI = "b101".U(3.W)
    val CSRRSI = "b110".U(3.W)
    val CSRRCI = "b111".U(3.W)
  }
  
  // ============================================================
  // SYSTEM INSTRUCTION TYPES
  // ============================================================
  object SystemInstr {
    val NONE   = 0.U(3.W)
    val ECALL  = 1.U(3.W)
    val EBREAK = 2.U(3.W)
    val MRET   = 3.U(3.W)
    val SRET   = 4.U(3.W)
    val WFI    = 5.U(3.W)
    val CSRRW  = 6.U(3.W)
    val CSRRS  = 7.U(3.W)
  }
}
