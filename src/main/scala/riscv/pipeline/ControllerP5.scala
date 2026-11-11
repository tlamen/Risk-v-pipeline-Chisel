package riscv.elementosbasicos

import chisel3._
import chisel3.util._

class ControlSignalsP5 extends Bundle {
  val regWrite     = Bool()
  val aluOp        = UInt(4.W)
  val operandASel  = UInt(2.W)
  val operandBSel  = UInt(1.W)
  val memWrite     = Bool()
  val memSize      = UInt(2.W)
  val memUnsigned  = Bool()
  val branchType   = UInt(3.W)
  val writebackSel = UInt(3.W)
  val jump         = Bool()
  val jalr         = Bool()
  val illegal      = Bool()
  val csrWrite     = Bool()
  val csrRead      = Bool()
  val csrOp        = UInt(3.W)
}

class ControllerP5 extends Module {
  val io = IO(new Bundle {
    val opcode     = Input(UInt(7.W))
    val funct3     = Input(UInt(3.W))
    val funct7     = Input(UInt(7.W))
    val csrAddress = Input(UInt(12.W))
    val rs1        = Input(UInt(5.W))
    val signals    = Output(new ControlSignalsP5)
  })

  import RV32I._

  // ---------- defaults ----------
  io.signals.regWrite     := false.B
  io.signals.aluOp        := ALUOp.ADD
  io.signals.operandASel  := OperandASel.RS1
  io.signals.operandBSel  := OperandBSel.RS2
  io.signals.memWrite     := false.B
  io.signals.memSize      := MemorySize.WORD
  io.signals.memUnsigned  := false.B
  io.signals.branchType   := BranchType.NONE
  io.signals.writebackSel := WritebackSel.ALU
  io.signals.jump         := false.B
  io.signals.jalr         := false.B
  io.signals.csrWrite     := false.B
  io.signals.csrRead      := false.B
  io.signals.csrOp        := 0.U

  val illegal = WireDefault(true.B)
  io.signals.illegal := illegal

  switch(io.opcode) {
    is(Opcode.OP) {
      illegal := false.B
      io.signals.regWrite := true.B
      when(io.funct7 === "b0000001".U) {
        // Extensão M: multiplicação
        switch(io.funct3) {
          is("b000".U) { io.signals.aluOp := ALUOp.MUL }
          is("b001".U) { io.signals.aluOp := ALUOp.MULH }
          is("b010".U) { io.signals.aluOp := ALUOp.MULHSU }
          is("b011".U) { io.signals.aluOp := ALUOp.MULHU }
        }
      } .otherwise {
        switch(io.funct3) {
          is("b000".U) {
            when(io.funct7 === "b0000000".U) { io.signals.aluOp := ALUOp.ADD }
              .elsewhen(io.funct7 === "b0100000".U) { io.signals.aluOp := ALUOp.SUB }
              .otherwise { illegal := true.B }
          }
          is("b001".U) { io.signals.aluOp := ALUOp.SLL;  illegal := io.funct7 =/= "b0000000".U }
          is("b010".U) { io.signals.aluOp := ALUOp.SLT;  illegal := io.funct7 =/= "b0000000".U }
          is("b011".U) { io.signals.aluOp := ALUOp.SLTU; illegal := io.funct7 =/= "b0000000".U }
          is("b100".U) { io.signals.aluOp := ALUOp.XOR;  illegal := io.funct7 =/= "b0000000".U }
          is("b101".U) {
            when(io.funct7 === "b0000000".U) { io.signals.aluOp := ALUOp.SRL }
              .elsewhen(io.funct7 === "b0100000".U) { io.signals.aluOp := ALUOp.SRA }
              .otherwise { illegal := true.B }
          }
          is("b110".U) { io.signals.aluOp := ALUOp.OR;   illegal := io.funct7 =/= "b0000000".U }
          is("b111".U) { io.signals.aluOp := ALUOp.AND;  illegal := io.funct7 =/= "b0000000".U }
        }
      }
    }

    is(Opcode.OP_IMM) {
      illegal := false.B
      io.signals.regWrite := true.B
      io.signals.operandBSel := OperandBSel.IMM
      switch(io.funct3) {
        is("b000".U) { io.signals.aluOp := ALUOp.ADD }
        is("b001".U) { io.signals.aluOp := ALUOp.SLL;  illegal := io.funct7 =/= "b0000000".U }
        is("b010".U) { io.signals.aluOp := ALUOp.SLT }
        is("b011".U) { io.signals.aluOp := ALUOp.SLTU }
        is("b100".U) { io.signals.aluOp := ALUOp.XOR }
        is("b101".U) {
          when(io.funct7 === "b0000000".U) { io.signals.aluOp := ALUOp.SRL }
            .elsewhen(io.funct7 === "b0100000".U) { io.signals.aluOp := ALUOp.SRA }
            .otherwise { illegal := true.B }
        }
        is("b110".U) { io.signals.aluOp := ALUOp.OR }
        is("b111".U) { io.signals.aluOp := ALUOp.AND }
      }
    }

    is(Opcode.LOAD) {
      illegal := false.B
      io.signals.regWrite := true.B
      io.signals.operandBSel := OperandBSel.IMM
      io.signals.writebackSel := WritebackSel.MEM
      io.signals.aluOp := ALUOp.ADD
      switch(io.funct3) {
        is("b000".U) { io.signals.memSize := MemorySize.BYTE }
        is("b001".U) { io.signals.memSize := MemorySize.HALF }
        is("b010".U) { io.signals.memSize := MemorySize.WORD }
        is("b100".U) { io.signals.memSize := MemorySize.BYTE; io.signals.memUnsigned := true.B }
        is("b101".U) { io.signals.memSize := MemorySize.HALF; io.signals.memUnsigned := true.B }
      }
      when(!(io.funct3 === "b000".U || io.funct3 === "b001".U || io.funct3 === "b010".U ||
             io.funct3 === "b100".U || io.funct3 === "b101".U)) { illegal := true.B }
    }

    is(Opcode.STORE) {
      illegal := false.B
      io.signals.operandBSel := OperandBSel.IMM
      io.signals.memWrite := true.B
      io.signals.aluOp := ALUOp.ADD
      switch(io.funct3) {
        is("b000".U) { io.signals.memSize := MemorySize.BYTE }
        is("b001".U) { io.signals.memSize := MemorySize.HALF }
        is("b010".U) { io.signals.memSize := MemorySize.WORD }
      }
      when(!(io.funct3 === "b000".U || io.funct3 === "b001".U || io.funct3 === "b010".U)) {
        illegal := true.B
      }
    }

    is(Opcode.BRANCH) {
      illegal := false.B
      switch(io.funct3) {
        is("b000".U) { io.signals.branchType := BranchType.BEQ;  io.signals.aluOp := ALUOp.SUB }
        is("b001".U) { io.signals.branchType := BranchType.BNE;  io.signals.aluOp := ALUOp.SUB }
        is("b100".U) { io.signals.branchType := BranchType.BLT;  io.signals.aluOp := ALUOp.SLT }
        is("b101".U) { io.signals.branchType := BranchType.BGE;  io.signals.aluOp := ALUOp.SLT }
        is("b110".U) { io.signals.branchType := BranchType.BLTU; io.signals.aluOp := ALUOp.SLTU }
        is("b111".U) { io.signals.branchType := BranchType.BGEU; io.signals.aluOp := ALUOp.SLTU }
      }
      when(!(io.funct3 === "b000".U || io.funct3 === "b001".U || io.funct3 === "b100".U ||
             io.funct3 === "b101".U || io.funct3 === "b110".U || io.funct3 === "b111".U)) {
        illegal := true.B
      }
    }

    is(Opcode.LUI) {
      illegal := false.B
      io.signals.regWrite := true.B
      io.signals.operandASel := OperandASel.ZERO
      io.signals.operandBSel := OperandBSel.IMM
      io.signals.writebackSel := WritebackSel.IMM
    }

    is(Opcode.AUIPC) {
      illegal := false.B
      io.signals.regWrite := true.B
      io.signals.operandASel := OperandASel.PC
      io.signals.operandBSel := OperandBSel.IMM
      io.signals.aluOp := ALUOp.ADD
    }

    is(Opcode.JAL) {
      illegal := false.B
      io.signals.regWrite := true.B
      io.signals.jump := true.B
      io.signals.writebackSel := WritebackSel.PC4
    }

    is(Opcode.JALR) {
      illegal := false.B
      io.signals.regWrite := true.B
      io.signals.jump := true.B
      io.signals.jalr := true.B
      io.signals.operandBSel := OperandBSel.IMM
      io.signals.writebackSel := WritebackSel.PC4
      io.signals.aluOp := ALUOp.ADD
      illegal := io.funct3 =/= "b000".U
    }

    is(Opcode.SYSTEM) {
      illegal := false.B
      when(io.funct3 === "b000".U) {
        // ECALL / EBREAK / MRET / SRET: detectados pelo valor da instrução
        io.signals.regWrite := false.B
      } .elsewhen(io.funct3 === "b100".U) {
        // WFI: NOP
        io.signals.regWrite := false.B
      } .otherwise {
        // CSRs
        io.signals.regWrite := true.B
        io.signals.csrWrite := true.B
        io.signals.csrRead  := true.B
        io.signals.writebackSel := WritebackSel.CSR
        switch(io.funct3) {
          is("b001".U) { io.signals.csrOp := 1.U }  // CSRRW
          is("b010".U) { io.signals.csrOp := 2.U }  // CSRRS
          is("b011".U) { io.signals.csrOp := 3.U }  // CSRRC
          is("b101".U) { io.signals.csrOp := 5.U }  // CSRRWI
          is("b110".U) { io.signals.csrOp := 6.U }  // CSRRSI
          is("b111".U) { io.signals.csrOp := 7.U }  // CSRRCI
        }
        val csrAddrValido = MuxLookup(io.csrAddress, false.B)(Seq(
          CSRAddress.mstatus  -> true.B, CSRAddress.misa     -> true.B,
          CSRAddress.mie      -> true.B, CSRAddress.mtvec    -> true.B,
          CSRAddress.mscratch -> true.B, CSRAddress.mepc     -> true.B,
          CSRAddress.mcause   -> true.B, CSRAddress.mtval    -> true.B,
          CSRAddress.mip      -> true.B, CSRAddress.medeleg  -> true.B,
          CSRAddress.mideleg  -> true.B,
          CSRAddress.sstatus  -> true.B, CSRAddress.sie      -> true.B,
          CSRAddress.stvec    -> true.B, CSRAddress.sscratch -> true.B,
          CSRAddress.sepc     -> true.B, CSRAddress.scause   -> true.B,
          CSRAddress.stval    -> true.B, CSRAddress.satp     -> true.B,
          CSRAddress.mvendorid-> true.B, CSRAddress.marchid  -> true.B,
          CSRAddress.mimpid   -> true.B, CSRAddress.mhartid  -> true.B
        ))
        val csrAddrRO = io.csrAddress(11, 10) === "b11".U
        val csrOpSetClr = io.signals.csrOp === 2.U || io.signals.csrOp === 3.U ||
                          io.signals.csrOp === 6.U || io.signals.csrOp === 7.U
        val csrWriteEfetiva = (io.signals.csrOp === 1.U || io.signals.csrOp === 5.U) ||
                              (csrOpSetClr && io.rs1 =/= 0.U)
        when(!csrAddrValido) { illegal := true.B }
        when(csrAddrRO && csrWriteEfetiva) { illegal := true.B }
      }
    }
  }

  when(illegal) {
    io.signals.regWrite := false.B
    io.signals.memWrite := false.B
    io.signals.jump := false.B
    io.signals.branchType := BranchType.NONE
    io.signals.csrWrite := false.B
    io.signals.csrRead := false.B
  }
}