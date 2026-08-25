package riscv.elementosbasicos

import chisel3._
import chisel3.util._

class ControlSignals extends Bundle {
  val regWrite = Bool()
  val aluOp = UInt(4.W)
  val operandASel = UInt(2.W)
  val operandBSel = UInt(1.W)
  val memWrite = Bool()
  val memSize = UInt(2.W)
  val memUnsigned = Bool()
  val branchType = UInt(3.W)
  val writebackSel = UInt(2.W)
  val jump = Bool()
  val jalr = Bool()
  val illegal = Bool()
}

class Controller extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))
    val funct3 = Input(UInt(3.W))
    val funct7 = Input(UInt(7.W))

    val signals = Output(new ControlSignals)
  })

  import RV32I._

  io.signals.regWrite := false.B
  io.signals.aluOp := ALUOp.ADD
  io.signals.operandASel := OperandASel.RS1
  io.signals.operandBSel := OperandBSel.RS2
  io.signals.memWrite := false.B
  io.signals.memSize := MemorySize.WORD
  io.signals.memUnsigned := false.B
  io.signals.branchType := BranchType.NONE
  io.signals.writebackSel := WritebackSel.ALU
  io.signals.jump := false.B
  io.signals.jalr := false.B
  val illegal = WireDefault(true.B)
  io.signals.illegal := illegal

  switch(io.opcode) {
    is(Opcode.OP) {
      illegal := false.B
      io.signals.regWrite := true.B

      when(io.funct7 === "b0000001".U) {
        // --- EXTENSÃO M: MULTIPLICAÇÃO ---
        // funct7 = 0000001, funct3 define a operação
        switch(io.funct3) {
          is("b000".U) { io.signals.aluOp := ALUOp.MUL }      // MUL
          is("b001".U) { io.signals.aluOp := ALUOp.MULH }     // MULH
          is("b010".U) { io.signals.aluOp := ALUOp.MULHSU }   // MULHSU
          is("b011".U) { io.signals.aluOp := ALUOp.MULHU }    // MULHU
        }
      } .otherwise {
        switch(io.funct3) {
          is("b000".U) {
            when(io.funct7 === "b0000000".U) { io.signals.aluOp := ALUOp.ADD }
              .elsewhen(io.funct7 === "b0100000".U) {
                io.signals.aluOp := ALUOp.SUB
              }
              .otherwise { illegal := true.B }
          }
          is("b001".U) {
            io.signals.aluOp := ALUOp.SLL
            illegal := io.funct7 =/= "b0000000".U
          }
          is("b010".U) {
            io.signals.aluOp := ALUOp.SLT
            illegal := io.funct7 =/= "b0000000".U
          }
          is("b011".U) {
            io.signals.aluOp := ALUOp.SLTU
            illegal := io.funct7 =/= "b0000000".U
          }
          is("b100".U) {
            io.signals.aluOp := ALUOp.XOR
            illegal := io.funct7 =/= "b0000000".U
          }
          is("b101".U) {
            when(io.funct7 === "b0000000".U) { io.signals.aluOp := ALUOp.SRL }
              .elsewhen(io.funct7 === "b0100000".U) {
                io.signals.aluOp := ALUOp.SRA
              }
              .otherwise { illegal := true.B }
          }
          is("b110".U) {
            io.signals.aluOp := ALUOp.OR
            illegal := io.funct7 =/= "b0000000".U
          }
          is("b111".U) {
            io.signals.aluOp := ALUOp.AND
            illegal := io.funct7 =/= "b0000000".U
          }
        }
      }
    }

    is(Opcode.OP_IMM) {
      illegal := false.B
      io.signals.regWrite := true.B
      io.signals.operandBSel := OperandBSel.IMM
      switch(io.funct3) {
        is("b000".U) { io.signals.aluOp := ALUOp.ADD }
        is("b001".U) {
          io.signals.aluOp := ALUOp.SLL
          illegal := io.funct7 =/= "b0000000".U
        }
        is("b010".U) { io.signals.aluOp := ALUOp.SLT }
        is("b011".U) { io.signals.aluOp := ALUOp.SLTU }
        is("b100".U) { io.signals.aluOp := ALUOp.XOR }
        is("b101".U) {
          when(io.funct7 === "b0000000".U) { io.signals.aluOp := ALUOp.SRL }
            .elsewhen(io.funct7 === "b0100000".U) {
              io.signals.aluOp := ALUOp.SRA
            }
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
        is("b100".U) {
          io.signals.memSize := MemorySize.BYTE
          io.signals.memUnsigned := true.B
        }
        is("b101".U) {
          io.signals.memSize := MemorySize.HALF
          io.signals.memUnsigned := true.B
        }
      }
      when(
        !(io.funct3 === "b000".U || io.funct3 === "b001".U || io.funct3 === "b010".U ||
          io.funct3 === "b100".U || io.funct3 === "b101".U)
      ) {
        illegal := true.B
      }
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
      when(
        !(io.funct3 === "b000".U || io.funct3 === "b001".U || io.funct3 === "b010".U)
      ) {
        illegal := true.B
      }
    }

    is(Opcode.BRANCH) {
      illegal := false.B
      switch(io.funct3) {
        is("b000".U) {
          io.signals.branchType := BranchType.BEQ
          io.signals.aluOp := ALUOp.SUB
        }
        is("b001".U) {
          io.signals.branchType := BranchType.BNE
          io.signals.aluOp := ALUOp.SUB
        }
        is("b100".U) {
          io.signals.branchType := BranchType.BLT
          io.signals.aluOp := ALUOp.SLT
        }
        is("b101".U) {
          io.signals.branchType := BranchType.BGE
          io.signals.aluOp := ALUOp.SLT
        }
        is("b110".U) {
          io.signals.branchType := BranchType.BLTU
          io.signals.aluOp := ALUOp.SLTU
        }
        is("b111".U) {
          io.signals.branchType := BranchType.BGEU
          io.signals.aluOp := ALUOp.SLTU
        }
      }
      when(
        !(io.funct3 === "b000".U || io.funct3 === "b001".U || io.funct3 === "b100".U ||
          io.funct3 === "b101".U || io.funct3 === "b110".U || io.funct3 === "b111".U)
      ) {
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

  }

  when(illegal) {
    io.signals.regWrite := false.B
    io.signals.memWrite := false.B
    io.signals.jump := false.B
    io.signals.branchType := BranchType.NONE
  }
}
