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
  val writebackSel = UInt(3.W)
  val jump = Bool()
  val jalr = Bool()
  val illegal = Bool()
  val isLR = Bool()
  val isSC = Bool()
  val isAMO = Bool()
  val amoOp = UInt(4.W)
  val amoAq = Bool()
  val amoRl = Bool()
  val csrWrite = Bool()
  val csrRead = Bool()
  val csrOp = UInt(3.W) 
}

class ControllerP3 extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))
    val funct3 = Input(UInt(3.W))
    val funct7 = Input(UInt(7.W))
    val csrAddress = Input(UInt(12.W))
    val rs1        = Input(UInt(5.W))

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
  io.signals.isLR := false.B
  io.signals.isSC := false.B
  io.signals.isAMO := false.B
  io.signals.amoOp := AMOOp.ADD
  io.signals.amoAq := false.B
  io.signals.amoRl := false.B
  val illegal = WireDefault(true.B)
  io.signals.illegal := illegal
  io.signals.csrWrite := false.B
  io.signals.csrRead := false.B
  io.signals.csrOp := 0.U

  val amoFunct5 = io.funct7(6, 2)
  val amoAq = io.funct7(1)  // bit 1 = acquire
  val amoRl = io.funct7(0)  // bit 0 = release

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

    is(Opcode.ATOMIC) {
      // ----------------------------------------------------------
      // Decodificação bruta (não depende de illegal)
      // ----------------------------------------------------------
      val isLR_raw  = WireDefault(false.B)
      val isSC_raw  = WireDefault(false.B)
      val isAMO_raw = WireDefault(false.B)

      switch(amoFunct5) {
        is("b00010".U) {  // LR.W
          isLR_raw := true.B
          io.signals.regWrite := true.B
          io.signals.writebackSel := WritebackSel.MEM
          io.signals.memSize := MemorySize.WORD
          io.signals.memUnsigned := false.B
          io.signals.aluOp := ALUOp.ADD
          io.signals.operandASel := OperandASel.RS1
          io.signals.operandBSel := OperandBSel.IMM
          io.signals.amoAq := amoAq
          io.signals.amoRl := amoRl
        }
        is("b00011".U) {  // SC.W
          isSC_raw := true.B
          io.signals.regWrite := true.B
          io.signals.writebackSel := WritebackSel.ALU
          io.signals.memWrite := true.B
          io.signals.memSize := MemorySize.WORD
          io.signals.aluOp := ALUOp.ADD
          io.signals.operandASel := OperandASel.RS1
          io.signals.operandBSel := OperandBSel.IMM
          io.signals.amoAq := amoAq
          io.signals.amoRl := amoRl
        }
        is("b00001".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.SWAP }
        is("b00000".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.ADD  }
        is("b01100".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.AND  }
        is("b01000".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.OR   }
        is("b00100".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.XOR  }
        is("b10100".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.MAX  }
        is("b10000".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.MIN  }
        is("b10110".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.MAXU }
        is("b10010".U) { isAMO_raw := true.B; io.signals.amoOp := AMOOp.MINU }
      }

      // Cálculo de illegal usa SOMENTE os wires brutos
      when(!(isLR_raw || isSC_raw || isAMO_raw)) { illegal := true.B }

      // Configuração comum dos AMOs
      when(isAMO_raw) {
        io.signals.regWrite := true.B
        io.signals.writebackSel := WritebackSel.MEM
        io.signals.memWrite := true.B
        io.signals.memSize := MemorySize.WORD
        io.signals.memUnsigned := false.B
        io.signals.operandASel := OperandASel.RS1
        io.signals.operandBSel := OperandBSel.RS2
        io.signals.amoAq := amoAq
        io.signals.amoRl := amoRl
        switch(io.signals.amoOp) {
          is(AMOOp.SWAP) { io.signals.aluOp := ALUOp.ADD }
          is(AMOOp.ADD)  { io.signals.aluOp := ALUOp.ADD }
          is(AMOOp.AND)  { io.signals.aluOp := ALUOp.AND }
          is(AMOOp.OR)   { io.signals.aluOp := ALUOp.OR  }
          is(AMOOp.XOR)  { io.signals.aluOp := ALUOp.XOR }
          is(AMOOp.MAX)  { io.signals.aluOp := ALUOp.SLT }
          is(AMOOp.MIN)  { io.signals.aluOp := ALUOp.SLT }
          is(AMOOp.MAXU) { io.signals.aluOp := ALUOp.SLTU }
          is(AMOOp.MINU) { io.signals.aluOp := ALUOp.SLTU }
        }
      }

      // Só agora publica nas saídas (sem realimentar illegal)
      io.signals.isLR  := isLR_raw
      io.signals.isSC  := isSC_raw
      io.signals.isAMO := isAMO_raw
    }

    is(Opcode.SYSTEM) {
      illegal := false.B
      
      when(io.funct3 === "b000".U) {
        // ============================================================
        // INSTRUÇÕES PRIVILEGIADAS (ECALL, EBREAK, MRET, SRET, WFI)
        // ============================================================
        // Não fazem writeback. O CLINT detecta pelo valor completo da instrução.
        // Não ativam csrWrite (o CLINT cuida da trap).
        io.signals.regWrite := false.B
        io.signals.csrWrite := false.B
        io.signals.csrRead := false.B
        
      } .elsewhen(io.funct3 === "b100".U) {
        // WFI tratado como NOP
        illegal := false.B
        io.signals.regWrite := false.B
      } .otherwise {
        // ============================================================
        // INSTRUÇÕES CSR (CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI)
        // ============================================================
        io.signals.regWrite := true.B        // Escreve o valor lido em rd
        io.signals.csrWrite := true.B        // Escreve no CSR
        io.signals.csrRead := true.B         // Lê do CSR
        io.signals.writebackSel := WritebackSel.CSR  // Valor lido do CSR → rd
        
        switch(io.funct3) {
          is("b001".U) { io.signals.csrOp := 1.U }  // CSRRW
          is("b010".U) { io.signals.csrOp := 2.U }  // CSRRS
          is("b011".U) { io.signals.csrOp := 3.U }  // CSRRC
          is("b101".U) { io.signals.csrOp := 5.U }  // CSRRWI
          is("b110".U) { io.signals.csrOp := 6.U }  // CSRRSI
          is("b111".U) { io.signals.csrOp := 7.U }  // CSRRCI
        }

        val csrAddrValido = MuxLookup(io.csrAddress, false.B)(Seq(
          CSRAddress.mstatus  -> true.B, CSRAddress.misa    -> true.B,
          CSRAddress.mie      -> true.B, CSRAddress.mtvec   -> true.B,
          CSRAddress.mscratch -> true.B, CSRAddress.mepc    -> true.B,
          CSRAddress.mcause   -> true.B, CSRAddress.mtval   -> true.B,
          CSRAddress.mip      -> true.B, CSRAddress.medeleg -> true.B,
          CSRAddress.mideleg  -> true.B,
          CSRAddress.sstatus  -> true.B, CSRAddress.sie     -> true.B,
          CSRAddress.stvec    -> true.B, CSRAddress.sscratch-> true.B,
          CSRAddress.sepc     -> true.B, CSRAddress.scause  -> true.B,
          CSRAddress.stval    -> true.B, CSRAddress.satp    -> true.B,
          CSRAddress.mvendorid-> true.B, CSRAddress.marchid -> true.B,
          CSRAddress.mimpid   -> true.B, CSRAddress.mhartid -> true.B
        ))

        val csrAddrReadOnly = io.csrAddress(11, 10) === "b11".U
        val csrSrcZero      = io.rs1 === 0.U
        val csrOpIsSetClear = io.signals.csrOp === 2.U || io.signals.csrOp === 3.U ||
                              io.signals.csrOp === 6.U || io.signals.csrOp === 7.U
        val csrWriteEfetiva = (io.signals.csrOp === 1.U || io.signals.csrOp === 5.U) ||
                              (csrOpIsSetClear && !csrSrcZero)

        when(!csrAddrValido) { illegal := true.B }
        when(csrAddrReadOnly && csrWriteEfetiva) { illegal := true.B }

        val csrFunct3Valido = io.funct3 === "b001".U || io.funct3 === "b010".U ||
                              io.funct3 === "b011".U || io.funct3 === "b101".U ||
                              io.funct3 === "b110".U || io.funct3 === "b111".U

        when(!csrFunct3Valido) {
          illegal := true.B
        }
      }
    }
  }

  when(illegal) {
    io.signals.regWrite    := false.B
    io.signals.memWrite    := false.B
    io.signals.jump        := false.B
    io.signals.branchType  := BranchType.NONE
    io.signals.isLR        := false.B
    io.signals.isSC        := false.B
    io.signals.isAMO       := false.B
    io.signals.csrWrite    := false.B
    io.signals.csrRead     := false.B
  }
}
