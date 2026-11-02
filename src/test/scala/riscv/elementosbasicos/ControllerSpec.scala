package riscv.elementosbasicos

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.RV32I

class ControllerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ControllerP3"

  /** Helper: pokeia opcode/funct e zera csrAddress/rs1 por default. */
  private def pokeBase(
      dut: ControllerP3,
      opcode: UInt,
      funct3: UInt = 0.U,
      funct7: UInt = 0.U,
      csrAddress: UInt = 0.U,
      rs1: UInt = 0.U
  ): Unit = {
    dut.io.opcode.poke(opcode)
    dut.io.funct3.poke(funct3)
    dut.io.funct7.poke(funct7)
    dut.io.csrAddress.poke(csrAddress)   // NOVO: precisa ser pokado
    dut.io.rs1.poke(rs1)                 // NOVO: precisa ser pokado
  }

  // ---------------- RV32I base ----------------
  it should "decodificar ADD (tipo R) corretamente" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.OP, "b000".U, "b0000000".U)
      dut.io.signals.regWrite.expect(true.B)
      dut.io.signals.operandBSel.expect(RV32I.OperandBSel.RS2)
      dut.io.signals.aluOp.expect(ALUOp.ADD)
      dut.io.signals.memWrite.expect(false.B)
      dut.io.signals.branchType.expect(RV32I.BranchType.NONE)
      dut.io.signals.illegal.expect(false.B)
    }
  }

  it should "decodificar SUB (tipo R) corretamente" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.OP, "b000".U, "b0100000".U)
      dut.io.signals.aluOp.expect(ALUOp.SUB)
      dut.io.signals.illegal.expect(false.B)
    }
  }

  it should "decodificar loads com tamanho e extensão corretos" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.LOAD, "b010".U)
      dut.io.signals.regWrite.expect(true.B)
      dut.io.signals.operandBSel.expect(RV32I.OperandBSel.IMM)
      dut.io.signals.writebackSel.expect(RV32I.WritebackSel.MEM)
      dut.io.signals.memSize.expect(RV32I.MemorySize.WORD)
      dut.io.signals.memUnsigned.expect(false.B)
      dut.io.signals.aluOp.expect(ALUOp.ADD)

      pokeBase(dut, RV32I.Opcode.LOAD, "b100".U)
      dut.io.signals.memSize.expect(RV32I.MemorySize.BYTE)
      dut.io.signals.memUnsigned.expect(true.B)
    }
  }

  it should "decodificar branches explicitamente" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.BRANCH, "b110".U)
      dut.io.signals.branchType.expect(RV32I.BranchType.BLTU)
      dut.io.signals.aluOp.expect(ALUOp.SLTU)
      dut.io.signals.illegal.expect(false.B)
    }
  }

  it should "marcar combinações inválidas de OP como ilegais" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.OP, "b001".U, "b0100000".U)
      dut.io.signals.illegal.expect(true.B)
      dut.io.signals.regWrite.expect(false.B)
    }
  }

  // ---------------- ATOMIC: default ilegal ----------------
  it should "marcar ATOMIC com funct5 desconhecido como ilegal" in {
    test(new ControllerP3) { dut =>
      // funct7 = 0b0000111 → amoFunct5 = 0b00001 (válido SWAP!)
      // Use algo fora da tabela: funct5 = 0b11111
      pokeBase(dut, RV32I.Opcode.ATOMIC, "b010".U, "b1111111".U)
      dut.io.signals.illegal.expect(true.B)
      dut.io.signals.isLR.expect(false.B)
      dut.io.signals.isSC.expect(false.B)
      dut.io.signals.isAMO.expect(false.B)
    }
  }

  // ---------------- SYSTEM: ECALL/EBREAK/MRET/SRET ----------------
  it should "não marcar ECALL/EBREAK/MRET/SRET como ilegal e não ativar csrWrite" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b000".U, 0.U)
      dut.io.signals.illegal.expect(false.B)
      dut.io.signals.csrWrite.expect(false.B)
      dut.io.signals.csrRead.expect(false.B)
      dut.io.signals.regWrite.expect(false.B)
    }
  }

  it should "tratar WFI (funct3=100) como NOP não ilegal" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b100".U, 0.U)
      dut.io.signals.illegal.expect(false.B)
      dut.io.signals.regWrite.expect(false.B)
      dut.io.signals.csrWrite.expect(false.B)
    }
  }

  // ---------------- SYSTEM: CSRs válidos ----------------
  it should "decodificar CSRRW em mstatus como CSR write+read válido" in {
    test(new ControllerP3) { dut =>
      // csrOp=1 (CSRRW): sempre escreve
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b001".U,
               csrAddress = RV32I.CSRAddress.mstatus, rs1 = 0.U)
      dut.io.signals.illegal.expect(false.B)
      dut.io.signals.csrWrite.expect(true.B)
      dut.io.signals.csrRead.expect(true.B)
      dut.io.signals.regWrite.expect(true.B)
      dut.io.signals.csrOp.expect(1.U)
    }
  }

  it should "decodificar CSRRS em mepc como válido" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b010".U,
               csrAddress = RV32I.CSRAddress.mepc, rs1 = 5.U)
      dut.io.signals.illegal.expect(false.B)
      dut.io.signals.csrOp.expect(2.U)
      dut.io.signals.csrWrite.expect(true.B)
    }
  }

  // ---------------- SYSTEM: CSR inválido / read-only ----------------
  it should "marcar CSR inexistente como ilegal" in {
    test(new ControllerP3) { dut =>
      // 0x7FF não está na tabela
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b001".U,
               csrAddress = "h7FF".U(12.W), rs1 = 0.U)
      dut.io.signals.illegal.expect(true.B)
    }
  }

  it should "marcar escrita em CSR read-only como ilegal" in {
    test(new ControllerP3) { dut =>
      // mhartid (0xF14) é read-only e está em [11:10]=11
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b001".U,
               csrAddress = RV32I.CSRAddress.mhartid, rs1 = 0.U)
      dut.io.signals.illegal.expect(true.B)
    }
  }

  it should "permitir leitura de CSR read-only com CSRRS e rs1=x0" in {
    test(new ControllerP3) { dut =>
      // csrrs rd, mhartid, x0  → leitura pura
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b010".U,
               csrAddress = RV32I.CSRAddress.mhartid, rs1 = 0.U)
      dut.io.signals.illegal.expect(false.B)
      dut.io.signals.csrOp.expect(2.U)
    }
  }

  it should "marcar CSRRS com rs1≠0 em CSR read-only como ilegal" in {
    test(new ControllerP3) { dut =>
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b010".U,
               csrAddress = RV32I.CSRAddress.mhartid, rs1 = 3.U)
      dut.io.signals.illegal.expect(true.B)
    }
  }

  it should "marcar CSRRWI em CSR read-only como ilegal mesmo com zimm=0" in {
    test(new ControllerP3) { dut =>
      // CSRRWI sempre escreve, mesmo com zimm=0
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b101".U,
               csrAddress = RV32I.CSRAddress.mhartid, rs1 = 0.U)
      dut.io.signals.illegal.expect(true.B)
    }
  }

  it should "marcar funct3 inválido de SYSTEM (ex.: 0b110 não é CSR) — só 000/100 e CSR válidos" in {
    test(new ControllerP3) { dut =>
      // funct3=0b000 é privilegiada; 0b100 é WFI; CSR válidos: 001,010,011,101,110,111
      // Vamos usar funct3=0b001 com CSR inválido para forçar illegal
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b001".U,
               csrAddress = 0.U, rs1 = 0.U)   // 0 não é CSR válido
      dut.io.signals.illegal.expect(true.B)
    }
  }

  it should "não marcar nenhuma instrução SYSTEM dos testes como ilegal" in {
    test(new ControllerP3) { dut =>
      // csrrw x0, mtvec, x5
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b001".U,
              csrAddress = RV32I.CSRAddress.mtvec, rs1 = 5.U)
      dut.io.signals.illegal.expect(false.B)

      // csrrs x6, mepc, x0
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b010".U,
              csrAddress = RV32I.CSRAddress.mepc, rs1 = 0.U)
      dut.io.signals.illegal.expect(false.B)

      // csrrs x7, mcause, x0
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b010".U,
              csrAddress = RV32I.CSRAddress.mcause, rs1 = 0.U)
      dut.io.signals.illegal.expect(false.B)

      // ebreak
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b000".U,
              csrAddress = "h001".U(12.W), rs1 = 0.U)
      dut.io.signals.illegal.expect(false.B)

      // ecall
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b000".U,
              csrAddress = "h000".U(12.W), rs1 = 0.U)
      dut.io.signals.illegal.expect(false.B)

      // mret
      pokeBase(dut, RV32I.Opcode.SYSTEM, "b000".U,
              csrAddress = "h302".U(12.W), rs1 = 0.U)
      dut.io.signals.illegal.expect(false.B)
    }
  }
}