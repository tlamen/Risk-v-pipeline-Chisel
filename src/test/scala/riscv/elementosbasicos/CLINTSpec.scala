package riscv.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.RV32I
import riscv.elementosbasicos.RV32I._

class CLINTSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CLINT"

  // ============================================================
  // HELPERS
  // ============================================================

  /** Cria um mstatus inicial com MIE habilitado */
  private def mstatusComMIE(mie: Boolean): BigInt = {
    val base = BigInt(0)
    if (mie) base | (1 << 3) else base
  }

  /** Cria um mstatus com MPP = S-Mode (0b01) */
  private def mstatusComMPP_S: BigInt = {
    mstatusComMIE(true) | (1 << 11)
  }

  /** Cria um mstatus com MPP = U-Mode (0b00) */
  private def mstatusComMPP_U: BigInt = {
    mstatusComMIE(true)
  }

  // ============================================================
  // TESTE 1: ECALL sem delegação → trap para M-Mode - OK
  // ============================================================
  it should "gerar trap para M-Mode em ECALL sem delegação" in {
    test(new CLINT) { dut =>
      // Configuração inicial
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mepc.poke(0.U)
      dut.io.mcause.poke(0.U)
      dut.io.mtvec.poke(0x8000.U)   // handler em M-Mode
      dut.io.mie.poke(0.U)
      dut.io.mtval.poke(0.U)
      
      dut.io.sstatus.poke(0.U)
      dut.io.sepc.poke(0.U)
      dut.io.scause.poke(0.U)
      dut.io.stvec.poke(0.U)
      dut.io.sie.poke(0.U)
      dut.io.stval.poke(0.U)
      
      dut.io.medeleg.poke(0.U)      // NÃO delega para S-Mode
      dut.io.mideleg.poke(0.U)
      dut.io.current_priv.poke(PrivilegeMode.M)
      
      dut.io.pc.poke(0x1000.U)
      dut.io.instr.poke(RV32I.ECALL)
      dut.io.valid.poke(true.B)
      dut.io.interrupt_flag.poke(InterruptCode.None)
      
      dut.clock.step(1)
      
      // Verificações
      dut.io.trap_assert.expect(true.B)
      dut.io.trap_to_smode.expect(false.B)
      dut.io.trap_address.expect(0x8000.U)
      dut.io.direct_write_enable.expect(true.B)
      
      // mcause = 11 (ECALL from M-Mode), bit 31 = 0 (exceção)
      dut.io.mcause_write_data.expect(11.U)
      
      // mepc = PC (não PC+4 para exceções)
      dut.io.mepc_write_data.expect(0x1000.U)
      
      // mstatus: MPIE ← MIE (1), MIE ← 0, MPP ← 3 (M-Mode)
      val mstatus_esperado = mstatusComMIE(false) | (1 << 7) | (3 << 11)
      dut.io.mstatus_write_data.expect(mstatus_esperado.U(32.W))
    }
  }

  // ============================================================
  // TESTE 2: Interrupção do Timer → trap para M-Mode
  // ============================================================
  it should "gerar trap para M-Mode em interrupção do Timer" in {
    test(new CLINT) { dut =>
      // Configuração: MIE=1, MTIE=1
      val mstatus = mstatusComMIE(true).U(32.W)
      val mie = (1 << 7).U(32.W)   // MTIE habilitado
      
      dut.io.mstatus.poke(mstatus)
      dut.io.mepc.poke(0.U)
      dut.io.mcause.poke(0.U)
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mie.poke(mie)
      dut.io.mtval.poke(0.U)
      
      dut.io.sstatus.poke(0.U)
      dut.io.sepc.poke(0.U)
      dut.io.scause.poke(0.U)
      dut.io.stvec.poke(0.U)
      dut.io.sie.poke(0.U)
      dut.io.stval.poke(0.U)
      
      dut.io.medeleg.poke(0.U)
      dut.io.mideleg.poke(0.U)
      dut.io.current_priv.poke(PrivilegeMode.M)
      
      dut.io.pc.poke(0x1004.U)
      dut.io.instr.poke(0.U)        // não importa
      dut.io.valid.poke(true.B)
      dut.io.interrupt_flag.poke(InterruptCode.Timer0)
      
      dut.clock.step(1)
      
      // Verificações
      dut.io.trap_assert.expect(true.B)
      dut.io.trap_to_smode.expect(false.B)
      dut.io.trap_address.expect(0x8000.U)
      dut.io.direct_write_enable.expect(true.B)
      
      // mcause = bit 31 (interrupção) + 7 (Machine Timer Interrupt)
      val mcause_esperado = BigInt(1) << 31 | BigInt(5)
      dut.io.mcause_write_data.expect(mcause_esperado.U(32.W))
      
      // mepc = PC (para interrupções, o RISC-V usa PC+4, mas no seu CLINT atual é PC)
      dut.io.mepc_write_data.expect(0x1004.U)
      
      // mstatus: MPIE ← MIE, MIE ← 0
      val mstatus_esperado = mstatusComMIE(false) | (1 << 7) | (3 << 11)
      dut.io.mstatus_write_data.expect(mstatus_esperado.U(32.W))
    }
  }

  // ============================================================
  // TESTE 3: ECALL com delegação → trap para S-Mode
  // ============================================================
  it should "delegar trap para S-Mode quando medeleg estiver configurado" in {
    test(new CLINT) { dut =>
      // Configuração: delega ECALL from U-Mode (bit 8) para S-Mode
      val medeleg = (1 << 8).U(32.W)
      
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mepc.poke(0.U)
      dut.io.mcause.poke(0.U)
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mie.poke(0.U)
      dut.io.mtval.poke(0.U)
      
      dut.io.sstatus.poke((1 << 1).U(32.W))
      dut.io.sepc.poke(0.U)
      dut.io.scause.poke(0.U)
      dut.io.stvec.poke(0x9000.U)   // handler em S-Mode
      dut.io.sie.poke(0.U)
      dut.io.stval.poke(0.U)
      
      dut.io.medeleg.poke(medeleg)
      dut.io.mideleg.poke(0.U)
      dut.io.current_priv.poke(PrivilegeMode.U)  // executando em U-Mode
      
      dut.io.pc.poke(0x1000.U)
      dut.io.instr.poke(RV32I.ECALL)
      dut.io.valid.poke(true.B)
      dut.io.interrupt_flag.poke(InterruptCode.None)
      
      dut.clock.step(1)
      
      // Verificações
      dut.io.trap_assert.expect(true.B)
      dut.io.trap_to_smode.expect(true.B)   // delegado para S-Mode
      dut.io.trap_address.expect(0x9000.U)  // stvec
      
      // scause = 8 (ECALL from U-Mode), bit 31 = 0
      dut.io.scause_write_data.expect(8.U)
      
      // sepc = PC
      dut.io.sepc_write_data.expect(0x1000.U)
      
      // sstatus: SPIE ← SIE, SIE ← 0
      val sstatus_esperado = (1 << 5)  // SPIE = 1
      dut.io.sstatus_write_data.expect(sstatus_esperado.U(32.W))
    }
  }

  // ============================================================
  // TESTE 4: MRET restaura mstatus.MIE de MPIE
  // ============================================================
  it should "restaurar mstatus.MIE de MPIE no MRET" in {
    test(new CLINT) { dut =>
      // Estado após trap: MPIE=1, MIE=0
      val mstatus_trap = mstatusComMIE(false) | (1 << 7) | (3 << 11)
      
      dut.io.mstatus.poke(mstatus_trap.U(32.W))
      dut.io.mepc.poke(0x2000.U)     // endereço de retorno
      dut.io.mcause.poke((BigInt(1) << 31 | BigInt(7)).U(32.W))
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mie.poke(0.U)
      dut.io.mtval.poke(0.U)
      
      dut.io.sstatus.poke(0.U)
      dut.io.sepc.poke(0.U)
      dut.io.scause.poke(0.U)
      dut.io.stvec.poke(0.U)
      dut.io.sie.poke(0.U)
      dut.io.stval.poke(0.U)
      
      dut.io.medeleg.poke(0.U)
      dut.io.mideleg.poke(0.U)
      dut.io.current_priv.poke(PrivilegeMode.M)
      
      dut.io.pc.poke(0x8004.U)
      dut.io.instr.poke(RV32I.MRET)
      dut.io.valid.poke(true.B)
      dut.io.interrupt_flag.poke(InterruptCode.None)
      
      dut.clock.step(1)
      
      // Verificações
      dut.io.trap_assert.expect(true.B)
      dut.io.trap_address.expect(0x2000.U)  // mepc
      dut.io.direct_write_enable.expect(true.B)
      
      // mstatus: MIE ← MPIE (1), MPIE ← 1
      val mstatus_esperado = mstatusComMIE(true) | (1 << 7)
      dut.io.mstatus_write_data.expect(mstatus_esperado.U(32.W))
      
      // mepc e mcause mantidos
      dut.io.mepc_write_data.expect(0x2000.U)
    }
  }

  // ============================================================
  // TESTE 5: SRET restaura sstatus.SIE de SPIE
  // ============================================================
  it should "restaurar sstatus.SIE de SPIE no SRET" in {
    test(new CLINT) { dut =>
      // Estado após trap em S-Mode: SPIE=1, SIE=0
      val sstatus_trap = (1 << 5)  // SPIE = 1
      
      dut.io.mstatus.poke(0.U)
      dut.io.mepc.poke(0.U)
      dut.io.mcause.poke(0.U)
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mie.poke(0.U)
      dut.io.mtval.poke(0.U)
      
      dut.io.sstatus.poke(sstatus_trap.U(32.W))
      dut.io.sepc.poke(0x3000.U)     // endereço de retorno
      dut.io.scause.poke(8.U)
      dut.io.stvec.poke(0x9000.U)
      dut.io.sie.poke(0.U)
      dut.io.stval.poke(0.U)
      
      dut.io.medeleg.poke(0.U)
      dut.io.mideleg.poke(0.U)
      dut.io.current_priv.poke(PrivilegeMode.S)
      
      dut.io.pc.poke(0x9004.U)
      dut.io.instr.poke(RV32I.SRET)
      dut.io.valid.poke(true.B)
      dut.io.interrupt_flag.poke(InterruptCode.None)
      
      dut.clock.step(1)
      
      // Verificações
      dut.io.trap_assert.expect(true.B)
      dut.io.trap_address.expect(0x3000.U)  // sepc
      dut.io.direct_write_enable.expect(true.B)
      
      // sstatus: SIE ← SPIE (1), SPIE ← 1
      val sstatus_esperado = (1 << 1) | (1 << 5)
      dut.io.sstatus_write_data.expect(sstatus_esperado.U(32.W))
      
      // sepc mantido
      dut.io.sepc_write_data.expect(0x3000.U)
    }
  }

  // ============================================================
  // TESTE 6: Estado normal — nenhuma trap
  // ============================================================
  it should "não gerar trap em estado normal" in {
    test(new CLINT) { dut =>
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mepc.poke(0x1000.U)
      dut.io.mcause.poke(0.U)
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mie.poke(0.U)
      dut.io.mtval.poke(0.U)
      
      dut.io.sstatus.poke(0.U)
      dut.io.sepc.poke(0.U)
      dut.io.scause.poke(0.U)
      dut.io.stvec.poke(0.U)
      dut.io.sie.poke(0.U)
      dut.io.stval.poke(0.U)
      
      dut.io.medeleg.poke(0.U)
      dut.io.mideleg.poke(0.U)
      dut.io.current_priv.poke(PrivilegeMode.M)
      
      dut.io.pc.poke(0x1004.U)
      dut.io.instr.poke(0x00000013.U)  // NOP
      dut.io.valid.poke(true.B)
      dut.io.interrupt_flag.poke(InterruptCode.None)
      
      dut.clock.step(1)
      
      // Nenhuma trap
      dut.io.trap_assert.expect(false.B)
      dut.io.direct_write_enable.expect(false.B)
    }
  }

  // ============================================================
  // TESTE 7: Interrupção desabilitada (MIE=0) — sem trap
  // ============================================================
  it should "não gerar trap se MIE estiver desabilitado" in {
    test(new CLINT) { dut =>
      // MIE = 0 (interrupções globais desabilitadas)
      dut.io.mstatus.poke(mstatusComMIE(false).U(32.W))
      dut.io.mepc.poke(0.U)
      dut.io.mcause.poke(0.U)
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mie.poke((1 << 7).U(32.W))  // MTIE habilitado, mas MIE=0
      dut.io.mtval.poke(0.U)
      
      dut.io.sstatus.poke(0.U)
      dut.io.sepc.poke(0.U)
      dut.io.scause.poke(0.U)
      dut.io.stvec.poke(0.U)
      dut.io.sie.poke(0.U)
      dut.io.stval.poke(0.U)
      
      dut.io.medeleg.poke(0.U)
      dut.io.mideleg.poke(0.U)
      dut.io.current_priv.poke(PrivilegeMode.M)
      
      dut.io.pc.poke(0x1000.U)
      dut.io.instr.poke(0.U)
      dut.io.valid.poke(true.B)
      dut.io.interrupt_flag.poke(InterruptCode.Timer0)
      
      dut.clock.step(1)
      
      // Sem trap porque MIE=0
      dut.io.trap_assert.expect(false.B)
    }
  }
}