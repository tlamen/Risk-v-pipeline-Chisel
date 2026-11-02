package riscv.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.RV32I
import riscv.elementosbasicos.RV32I._

class TrapControllerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TrapController"

  // ============================================================
  // HELPERS
  // ============================================================
  private def mstatusComMIE(mie: Boolean): BigInt =
    if (mie) BigInt(1) << 3 else BigInt(0)

  private def mstatusComMPP_S: BigInt =
    mstatusComMIE(true) | (BigInt(1) << 11)

  private def mstatusComMPP_U: BigInt =
    mstatusComMIE(true)

  /** Pokeia todos os campos de estado com valores neutros. */
  private def pokeEstadoNeutro(dut: TrapController): Unit = {
    dut.io.mstatus.poke(0.U)
    dut.io.mepc.poke(0.U)
    dut.io.mtvec.poke(0.U)
    dut.io.mtval.poke(0.U)
    dut.io.sstatus.poke(0.U)
    dut.io.sepc.poke(0.U)
    dut.io.stvec.poke(0.U)
    dut.io.stval.poke(0.U)
    dut.io.medeleg.poke(0.U)
    dut.io.mideleg.poke(0.U)
    dut.io.priv.poke(PrivilegeMode.M)
  }

  /** Pokeia o evento de trap como "nenhum". */
  private def pokeSemEvento(dut: TrapController): Unit = {
    dut.io.cause_valid.poke(false.B)
    dut.io.cause_code.poke(0.U)
    dut.io.cause_is_int.poke(false.B)
    dut.io.cause_pc.poke(0.U)
    dut.io.cause_tval.poke(0.U)
    dut.io.mret_valid.poke(false.B)
    dut.io.sret_valid.poke(false.B)
    dut.io.commit.poke(false.B)
  }

  // ============================================================
  // TESTE 1: ECALL em M-mode → trap para M-mode
  // ============================================================
  it should "gerar trap para M-Mode em ECALL de M-Mode sem delegação" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mtvec.poke(0x8000.U)
      dut.io.priv.poke(PrivilegeMode.M)

      // Evento: ECALL from M (causa 11)
      dut.io.cause_valid.poke(true.B)
      dut.io.cause_code.poke(TrapCause.EcallM)
      dut.io.cause_is_int.poke(false.B)
      dut.io.cause_pc.poke(0x1000.U)
      dut.io.cause_tval.poke(0.U)
      dut.io.mret_valid.poke(false.B)
      dut.io.sret_valid.poke(false.B)
      dut.io.commit.poke(true.B)

      dut.clock.step(1)

      dut.io.redirect_valid.expect(true.B)
      dut.io.redirect_pc.expect(0x8000.U)
      dut.io.commit_suppress.expect(true.B)
      dut.io.trap_to_smode.expect(false.B)
      dut.io.csr_we.expect(true.B)
      dut.io.new_priv.expect(PrivilegeMode.M)

      dut.io.mcause_w.expect(11.U)
      dut.io.mepc_w.expect(0x1000.U)
      dut.io.mtval_w.expect(0.U)

      // MPIE <- MIE (1), MIE <- 0, MPP <- 3 (M)
      val mstatus_esperado = mstatusComMIE(false) | (BigInt(1) << 7) | (BigInt(3) << 11)
      dut.io.mstatus_w.expect(mstatus_esperado.U(32.W))

      // S-mode intocado
      dut.io.sstatus_w.expect(0.U)
      dut.io.sepc_w.expect(0.U)
      dut.io.scause_w.expect(0.U)
      dut.io.stval_w.expect(0.U)
    }
  }

  // ============================================================
  // TESTE 2: ECALL em M-mode NÃO delega nem com medeleg setado
  // ============================================================
  it should "não delegar para S-Mode quando priv = M" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mtvec.poke(0x8000.U)
      dut.io.stvec.poke(0x9000.U)
      dut.io.medeleg.poke(("hFFFFFFFF".U).asUInt)   // delega tudo
      dut.io.priv.poke(PrivilegeMode.M)

      dut.io.cause_valid.poke(true.B)
      dut.io.cause_code.poke(TrapCause.EcallM)
      dut.io.cause_pc.poke(0x1000.U)
      dut.io.commit.poke(true.B)

      dut.clock.step(1)

      dut.io.trap_to_smode.expect(false.B)
      dut.io.redirect_pc.expect(0x8000.U)
      dut.io.new_priv.expect(PrivilegeMode.M)
    }
  }

  // ============================================================
  // TESTE 3: ECALL em U-mode com delegação → trap para S-mode
  // ============================================================
  it should "delegar trap para S-Mode quando medeleg estiver configurado" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      val medeleg = (BigInt(1) << 8).U(32.W)   // delega ECALL from U (causa 8)
      dut.io.medeleg.poke(medeleg)
      dut.io.stvec.poke(0x9000.U)
      dut.io.mtvec.poke(0x8000.U)
      dut.io.sstatus.poke((BigInt(1) << 1).U(32.W))   // SIE = 1
      dut.io.priv.poke(PrivilegeMode.U)

      dut.io.cause_valid.poke(true.B)
      dut.io.cause_code.poke(TrapCause.EcallU)         // 8
      dut.io.cause_pc.poke(0x1000.U)
      dut.io.cause_tval.poke(0.U)
      dut.io.commit.poke(true.B)

      dut.clock.step(1)

      dut.io.redirect_valid.expect(true.B)
      dut.io.trap_to_smode.expect(true.B)
      dut.io.redirect_pc.expect(0x9000.U)
      dut.io.commit_suppress.expect(true.B)
      dut.io.csr_we.expect(true.B)
      dut.io.new_priv.expect(PrivilegeMode.S)

      dut.io.scause_w.expect(8.U)
      dut.io.sepc_w.expect(0x1000.U)
      dut.io.stval_w.expect(0.U)

      // SPP <- 0 (era U), SPIE <- SIE (1), SIE <- 0
      val sstatus_esperado = (BigInt(1) << 5)   // SPIE = 1
      dut.io.sstatus_w.expect(sstatus_esperado.U(32.W))

      // M-mode intocado
      dut.io.mstatus_w.expect(0.U)
      dut.io.mepc_w.expect(0.U)
      dut.io.mcause_w.expect(0.U)
    }
  }

  // ============================================================
  // TESTE 4: MRET restaura MIE de MPIE e volta para MPP
  // ============================================================
  it should "restaurar mstatus.MIE de MPIE no MRET" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      // Estado após trap: MPIE=1, MIE=0, MPP=M(3)
      val mstatus_trap = mstatusComMIE(false) | (BigInt(1) << 7) | (BigInt(3) << 11)
      dut.io.mstatus.poke(mstatus_trap.U(32.W))
      dut.io.mepc.poke(0x2000.U)
      dut.io.priv.poke(PrivilegeMode.M)

      dut.io.cause_valid.poke(false.B)
      dut.io.mret_valid.poke(true.B)
      dut.io.sret_valid.poke(false.B)
      dut.io.commit.poke(true.B)

      dut.clock.step(1)

      dut.io.redirect_valid.expect(true.B)
      dut.io.redirect_pc.expect(0x2000.U)
      dut.io.commit_suppress.expect(true.B)
      dut.io.csr_we.expect(true.B)
      dut.io.new_priv.expect(PrivilegeMode.M)   // MPP = M

      // MIE <- MPIE (1), MPIE <- 1, MPP <- U (0)
      val mstatus_esperado = mstatusComMIE(true) | (BigInt(1) << 7)
      dut.io.mstatus_w.expect(mstatus_esperado.U(32.W))

      // mepc mantido (default = io.mepc)
      dut.io.mepc_w.expect(0x2000.U)
    }
  }

  // ============================================================
  // TESTE 5: SRET restaura SIE de SPIE e volta para S/U
  // ============================================================
  it should "restaurar sstatus.SIE de SPIE no SRET" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      val sstatus_trap = (BigInt(1) << 5) | (BigInt(1) << 8)  // SPIE=1, SPP=1(S)
      dut.io.sstatus.poke(sstatus_trap.U(32.W))
      dut.io.sepc.poke(0x3000.U)
      dut.io.priv.poke(PrivilegeMode.S)

      dut.io.cause_valid.poke(false.B)
      dut.io.mret_valid.poke(false.B)
      dut.io.sret_valid.poke(true.B)
      dut.io.commit.poke(true.B)

      dut.clock.step(1)

      dut.io.redirect_valid.expect(true.B)
      dut.io.redirect_pc.expect(0x3000.U)
      dut.io.commit_suppress.expect(true.B)
      dut.io.csr_we.expect(true.B)
      dut.io.new_priv.expect(PrivilegeMode.S)   // SPP=1 → S

      // SIE <- SPIE (1), SPIE <- 1, SPP <- 0
      val sstatus_esperado = (BigInt(1) << 1) | (BigInt(1) << 5)
      dut.io.sstatus_w.expect(sstatus_esperado.U(32.W))
    }
  }

  // ============================================================
  // TESTE 6: Interrupção (cause_is_int=true) → mcause[31]=1
  // ============================================================
  it should "marcar mcause[31]=1 para interrupção" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mtvec.poke(0x8000.U)
      dut.io.priv.poke(PrivilegeMode.M)

      dut.io.cause_valid.poke(true.B)
      dut.io.cause_code.poke(TrapCause.MachineTimer)   // 7
      dut.io.cause_is_int.poke(true.B)
      dut.io.cause_pc.poke(0x1004.U)
      dut.io.commit.poke(true.B)

      dut.clock.step(1)

      dut.io.redirect_valid.expect(true.B)
      dut.io.trap_to_smode.expect(false.B)
      dut.io.redirect_pc.expect(0x8000.U)

      val mcause_esperado = (BigInt(1) << 31) | BigInt(7)
      dut.io.mcause_w.expect(mcause_esperado.U(32.W))
      dut.io.mepc_w.expect(0x1004.U)
    }
  }

  // ============================================================
  // TESTE 7: sem commit → nenhum efeito
  // ============================================================
  it should "não efetivar trap quando commit=false" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mtvec.poke(0x8000.U)
      dut.io.priv.poke(PrivilegeMode.M)

      dut.io.cause_valid.poke(true.B)
      dut.io.cause_code.poke(TrapCause.EcallM)
      dut.io.cause_pc.poke(0x1000.U)
      dut.io.commit.poke(false.B)

      dut.clock.step(1)

      dut.io.redirect_valid.expect(false.B)
      dut.io.commit_suppress.expect(false.B)
      dut.io.csr_we.expect(false.B)
      dut.io.new_priv.expect(PrivilegeMode.M)
      dut.io.mstatus_w.expect(mstatusComMIE(true).U(32.W))  // inalterado
    }
  }

  // ============================================================
  // TESTE 8: Estado normal (sem evento) — nenhuma trap
  // ============================================================
  it should "não gerar trap em estado normal" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      dut.io.mstatus.poke(mstatusComMIE(true).U(32.W))
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mepc.poke(0x1000.U)
      dut.io.priv.poke(PrivilegeMode.M)

      pokeSemEvento(dut)
      dut.clock.step(1)

      dut.io.redirect_valid.expect(false.B)
      dut.io.commit_suppress.expect(false.B)
      dut.io.csr_we.expect(false.B)
      dut.io.redirect_pc.expect(0x8000.U)    // default = mtvec
      dut.io.mstatus_w.expect(mstatusComMIE(true).U(32.W))
      dut.io.mepc_w.expect(0x1000.U)
    }
  }

  // ============================================================
  // TESTE 9: MRET com MPP=U → new_priv=U
  // ============================================================
  it should "voltar para U-mode no MRET quando MPP=U" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      // MPIE=1, MPP=U(0), MIE=0
      val mstatus_trap = (BigInt(1) << 7)
      dut.io.mstatus.poke(mstatus_trap.U(32.W))
      dut.io.mepc.poke(0x2000.U)
      dut.io.priv.poke(PrivilegeMode.M)

      dut.io.mret_valid.poke(true.B)
      dut.io.commit.poke(true.B)
      dut.clock.step(1)

      dut.io.new_priv.expect(PrivilegeMode.U)
      dut.io.redirect_pc.expect(0x2000.U)
    }
  }

  // ============================================================
  // TESTE 10: SRET com SPP=U → new_priv=U
  // ============================================================
  it should "voltar para U-mode no SRET quando SPP=0" in {
    test(new TrapController) { dut =>
      pokeEstadoNeutro(dut)
      dut.io.sstatus.poke((BigInt(1) << 5).U(32.W))   // SPIE=1, SPP=0
      dut.io.sepc.poke(0x3000.U)
      dut.io.priv.poke(PrivilegeMode.S)

      dut.io.sret_valid.poke(true.B)
      dut.io.commit.poke(true.B)
      dut.clock.step(1)

      dut.io.new_priv.expect(PrivilegeMode.U)
      dut.io.redirect_pc.expect(0x3000.U)
    }
  }
}