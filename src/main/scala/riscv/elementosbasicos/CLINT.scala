package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos.RV32I

object InterruptCode {
  val None   = 0x0.U(8.W)
  val Timer0 = 0x1.U(8.W)
  val Ret    = 0xff.U(8.W)
}

class CLINTInterface extends Bundle {
  // ENTRADAS: ESTADO ATUAL
  val mstatus = Input(UInt(32.W))
  val mepc    = Input(UInt(32.W))
  val mcause  = Input(UInt(32.W))
  val mtvec   = Input(UInt(32.W))
  val mie     = Input(UInt(32.W))
  val mtval   = Input(UInt(32.W))
  
  // CSRs de S-Mode
  val sstatus = Input(UInt(32.W))
  val sepc    = Input(UInt(32.W))
  val scause  = Input(UInt(32.W))
  val stvec   = Input(UInt(32.W))
  val sie     = Input(UInt(32.W))
  val stval   = Input(UInt(32.W))
  
  // CSRs de delegação
  val medeleg = Input(UInt(32.W))
  val mideleg = Input(UInt(32.W))
  
  // Modo de privilégio atual
  val current_priv = Input(UInt(2.W))
  
  // ENTRADAS: SINAIS DO PIPELINE
  val pc     = Input(UInt(32.W))
  val instr  = Input(UInt(32.W))
  val valid  = Input(Bool())
  
  // ENTRADAS: INTERRUPÇÕES
  val interrupt_flag = Input(UInt(8.W))
  
  // SAÍDAS: ESCRITA NOS CSRs
  // M-Mode
  val mstatus_write_data = Output(UInt(32.W))
  val mepc_write_data    = Output(UInt(32.W))
  val mcause_write_data  = Output(UInt(32.W))
  val mtval_write_data   = Output(UInt(32.W)) 
  
  // S-Mode
  val sstatus_write_data = Output(UInt(32.W))
  val sepc_write_data    = Output(UInt(32.W))
  val scause_write_data  = Output(UInt(32.W))
  val stval_write_data   = Output(UInt(32.W))
  
  // Controle
  val direct_write_enable = Output(Bool())
  
  // SAÍDAS: CONTROLE DE FLUXO
  val trap_assert   = Output(Bool())
  val trap_address  = Output(UInt(32.W))
  val trap_cause    = Output(UInt(32.W))
  val trap_to_smode = Output(Bool())  // NOVO: indica trap para S-Mode
}

class CLINT extends Module {
  val io = IO(new CLINTInterface)
  
  import RV32I._
  
  // 1. ESTADO ATUAL
  val current_priv = io.current_priv
  
  // mstatus bits (M-Mode)
  val m_mie  = io.mstatus(3)
  val m_mpie = io.mstatus(7)
  
  // sstatus bits (S-Mode)
  val s_mie  = io.sstatus(1)
  val s_spie = io.sstatus(5)
  val s_spp  = io.sstatus(8)
  
  // 2. VERIFICAÇÃO DE INTERRUPÇÕES
  val interrupt_enable_global_m = io.mstatus(3)
  val interrupt_enable_timer_m  = io.mie(7)
  val interrupt_enable_external_m = io.mie(11)
  
  val interrupt_enable_global_s = io.sstatus(1)
  val interrupt_enable_timer_s  = io.sie(5)   // STIE
  val interrupt_enable_external_s = io.sie(9)  // SEIE

  // 3. DELEGAÇÃO DE TRAPS
  // Uma trap é delegada para S-Mode se o bit correspondente
  // em medeleg (exceções) ou mideleg (interrupções) está setado

  val exception_code = WireDefault(0.U(31.W))
  val is_interrupt = WireDefault(false.B)
  
  // Determinar a causa da trap (preliminar)
  when(io.interrupt_flag =/= InterruptCode.None) {
    is_interrupt := true.B
    exception_code := MuxLookup(io.interrupt_flag, 11.U(31.W))(
      Seq(InterruptCode.Timer0 -> 5.U(31.W))  // Supervisor Timer Interrupt
    )
  } .elsewhen(io.valid && io.instr === EBREAK) {
    exception_code := 3.U(31.W)   // Breakpoint
  } .elsewhen(io.valid && io.instr === ECALL) {
    exception_code := MuxLookup(current_priv, 11.U(31.W))(
      Seq(
        0.U -> 8.U(31.W),   // ECALL from U-mode
        1.U -> 9.U(31.W),   // ECALL from S-mode
        3.U -> 11.U(31.W)   // ECALL from M-mode
      )
    )
  } .elsewhen(io.valid && io.instr === MRET) {
    exception_code := 0.U
  } .elsewhen(io.valid && io.instr === SRET) {
    exception_code := 0.U
  }
  
  // Verificar se a trap é delegada para S-Mode
  val delegate_to_smode = WireDefault(false.B)
  when(current_priv === PrivilegeMode.M) {
    delegate_to_smode := false.B
  } .elsewhen(is_interrupt) {
    delegate_to_smode := io.mideleg(exception_code)
  } .otherwise {
    delegate_to_smode := io.medeleg(exception_code)
  }

  // 4. TRAP ENTRY - M-MODE
  val trap_assert_internal  = WireDefault(false.B)
  val trap_address_internal = WireDefault(io.mtvec)
  val trap_cause_internal   = WireDefault(0.U(32.W))
  val trap_to_smode_internal = WireDefault(false.B)
  
  when(io.interrupt_flag =/= InterruptCode.None && 
       (interrupt_enable_global_m || interrupt_enable_global_s)) {
    // Interrupção detectada
    trap_assert_internal := true.B
    trap_to_smode_internal := delegate_to_smode
    
    when(delegate_to_smode) {
      // Trap para S-Mode
      trap_address_internal := io.stvec
      
      io.sstatus_write_data := Cat(
        io.sstatus(31, 9),
        0.U(1.W),                     // SPP ← 0 (U-mode era anterior)
        io.sstatus(7, 6),
        s_mie,                        // SPIE ← SIE
        io.sstatus(4, 2),
        0.U(1.W),                     // SIE ← 0
        io.sstatus(1, 0)
      )
      io.sepc_write_data := io.pc
      io.scause_write_data := Cat(1.U(1.W), exception_code)
      io.stval_write_data := 0.U
      
      // Mantém M-Mode inalterado
      io.mstatus_write_data := io.mstatus
      io.mepc_write_data := io.mepc
      io.mcause_write_data := io.mcause
      io.mtval_write_data := io.mtval

    } .otherwise {
      // Trap para M-Mode
      trap_address_internal := io.mtvec
      
      io.mstatus_write_data := Cat(
        io.mstatus(31, 13),
        3.U(2.W),                     // MPP ← M-Mode
        io.mstatus(10, 8),
        m_mie,                        // MPIE ← MIE
        io.mstatus(6, 4),
        0.U(1.W),                     // MIE ← 0
        io.mstatus(2, 0)
      )
      io.mepc_write_data := io.pc
      io.mcause_write_data := Cat(1.U(1.W), exception_code)
      
      // Mantém S-Mode inalterado
      io.sstatus_write_data := io.sstatus
      io.sepc_write_data := io.sepc
      io.scause_write_data := io.scause
      io.stval_write_data := io.stval
    }
    
    io.direct_write_enable := true.B
    
  
  // 5. EXCEÇÃO (ECALL/EBREAK)
  
  } .elsewhen(io.valid && (io.instr === ECALL || io.instr === EBREAK)) {
    trap_assert_internal := true.B
    trap_to_smode_internal := delegate_to_smode
    
    when(delegate_to_smode) {
      // Exceção para S-Mode
      trap_address_internal := io.stvec
      
      io.sstatus_write_data := Cat(
        io.sstatus(31, 9),
        Mux(current_priv === PrivilegeMode.U, 0.U(1.W), 1.U(1.W)),  // SPP
        io.sstatus(7, 6),
        s_mie,
        io.sstatus(4, 2),
        0.U(1.W),
        io.sstatus(1, 0)
      )
      io.sepc_write_data := io.pc
      io.scause_write_data := Cat(0.U(1.W), exception_code)
      io.stval_write_data := 0.U
      
      io.mstatus_write_data := io.mstatus
      io.mepc_write_data := io.mepc
      io.mcause_write_data := io.mcause
      io.mtval_write_data := io.mtval
      
    } .otherwise {
      // Exceção para M-Mode
      trap_address_internal := io.mtvec
      
      io.mstatus_write_data := Cat(
        io.mstatus(31, 13),
        Mux(current_priv === PrivilegeMode.U, 0.U(2.W),
            Mux(current_priv === PrivilegeMode.S, 1.U(2.W), 3.U(2.W))),
        io.mstatus(10, 8),
        m_mie,
        io.mstatus(6, 4),
        0.U(1.W),
        io.mstatus(2, 0)
      )
      io.mepc_write_data := io.pc
      io.mcause_write_data := Cat(0.U(1.W), exception_code)
      
      io.sstatus_write_data := io.sstatus
      io.sepc_write_data := io.sepc
      io.scause_write_data := io.scause
      io.stval_write_data := io.stval
    }
    
    io.direct_write_enable := true.B
    
  
  // 6. RETORNO DE TRAP - MRET (M-Mode)
  
  } .elsewhen(io.valid && io.instr === MRET) {
    trap_assert_internal := true.B
    trap_address_internal := io.mepc
    
    val mpp = io.mstatus(12, 11)
    
    io.mstatus_write_data := Cat(
      io.mstatus(31, 13),
      0.U(2.W),                       // MPP ← U-mode
      io.mstatus(10, 8),
      1.U(1.W),                       // MPIE ← 1
      io.mstatus(6, 4),
      m_mpie,                         // MIE ← MPIE
      io.mstatus(2, 0)
    )
    io.mepc_write_data := io.mepc
    io.mcause_write_data := io.mcause
    io.mtval_write_data := io.mtval
    
    io.sstatus_write_data := io.sstatus
    io.sepc_write_data := io.sepc
    io.scause_write_data := io.scause
    io.stval_write_data := io.stval
    
    io.direct_write_enable := true.B
    
  
  // 7. RETORNO DE TRAP - SRET (S-Mode)
  
  } .elsewhen(io.valid && io.instr === SRET) {
    trap_assert_internal := true.B
    trap_address_internal := io.sepc
    
    val spp = io.sstatus(8)
    
    io.sstatus_write_data := Cat(
      io.sstatus(31, 9),
      0.U(1.W),                       // SPP ← U-mode
      io.sstatus(7, 6),
      1.U(1.W),                       // SPIE ← 1
      io.sstatus(4, 2),
      s_spie,                         // SIE ← SPIE
      io.sstatus(1, 0)
    )
    io.sepc_write_data := io.sepc
    io.scause_write_data := io.scause
    io.stval_write_data := io.stval
    io.mtval_write_data := io.mtval
    
    io.mstatus_write_data := io.mstatus
    io.mepc_write_data := io.mepc
    io.mcause_write_data := io.mcause
    io.direct_write_enable := true.B
    
  // 8. ESTADO NORMAL
  } .otherwise {
    trap_assert_internal  := false.B
    trap_to_smode_internal := false.B
    trap_address_internal := io.mtvec
    
    io.mstatus_write_data := io.mstatus
    io.mepc_write_data := io.mepc
    io.mcause_write_data := io.mcause
    io.mtval_write_data := io.mtval
    io.sstatus_write_data := io.sstatus
    io.sepc_write_data := io.sepc
    io.scause_write_data := io.scause
    io.stval_write_data := io.stval
    io.direct_write_enable := false.B
  }
  
  
  // 9. SAÍDAS
  
  io.trap_assert   := trap_assert_internal
  io.trap_address  := trap_address_internal
  io.trap_cause    := trap_cause_internal
  io.trap_to_smode := trap_to_smode_internal
}