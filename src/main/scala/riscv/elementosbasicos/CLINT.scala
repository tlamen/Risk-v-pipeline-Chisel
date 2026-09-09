package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos.RV32I

// Códigos de interrupção para o registrador mcause
object InterruptCode {
  val None   = 0x0.U(8.W)
  val Timer0 = 0x1.U(8.W)
  val Ret    = 0xff.U(8.W)
}

// Interface do CLINT para comunicação com o pipeline
class CLINTInterface extends Bundle {
  // Entradas: estado atual do processador
  val mstatus = Input(UInt(32.W))
  val mepc    = Input(UInt(32.W))
  val mcause  = Input(UInt(32.W))
  val mtvec   = Input(UInt(32.W))
  val mie     = Input(UInt(32.W))
  val priv_mode = Input(UInt(2.W))
  
  // Entradas: sinais do pipeline
  val pc     = Input(UInt(32.W))
  val instr  = Input(UInt(32.W))
  val valid  = Input(Bool())
  
  // Entradas: interrupções de periféricos
  val interrupt_flag = Input(UInt(8.W))
  
  // Saídas: dados para escrita nos CSRs
  val mstatus_write_data = Output(UInt(32.W))
  val mepc_write_data    = Output(UInt(32.W))
  val mcause_write_data  = Output(UInt(32.W))
  val direct_write_enable = Output(Bool())
  
  // Saídas: controle de fluxo
  val trap_assert   = Output(Bool())        // Indica que uma trap ocorreu
  val trap_address  = Output(UInt(32.W))    // Endereço do handler
  val trap_cause    = Output(UInt(32.W))    // Causa da trap (para mcause)
}

/**
  * Core Local Interrupt Controller (CLINT)
  * 
  * Responsabilidades:
  * - Gerenciar interrupções de periféricos (Timer, UART, etc.)
  * - Gerenciar exceções (ECALL, EBREAK)
  * - Executar MRET (retorno de trap)
  * - Atualizar CSRs atomicamente durante traps
  * 
  * Transições de estado:
  * - Entrada de interrupção: Salva PC em mepc, mcause, desabilita MIE, pula para mtvec
  * - Entrada de exceção: Mesmo que interrupção, mas mcause[31]=0
  * - MRET: Restaura PC de mepc, reabilita MIE, MPIE=1
  */
class CLINT extends Module {
  val io = IO(new CLINTInterface)
  
  import RV32I._
  
  // Sinais internos
  val interrupt_enable_global   = io.mstatus(3)   // MIE bit
  val interrupt_enable_timer    = io.mie(7)       // MTIE bit
  val interrupt_enable_external = io.mie(11)      // MEIE bit
  
  // Estado anterior de MIE e MPIE
  val mpie = io.mstatus(7)
  val mie  = io.mstatus(3)
  
  // Sinais internos
  val trap_assert_internal = WireDefault(false.B)
  val trap_address_internal = WireDefault(io.mtvec)
  val trap_cause_internal = WireDefault(0.U(32.W))
  
  // ============================================================
  // 1. VERIFICAÇÃO DE INTERRUPÇÕES
  // ============================================================
  
  // Verifica se uma interrupção está habilitada
  val interrupt_source_enabled = MuxLookup(io.interrupt_flag, false.B)(
    Seq(
      InterruptCode.Timer0 -> (interrupt_enable_global && interrupt_enable_timer),
      InterruptCode.None   -> false.B
    )
  )
  
  // ============================================================
  // 2. ENTRADA DE INTERRUPÇÃO
  // ============================================================
  // 
  // mstatus bit positions (RISC-V Privileged Spec):
  // - Bit 3 (MIE): Machine Interrupt Enable
  // - Bit 7 (MPIE): Machine Previous Interrupt Enable
  //
  // Trap entry state transition:
  // 1. Save current interrupt enable: MPIE ← MIE
  // 2. Disable interrupts: MIE ← 0
  // 3. Save return address: mepc ← PC+4
  // 4. Record cause: mcause ← interrupt code (bit 31=1)
  // 5. Jump to handler: PC ← mtvec
  
  when(io.interrupt_flag =/= InterruptCode.None && interrupt_source_enabled) {
    // Entrada de interrupção
    trap_assert_internal := true.B
    trap_address_internal := io.mtvec
    
    // Atualização do mstatus:
    // [31:13] | [12:11:MPP] | [10:8] | [7:MPIE] | [6:4] | [3:MIE] | [2:0]
    io.mstatus_write_data := Cat(
      io.mstatus(31, 13),
      io.priv_mode,                     // MPP ← 0b11 (Machine mode)
      io.mstatus(10, 8),
      mie,                          // MPIE ← MIE (salva estado atual)
      io.mstatus(6, 4),
      0.U(1.W),                     // MIE ← 0 (desabilita interrupções)
      io.mstatus(2, 0)
    )
    
    // Salva PC+4 em mepc
    io.mepc_write_data := io.pc
    
    // Codifica a causa (bit 31 = 1 para interrupção)
    trap_cause_internal := Cat(
      1.U(1.W),
      MuxLookup(io.interrupt_flag, 11.U(31.W))(  // default: external interrupt
        Seq(
          InterruptCode.Timer0 -> 7.U(31.W)      // Timer interrupt
        )
      )
    )
    io.mcause_write_data := trap_cause_internal
    
    // Habilita escrita direta no CSR
    io.direct_write_enable := true.B
    
  // ============================================================
  // 3. ENTRADA DE EXCEÇÃO (ECALL/EBREAK)
  // ============================================================
  } .elsewhen(io.valid && (io.instr === ECALL || io.instr === EBREAK)) {
    // Entrada de exceção
    trap_assert_internal := true.B
    trap_address_internal := io.mtvec
    
    // Mesmo comportamento que interrupção, mas mcause[31]=0
    io.mstatus_write_data := Cat(
      io.mstatus(31, 13),
      io.priv_mode,                     // MPP ← 0b11 (Machine mode)
      io.mstatus(10, 8),
      mie,                          // MPIE ← MIE
      io.mstatus(6, 4),
      0.U(1.W),                     // MIE ← 0
      io.mstatus(2, 0)
    )
    
    io.mepc_write_data := io.pc
    
    // Codifica a causa (bit 31 = 0 para exceção)
    val ecall_cause = MuxLookup(io.priv_mode, 11.U(31.W))(Seq(
      0.U -> 8.U(31.W),   // ECALL from U-mode
      1.U -> 9.U(31.W),   // ECALL from S-mode
      3.U -> 11.U(31.W)   // ECALL from M-mode
    ))

    // Codifica a causa (Bit 31 = 0 para Exceções)
    trap_cause_internal := Cat(
      0.U(1.W),
      Mux(io.instr === EBREAK, 3.U(31.W), ecall_cause)
    )
    io.mcause_write_data := trap_cause_internal
    io.direct_write_enable := true.B
    
  // ============================================================
  // 4. RETORNO DE TRAP (MRET)
  // ============================================================
  // 
  // MRET state transition:
  // 1. Restore interrupt enable: MIE ← MPIE
  // 2. Set MPIE to 1: MPIE ← 1
  // 3. Return to saved PC: PC ← mepc
  //
  // Inverse of trap entry:
  // - Trap entry: MPIE←MIE, MIE←0
  // - MRET: MIE←MPIE, MPIE←1
  } .elsewhen(io.valid && io.instr === MRET) {
    // Retorno de trap
    trap_assert_internal := true.B
    trap_address_internal := io.mepc
    
    // Atualização do mstatus:
    // [31:13] | [12:11:MPP] | [10:8] | [7:MPIE] | [6:4] | [3:MIE] | [2:0]
    io.mstatus_write_data := Cat(
      io.mstatus(31, 13),
      0.U(2.W),                     // MPP ← 0b11 (Machine mode)
      io.mstatus(10, 8),
      1.U(1.W),                     // MPIE ← 1 (reset MPIE)
      io.mstatus(6, 4),
      mpie,                         // MIE ← MPIE (restaura estado salvo)
      io.mstatus(2, 0)
    )
    
    // Mantém mepc e mcause inalterados
    io.mepc_write_data := io.mepc
    io.mcause_write_data := io.mcause
    io.direct_write_enable := true.B
    
  // ============================================================
  // 5. ESTADO NORMAL (SEM TRAP)
  // ============================================================
  } .otherwise {
    trap_assert_internal := false.B
    trap_address_internal := io.mtvec
    
    // Mantém valores dos CSRs
    io.mstatus_write_data := io.mstatus
    io.mepc_write_data := io.mepc
    io.mcause_write_data := io.mcause
    io.direct_write_enable := false.B
  }
  
  // ============================================================
  // 6. SAÍDAS
  // ============================================================
  
  io.trap_assert := trap_assert_internal
  io.trap_address := trap_address_internal
  io.trap_cause := trap_cause_internal
}