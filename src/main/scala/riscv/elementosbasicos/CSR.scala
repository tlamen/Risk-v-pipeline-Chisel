package riscv

import chisel3._
import chisel3.util._
import riscv.elementosbasicos.RV32I

class CSR extends Module {
  val io = IO(new Bundle {
    // ============================================================
    // LEITURA
    // ============================================================
    val read_address = Input(UInt(12.W))
    val read_data    = Output(UInt(32.W))

    // Leituras específicas para o CLINT (M-Mode)
    val mstatus_read = Output(UInt(32.W))
    val mepc_read    = Output(UInt(32.W))
    val mcause_read  = Output(UInt(32.W))
    val mtvec_read   = Output(UInt(32.W))
    val mie_read     = Output(UInt(32.W))
    val medeleg_read = Output(UInt(32.W))
    val mideleg_read = Output(UInt(32.W))
    val mtval_read   = Output(UInt(32.W))  // NOVO

    // Leituras específicas para o CLINT (S-Mode)
    val sstatus_read = Output(UInt(32.W))
    val sepc_read    = Output(UInt(32.W))
    val scause_read  = Output(UInt(32.W))
    val stvec_read   = Output(UInt(32.W))
    val sie_read     = Output(UInt(32.W))
    val stval_read   = Output(UInt(32.W))  // NOVO
    val satp_read    = Output(UInt(32.W))

    // ============================================================
    // ESCRITA VIA CPU
    // ============================================================
    val cpu_write_address = Input(UInt(12.W))
    val cpu_write_data    = Input(UInt(32.W))
    val cpu_write_enable  = Input(Bool())

    // ============================================================
    // ESCRITA VIA CLINT (prioridade)
    // ============================================================
    val clint_write_enable  = Input(Bool())
    val clint_mstatus_write = Input(UInt(32.W))
    val clint_mepc_write    = Input(UInt(32.W))
    val clint_mcause_write  = Input(UInt(32.W))
    val clint_mtval_write   = Input(UInt(32.W))  // NOVO
    val clint_sstatus_write = Input(UInt(32.W))
    val clint_sepc_write    = Input(UInt(32.W))
    val clint_scause_write  = Input(UInt(32.W))
    val clint_stval_write   = Input(UInt(32.W))

    // ============================================================
    // MODO DE PRIVILÉGIO
    // ============================================================
    val current_priv = Output(UInt(2.W))
  })

  import RV32I._

  // ============================================================
  // REGISTRADORES CSR
  // ============================================================
  // M-Mode
  val mstatus  = RegInit(0.U(32.W))
  val mie      = RegInit(0.U(32.W))
  val mtvec    = RegInit(0.U(32.W))
  val mscratch = RegInit(0.U(32.W))
  val mepc     = RegInit(0.U(32.W))
  val mcause   = RegInit(0.U(32.W))
  val mtval    = RegInit(0.U(32.W))
  val medeleg  = RegInit(0.U(32.W))
  val mideleg  = RegInit(0.U(32.W))

  // S-Mode
  val sstatus  = RegInit(0.U(32.W))
  val sie      = RegInit(0.U(32.W))
  val stvec    = RegInit(0.U(32.W))
  val sscratch = RegInit(0.U(32.W))
  val sepc     = RegInit(0.U(32.W))
  val scause   = RegInit(0.U(32.W))
  val stval    = RegInit(0.U(32.W))
  val satp     = RegInit(0.U(32.W))

  // Modo de privilégio
  val current_priv = RegInit(3.U(2.W))  // Começa em M-mode
  io.current_priv := current_priv

  // ============================================================
  // LEITURA (MuxLookup)
  // ============================================================
  io.read_data := MuxLookup(io.read_address, 0.U(32.W))(
    Seq(
      // M-Mode
      CSRAddress.mstatus  -> mstatus,
      CSRAddress.mie      -> mie,
      CSRAddress.mtvec    -> mtvec,
      CSRAddress.mscratch -> mscratch,
      CSRAddress.mepc     -> mepc,
      CSRAddress.mcause   -> mcause,
      CSRAddress.mtval    -> mtval,
      CSRAddress.medeleg  -> medeleg,
      CSRAddress.mideleg  -> mideleg,
      // S-Mode
      CSRAddress.sstatus  -> sstatus,
      CSRAddress.sie      -> sie,
      CSRAddress.stvec    -> stvec,
      CSRAddress.sscratch -> sscratch,
      CSRAddress.sepc     -> sepc,
      CSRAddress.scause   -> scause,
      CSRAddress.stval    -> stval,
      CSRAddress.satp     -> satp,
      // Read-only
      CSRAddress.mvendorid -> 0.U(32.W),
      CSRAddress.marchid   -> 0.U(32.W),
      CSRAddress.mimpid    -> 0.U(32.W),
      CSRAddress.mhartid   -> 0.U(32.W),
    )
  )

  // ============================================================
  // LEITURAS ESPECÍFICAS (para o CLINT)
  // ============================================================
  // M-Mode
  io.mstatus_read  := mstatus
  io.mepc_read     := mepc
  io.mcause_read   := mcause
  io.mtvec_read    := mtvec
  io.mie_read      := mie
  io.medeleg_read  := medeleg
  io.mideleg_read  := mideleg
  io.mtval_read    := mtval

  // S-Mode (CORRIGIDO: agora todas as saídas são atribuídas)
  io.sstatus_read  := sstatus
  io.sepc_read     := sepc
  io.scause_read   := scause
  io.stvec_read    := stvec
  io.sie_read      := sie
  io.stval_read    := stval
  io.satp_read     := satp

  // ============================================================
  // ESCRITA COM PRIORIDADE: CLINT > CPU
  // ============================================================
  when(io.clint_write_enable) {
    // CLINT escreve nos CSRs de trap
    mstatus := io.clint_mstatus_write
    mepc    := io.clint_mepc_write
    mcause  := io.clint_mcause_write
    mtval   := io.clint_mtval_write   // NOVO
    sstatus := io.clint_sstatus_write
    sepc    := io.clint_sepc_write
    scause  := io.clint_scause_write
    stval   := io.clint_stval_write
    
    // Atualiza o modo de privilégio baseado no MPP
    current_priv := io.clint_mstatus_write(12, 11)
    
  } .elsewhen(io.cpu_write_enable) {
    switch(io.cpu_write_address) {
      // M-Mode
      is(CSRAddress.mstatus)  { mstatus  := io.cpu_write_data }
      is(CSRAddress.mie)      { mie      := io.cpu_write_data }
      is(CSRAddress.mtvec)    { mtvec    := io.cpu_write_data }
      is(CSRAddress.mscratch) { mscratch := io.cpu_write_data }
      is(CSRAddress.mepc)     { mepc     := io.cpu_write_data }
      is(CSRAddress.mcause)   { mcause   := io.cpu_write_data }
      is(CSRAddress.mtval)    { mtval    := io.cpu_write_data }
      is(CSRAddress.medeleg)  { medeleg  := io.cpu_write_data }
      is(CSRAddress.mideleg)  { mideleg  := io.cpu_write_data }
      
      // S-Mode
      is(CSRAddress.sstatus)  { sstatus  := io.cpu_write_data }
      is(CSRAddress.sie)      { sie      := io.cpu_write_data }
      is(CSRAddress.stvec)    { stvec    := io.cpu_write_data }
      is(CSRAddress.sscratch) { sscratch := io.cpu_write_data }
      is(CSRAddress.sepc)     { sepc     := io.cpu_write_data }
      is(CSRAddress.scause)   { scause   := io.cpu_write_data }
      is(CSRAddress.stval)    { stval    := io.cpu_write_data }
      is(CSRAddress.satp)     { satp     := io.cpu_write_data }
    }
  }
}