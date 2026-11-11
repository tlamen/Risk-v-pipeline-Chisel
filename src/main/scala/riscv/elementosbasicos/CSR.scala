package riscv

import chisel3._
import chisel3.util._
import riscv.elementosbasicos.RV32I

class CSR extends Module {
  val io = IO(new Bundle {
    // Leitura
    val read_address = Input(UInt(12.W))
    val read_data    = Output(UInt(32.W))

    // Leituras para o TrapController
    val mstatus_read = Output(UInt(32.W))
    val mepc_read    = Output(UInt(32.W))
    val mcause_read  = Output(UInt(32.W))
    val mtvec_read   = Output(UInt(32.W))
    val mie_read     = Output(UInt(32.W))
    val medeleg_read = Output(UInt(32.W))
    val mideleg_read = Output(UInt(32.W))
    val mtval_read   = Output(UInt(32.W))

    val sstatus_read = Output(UInt(32.W))
    val sepc_read    = Output(UInt(32.W))
    val scause_read  = Output(UInt(32.W))
    val stvec_read   = Output(UInt(32.W))
    val sie_read     = Output(UInt(32.W))
    val stval_read   = Output(UInt(32.W))
    val satp_read    = Output(UInt(32.W))

    // Escrita via CPU (instrução CSR)
    val cpu_write_address = Input(UInt(12.W))
    val cpu_write_data    = Input(UInt(32.W))
    val cpu_write_enable  = Input(Bool())
    // zimm==0 / rs1==x0 -> não escreve (regra Zicsr)
    val cpu_write_is_noop = Input(Bool())

    // Escrita via TrapController (prioridade)
    val trap_we      = Input(Bool())
    val trap_mstatus = Input(UInt(32.W))
    val trap_mepc    = Input(UInt(32.W))
    val trap_mcause  = Input(UInt(32.W))
    val trap_mtval   = Input(UInt(32.W))
    val trap_sstatus = Input(UInt(32.W))
    val trap_sepc    = Input(UInt(32.W))
    val trap_scause  = Input(UInt(32.W))
    val trap_stval   = Input(UInt(32.W))

    // Privilégio: entrada do TrapController (new_priv) tem prioridade
    val trap_new_priv = Input(UInt(2.W))
    val trap_priv_we  = Input(Bool())   // true no commit de trap/mret/sret
    val current_priv  = Output(UInt(2.W))

    val debug_cpu_addr_ro = Output(Bool())
    val debug_cpu_write_is_noop = Output(Bool())
  })

  import RV32I._

  // ---------------- Registradores ----------------
  val mstatus  = RegInit(0.U(32.W))
  val mie      = RegInit(0.U(32.W))
  val mtvec    = RegInit(0.U(32.W))
  val mscratch = RegInit(0.U(32.W))
  val mepc     = RegInit(0.U(32.W))
  val mcause   = RegInit(0.U(32.W))
  val mtval    = RegInit(0.U(32.W))
  val medeleg  = RegInit(0.U(32.W))
  val mideleg  = RegInit(0.U(32.W))

  val sstatus  = RegInit(0.U(32.W))
  val sie      = RegInit(0.U(32.W))
  val stvec    = RegInit(0.U(32.W))
  val sscratch = RegInit(0.U(32.W))
  val sepc     = RegInit(0.U(32.W))
  val scause   = RegInit(0.U(32.W))
  val stval    = RegInit(0.U(32.W))
  val satp     = RegInit(0.U(32.W))

  val priv = RegInit(3.U(2.W))
  io.current_priv := priv

  // ---------------- Leitura com máscaras ----------------
  // mtvec/mepc/sepc: bits [1:0] leem 0 (modo direto)
  val mtvec_r = Cat(mtvec(31, 2), 0.U(2.W))
  val mepc_r  = Cat(mepc(31, 2),  0.U(2.W))
  val sepc_r  = Cat(sepc(31, 2),  0.U(2.W))

  io.read_data := MuxLookup(io.read_address, 0.U(32.W))(Seq(
    // M
    CSRAddress.mstatus  -> mstatus,
    CSRAddress.misa      -> 0.U(32.W),
    CSRAddress.mie       -> mie,
    CSRAddress.mtvec     -> mtvec_r,
    CSRAddress.mscratch  -> mscratch,
    CSRAddress.mepc      -> mepc_r,
    CSRAddress.mcause    -> mcause,
    CSRAddress.mtval     -> mtval,
    CSRAddress.mip       -> 0.U(32.W),
    CSRAddress.medeleg   -> medeleg,
    CSRAddress.mideleg   -> mideleg,
    // S
    CSRAddress.sstatus   -> sstatus,
    CSRAddress.sie       -> sie,
    CSRAddress.stvec     -> stvec,
    CSRAddress.sscratch  -> sscratch,
    CSRAddress.sepc      -> sepc_r,
    CSRAddress.scause    -> scause,
    CSRAddress.stval     -> stval,
    CSRAddress.satp      -> satp,
    // Read-only
    CSRAddress.mvendorid -> 0.U(32.W),
    CSRAddress.marchid   -> 0.U(32.W),
    CSRAddress.mimpid    -> 0.U(32.W),
    CSRAddress.mhartid   -> 0.U(32.W)
  ))

  // Saídas para o TrapController
  io.mstatus_read := mstatus
  io.mepc_read    := mepc
  io.mcause_read  := mcause
  io.mtvec_read   := mtvec
  io.mie_read     := mie
  io.medeleg_read := medeleg
  io.mideleg_read := mideleg
  io.mtval_read   := mtval
  io.sstatus_read := sstatus
  io.sepc_read    := sepc
  io.scause_read  := scause
  io.stvec_read   := stvec
  io.sie_read     := sie
  io.stval_read   := stval
  io.satp_read    := satp

  // ---------------- Escrita ----------------
  // Checagem read-only: csr[11:10] = 11 -> não escreve
  val cpu_addr_ro = io.cpu_write_address(11, 10) === "b11".U

  when(io.trap_we) {
    // Prioridade do TrapController
    mstatus := io.trap_mstatus
    mepc    := Cat(io.trap_mepc(31, 2), 0.U(2.W))
    mcause  := io.trap_mcause
    mtval   := io.trap_mtval
    sstatus := io.trap_sstatus
    sepc    := Cat(io.trap_sepc(31, 2), 0.U(2.W))
    scause  := io.trap_scause
    stval   := io.trap_stval
  }.elsewhen(io.cpu_write_enable && !cpu_addr_ro && !io.cpu_write_is_noop) {
    switch(io.cpu_write_address) {
      is(CSRAddress.mstatus)  { mstatus  := io.cpu_write_data }
      is(CSRAddress.mie)      { mie      := io.cpu_write_data }
      is(CSRAddress.mtvec)    { mtvec    := io.cpu_write_data }
      is(CSRAddress.mscratch) { mscratch := io.cpu_write_data }
      is(CSRAddress.mepc)     { mepc     := io.cpu_write_data }
      is(CSRAddress.mcause)   { mcause   := io.cpu_write_data }
      is(CSRAddress.mtval)    { mtval    := io.cpu_write_data }
      is(CSRAddress.medeleg)  { medeleg  := io.cpu_write_data }
      is(CSRAddress.mideleg)  { mideleg  := io.cpu_write_data }
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

  // Privilégio: só muda no commit do TrapController
  when(io.trap_priv_we) { priv := io.trap_new_priv }
  io.debug_cpu_addr_ro := cpu_addr_ro
  io.debug_cpu_write_is_noop := io.cpu_write_is_noop
}