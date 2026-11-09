package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos.RV32I

/** Códigos de causa suportados no escopo mínimo (RV32I, M-mode). */
object TrapCause {
  val InstrMisaligned = 0.U(31.W)   // expansão futura
  val IllegalInstr    = 2.U(31.W)
  val Breakpoint      = 3.U(31.W)
  val LoadMisaligned  = 4.U(31.W)   // expansão futura
  val StoreMisaligned = 6.U(31.W)   // expansão futura
  val EcallU          = 8.U(31.W)
  val EcallS          = 9.U(31.W)
  val EcallM          = 11.U(31.W)
  // Interrupções (bit 31=1); fora do escopo mínimo, mantidas para expansão
  val SupervisorTimer = 5.U(31.W)
  val MachineTimer    = 7.U(31.W)
}

/**
  * Controlador de traps/interrupções DESACOPLADO do pipeline.
  *
  * Contrato:
  *  - O pipeline entrega um EVENTO já resolvido (cause_valid, code, is_int,
  *    pc, tval, privilege) e um sinal de COMMIT (o ponto único onde o trap
  *    deve ser efetivado: fim do MEM no 5 estágios, fim do EX/MEM no 3).
  *  - O controlador devolve:
  *      * redirect_valid / redirect_pc  -> o pipeline redireciona o PC;
  *      * commit_suppress               -> o pipeline anula rd/mem/CSR da
  *                                         instrução que está commitando;
  *      * *_write / *_we                -> valores a gravar nos CSRs de trap.
  *  - Nenhuma referência a estágios internos do pipeline.
  *
  * Prioridade: trap > mret > sret > nada.
  * Serialização: o pipeline deve segurar o commit da instrução CSR/mret até
  * que o controlador confirme (ver commit_suppress e redirect_valid).
  */
class TrapController extends Module {
  val io = IO(new Bundle {
    // ---------------- Estado atual (vindo dos CSRs) ----------------
    val mstatus  = Input(UInt(32.W))
    val mepc     = Input(UInt(32.W))
    val mtvec    = Input(UInt(32.W))
    val mtval    = Input(UInt(32.W))
    val sstatus  = Input(UInt(32.W))
    val sepc     = Input(UInt(32.W))
    val stvec    = Input(UInt(32.W))
    val stval    = Input(UInt(32.W))
    val medeleg  = Input(UInt(32.W))
    val mideleg  = Input(UInt(32.W))
    val priv     = Input(UInt(2.W))   // privilégio corrente
    val mcause = Input(UInt(32.W))
    val scause = Input(UInt(32.W))

    // ---------------- Evento de trap (resolvido pelo pipeline) ----------------
    val cause_valid = Input(Bool())
    val cause_code  = Input(UInt(31.W)) // sem o bit 31
    val cause_is_int= Input(Bool())
    val cause_pc    = Input(UInt(32.W)) // PC da instrução que causou o trap
    val cause_tval  = Input(UInt(32.W)) // instrução / endereço / 0

    // ---------------- Instruções de retorno ----------------
    val mret_valid = Input(Bool())
    val sret_valid = Input(Bool())

    // ---------------- Ponto de commit ----------------
    val commit = Input(Bool())        // "efetive o evento agora"

    // ---------------- Saídas: redirecionamento e supressão ----------------
    val redirect_valid  = Output(Bool())
    val redirect_pc     = Output(UInt(32.W))
    val commit_suppress = Output(Bool()) // anula rd/mem/CSR da instr. que commita
    val trap_to_smode   = Output(Bool())

    // ---------------- Saídas: escrita dos CSRs de trap ----------------
    val csr_we      = Output(Bool())
    val mstatus_w   = Output(UInt(32.W))
    val mepc_w      = Output(UInt(32.W))
    val mcause_w    = Output(UInt(32.W))
    val mtval_w     = Output(UInt(32.W))
    val sstatus_w   = Output(UInt(32.W))
    val sepc_w      = Output(UInt(32.W))
    val scause_w    = Output(UInt(32.W))
    val stval_w     = Output(UInt(32.W))

    // ---------------- Novo privilégio após o evento ----------------
    val new_priv    = Output(UInt(2.W))

    // ---------------- Debug ----------------
    val dbg_delegate = Output(Bool())
    val dbg_is_int   = Output(Bool())
    val dbg_code     = Output(UInt(31.W))
  })

  import RV32I._

  // ============================================================
  // 0. Defaults (todas as saídas inicializadas)
  // ============================================================
  io.redirect_valid  := false.B
  io.redirect_pc     := io.mtvec
  io.commit_suppress := false.B
  io.trap_to_smode   := false.B
  io.csr_we          := false.B
  io.mstatus_w       := io.mstatus
  io.mepc_w          := io.mepc
  io.mcause_w        := 0.U
  io.mtval_w         := io.mtval
  io.sstatus_w       := io.sstatus
  io.sepc_w          := io.sepc
  io.scause_w        := 0.U
  io.stval_w         := io.stval
  io.new_priv        := io.priv
  io.dbg_delegate    := false.B
  io.dbg_is_int      := io.cause_is_int
  io.dbg_code        := io.cause_code

  // ============================================================
  // 1. Delegação (política centralizada)
  //    M-mode nunca delega. Fora de M-mode, bit do medeleg/mideleg decide.
  //    No escopo mínimo (M-only) isso fica sempre false; mantido para a
  //    expansão não exigir reescrita.
  // ============================================================
  val code5 = io.cause_code(4, 0)
  val delegate = WireDefault(false.B)
  when(io.priv =/= PrivilegeMode.M) {
    delegate := Mux(io.cause_is_int, io.mideleg(code5), io.medeleg(code5))
  }
  io.dbg_delegate := delegate
  io.trap_to_smode := delegate

  // ============================================================
  // 2. Detecção de evento (combinacional, mas SÓ consumida no commit)
  // ============================================================
  val trap_evt  = io.cause_valid
  val mret_evt  = io.mret_valid
  val sret_evt  = io.sret_valid && !delegate   // sret só faz sentido se S existe
  val any_evt   = trap_evt || mret_evt || sret_evt

  // ============================================================
  // 3. Efetivação no ponto de commit
  // ============================================================
  when(io.commit && trap_evt) {
    io.redirect_valid  := true.B
    io.commit_suppress := true.B

    when(delegate) {
      // ---------- Trap para S-mode ----------
      io.redirect_pc := io.stvec
      io.new_priv    := PrivilegeMode.S

      // SPP <- priv anterior (U=0, S=1); SPIE <- SIE; SIE <- 0
      val spp  = Mux(io.priv === PrivilegeMode.U, 0.U(1.W), 1.U(1.W))
      val smie = io.sstatus(1)
      io.sstatus_w := Cat(
        io.sstatus(31, 9),
        spp,
        io.sstatus(7, 6),
        smie,                    // SPIE
        io.sstatus(4, 2),
        0.U(1.W),                // SIE <- 0
        io.sstatus(0, 0)
      )
      io.sepc_w   := io.cause_pc
      io.scause_w := Cat(io.cause_is_int, io.cause_code)
      io.stval_w  := io.cause_tval
      io.csr_we   := true.B
    }.otherwise {
      // ---------- Trap para M-mode ----------
      io.redirect_pc := io.mtvec
      io.new_priv    := PrivilegeMode.M

      // MPP <- priv anterior; MPIE <- MIE; MIE <- 0
      val mpp = MuxLookup(io.priv, 3.U(2.W))(Seq(
        PrivilegeMode.U -> 0.U(2.W),
        PrivilegeMode.S -> 1.U(2.W),
        PrivilegeMode.M -> 3.U(2.W)
      ))
      val mmie = io.mstatus(3)
      io.mstatus_w := Cat(
        io.mstatus(31, 13),
        mpp,
        io.mstatus(10, 8),
        mmie,                    // MPIE
        io.mstatus(6, 4),
        0.U(1.W),                // MIE <- 0
        io.mstatus(2, 0)
      )
      io.mepc_w   := io.cause_pc
      io.mcause_w := Cat(io.cause_is_int, io.cause_code)
      io.mtval_w  := io.cause_tval
      io.csr_we   := true.B
    }
  }.elsewhen(io.commit && mret_evt) {
    // ---------- MRET ----------
    io.redirect_valid  := true.B
    io.redirect_pc     := io.mepc
    io.commit_suppress := true.B
    io.new_priv        := io.mstatus(12, 11)  // volta para MPP

    val mpie = io.mstatus(7)
    io.mstatus_w := Cat(
      io.mstatus(31, 13), 0.U(2.W), io.mstatus(10, 8),
      1.U(1.W), io.mstatus(6, 4), mpie, io.mstatus(2, 0)
    )
    io.mepc_w   := io.mepc
    io.mtval_w  := io.mtval     // ← preserva
    io.sepc_w   := io.sepc
    io.stval_w  := io.stval     // ← preserva
    io.sstatus_w := io.sstatus
    io.mcause_w := io.mcause
    io.scause_w := io.scause
    io.csr_we   := true.B
  }.elsewhen(io.commit && sret_evt) {
    // ---------- SRET ----------
    io.redirect_valid  := true.B
    io.redirect_pc     := io.sepc
    io.commit_suppress := true.B
    io.new_priv        := Mux(io.sstatus(8), PrivilegeMode.S, PrivilegeMode.U)

    val spie = io.sstatus(5)
    io.sstatus_w := Cat(
      io.sstatus(31, 9),
      0.U(1.W),                    // SPP <- U
      io.sstatus(7, 6),
      1.U(1.W),                    // SPIE <- 1
      io.sstatus(4, 2),
      spie,                        // SIE <- SPIE
      io.sstatus(0, 0)
    )
    io.sepc_w    := io.sepc
    io.mcause_w  := io.mcause     // ← preserva (default zeraria)
    io.mtval_w   := io.mtval
    io.scause_w  := io.scause     // ← preserva (default zeraria)
    io.stval_w   := io.stval
    io.mepc_w    := io.mepc
    io.mstatus_w := io.mstatus
    io.csr_we := true.B
  }

  // Nota: any_evt sem commit NÃO gera redirect nem escrita; o pipeline
  // deve segurar a instrução no ponto de commit até commit=1 (ou descartá-la
  // em caso de flush de branch mais velho).
}