package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos._
import riscv._

// ============================================================
// BUNDLE: registradores do estágio ID/EX
// ============================================================
class DecodeExecuteBundle extends Bundle {
  val valid       = Bool()
  val pc          = UInt(32.W)
  val instr       = UInt(32.W)
  val rs1         = UInt(5.W)
  val rs2         = UInt(5.W)
  val rd          = UInt(5.W)
  val rs1Value    = UInt(32.W)
  val rs2Value    = UInt(32.W)
  val imm         = UInt(32.W)
  val memAddress  = UInt(32.W)
  val memWriteData = UInt(32.W)
  val signals     = new ControlSignals
  val csrAddress  = UInt(12.W)
  val csrWriteData = UInt(32.W)
  val csrReadData  = UInt(32.W)
  val exc_valid   = Bool()
  val exc_code    = UInt(31.W)
  val exc_tval    = UInt(32.W)
  val mret_valid  = Bool()
  val sret_valid  = Bool()
  val csr_is_noop = Bool()
}

class Pipeline3(
    initialProgram: Seq[Long] = Seq.empty,
    memoryWords: Int = 1024,
    programFile: String = ""
) extends Module {
  val io = IO(new Bundle {
    val pc              = Output(UInt(32.W))
    val instr           = Output(UInt(32.W))
    val aluResult       = Output(UInt(32.W))
    val writebackData   = Output(UInt(32.W))
    val writebackRd     = Output(UInt(5.W))
    val writebackEnable = Output(Bool())
    val illegal         = Output(Bool())
    val stalled         = Output(Bool())

    // Debug do TrapController
    val debug_trap_redirect      = Output(Bool())
    val debug_trap_to_smode      = Output(Bool())
    val debug_trap_delegate      = Output(Bool())
    val debug_trap_code          = Output(UInt(31.W))
    val debug_trap_is_interrupt  = Output(Bool())
    val debug_trap_redirect_pc   = Output(UInt(32.W))
    val debug_commit_suppress    = Output(Bool())

    // Debug do CSR forwarding
    val debug_csrReadData    = Output(UInt(32.W))
    val debug_csrForwarding  = Output(Bool())
    val debug_csr_address_id = Output(UInt(12.W))
    val debug_csr_address_ex = Output(UInt(12.W))

    // Debug de valores dos CSRs
    val debug_mepc    = Output(UInt(32.W))
    val debug_mcause  = Output(UInt(32.W))
    val debug_mtvec   = Output(UInt(32.W))
    val debug_mstatus = Output(UInt(32.W))
    val debug_priv    = Output(UInt(2.W))

    val debug_cpu_write_enable   = Output(Bool())
    val debug_cpu_write_address  = Output(UInt(12.W))
    val debug_cpu_write_data     = Output(UInt(32.W))
    val debug_idEx_csrWriteData  = Output(UInt(32.W))
    val debug_idEx_valid         = Output(Bool())
    val debug_idEx_csrWrite      = Output(Bool())
    val debug_idEx_rd            = Output(UInt(5.W))
    val debug_cpu_addr_ro        = Output(Bool())
    val debug_cpu_write_is_noop  = Output(Bool())
    val debug_trap_csr_we        = Output(Bool())
    val debug_idEx_exc_valid = Output(Bool())
    val debug_idEx_exc_code  = Output(UInt(31.W))
    val debug_idEx_mret_valid = Output(Bool())
    val debug_idEx_sret_valid = Output(Bool())
    val debug_idEx_illegal    = Output(Bool())
    val debug_idEx_jump       = Output(Bool())
  })

  import RV32I._

  val nop = "h00000013".U(32.W) // addi x0, x0, 0

  // ============================================================
  // 1. INSTANCIAÇÃO
  // ============================================================
  val instrMem   = Module(new InstructionMemory(
                     depthWords = memoryWords,
                     initialData = initialProgram,
                     programFile = programFile))
  val dataMem    = Module(new DataMemory(depthWords = memoryWords))
  val regFile    = Module(new RegisterFile)
  val immGen     = Module(new ImmGen)
  val controller = Module(new ControllerP3)
  val ula        = Module(new ULA)
  val csr        = Module(new CSR)
  val trap       = Module(new TrapController)

  // ============================================================
  // 2. REGISTRADORES DE PIPELINE
  // ============================================================
  val pcReg     = RegInit(0.U(32.W))
  val ifIdPc    = RegInit(0.U(32.W))
  val ifIdInstr = RegInit(nop)
  val idEx      = RegInit(0.U.asTypeOf(new DecodeExecuteBundle))

  val lwWbValid = RegInit(false.B)
  val lwWbRd    = RegInit(0.U(5.W))
  val lwWbData  = RegInit(0.U(32.W))

  val reservationAddr  = RegInit(0.U(32.W))
  val reservationValid = RegInit(false.B)

  // ============================================================
  // 3. ESTÁGIO IF
  // ============================================================
  instrMem.io.address := pcReg

  // ============================================================
  // 4. ESTÁGIO ID: decodificação, leitura de registradores, CSR
  // ============================================================
  val idRs1 = ifIdInstr(19, 15)
  val idRs2 = ifIdInstr(24, 20)

  immGen.io.instr      := ifIdInstr
  controller.io.opcode := ifIdInstr(6, 0)
  controller.io.funct3 := ifIdInstr(14, 12)
  controller.io.funct7 := ifIdInstr(31, 25)
  controller.io.csrAddress := ifIdInstr(31, 20)
  controller.io.rs1        := idRs1

  regFile.io.rs1 := idRs1
  regFile.io.rs2 := idRs2

  val csrAddressID = ifIdInstr(31, 20)
  csr.io.read_address := csrAddressID

  // ============================================================
  // 5. ESTADO DOS CSRs -> TrapController
  // ============================================================
  trap.io.mstatus  := csr.io.mstatus_read
  trap.io.mepc     := csr.io.mepc_read
  trap.io.mtvec    := csr.io.mtvec_read
  trap.io.mtval    := csr.io.mtval_read
  trap.io.sstatus  := csr.io.sstatus_read
  trap.io.sepc     := csr.io.sepc_read
  trap.io.stvec    := csr.io.stvec_read
  trap.io.stval    := csr.io.stval_read
  trap.io.medeleg  := csr.io.medeleg_read
  trap.io.mideleg  := csr.io.mideleg_read
  trap.io.priv     := csr.io.current_priv
  trap.io.mcause := csr.io.mcause_read
  trap.io.scause := csr.io.scause_read

  // ============================================================
  // 6. ESTÁGIO EX: ULA, branches, CSR forwarding
  // ============================================================
  val exOperandA = WireDefault(idEx.rs1Value)
  val exOperandB = WireDefault(idEx.rs2Value)
  switch(idEx.signals.operandASel) {
    is(OperandASel.PC)   { exOperandA := idEx.pc }
    is(OperandASel.ZERO) { exOperandA := 0.U }
  }
  when(idEx.signals.operandBSel === OperandBSel.IMM) {
    exOperandB := idEx.imm
  }

  ula.io.a  := exOperandA
  ula.io.b  := exOperandB
  ula.io.op := idEx.signals.aluOp

  val jalrTarget       = ula.io.result & "hFFFFFFFE".U(32.W)
  val pcRelativeTarget = (idEx.pc.asSInt + idEx.imm.asSInt).asUInt

  val branchTaken = WireDefault(false.B)
  switch(idEx.signals.branchType) {
    is(BranchType.BEQ)  { branchTaken := idEx.rs1Value === idEx.rs2Value }
    is(BranchType.BNE)  { branchTaken := idEx.rs1Value =/= idEx.rs2Value }
    is(BranchType.BLT)  { branchTaken := idEx.rs1Value.asSInt <  idEx.rs2Value.asSInt }
    is(BranchType.BGE)  { branchTaken := idEx.rs1Value.asSInt >= idEx.rs2Value.asSInt }
    is(BranchType.BLTU) { branchTaken := idEx.rs1Value <  idEx.rs2Value }
    is(BranchType.BGEU) { branchTaken := idEx.rs1Value >= idEx.rs2Value }
  }

  val controlRedirect = idEx.valid && !idEx.signals.illegal &&
    (idEx.signals.jump || (idEx.signals.branchType =/= BranchType.NONE && branchTaken))
  val redirectTarget = Mux(idEx.signals.jalr, jalrTarget, pcRelativeTarget)

  // ============================================================
  // 7. HAZARD DETECTION (load-use)
  // ============================================================
  val currentUsesRd = (idRs1 === idEx.rd && idRs1 =/= 0.U) ||
                      (idRs2 === idEx.rd && idRs2 =/= 0.U)
  val isLoadInEx = idEx.valid &&
                   !idEx.signals.illegal &&
                   idEx.signals.writebackSel === WritebackSel.MEM &&
                   !idEx.signals.memWrite &&
                   idEx.signals.regWrite &&
                   idEx.signals.branchType === BranchType.NONE &&
                   !idEx.signals.jump && !idEx.signals.jalr
  val loadUseHazard = isLoadInEx && currentUsesRd

  val stallPipeline = loadUseHazard

  // ============================================================
  // 8. EXTENSÃO A (LR/SC/AMO)
  // ============================================================
  val isLR  = idEx.valid && !idEx.signals.illegal && idEx.signals.isLR
  val isSC  = idEx.valid && !idEx.signals.illegal && idEx.signals.isSC
  val isAMO = idEx.valid && !idEx.signals.illegal && idEx.signals.isAMO

  when(isLR) {
    reservationAddr  := idEx.memAddress
    reservationValid := true.B
  }

  val scSuccess = WireDefault(false.B)
  when(isSC) {
    scSuccess := reservationValid && (reservationAddr === idEx.memAddress)
    reservationValid := false.B
  }
  when(isAMO) { reservationValid := false.B }

  when(dataMem.io.writeEnable && (idEx.memAddress === reservationAddr)) {
    reservationValid := false.B
  }
  when(io.illegal || idEx.signals.illegal) { reservationValid := false.B }

  // ============================================================
  // 9. FORWARDING (registradores e CSR)
  // ============================================================
  val memReadData = dataMem.io.readData

  val writebackData = WireDefault(ula.io.result)
  switch(idEx.signals.writebackSel) {
    is(WritebackSel.MEM) { writebackData := memReadData }
    is(WritebackSel.PC4) { writebackData := idEx.pc + 4.U }
    is(WritebackSel.IMM) { writebackData := idEx.imm }
    is(WritebackSel.CSR) { writebackData := idEx.csrReadData }
  }
  when(isLR)  { writebackData := memReadData }
  when(isSC)  { writebackData := Mux(scSuccess, 0.U, 1.U) }
  when(isAMO) { writebackData := memReadData }
  when(lwWbValid) { writebackData := lwWbData }

  val writebackEnableRaw = (idEx.valid && !idEx.signals.illegal && idEx.signals.regWrite) || lwWbValid
  val writebackRd        = Mux(lwWbValid, lwWbRd, idEx.rd)

  val csrAddressEX = idEx.csrAddress
  val csrForwarding = idEx.valid && !idEx.signals.illegal &&
                      idEx.signals.csrWrite &&
                      (csrAddressID === csrAddressEX)
  val csrReadData = Mux(csrForwarding, idEx.csrWriteData, csr.io.read_data)

  val forwardedRs1 = Mux(
    writebackEnableRaw && idEx.rd =/= 0.U && idEx.rd === idRs1,
    writebackData, regFile.io.readData1)
  val forwardedRs2 = Mux(
    writebackEnableRaw && idEx.rd =/= 0.U && idEx.rd === idRs2,
    writebackData, regFile.io.readData2)

  val decodedMemAddress = (forwardedRs1.asSInt + immGen.io.imm.asSInt).asUInt

  // ============================================================
  // 10. CSR: dados de escrita e noop Zicsr (C10)
  // ============================================================
  val csrSrc = Mux(controller.io.signals.csrOp(2),
                   ifIdInstr(19, 15), forwardedRs1)

  val csrWriteData = WireDefault(0.U(32.W))
  switch(controller.io.signals.csrOp) {
    is(1.U) { csrWriteData := csrSrc }
    is(2.U) { csrWriteData := csrReadData | csrSrc }
    is(3.U) { csrWriteData := csrReadData & ~csrSrc }
    is(5.U) { csrWriteData := csrSrc }
    is(6.U) { csrWriteData := csrReadData | csrSrc }
    is(7.U) { csrWriteData := csrReadData & ~csrSrc }
  }

  val csrOpIsSetClear = controller.io.signals.csrOp === 2.U ||
                        controller.io.signals.csrOp === 3.U ||
                        controller.io.signals.csrOp === 6.U ||
                        controller.io.signals.csrOp === 7.U
  val csrSrcIsZero    = csrSrc === 0.U
  val csrIsNoop = csrOpIsSetClear && csrSrcIsZero

  // ============================================================
  // 11. RESOLUÇÃO DA CAUSA NO ID
  // ============================================================
  val idExcValid = WireDefault(false.B)
  val idExcCode  = WireDefault(0.U(31.W))
  val idExcTval  = WireDefault(0.U(32.W))
  val idMret     = WireDefault(false.B)
  val idSret     = WireDefault(false.B)

  when(ifIdInstr === ECALL) {
    idExcValid := true.B
    idExcCode  := MuxLookup(csr.io.current_priv, TrapCause.EcallM)(Seq(
      PrivilegeMode.U -> TrapCause.EcallU,
      PrivilegeMode.S -> TrapCause.EcallS,
      PrivilegeMode.M -> TrapCause.EcallM))
    idExcTval  := 0.U
  }.elsewhen(ifIdInstr === EBREAK) {
    idExcValid := true.B
    idExcCode  := TrapCause.Breakpoint
    idExcTval  := ifIdPc
  }.elsewhen(controller.io.signals.illegal) {
    idExcValid := true.B
    idExcCode  := TrapCause.IllegalInstr
    idExcTval  := ifIdInstr
  }.elsewhen(ifIdInstr === MRET) { idMret := true.B
  }.elsewhen(ifIdInstr === SRET) { idSret := true.B }

  // ============================================================
  // 12. EVENTO E COMMIT -> TrapController
  // ============================================================
  trap.io.cause_valid := idEx.valid && idEx.exc_valid
  trap.io.cause_code  := idEx.exc_code
  trap.io.cause_is_int:= false.B
  trap.io.cause_pc    := idEx.pc
  trap.io.cause_tval  := idEx.exc_tval
  trap.io.mret_valid  := idEx.valid && idEx.mret_valid
  trap.io.sret_valid  := idEx.valid && idEx.sret_valid

  trap.io.commit := idEx.valid && !stallPipeline

  // ============================================================
  // 13. FLUSH / REDIRECT — expressão única
  // ============================================================
  val trapRedirect = trap.io.redirect_valid

  // Serialização de CSR/mret/sret
  // flush por branch/jump mais velho: só faz sentido se NÃO há trap pendente
  val branchFlush   = controlRedirect && !trapRedirect
  // flush final (única fonte de verdade)
  val flushPipeline = branchFlush || trapRedirect

  // ============================================================
  // 14. ESCRITA: CSR via trap, via CPU, memória, writeback
  // ============================================================
  csr.io.trap_we      := trap.io.csr_we
  csr.io.trap_mstatus := trap.io.mstatus_w
  csr.io.trap_mepc    := trap.io.mepc_w
  csr.io.trap_mcause  := trap.io.mcause_w
  csr.io.trap_mtval   := trap.io.mtval_w
  csr.io.trap_sstatus := trap.io.sstatus_w
  csr.io.trap_sepc    := trap.io.sepc_w
  csr.io.trap_scause  := trap.io.scause_w
  csr.io.trap_stval   := trap.io.stval_w
  csr.io.trap_new_priv:= trap.io.new_priv
  csr.io.trap_priv_we := trap.io.csr_we

  csr.io.cpu_write_enable  := idEx.valid && !idEx.signals.illegal &&
                              idEx.signals.csrWrite &&
                              !trap.io.csr_we &&              // trap tem prioridade
                              !trap.io.commit_suppress
  csr.io.cpu_write_address := idEx.csrAddress
  csr.io.cpu_write_data    := idEx.csrWriteData
  csr.io.cpu_write_is_noop := idEx.csr_is_noop

  dataMem.io.address     := idEx.memAddress
  dataMem.io.writeData   := idEx.memWriteData
  dataMem.io.writeEnable := idEx.valid && !idEx.signals.illegal &&
                            idEx.signals.memWrite &&
                            !trap.io.commit_suppress
  dataMem.io.memSize      := idEx.signals.memSize
  dataMem.io.unsignedLoad := idEx.signals.memUnsigned

  val writebackEnable = writebackEnableRaw && !trap.io.commit_suppress

  regFile.io.rd        := idEx.rd
  regFile.io.writeData := writebackData
  regFile.io.regWrite  := writebackEnable

  // ============================================================
  // 15. ATUALIZAÇÃO DOS REGISTRADORES DE PIPELINE
  // ============================================================
  val pcNextBase = WireDefault(pcReg + 4.U)
  when(stallPipeline)        { pcNextBase := pcReg }
  .elsewhen(branchFlush)     { pcNextBase := redirectTarget }

  val pcNext = Mux(trapRedirect, trap.io.redirect_pc, pcNextBase)
  pcReg := pcNext

  when(stallPipeline) {
    lwWbValid := true.B
    lwWbRd    := idEx.rd
    lwWbData  := dataMem.io.readData
    idEx      := 0.U.asTypeOf(new DecodeExecuteBundle)
  } .elsewhen(flushPipeline) {
    // inclui branchFlush e trapRedirect
    lwWbValid := false.B
    ifIdPc    := pcReg
    ifIdInstr := nop
    idEx      := 0.U.asTypeOf(new DecodeExecuteBundle)
  } .otherwise {
    lwWbValid := false.B
    ifIdPc    := pcReg
    ifIdInstr := instrMem.io.readData

    idEx.valid        := true.B
    idEx.pc           := ifIdPc
    idEx.instr        := ifIdInstr
    idEx.rs1          := idRs1
    idEx.rs2          := idRs2
    idEx.rd           := ifIdInstr(11, 7)
    idEx.rs1Value     := forwardedRs1
    idEx.rs2Value     := forwardedRs2
    idEx.imm          := immGen.io.imm
    idEx.memAddress   := decodedMemAddress
    idEx.memWriteData := forwardedRs2
    idEx.signals      := controller.io.signals
    idEx.csrAddress   := ifIdInstr(31, 20)
    idEx.csrWriteData := csrWriteData
    idEx.csrReadData  := csrReadData
    idEx.exc_valid   := idExcValid
    idEx.exc_code    := idExcCode
    idEx.exc_tval    := idExcTval
    idEx.mret_valid  := idMret
    idEx.sret_valid  := idSret
    idEx.csr_is_noop := csrIsNoop
  }

  // ============================================================
  // 16. SAÍDAS
  // ============================================================
  io.pc              := pcReg
  io.instr           := ifIdInstr
  io.aluResult       := ula.io.result
  io.writebackData   := writebackData
  io.writebackRd     := writebackRd
  io.writebackEnable := writebackEnable
  io.illegal         := idEx.valid && idEx.signals.illegal
  io.stalled         := stallPipeline

  io.debug_trap_redirect      := trap.io.redirect_valid
  io.debug_trap_to_smode      := trap.io.trap_to_smode
  io.debug_trap_delegate      := trap.io.dbg_delegate
  io.debug_trap_code          := trap.io.dbg_code
  io.debug_trap_is_interrupt  := trap.io.dbg_is_int
  io.debug_trap_redirect_pc   := trap.io.redirect_pc
  io.debug_commit_suppress    := trap.io.commit_suppress

  io.debug_csrReadData    := csrReadData
  io.debug_csrForwarding  := csrForwarding
  io.debug_csr_address_id := csrAddressID
  io.debug_csr_address_ex := csrAddressEX

  io.debug_mepc    := csr.io.mepc_read
  io.debug_mcause  := csr.io.mcause_read
  io.debug_mtvec   := csr.io.mtvec_read
  io.debug_mstatus := csr.io.mstatus_read
  io.debug_priv    := csr.io.current_priv

  io.debug_cpu_write_enable  := csr.io.cpu_write_enable
  io.debug_cpu_write_address := csr.io.cpu_write_address
  io.debug_cpu_write_data    := csr.io.cpu_write_data
  io.debug_idEx_csrWriteData := idEx.csrWriteData
  io.debug_idEx_valid        := idEx.valid
  io.debug_idEx_csrWrite     := idEx.signals.csrWrite
  io.debug_idEx_rd           := idEx.rd
  io.debug_cpu_addr_ro := csr.io.debug_cpu_addr_ro
  io.debug_cpu_write_is_noop := csr.io.debug_cpu_write_is_noop
  io.debug_trap_csr_we := trap.io.csr_we
  io.debug_idEx_exc_valid := idEx.exc_valid
  io.debug_idEx_exc_code  := idEx.exc_code
  io.debug_idEx_mret_valid := idEx.mret_valid
  io.debug_idEx_sret_valid := idEx.sret_valid
  io.debug_idEx_illegal    := idEx.signals.illegal
  io.debug_idEx_jump       := idEx.signals.jump
}