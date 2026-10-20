package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos._
import riscv._

// ============================================================
// BUNDLE: registradores do estágio ID/EX
// ============================================================
class DecodeExecuteBundle extends Bundle {
  val valid        = Bool()
  val pc           = UInt(32.W)
  val instr        = UInt(32.W)
  val rs1          = UInt(5.W)
  val rs2          = UInt(5.W)
  val rd           = UInt(5.W)
  val rs1Value     = UInt(32.W)
  val rs2Value     = UInt(32.W)
  val imm          = UInt(32.W)
  val memAddress   = UInt(32.W)
  val memWriteData = UInt(32.W)
  val signals      = new ControlSignals
  val csrAddress   = UInt(12.W)
  val csrWriteData = UInt(32.W)
  val csrReadData  = UInt(32.W)
}

/** Pipeline RV32I educacional de 3 estágios, inspirado no Wildcat.
  *
  * Estágios:
  *   - IF:       busca de instrução (instrMem + pcReg)
  *   - ID/RF:    decodificação, leitura do banco de registradores e preparação
  *   - EX/MEM/WB: execução (ULA), acesso à memória (via MMU) e writeback
  *
  * Características:
  *   - memórias de instrução e dados internas;
  *   - forwarding do resultado do estágio EX para o ID;
  *   - stall de um ciclo para load-use hazard (com registrador de WB);
  *   - suporte a CSRs, CLINT e extensão A (LR/SC e AMOs);
  *   - MMU Sv32 integrada ao acesso à memória;
  *   - Timer periférico (MMIO) em 0x80000000.
  */
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

    // Debug do CLINT
    val debug_clint_trap_assert       = Output(Bool())
    val debug_clint_trap_to_smode     = Output(Bool())
    val debug_clint_delegate_to_smode = Output(Bool())
    val debug_clint_exception_code    = Output(UInt(31.W))
    val debug_clint_is_interrupt      = Output(Bool())
    val debug_clint_trap_address      = Output(UInt(32.W))

    // Debug do CSR forwarding
    val debug_csrReadData    = Output(UInt(32.W))
    val debug_csrForwarding  = Output(Bool())
    val debug_csr_address_id = Output(UInt(12.W))
    val debug_csr_address_ex = Output(UInt(12.W))

    // Debug da MMU
    val debug_mmu_physical_addr = Output(UInt(32.W))
    val debug_mmu_page_fault    = Output(Bool())
    val debug_mmu_fault_cause   = Output(UInt(4.W))
    val debug_mmu_fault_addr    = Output(UInt(32.W))

    // Debug do Timer
    val debug_timer_limit     = Output(UInt(32.W))
    val debug_timer_enabled   = Output(Bool())
    val debug_timer_interrupt = Output(Bool())
  })

  import RV32I._

  val nop = "h00000013".U(32.W)  // addi x0, x0, 0

  // ============================================================
  // 1. INSTANCIAÇÃO DOS MÓDULOS
  // ============================================================
  val instrMem   = Module(new InstructionMemory(
                     depthWords = memoryWords,
                     initialData = initialProgram,
                     programFile = programFile))
  val dataMem    = Module(new DataMemory(depthWords = memoryWords))
  val regFile    = Module(new RegisterFile)
  val immGen     = Module(new ImmGen)
  val controller = Module(new Controller)
  val ula        = Module(new ULA)
  val csr        = Module(new CSR)
  val clint      = Module(new CLINT)
  val mmu        = Module(new MMU)
  val timer      = Module(new Timer)

  // ============================================================
  // 2. REGISTRADORES DE PIPELINE
  // ============================================================
  val pcReg     = RegInit(0.U(32.W))
  val ifIdPc    = RegInit(0.U(32.W))
  val ifIdInstr = RegInit(nop)
  val idEx      = RegInit(0.U.asTypeOf(new DecodeExecuteBundle))

  // Registrador de WB para o load durante stall
  val lwWbValid = RegInit(false.B)
  val lwWbRd    = RegInit(0.U(5.W))
  val lwWbData  = RegInit(0.U(32.W))

  // Mecanismo de reserva para LR/SC
  val reservationAddr  = RegInit(0.U(32.W))
  val reservationValid = RegInit(false.B)

  // ============================================================
  // 3. SINAIS DE CONTROLE DE FLUXO
  // ============================================================
  val stallPipeline = WireDefault(false.B)
  val flushPipeline = WireDefault(false.B)

  // Sinais da extensão A (dependem apenas do idEx)
  val isLR  = idEx.valid && !idEx.signals.illegal && idEx.signals.isLR
  val isSC  = idEx.valid && !idEx.signals.illegal && idEx.signals.isSC
  val isAMO = idEx.valid && !idEx.signals.illegal && idEx.signals.isAMO

  // ============================================================
  // 4. CONEXÃO CLINT <-> CSR
  // ============================================================
  clint.io.mstatus := csr.io.mstatus_read
  clint.io.mepc    := csr.io.mepc_read
  clint.io.mcause  := csr.io.mcause_read
  clint.io.mtvec   := csr.io.mtvec_read
  clint.io.mie     := csr.io.mie_read
  clint.io.mtval   := csr.io.mtval_read

  clint.io.sstatus := csr.io.sstatus_read
  clint.io.sepc    := csr.io.sepc_read
  clint.io.scause  := csr.io.scause_read
  clint.io.stvec   := csr.io.stvec_read
  clint.io.sie     := csr.io.sie_read
  clint.io.stval   := csr.io.stval_read

  clint.io.medeleg      := csr.io.medeleg_read
  clint.io.mideleg      := csr.io.mideleg_read
  clint.io.current_priv := csr.io.current_priv

  // ============================================================
  // 5. INTERRUPÇÕES (Timer)
  // ============================================================
  val timer_interrupt = timer.io.signal_interrupt

  val interrupt_flag = WireDefault(InterruptCode.None)
  when(timer_interrupt) {
    interrupt_flag := InterruptCode.Timer0
  }

  clint.io.pc             := pcReg
  clint.io.instr          := ifIdInstr
  clint.io.valid          := !stallPipeline && !flushPipeline
  clint.io.interrupt_flag := interrupt_flag

  // ============================================================
  // 6. CONEXÃO CLINT -> CSR (escrita com prioridade)
  // ============================================================
  csr.io.clint_write_enable  := clint.io.direct_write_enable
  csr.io.clint_mstatus_write := clint.io.mstatus_write_data
  csr.io.clint_mepc_write    := clint.io.mepc_write_data
  csr.io.clint_mcause_write  := clint.io.mcause_write_data
  csr.io.clint_mtval_write   := clint.io.mtval_write_data
  csr.io.clint_sepc_write    := clint.io.sepc_write_data
  csr.io.clint_scause_write  := clint.io.scause_write_data
  csr.io.clint_stval_write   := clint.io.stval_write_data
  csr.io.clint_sstatus_write := clint.io.sstatus_write_data

  // ============================================================
  // 7. ESTÁGIO IF: busca de instrução
  // ============================================================
  instrMem.io.address := pcReg

  // ============================================================
  // 8. ESTÁGIO ID: decodificação e leitura do banco de registradores
  // ============================================================
  val idRs1 = ifIdInstr(19, 15)
  val idRs2 = ifIdInstr(24, 20)

  immGen.io.instr      := ifIdInstr
  controller.io.opcode := ifIdInstr(6, 0)
  controller.io.funct3 := ifIdInstr(14, 12)
  controller.io.funct7 := ifIdInstr(31, 25)

  regFile.io.rs1 := idRs1
  regFile.io.rs2 := idRs2

  // CSR: endereço lido pelo ID
  val csrAddressID = ifIdInstr(31, 20)
  csr.io.read_address := csrAddressID

  // ============================================================
  // 9. ESTÁGIO EX: ULA, branches, CSR forwarding e writeback data
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

  // Endereços de desvio
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
  // 10. MMU: TRADUÇÃO DE ENDEREÇO VIRTUAL
  // ============================================================
  // Entradas da MMU
  mmu.io.satp         := csr.io.satp_read
  mmu.io.sstatus      := csr.io.sstatus_read
  mmu.io.current_priv := csr.io.current_priv

  // Endereço virtual (do estágio EX)
  mmu.io.virtual_addr := idEx.memAddress

  // Tipo de acesso: STORE, LOAD ou FETCH
  val access_type = Mux(idEx.signals.memWrite, AccessType.STORE,
                    Mux(idEx.signals.writebackSel === WritebackSel.MEM, AccessType.LOAD,
                        AccessType.FETCH))
  mmu.io.access_type := access_type

  // Endereço físico (após tradução)
  val physical_addr = mmu.io.physical_addr

  // ============================================================
  // 11. HAZARD DETECTION (load-use)
  // ============================================================
  val currentUsesRd = (idRs1 === idEx.rd && idRs1 =/= 0.U) ||
                      (idRs2 === idEx.rd && idRs2 =/= 0.U)
  val isLoadInEx = idEx.valid &&
                   !idEx.signals.illegal &&
                   idEx.signals.writebackSel === WritebackSel.MEM &&
                   !idEx.signals.memWrite &&
                   idEx.signals.regWrite &&
                   idEx.signals.branchType === BranchType.NONE &&
                   !idEx.signals.jump &&
                   !idEx.signals.jalr
  val loadUseHazard = isLoadInEx && currentUsesRd
  val hazardDetected = loadUseHazard

  stallPipeline := hazardDetected
  flushPipeline := controlRedirect

  // ============================================================
  // 12. CONEXÃO CLINT <-> MMU (page faults)
  // ============================================================
  clint.io.mmu_fault_valid := mmu.io.page_fault
  clint.io.mmu_fault_cause := mmu.io.fault_cause
  clint.io.mmu_fault_addr  := mmu.io.fault_addr

  // ============================================================
  // 13. ACESSO À MEMÓRIA (com MMU e Timer)
  // ============================================================
  // Decodificação do endereço do Timer (MMIO)
  val TIMER_BASE = Parameters.TimerBaseAddress.U(32.W)
  val isTimerAddress = idEx.memAddress(31, 12) === TIMER_BASE(31, 12)

  // Endereço da memória: page walker da MMU ou CPU
  val mem_addr = Mux(mmu.io.mem_read_en, mmu.io.mem_read_addr, physical_addr)
  dataMem.io.address := mem_addr

  when(idEx.valid && idEx.signals.writebackSel === WritebackSel.MEM) {
    printf(p"[LW] vaddr=0x${idEx.memAddress}%, paddr=0x${physical_addr}%, " +
          p"isTimer=${isTimerAddress}%, mmu_fault=${mmu.io.page_fault}%, " +
          p"memReadEn=${mmu.io.mem_read_en}%\n")
  }

  // Dados lidos retornam para a MMU (page walker)
  mmu.io.mem_read_data := dataMem.io.readData

  // Sinais de escrita
  dataMem.io.writeData   := idEx.memWriteData
  dataMem.io.writeEnable := idEx.valid && 
                            !idEx.signals.illegal && 
                            idEx.signals.memWrite && 
                            !mmu.io.page_fault && 
                            !isTimerAddress
  dataMem.io.memSize     := idEx.signals.memSize
  dataMem.io.unsignedLoad := idEx.signals.memUnsigned

  // Conexão do Timer ao barramento MMIO
  timer.io.bundle.address      := idEx.memAddress
  timer.io.bundle.write_data   := idEx.memWriteData
  timer.io.bundle.write_enable := idEx.valid && 
                                  !idEx.signals.illegal && 
                                  idEx.signals.memWrite && 
                                  isTimerAddress

  // Mux de leitura: memória ou Timer
  val memReadData = Mux(isTimerAddress, 
                        timer.io.bundle.read_data, 
                        dataMem.io.readData)

  // ============================================================
  // 14. EXTENSÃO A: LR/SC e AMOs
  // ============================================================
  // LR/SC: sucesso do SC
  val scSuccess = WireDefault(false.B)
  when(isSC) {
    scSuccess := reservationValid && (reservationAddr === idEx.memAddress)
  }

  // LR: estabelece reserva
  when(isLR) {
    reservationAddr  := idEx.memAddress
    reservationValid := true.B
  }

  // SC: invalida reserva
  when(isSC) {
    reservationValid := false.B
  }

  // AMO: invalida reserva
  when(isAMO) {
    reservationValid := false.B
  }

  // Qualquer escrita no endereço reservado invalida a reserva
  when(dataMem.io.writeEnable && (idEx.memAddress === reservationAddr)) {
    reservationValid := false.B
  }

  // Interrupções invalidam a reserva
  when(io.illegal || idEx.signals.illegal) {
    reservationValid := false.B
  }

  // ============================================================
  // 15. FORWARDING (registradores e CSR)
  // ============================================================
  // 15a. Writeback data (prioridade: lwWb > LR/SC/AMO > writebackSel > ULA)
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

  // 15b. Writeback enable/rd
  val writebackEnable = (idEx.valid && !idEx.signals.illegal && 
                         idEx.signals.regWrite && !mmu.io.page_fault) || lwWbValid
  val writebackRd     = Mux(lwWbValid, lwWbRd, idEx.rd)

  // when(clint.io.trap_assert) {
  //   printf(p"[TRAP] assert=1, address=0x${clint.io.trap_address}%, " +
  //         p"cause=0x${clint.io.trap_cause}%\n")
  // }

  // 15c. CSR forwarding
  val csrAddressEX = idEx.csrAddress
  val csrForwarding = idEx.valid && !idEx.signals.illegal &&
                      idEx.signals.csrWrite &&
                      (csrAddressID === csrAddressEX)
  val csrReadData = Mux(csrForwarding, idEx.csrWriteData, csr.io.read_data)

  // 15d. Forwarding de registradores (EX -> ID)
  val forwardedRs1 = Mux(
    writebackEnable && idEx.rd =/= 0.U && idEx.rd === idRs1,
    writebackData,
    regFile.io.readData1
  )
  val forwardedRs2 = Mux(
    writebackEnable && idEx.rd =/= 0.U && idEx.rd === idRs2,
    writebackData,
    regFile.io.readData2
  )

  // Endereço de memória calculado no ID
  val decodedMemAddress = (forwardedRs1.asSInt + immGen.io.imm.asSInt).asUInt

  // ============================================================
  // 16. CSR: dados a serem escritos e conexão CPU -> CSR
  // ============================================================
  val csrSrc = Mux(
    controller.io.signals.csrOp(2),
    ifIdInstr(19, 15),
    forwardedRs1
  )

  val csrWriteData = WireDefault(0.U(32.W))
  switch(controller.io.signals.csrOp) {
    is(1.U) { csrWriteData := csrSrc }
    is(2.U) { csrWriteData := csrReadData | csrSrc }
    is(3.U) { csrWriteData := csrReadData & ~csrSrc }
    is(5.U) { csrWriteData := csrSrc }
    is(6.U) { csrWriteData := csrReadData | csrSrc }
    is(7.U) { csrWriteData := csrReadData & ~csrSrc }
  }

  csr.io.cpu_write_enable  := idEx.valid && !idEx.signals.illegal &&
                              idEx.signals.csrWrite &&
                              !clint.io.direct_write_enable
  csr.io.cpu_write_address := idEx.csrAddress
  csr.io.cpu_write_data    := idEx.csrWriteData

  // ============================================================
  // 17. CONEXÃO AO BANCO DE REGISTRADORES (writeback)
  // ============================================================
  regFile.io.rd        := idEx.rd
  regFile.io.writeData := writebackData
  regFile.io.regWrite  := writebackEnable

  // ============================================================
  // 18. ATUALIZAÇÃO DOS REGISTRADORES DE PIPELINE
  // ============================================================
  val pcNext = WireDefault(pcReg + 4.U)
  when(stallPipeline) {
    pcNext := pcReg
  } .elsewhen(clint.io.trap_assert) {
    pcNext := clint.io.redirect_address  // ← page fault ou trap
  } .elsewhen(controlRedirect) {
    pcNext := redirectTarget
  }

  pcReg := pcNext

  when(stallPipeline) {
    // STALL: IF/ID congelados, lw avança para o registrador de WB
    lwWbValid := true.B
    lwWbRd    := idEx.rd
    lwWbData  := dataMem.io.readData
    idEx      := 0.U.asTypeOf(new DecodeExecuteBundle)
  } .elsewhen(clint.io.trap_assert) {
    // TRAP (page fault ou exceção): limpa o pipeline
    lwWbValid := false.B
    ifIdPc    := pcReg
    ifIdInstr := nop
    idEx      := 0.U.asTypeOf(new DecodeExecuteBundle)
  } .elsewhen(clint.io.redirect_assert) {
    // MRET/SRET: redireciona PC, mas NÃO faz flush
    lwWbValid := false.B
    ifIdPc    := pcReg
    ifIdInstr := nop
  } .elsewhen(flushPipeline || controlRedirect) {
    // FLUSH: insere NOP no IF/ID e limpa o ID/EX
    lwWbValid := false.B
    ifIdPc    := pcReg
    ifIdInstr := nop
    idEx      := 0.U.asTypeOf(new DecodeExecuteBundle)
  } .otherwise {
    // ATUALIZAÇÃO NORMAL
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
  }

  // ============================================================
  // 19. SAÍDAS
  // ============================================================
  io.pc              := pcReg
  io.instr           := ifIdInstr
  io.aluResult       := ula.io.result
  io.writebackData   := writebackData
  io.writebackRd     := writebackRd
  io.writebackEnable := writebackEnable
  io.illegal         := idEx.valid && idEx.signals.illegal
  io.stalled         := stallPipeline

  // Debug do CLINT
  io.debug_clint_trap_assert       := clint.io.debug_trap_assert
  io.debug_clint_trap_to_smode     := clint.io.debug_trap_to_smode
  io.debug_clint_delegate_to_smode := clint.io.debug_delegate_to_smode
  io.debug_clint_exception_code    := clint.io.debug_exception_code
  io.debug_clint_is_interrupt      := clint.io.debug_is_interrupt
  io.debug_clint_trap_address      := clint.io.debug_trap_address

  // Debug do CSR forwarding
  io.debug_csrReadData    := csrReadData
  io.debug_csrForwarding  := csrForwarding
  io.debug_csr_address_id := csrAddressID
  io.debug_csr_address_ex := csrAddressEX

  // Debug da MMU
  io.debug_mmu_physical_addr := mmu.io.physical_addr
  io.debug_mmu_page_fault    := mmu.io.page_fault
  io.debug_mmu_fault_cause   := mmu.io.fault_cause
  io.debug_mmu_fault_addr    := mmu.io.fault_addr

  // Debug do Timer
  io.debug_timer_limit     := timer.io.debug_limit
  io.debug_timer_enabled   := timer.io.debug_enabled
  io.debug_timer_interrupt := timer.io.signal_interrupt
}