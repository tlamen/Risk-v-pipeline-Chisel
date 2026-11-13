package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos._
import riscv._

// IF/ID
class IfIdBundle extends Bundle {
  val pc    = UInt(32.W)
  val instr = UInt(32.W)
  val valid = Bool()
}

// ID/EX
class IdExBundle extends Bundle {
  val valid       = Bool()
  val pc          = UInt(32.W)
  val instr       = UInt(32.W)
  val rs1         = UInt(5.W)
  val rs2         = UInt(5.W)
  val rd          = UInt(5.W)
  val rs1Value    = UInt(32.W)
  val rs2Value    = UInt(32.W)
  val imm         = UInt(32.W)
  val signals     = new ControlSignalsP5
  val csrAddress  = UInt(12.W)
  val csrWriteData = UInt(32.W)
  val csrReadData  = UInt(32.W)
  val csrIsNoop    = Bool()
  // Evento de trap resolvido no ID e propagado ao commit
  val exc_valid   = Bool()
  val exc_code    = UInt(31.W)
  val exc_tval    = UInt(32.W)
  val mret_valid  = Bool()
  val sret_valid  = Bool()
}

// EX/MEM
class ExMemBundle extends Bundle {
  val valid       = Bool()
  val pc          = UInt(32.W)
  val instr       = UInt(32.W)
  val rd          = UInt(5.W)
  val aluResult   = UInt(32.W)
  val rs2Value    = UInt(32.W)     // valor do rs2 para store
  val memAddress  = UInt(32.W)
  val signals     = new ControlSignalsP5
  val csrAddress  = UInt(12.W)
  val csrWriteData = UInt(32.W)
  val csrReadData  = UInt(32.W)
  // Evento de trap (propagado)
  val exc_valid   = Bool()
  val exc_code    = UInt(31.W)
  val exc_tval    = UInt(32.W)
  val mret_valid  = Bool()
  val sret_valid  = Bool()
}

// MEM/WB
class MemWbBundle extends Bundle {
  val valid       = Bool()
  val rd          = UInt(5.W)
  val writebackSel = UInt(3.W)
  val aluResult   = UInt(32.W)
  val memReadData = UInt(32.W)
  val pcPlus4     = UInt(32.W)
  val imm         = UInt(32.W)
  val csrReadData = UInt(32.W)
}

class Pipeline5(
    initialProgram: Seq[Long] = Seq.empty,
    memoryWords: Int = 1024
    programFile: String = ""
) extends Module {
  val io = IO(new Bundle {
    val pc              = Output(UInt(32.W))
    val instr           = Output(UInt(32.W))
    val writebackData   = Output(UInt(32.W))
    val writebackRd     = Output(UInt(5.W))
    val writebackEnable = Output(Bool())
    val illegal         = Output(Bool())
    val stalled         = Output(Bool())

    // Debug de estado observável
    val debug_mepc    = Output(UInt(32.W))
    val debug_mcause  = Output(UInt(32.W))
    val debug_mtvec   = Output(UInt(32.W))
    val debug_mstatus = Output(UInt(32.W))
    val debug_priv    = Output(UInt(2.W))
  })

  import RV32I._
  val nop = "h00000013".U(32.W)

  // ============================================================
  // 1. Instanciação
  // ============================================================
  val instrMem   = Module(new InstructionMemory(
                            depthWords = memoryWords,
                            initialData = initialProgram,
                            programFile = programFile))
  val dataMem    = Module(new DataMemory(depthWords = memoryWords))
  val regFile    = Module(new RegisterFile)
  val immGen     = Module(new ImmGen)
  val controller = Module(new ControllerP5)
  val ula        = Module(new ULA)
  val csr        = Module(new CSR)
  val trap       = Module(new TrapController)

  // ============================================================
  // 2. Registradores de Pipeline
  // ============================================================
  val pcReg    = RegInit(0.U(32.W))
  val ifId     = RegInit(0.U.asTypeOf(new IfIdBundle))
  val idEx     = RegInit(0.U.asTypeOf(new IdExBundle))
  val exMem    = RegInit(0.U.asTypeOf(new ExMemBundle))
  val memWb    = RegInit(0.U.asTypeOf(new MemWbBundle))

  // ============================================================
  // 3. IF: fetch da instrução
  // ============================================================
  instrMem.io.address := pcReg
  val ifInstr = instrMem.io.readData

  // ============================================================
  // 4. ID: decodificação, leitura de registradores, CSR
  // ============================================================
  val idRs1 = ifId.instr(19, 15)
  val idRs2 = ifId.instr(24, 20)

  immGen.io.instr      := ifId.instr
  controller.io.opcode := ifId.instr(6, 0)
  controller.io.funct3 := ifId.instr(14, 12)
  controller.io.funct7 := ifId.instr(31, 25)
  controller.io.csrAddress := ifId.instr(31, 20)
  controller.io.rs1        := idRs1

  regFile.io.rs1 := idRs1
  regFile.io.rs2 := idRs2

  val csrAddressID = ifId.instr(31, 20)
  csr.io.read_address := csrAddressID

  // ============================================================
  // 5. Resolução da causa de trap no ID
  // ============================================================
  val idExcValid = WireDefault(false.B)
  val idExcCode  = WireDefault(0.U(31.W))
  val idExcTval  = WireDefault(0.U(32.W))
  val idMret     = WireDefault(false.B)
  val idSret     = WireDefault(false.B)

  when(ifId.instr === ECALL) {
    idExcValid := true.B
    idExcCode  := TrapCause.EcallM
    idExcTval  := 0.U
  }.elsewhen(ifId.instr === EBREAK) {
    idExcValid := true.B
    idExcCode  := TrapCause.Breakpoint
    idExcTval  := ifId.pc
  }.elsewhen(controller.io.signals.illegal) {
    idExcValid := true.B
    idExcCode  := TrapCause.IllegalInstr
    idExcTval  := ifId.instr
  }.elsewhen(ifId.instr === MRET) { idMret := true.B
  }.elsewhen(ifId.instr === SRET) { idSret := true.B }

  // ============================================================
  // 6. Forwarding para o ID
  // O valor lido do regfile é potencialmente sobrescrito pelo WB (memWb)
  // e pelo MEM (exMem), se houver dependência.
  // ============================================================
  val wbData = WireDefault(0.U(32.W))
  switch(memWb.writebackSel) {
    is(WritebackSel.ALU) { wbData := memWb.aluResult }
    is(WritebackSel.MEM) { wbData := memWb.memReadData }
    is(WritebackSel.PC4) { wbData := memWb.pcPlus4 }
    is(WritebackSel.IMM) { wbData := memWb.imm }
    is(WritebackSel.CSR) { wbData := memWb.csrReadData }
  }

  val idRs1Value = WireDefault(regFile.io.readData1)
  val idRs2Value = WireDefault(regFile.io.readData2)

  // Prioridade: MEM > WB (mais novo vence)
  when(memWb.valid && memWb.rd =/= 0.U && memWb.rd === idRs1) { idRs1Value := wbData }
  when(memWb.valid && memWb.rd =/= 0.U && memWb.rd === idRs2) { idRs2Value := wbData }

  // O forwarding de EX/MEM para o ID é feito no EX; aqui
  // só cobrimos o caso em que a instrução em ID depende de uma instrução
  // que já está em MEM ou WB.

  // ============================================================
  // 7. CSR forwarding (leitura do CSR pelo ID considera a escrita em EX/MEM)
  // ============================================================
  val csrForwarding = exMem.valid && !exMem.signals.illegal &&
                      exMem.signals.csrWrite &&
                      (csrAddressID === exMem.csrAddress)
  val csrReadData = Mux(csrForwarding, exMem.csrWriteData, csr.io.read_data)

  // Cálculo de csrSrc e csrWriteData (no ID, como no Pipeline3)
  val csrSrc = Mux(controller.io.signals.csrOp(2), ifId.instr(19, 15), idRs1Value)

  val csrWriteData = WireDefault(0.U(32.W))
  switch(controller.io.signals.csrOp) {
    is(1.U) { csrWriteData := csrSrc }
    is(2.U) { csrWriteData := csrReadData | csrSrc }
    is(3.U) { csrWriteData := csrReadData & ~csrSrc }
    is(5.U) { csrWriteData := csrSrc }
    is(6.U) { csrWriteData := csrReadData | csrSrc }
    is(7.U) { csrWriteData := csrReadData & ~csrSrc }
  }

  val csrOpSetClr = controller.io.signals.csrOp === 2.U || controller.io.signals.csrOp === 3.U ||
                    controller.io.signals.csrOp === 6.U || controller.io.signals.csrOp === 7.U
  val csrIsNoop = csrOpSetClr && (csrSrc === 0.U)

  // ============================================================
  // 8. ---------- Hazard detection (load-use) ----------
  // ============================================================
  val idUsesRs1 = idRs1 =/= 0.U
  val idUsesRs2 = idRs2 =/= 0.U
  val exIsLoad  = idEx.valid && !idEx.signals.illegal &&
                  idEx.signals.writebackSel === WritebackSel.MEM &&
                  !idEx.signals.memWrite && idEx.signals.regWrite
  val exRd = idEx.rd

  val loadUseHazard =
    (idEx.valid && !idEx.signals.illegal &&
     idEx.signals.writebackSel === WritebackSel.MEM &&
     !idEx.signals.memWrite &&
     idEx.signals.regWrite) &&
    ((exRd === idRs1 && idUsesRs1) || (exRd === idRs2 && idUsesRs2))

  val stallPipeline = loadUseHazard

  // ============================================================
  // 9. Forwarding para EX: EX/MEM e MEM/WB → operandos do EX
  // ============================================================
  val exRs1Value = WireDefault(idEx.rs1Value)
  val exRs2Value = WireDefault(idEx.rs2Value)

  // Prioridade: EX/MEM (mais novo) > MEM/WB
  when(exMem.valid && exMem.rd =/= 0.U &&
       exMem.rd === idEx.rs1 && !exMem.signals.illegal &&
       exMem.signals.regWrite && !(exMem.signals.memWrite && exMem.signals.writebackSel === WritebackSel.MEM)) {
    exRs1Value := exMem.aluResult
  }
  when(exMem.valid && exMem.rd =/= 0.U &&
       exMem.rd === idEx.rs2 && !exMem.signals.illegal &&
       exMem.signals.regWrite && !(exMem.signals.memWrite && exMem.signals.writebackSel === WritebackSel.MEM)) {
    exRs2Value := exMem.aluResult
  }
  when(memWb.valid && memWb.rd =/= 0.U &&
       memWb.rd === idEx.rs1 && !(exMem.valid && exMem.rd === idEx.rs1)) {
    exRs1Value := wbData
  }
  when(memWb.valid && memWb.rd =/= 0.U &&
       memWb.rd === idEx.rs2 && !(exMem.valid && exMem.rd === idEx.rs2)) {
    exRs2Value := wbData
  }

  // ============================================================
  // 10. EX: execução da instrução na ULA e cálculo de desvio
  // ============================================================
  val exOperandA = WireDefault(exRs1Value)
  val exOperandB = WireDefault(exRs2Value)
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

  val exAluResult = ula.io.result

  // Cálculo de desvio (resolvido no EX, mas efetivado no MEM, ver abaixo)
  val jalrTarget       = exAluResult & "hFFFFFFFE".U(32.W)
  val pcRelativeTarget = (idEx.pc.asSInt + idEx.imm.asSInt).asUInt

  val branchTaken = WireDefault(false.B)
  switch(idEx.signals.branchType) {
    is(BranchType.BEQ)  { branchTaken := exRs1Value === exRs2Value }
    is(BranchType.BNE)  { branchTaken := exRs1Value =/= exRs2Value }
    is(BranchType.BLT)  { branchTaken := exRs1Value.asSInt <  exRs2Value.asSInt }
    is(BranchType.BGE)  { branchTaken := exRs1Value.asSInt >= exRs2Value.asSInt }
    is(BranchType.BLTU) { branchTaken := exRs1Value <  exRs2Value }
    is(BranchType.BGEU) { branchTaken := exRs1Value >= exRs2Value }
  }

  // ============================================================
  // 11. MEM (ponto de commit)
  // ============================================================
  val memAddress = exMem.memAddress

  dataMem.io.address     := memAddress
  dataMem.io.writeData   := exMem.rs2Value
  dataMem.io.writeEnable := exMem.valid && !exMem.signals.illegal &&
                            exMem.signals.memWrite &&
                            !trap.io.commit_suppress
  dataMem.io.memSize     := exMem.signals.memSize
  dataMem.io.unsignedLoad := exMem.signals.memUnsigned

  // 11.1 CSRs -> TrapController; resolução da causa do evento e commit 
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


  trap.io.cause_valid := exMem.valid && exMem.exc_valid
  trap.io.cause_code  := exMem.exc_code
  trap.io.cause_is_int:= false.B
  trap.io.cause_pc    := exMem.pc
  trap.io.cause_tval  := exMem.exc_tval
  trap.io.mret_valid  := exMem.valid && exMem.mret_valid
  trap.io.sret_valid  := exMem.valid && exMem.sret_valid

  trap.io.commit := exMem.valid && !stallPipeline

  // Redirecionamento: trap no MEM tem prioridade sobre branch/jump
  val trapRedirect = trap.io.redirect_valid

  // Branch/jump só é efetivado se não houver trap pendente no MEM
  val branchRedirect = exMem.valid && !exMem.signals.illegal &&
                       (exMem.signals.jump ||
                        (exMem.signals.branchType =/= BranchType.NONE && exMem.branchTaken))

  // 11.2 PC e flush
  val pcNextBase = WireDefault(pcReg + 4.U)
  when(stallPipeline) { pcNextBase := pcReg }
  .elsewhen(branchRedirect && !trapRedirect) {
    pcNextBase := Mux(exMem.signals.jalr, exMem.jalrTarget, exMem.pcRelativeTarget)
  }
  val pcNext = Mux(trapRedirect, trap.io.redirect_pc, pcNextBase)

  val flushPipeline = trapRedirect || (branchRedirect && !trapRedirect)

  // 11.3 Escrita de CSR (CPU + trap)
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

  csr.io.cpu_write_enable  := exMem.valid && !exMem.signals.illegal &&
                              exMem.signals.csrWrite &&
                              !trap.io.csr_we && !trap.io.commit_suppress
  csr.io.cpu_write_address := exMem.csrAddress
  csr.io.cpu_write_data    := exMem.csrWriteData
  csr.io.cpu_write_is_noop := exMem.csrIsNoop

  // ============================================================
  // 12. WB 
  // ============================================================
  val wbMemData = memWb.memReadData
  val wbDataFinal = WireDefault(wbData)
  // (wbData já foi calculado acima a partir de memWb)
  regFile.io.rd        := memWb.rd
  regFile.io.writeData := wbDataFinal
  regFile.io.regWrite  := memWb.valid && !trap.io.commit_suppress &&
                          memWb.writebackSel =/= 0.U

  // ============================================================
  // 13. Atualização dos registradores de pipeline 
  // ============================================================
  pcReg := pcNext

  // IF/ID
  when(stallPipeline || flushPipeline) {
    ifId.valid := false.B
  } .otherwise {
    ifId.valid := true.B
    ifId.pc    := pcReg
    ifId.instr := ifInstr
  }

  // ID/EX
  when(stallPipeline || flushPipeline) {
    idEx.valid := false.B
    idEx.exc_valid := false.B
    idEx.mret_valid := false.B
    idEx.sret_valid := false.B
  } .otherwise {
    idEx.valid        := ifId.valid
    idEx.pc           := ifId.pc
    idEx.instr        := ifId.instr
    idEx.rs1          := idRs1
    idEx.rs2          := idRs2
    idEx.rd           := ifId.instr(11, 7)
    idEx.rs1Value     := idRs1Value
    idEx.rs2Value     := idRs2Value
    idEx.imm          := immGen.io.imm
    idEx.signals      := controller.io.signals
    idEx.csrAddress   := ifId.instr(31, 20)
    idEx.csrWriteData := csrWriteData
    idEx.csrReadData  := csrReadData
    idEx.csrIsNoop    := csrIsNoop
    idEx.exc_valid    := idExcValid
    idEx.exc_code     := idExcCode
    idEx.exc_tval     := idExcTval
    idEx.mret_valid   := idMret
    idEx.sret_valid   := idSret
  }

  // EX/MEM
  when(stallPipeline) {
    exMem.valid := false.B
  } .otherwise {
    exMem.valid       := idEx.valid
    exMem.pc          := idEx.pc
    exMem.instr       := idEx.instr
    exMem.rd          := idEx.rd
    exMem.aluResult   := exAluResult
    exMem.rs2Value    := exRs2Value
    exMem.memAddress  := exAluResult
    exMem.signals     := idEx.signals
    exMem.csrAddress  := idEx.csrAddress
    exMem.csrWriteData := idEx.csrWriteData
    exMem.csrReadData := idEx.csrReadData
    exMem.csrIsNoop   := idEx.csrIsNoop
    exMem.exc_valid   := idEx.exc_valid
    exMem.exc_code    := idEx.exc_code
    exMem.exc_tval    := idEx.exc_tval
    exMem.mret_valid  := idEx.mret_valid
    exMem.sret_valid  := idEx.sret_valid
    exMem.branchTaken := branchTaken
    exMem.jalrTarget  := jalrTarget
    exMem.pcRelativeTarget := pcRelativeTarget
  }

  // MEM/WB
  memWb.valid       := exMem.valid
  memWb.rd          := exMem.rd
  memWb.writebackSel := exMem.signals.writebackSel
  memWb.aluResult   := exMem.aluResult
  memWb.memReadData := dataMem.io.readData
  memWb.pcPlus4     := exMem.pc + 4.U
  memWb.imm         := exMem.signals.imm?  // não existe; usar exMem.aluResult
  memWb.csrReadData := exMem.csrReadData

  // ============================================================
  // 14. Saídas
  io.pc              := pcReg
  io.instr           := ifId.instr
  io.writebackData   := wbDataFinal
  io.writebackRd     := memWb.rd
  io.writebackEnable := memWb.valid && !trap.io.commit_suppress
  io.illegal         := exMem.valid && exMem.signals.illegal
  io.stalled         := stallPipeline

  io.debug_mepc    := csr.io.mepc_read
  io.debug_mcause  := csr.io.mcause_read
  io.debug_mtvec   := csr.io.mtvec_read
  io.debug_mstatus := csr.io.mstatus_read
  io.debug_priv    := csr.io.current_priv
}
