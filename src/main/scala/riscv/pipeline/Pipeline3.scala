package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos._ 
import riscv._

class DecodeExecuteBundle extends Bundle {
  val valid = Bool()
  val pc = UInt(32.W)
  val instr = UInt(32.W)
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val rd = UInt(5.W)
  val rs1Value = UInt(32.W)
  val rs2Value = UInt(32.W)
  val imm = UInt(32.W)
  val memAddress = UInt(32.W)
  val memWriteData = UInt(32.W)
  val signals = new ControlSignals
  val csrAddress = UInt(12.W)
  val csrWriteData = UInt(32.W)
  val csrReadData = UInt(32.W)
}

/** Pipeline RV32I educacional de 3 estagios, inspirado no Wildcat do livro: IF,
  * ID/RF/address-prep e EX/MEM/WB.
  *
  * Esta versao e intencionalmente pequena para servir de base ao pipeline:
  *   - memoria de instrucao e dados internas;
  *   - forwarding simples do resultado do estagio EX para o decode;
  */
class Pipeline3(
    initialProgram: Seq[Long] = Seq.empty,
    memoryWords: Int = 1024,
    programFile: String = ""
) extends Module {
  val io = IO(new Bundle {
    val pc = Output(UInt(32.W))
    val instr = Output(UInt(32.W))
    val aluResult = Output(UInt(32.W))
    val writebackData = Output(UInt(32.W))
    val writebackRd = Output(UInt(5.W))
    val writebackEnable = Output(Bool())
    val illegal = Output(Bool())
    val stalled = Output(Bool())

    //CLINT debug
    val debug_clint_trap_assert      = Output(Bool())
    val debug_clint_trap_to_smode    = Output(Bool())
    val debug_clint_delegate_to_smode = Output(Bool())
    val debug_clint_exception_code   = Output(UInt(31.W))
    val debug_clint_is_interrupt     = Output(Bool())
    val debug_clint_trap_address     = Output(UInt(32.W))
    
    //CSR debug
    val debug_csrReadData = Output(UInt(32.W))
    val debug_csrForwarding = Output(Bool())
    val debug_csr_address_id = Output(UInt(12.W))
    val debug_csr_address_ex = Output(UInt(12.W))
  })

  import RV32I._

  val nop = "h00000013".U(32.W) // addi x0, x0, 0

  // Mecanismo de reserva para LR/SC
  val reservationAddr = RegInit(0.U(32.W))
  val reservationValid = RegInit(false.B)

  val instrMem = Module(
    new InstructionMemory(
      depthWords = memoryWords,
      initialData = initialProgram,
      programFile = programFile
    )
  )
  val dataMem = Module(new DataMemory(depthWords = memoryWords))
  val regFile = Module(new RegisterFile)
  val immGen = Module(new ImmGen)
  val controller = Module(new Controller)
  val ula = Module(new ULA)

  val pcReg = RegInit(0.U(32.W))
  val ifIdPc = RegInit(0.U(32.W))
  val ifIdInstr = RegInit(nop)
  val idEx = RegInit(0.U.asTypeOf(new DecodeExecuteBundle))

  val stallPipeline = WireDefault(false.B)
  val flushPipeline = WireDefault(false.B)

  // Sinais para LR/SC
  val isLR = idEx.valid && !idEx.signals.illegal && idEx.signals.isLR
  val isSC = idEx.valid && !idEx.signals.illegal && idEx.signals.isSC
  val isAMO = idEx.valid && !idEx.signals.illegal && idEx.signals.isAMO

  // Interrupção do Timer (TODO)
  val timer_interrupt = WireDefault(false.B)

  val interrupt_flag = WireDefault(InterruptCode.None)

  when(timer_interrupt) {
    interrupt_flag := InterruptCode.Timer0
  }

  // CLINT e CSRs
  val csr = Module(new CSR)
  val clint = Module(new CLINT)

  clint.io.mstatus := csr.io.mstatus_read
  clint.io.mepc    := csr.io.mepc_read
  clint.io.mcause  := csr.io.mcause_read
  clint.io.mtvec   := csr.io.mtvec_read
  clint.io.mie     := csr.io.mie_read
  clint.io.mtval   := csr.io.mtval_read 

  // CSRs de S-Mode
  clint.io.sstatus := csr.io.sstatus_read
  clint.io.sepc    := csr.io.sepc_read
  clint.io.scause  := csr.io.scause_read
  clint.io.stvec   := csr.io.stvec_read
  clint.io.sie     := csr.io.sie_read
  clint.io.stval   := csr.io.stval_read

  // CSRs de delegação
  clint.io.medeleg := csr.io.medeleg_read
  clint.io.mideleg := csr.io.mideleg_read

  // Modo de privilégio atual
  clint.io.current_priv := csr.io.current_priv

  // Entradas do pipeline
  clint.io.pc     := pcReg
  clint.io.instr  := ifIdInstr
  clint.io.valid  := !stallPipeline && !flushPipeline
  clint.io.interrupt_flag := interrupt_flag

  // Conexão CLINT -> CSR (escrita com prioridade)
  csr.io.clint_write_enable   := clint.io.direct_write_enable
  csr.io.clint_mstatus_write  := clint.io.mstatus_write_data
  csr.io.clint_mepc_write     := clint.io.mepc_write_data
  csr.io.clint_mcause_write   := clint.io.mcause_write_data
  csr.io.clint_mtval_write    := clint.io.mtval_write_data
  csr.io.clint_sepc_write     := clint.io.sepc_write_data
  csr.io.clint_scause_write   := clint.io.scause_write_data
  csr.io.clint_stval_write    := clint.io.stval_write_data
  csr.io.clint_sstatus_write  := clint.io.sstatus_write_data

  // CONEXÃO CPU -> CSR
  csr.io.read_address    := ifIdInstr(31, 20)   // Endereço do CSR na instrução
  csr.io.cpu_write_enable  := idEx.valid && !idEx.signals.illegal && 
                              idEx.signals.csrWrite && 
                              !clint.io.direct_write_enable
  csr.io.cpu_write_address := idEx.csrAddress
  csr.io.cpu_write_data    := idEx.csrWriteData

  // Controle de CSR via CLINT
  val csr_write_enable = clint.io.direct_write_enable
  val csr_write_data   = MuxLookup(csr_write_enable, 0.U(32.W))(
    Seq(
      // Mapear endereços de CSR para dados de escrita
    )
  )

  // Quando uma LR é executada
  when(isLR) {
      reservationAddr := idEx.memAddress
      reservationValid := true.B
  }

  // Quando uma SC é executada
  val scSuccess = WireDefault(false.B)
  when(isSC) {
      scSuccess := reservationValid && (reservationAddr === idEx.memAddress)
      when(scSuccess) {
          // Escreve na memória
          dataMem.io.writeEnable := true.B
          dataMem.io.address := idEx.memAddress
          dataMem.io.writeData := idEx.memWriteData
          // Invalida a reserva
          reservationValid := false.B
      } .otherwise {
          // Não escreve, invalida a reserva
          reservationValid := false.B
      }
  }

  // Quando uma AMO é executada
  when(isAMO) {
      // A leitura e escrita são atômicas (garantidas pelo estágio MEM)
      // O valor antigo é lido e o novo é escrito
      // Invalida a reserva (qualquer AMO invalida)
      reservationValid := false.B
  }

  // Qualquer escrita na memória invalida a reserva (simplificação)
  when(dataMem.io.writeEnable && (idEx.memAddress === reservationAddr)) {
      reservationValid := false.B
  }

  // Interrupções invalidam a reserva
  when(io.illegal || idEx.signals.illegal) {
      reservationValid := false.B
  }


  val exOperandA = WireDefault(idEx.rs1Value)
  val exOperandB = WireDefault(idEx.rs2Value)
  switch(idEx.signals.operandASel) {
    is(OperandASel.PC) { exOperandA := idEx.pc }
    is(OperandASel.ZERO) { exOperandA := 0.U }
  }
  when(idEx.signals.operandBSel === OperandBSel.IMM) {
    exOperandB := idEx.imm
  }

  ula.io.a := exOperandA
  ula.io.b := exOperandB
  ula.io.op := idEx.signals.aluOp

  val jalrTarget = (ula.io.result & "hFFFFFFFE".U(32.W))
  val pcRelativeTarget = (idEx.pc.asSInt + idEx.imm.asSInt).asUInt
  val branchTaken = WireDefault(false.B)

  switch(idEx.signals.branchType) {
    is(BranchType.BEQ) { branchTaken := idEx.rs1Value === idEx.rs2Value }
    is(BranchType.BNE) { branchTaken := idEx.rs1Value =/= idEx.rs2Value }
    is(BranchType.BLT) {
      branchTaken := idEx.rs1Value.asSInt < idEx.rs2Value.asSInt
    }
    is(BranchType.BGE) {
      branchTaken := idEx.rs1Value.asSInt >= idEx.rs2Value.asSInt
    }
    is(BranchType.BLTU) { branchTaken := idEx.rs1Value < idEx.rs2Value }
    is(BranchType.BGEU) { branchTaken := idEx.rs1Value >= idEx.rs2Value }
  }

  val controlRedirect = idEx.valid && !idEx.signals.illegal &&
    (idEx.signals.jump || (idEx.signals.branchType =/= BranchType.NONE && branchTaken))
  val redirectTarget = Mux(idEx.signals.jalr, jalrTarget, pcRelativeTarget)
  val pcNext = WireDefault(pcReg + 4.U)
  when(stallPipeline) {
    pcNext := pcReg  // Mantém PC durante stall
  }.elsewhen(controlRedirect) {
    pcNext := redirectTarget
  }

  instrMem.io.address := pcReg

  // Sinais para controle da memória (com suporte à extensão A)
  val memRead = WireDefault(
    idEx.valid && !idEx.signals.illegal && 
    (idEx.signals.writebackSel === WritebackSel.MEM) &&
    !idEx.signals.memWrite
  )
  val memWrite = WireDefault(
    idEx.valid && !idEx.signals.illegal && idEx.signals.memWrite
  )

  // Para LR.W: sempre lê da memória
  when(isLR) {
    memRead := true.B
  }

  // Para SC.W e AMOs: escreve na memória
  when(isSC || isAMO) {
    memWrite := true.B
  }

  dataMem.io.address := idEx.memAddress
  dataMem.io.writeData := idEx.memWriteData
  dataMem.io.writeEnable := idEx.valid && !idEx.signals.illegal && idEx.signals.memWrite
  dataMem.io.memSize := idEx.signals.memSize
  dataMem.io.unsignedLoad := idEx.signals.memUnsigned

  val idRs1 = ifIdInstr(19, 15)
  val idRs2 = ifIdInstr(24, 20)

  val currentUsesRd = (idRs1 === idEx.rd && idRs1 =/= 0.U) ||
                      (idRs2 === idEx.rd && idRs2 =/= 0.U)
  val isLoadInEx = idEx.valid &&
                   idEx.signals.writebackSel === WritebackSel.MEM &&
                   !idEx.signals.memWrite &&
                   idEx.signals.regWrite &&
                   idEx.signals.branchType === BranchType.NONE &&
                   !idEx.signals.jump &&
                   !idEx.signals.jalr &&
                   !idEx.signals.illegal
  val loadUseHazard = isLoadInEx && currentUsesRd

  val hazardDetected = loadUseHazard

  stallPipeline := hazardDetected
  flushPipeline := controlRedirect

  val lwWbValid = RegInit(false.B)
  val lwWbRd    = RegInit(0.U(5.W))
  val lwWbData  = RegInit(0.U(32.W))

  val writebackEnable = (idEx.valid && !idEx.signals.illegal && idEx.signals.regWrite) || lwWbValid
  val writebackRd = Mux(lwWbValid, lwWbRd, idEx.rd)
  val writebackData = WireDefault(ula.io.result)

  when(lwWbValid) {
    writebackData := lwWbData
  }

  val csrAddressID = ifIdInstr(31, 20)
  val csrAddressEX = idEx.csrAddress

  val csrForwarding = idEx.valid && 
                      !idEx.signals.illegal && 
                      idEx.signals.csrWrite && 
                      (csrAddressID === csrAddressEX)

  val csrReadData = Mux(csrForwarding, idEx.csrWriteData, csr.io.read_data)

  val forwardedRs1 = Mux(
    writebackEnable && idEx.rd =/= 0.U && idEx.rd === idRs1,
    writebackData,
    Mux(
      writebackEnable && idEx.rd =/= 0.U && idEx.rd === idRs1,
      writebackData,
      regFile.io.readData1
    )
  )
  val forwardedRs2 = Mux(
    writebackEnable && idEx.rd =/= 0.U && idEx.rd === idRs2,
    writebackData,
    Mux(
      writebackEnable && idEx.rd =/= 0.U && idEx.rd === idRs2,
      writebackData,
      regFile.io.readData2
    )
  )
  val decodedMemAddress = (forwardedRs1.asSInt + immGen.io.imm.asSInt).asUInt

  // CÁLCULO DO VALOR A SER ESCRITO NO CSR
  val csrSrc = Mux(
    controller.io.signals.csrOp(2),  // 1 = imediato (CSRRWI, CSRRSI, CSRRCI)
    ifIdInstr(19, 15),               // Imediato de 5 bits (zero-extended)
    forwardedRs1          // Valor do registrador
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

  pcReg := pcNext
  when(stallPipeline) {
    lwWbValid := true.B
    lwWbRd := idEx.rd
    lwWbData := dataMem.io.readData
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle) 
  } .elsewhen(flushPipeline || controlRedirect) {
    // FLUSH: insere NOP no IF/ID e limpa o ID/EX
    lwWbValid := false.B 
    ifIdPc := pcReg
    ifIdInstr := nop
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle)
  } .otherwise {
    // ATUALIZAÇÃO NORMAL
    lwWbValid := false.B 
    ifIdPc := pcReg
    ifIdInstr := Mux(controlRedirect, nop, instrMem.io.readData)

    idEx.valid := !controlRedirect
    idEx.pc := ifIdPc
    idEx.instr := ifIdInstr
    idEx.rs1 := idRs1
    idEx.rs2 := idRs2
    idEx.rd := ifIdInstr(11, 7)
    idEx.rs1Value := forwardedRs1
    idEx.rs2Value := forwardedRs2
    idEx.imm := immGen.io.imm
    idEx.memAddress := decodedMemAddress
    idEx.memWriteData := forwardedRs2
    idEx.signals := controller.io.signals
    idEx.csrAddress := ifIdInstr(31, 20)
    idEx.csrWriteData := csrWriteData
    idEx.csrReadData := csrReadData
  }

  switch(idEx.signals.writebackSel) {
    is(WritebackSel.MEM) { writebackData := dataMem.io.readData }
    is(WritebackSel.PC4) { writebackData := idEx.pc + 4.U }
    is(WritebackSel.IMM) { writebackData := idEx.imm }
    is(WritebackSel.CSR) { writebackData := idEx.csrReadData }
  }

  when(lwWbValid) {
    writebackData := lwWbData
  }

  // LR.W: valor lido da memória
  when(isLR) {
    writebackData := dataMem.io.readData
  }

  // SC.W: resultado é 0 (sucesso) ou 1 (falha)
  when(isSC) {
    writebackData := Mux(scSuccess, 0.U, 1.U)
  }

  // AMOs: valor lido da memória (valor antigo)
  when(isAMO) {
    writebackData := dataMem.io.readData
  }

  // Endereço alinhado para LR/SC/AMO (deve ser word-aligned, bits 1:0 = 0)
  val addressMisaligned = WireDefault(false.B)
  when(idEx.valid && !idEx.signals.illegal && (isLR || isSC || isAMO)) {
    addressMisaligned := idEx.memAddress(1, 0) =/= 0.U
  }

  // Page Faults (simplificado: se a memória não tiver o endereço)
  val addressOutOfRange = WireDefault(false.B)
  when(idEx.valid && !idEx.signals.illegal && (isLR || isSC || isAMO)) {
    addressOutOfRange := idEx.memAddress >= memoryWords.U * 4.U
  }

  // Acessos a endereços não alinhados geram exceção
  when(addressMisaligned) {
    // Força a instrução a ser ilegal (gera exceção)
    idEx.signals.illegal := true.B
  }

  // Acessos fora do intervalo da memória (simula page fault)
  when(addressOutOfRange) {
    idEx.signals.illegal := true.B
  }

  regFile.io.rs1 := ifIdInstr(19, 15)
  regFile.io.rs2 := ifIdInstr(24, 20)
  regFile.io.rd := idEx.rd
  regFile.io.writeData := writebackData
  regFile.io.regWrite := writebackEnable

  immGen.io.instr := ifIdInstr
  controller.io.opcode := ifIdInstr(6, 0)
  controller.io.funct3 := ifIdInstr(14, 12)
  controller.io.funct7 := ifIdInstr(31, 25)

  when(controlRedirect) {
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle)
  }

  io.pc := pcReg
  io.instr := ifIdInstr
  io.aluResult := ula.io.result
  io.writebackData := writebackData
  io.writebackRd := writebackRd
  io.writebackEnable := writebackEnable
  io.illegal := idEx.valid && idEx.signals.illegal
  io.stalled := stallPipeline
  
  // Debug do CLINT
  io.debug_clint_trap_assert      := clint.io.debug_trap_assert
  io.debug_clint_trap_to_smode    := clint.io.debug_trap_to_smode
  io.debug_clint_delegate_to_smode := clint.io.debug_delegate_to_smode
  io.debug_clint_exception_code   := clint.io.debug_exception_code
  io.debug_clint_is_interrupt     := clint.io.debug_is_interrupt
  io.debug_clint_trap_address     := clint.io.debug_trap_address

  // Debug do CSR forwarding
  io.debug_csrReadData  := csrReadData
  io.debug_csrForwarding := csrForwarding
  io.debug_csr_address_id := csrAddressID
  io.debug_csr_address_ex := csrAddressEX
}
