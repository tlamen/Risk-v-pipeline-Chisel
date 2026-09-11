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

  val csrReadData = csr.io.read_data

  // CÁLCULO DO VALOR A SER ESCRITO NO CSR
  val csrSrc = Mux(
    idEx.signals.csrOp(2),  // 1 = imediato (CSRRWI, CSRRSI, CSRRCI)
    idEx.rs1,               // Imediato de 5 bits (zero-extended)
    idEx.rs1Value           // Valor do registrador
  )

  val csrWriteData = MuxLookup(idEx.signals.csrOp, 0.U(32.W))(
    Seq(
      1.U -> csrSrc,                              // CSRRW
      2.U -> (csrReadData | csrSrc),              // CSRRS
      3.U -> (csrReadData & ~csrSrc),             // CSRRC
      5.U -> csrSrc,                              // CSRRWI
      6.U -> (csrReadData | csrSrc),              // CSRRSI
      7.U -> (csrReadData & ~csrSrc),             // CSRRCI
    )
  )

  val writebackData = WireDefault(ula.io.result)
  switch(idEx.signals.writebackSel) {
    is(WritebackSel.MEM) { writebackData := dataMem.io.readData }
    is(WritebackSel.PC4) { writebackData := idEx.pc + 4.U }
    is(WritebackSel.IMM) { writebackData := idEx.imm }
    is(WritebackSel.CSR) { writebackData := csrReadData }
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

  val writebackEnable =
    idEx.valid && !idEx.signals.illegal && idEx.signals.regWrite

  regFile.io.rs1 := ifIdInstr(19, 15)
  regFile.io.rs2 := ifIdInstr(24, 20)
  regFile.io.rd := idEx.rd
  regFile.io.writeData := writebackData
  regFile.io.regWrite := writebackEnable

  immGen.io.instr := ifIdInstr
  controller.io.opcode := ifIdInstr(6, 0)
  controller.io.funct3 := ifIdInstr(14, 12)
  controller.io.funct7 := ifIdInstr(31, 25)

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
  val decodedMemAddress = (forwardedRs1.asSInt + immGen.io.imm.asSInt).asUInt

  pcReg := pcNext
  when(stallPipeline) {
    // Mantém valores (stall)
  }.elsewhen(flushPipeline) {
    // Flush: insere NOP
    ifIdPc := pcReg
    ifIdInstr := nop
  }.otherwise {
    // Atualização normal
    ifIdPc := pcReg
    ifIdInstr := Mux(controlRedirect, nop, instrMem.io.readData)
  }

  when(stallPipeline) {
    // Mantém valores (stall)
  }.elsewhen(flushPipeline || controlRedirect) {
    // Flush: zera o estágio
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle)
  }.otherwise {
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

  when(controlRedirect) {
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle)
  }

  io.pc := pcReg
  io.instr := ifIdInstr
  io.aluResult := ula.io.result
  io.writebackData := writebackData
  io.writebackRd := idEx.rd
  io.writebackEnable := writebackEnable
  io.illegal := idEx.valid && idEx.signals.illegal
  io.stalled := stallPipeline
}
