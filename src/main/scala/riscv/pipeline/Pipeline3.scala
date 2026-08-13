package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.elementosbasicos._

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

  dataMem.io.address := idEx.memAddress
  dataMem.io.writeData := idEx.memWriteData
  dataMem.io.writeEnable := idEx.valid && !idEx.signals.illegal && idEx.signals.memWrite
  dataMem.io.memSize := idEx.signals.memSize
  dataMem.io.unsignedLoad := idEx.signals.memUnsigned

  val writebackData = WireDefault(ula.io.result)
  switch(idEx.signals.writebackSel) {
    is(WritebackSel.MEM) { writebackData := dataMem.io.readData }
    is(WritebackSel.PC4) { writebackData := idEx.pc + 4.U }
    is(WritebackSel.IMM) { writebackData := idEx.imm }
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
