package riscv.elementosbasicos

import chisel3._
import chisel3.util._

/** Códigos de operação (opcodes) aceitos pela [[ULA]].
  *
  * Cada constante é codificada em 4 bits e usada na entrada `io.op` para
  * selecionar qual operação combinacional será executada.
  */
object ALUOp {

  /** Soma: `A + B` */
  val ADD = 0.U(4.W)

  /** Subtração: `A - B` */
  val SUB = 1.U(4.W)

  /** AND bit a bit: `A & B` */
  val AND = 2.U(4.W)

  /** OR bit a bit: `A | B` */
  val OR = 3.U(4.W)

  /** XOR bit a bit: `A ^ B` */
  val XOR = 4.U(4.W)

  /** Shift lógico à esquerda: `A << shamt` */
  val SLL = 5.U(4.W)

  /** Shift lógico à direita: `A >> shamt` com preenchimento por zero */
  val SRL = 6.U(4.W)

  /** Shift aritmético à direita: preserva bit de sinal */
  val SRA = 7.U(4.W)

  /** Set Less Than com sinal: `A < B` interpretando como `SInt` */
  val SLT = 8.U(4.W)

  /** Set Less Than sem sinal: `A < B` interpretando como `UInt` */
  val SLTU = 9.U(4.W)

  /** Multiplicação: parte baixa (32 bits menos significativos) de A * B */
  val MUL = 10.U(4.W)

  /** Multiplicação high: parte alta (32 bits mais significativos) de A * B */
  val MULH = 11.U(4.W)

  /** Multiplicação high sem sinal: parte altsa (32 bits mais significativos) de A * B interpretando como 'UInt' */
  val MULHU = 12.U(4.W)

  /** Multiplicação high com sinal misto: parte altsa (32 bits mais significativos) de A * B interpretando apenas B como 'UInt' */
  val MULHSU = 13.U(4.W)
}

// Importa os opcodes para uso direto no switch.
import ALUOp._

/** Unidade Lógica e Aritmética (ULA) de 32 bits para um núcleo RISC-V.
  *
  * O módulo é puramente combinacional: para cada par de operandos (`a`, `b`) e
  * opcode (`op`), produz imediatamente `result` conforme a operação escolhida.
  */
class ULA extends Module {

  /** Interface de entrada/saída da ULA. */
  val io = IO(new Bundle {

    /** Operando A (32 bits). */
    val a = Input(UInt(32.W))

    /** Operando B (32 bits). */
    val b = Input(UInt(32.W))

    /** Seletor da operação (opcode de 4 bits definido em [[ALUOp]]). */
    val op = Input(UInt(4.W))

    /** Resultado da operação selecionada (32 bits). */
    val result = Output(UInt(32.W))
  })

  // Valor padrão para evitar latch caso algum opcode não seja tratado.
  val res = WireDefault(0.U(32.W))

  /** Seleção combinacional da operação da ULA.
    *
    * Para shifts no RV32, apenas `b(4, 0)` é usado como quantidade de
    * deslocamento (`shamt`), permitindo valores de 0 a 31.
    */
  switch(io.op) {

    is(ADD) { res := io.a + io.b } // Gera um Somador em hardware
    is(SUB) { res := io.a - io.b } // Gera um Subtrator
    is(AND) { res := io.a & io.b } // Gera portas lógicas AND bit a bit
    is(OR) { res := io.a | io.b } // Gera portas lógicas OR bit a bit
    is(XOR) { res := io.a ^ io.b } // Gera portas lógicas XOR bit a bit

    is(SLL) { res := io.a << io.b(4, 0) } // Desloca A para a esquerda.
    is(SRL) { res := io.a >> io.b(4, 0) } // Desloca A para a direita.

    // Converte para SInt para preservar bit de sinal no shift aritmético.
    is(SRA) { res := (io.a.asSInt >> io.b(4, 0)).asUInt }

    is(SLT) { res := Mux(io.a.asSInt < io.b.asSInt, 1.U(32.W), 0.U(32.W)) }

    is(SLTU) { res := Mux(io.a < io.b, 1.U(32.W), 0.U(32.W)) }

    is(MUL) { res := (io.a * io.b)(31, 0) }
    is(MULH) {
      val produto = io.a.asSInt * io.b.asSInt
      res := produto(63, 32).asUInt
    }
    is(MULHU) { res := (io.a * io.b)(63, 32) }
    is(MULHSU) {
          val produto = io.a.asSInt * io.b
          res := produto(63, 32).asUInt
    }
  }

  // Encaminha o resultado final para a saída da ULA.
  io.result := res
}
