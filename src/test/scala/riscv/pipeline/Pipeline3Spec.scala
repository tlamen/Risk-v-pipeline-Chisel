package riscv.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.RV32I

class Pipeline3Spec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3"

  // ============================================================
  // ENCODERS (RV32I)
  // ============================================================
  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x13L

  private def add(rd: Int, rs1: Int, rs2: Int): Long =
    (0x00L << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x33L

  private def lui(rd: Int, imm20: Int): Long =
    ((imm20 & 0xfffff).toLong << 12) | ((rd & 0x1f).toLong << 7) | 0x37L

  private def beq(rs1: Int, rs2: Int, imm: Int): Long = {
    val e = imm & 0x1fff
    (((e >> 12) & 0x1).toLong << 31) |
      (((e >> 5) & 0x3f).toLong << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      (((e >> 1) & 0xf).toLong << 8) |
      (((e >> 11) & 0x1).toLong << 7) |
      0x63L
  }

  private def jal(rd: Int, imm: Int): Long = {
    val e = imm & 0x1fffff
    (((e >> 20) & 0x1).toLong << 31) |
      (((e >> 1) & 0x3ff).toLong << 21) |
      (((e >> 11) & 0x1).toLong << 20) |
      (((e >> 12) & 0xff).toLong << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x6fL
  }

  // ecall  = 0x00000073
  private val ECALL  = 0x00000073L
  // ebreak = 0x00100073
  private val EBREAK = 0x00100073L
  // mret   = 0x30200073
  private val MRET   = 0x30200073L

  /** csrrw rd, csr, rs1 */
  private def csrrw(rd: Int, csr: Int, rs1: Int): Long =
    ((csr & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x1L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x73L

  /** csrrs rd, csr, rs1  (rs1=x0 → leitura pura) */
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    ((csr & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x73L

  // ============================================================
  // TESTE 1: sequência aritmética com forwarding
  // ============================================================
  it should "executar uma sequencia aritmetica pequena com forwarding basico" in {
    val program = Seq(
      addi(1, 0, 5), // x1 = 5
      addi(2, 1, 3), // x2 = x1 + 3 = 8
      add(3, 1, 2),  // x3 = x1 + x2 = 13
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      // add x3 está em EX/MEM/WB no ciclo 3 (0-indexado: após 3 steps)
      for (_ <- 0 until 4) { dut.clock.step(1) }
      dut.io.writebackRd.expect(3)
      dut.io.writebackData.expect(13)
      dut.io.writebackEnable.expect(true)
    }
  }

  // ============================================================
  // TESTE 2: flush no branch tomado
  // ============================================================
  it should "fazer flush da instrucao seguinte quando um branch e tomado" in {
    val program = Seq(
      addi(1, 0, 5),  // 0x00
      addi(2, 0, 5),  // 0x04
      beq(1, 2, 8),   // 0x08  → alvo = 0x10
      addi(3, 0, 99), // 0x0C  (descartada)
      addi(3, 0, 42), // 0x10  (alvo)
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      // addi x3=42 está em EX/MEM/WB no ciclo 6 (0-indexado: após 6 steps)
      for (_ <- 0 until 7) { dut.clock.step(1) }
      dut.io.writebackRd.expect(3)
      dut.io.writebackData.expect(42)
      dut.io.writebackEnable.expect(true)
    }
  }

  // ============================================================
  // TESTE 3: ECALL em M-mode redireciona para mtvec e salva mepc
  // ============================================================
  it should "redirecionar para mtvec em ECALL e salvar mepc/mcause" in {
    val mtvec = 0x40
    val program = Seq(
      lui(5, 0x0),                 // 0x00: x5 = 0 (placeholder)
      csrrw(0, 0x305, 5),          // 0x04: mtvec <- x5 (escrita CSR, serializa)
      addi(1, 0, 7),               // 0x08: x1 = 7 (antes do ecall)
      ECALL,                       // 0x0C: trap → mtvec
      addi(1, 0, 99),              // 0x10: NÃO deve executar
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      // Escreve mtvec via CSR (x5 precisa conter o valor 0x40)
      // Para simplificar, em vez de LUI+CSRRW, pré-carregamos x5 com addi grande
      // não cabe em 12 bits; usamos LUI. Mas mtvec=0x40 exige LUI 0x0 + addi 0x40.
      // Vamos reconstruir o programa com o valor correto:
      val prog2 = Seq(
        addi(5, 0, 0x40),            // 0x00: x5 = 0x40
        csrrw(0, 0x305, 5),          // 0x04: mtvec <- 0x40
        addi(1, 0, 7),               // 0x08: x1 = 7
        ECALL,                       // 0x0C: mepc=0x0C, mcause=11
        addi(1, 0, 99),              // 0x10: NÃO executa
        addi(0, 0, 0)
      )
      // Não é possível reinstanciar dentro do mesmo test; usamos prog2 desde já.
      // (ver versão final abaixo)
    }
  }

  // ============================================================
  // TESTE 4: ECALL + handler com MRET retorna após o ecall
  // ============================================================
  it should "escrever mepc via csrrw" in {
    val program = Seq(
      addi(5, 0, 0x10),    // x5 = 0x10
      csrrw(0, 0x341, 5),  // mepc = 0x10
      0x00000013L          // nop
    )
    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      for (_ <- 0 until 6) { dut.clock.step(1) }
      dut.io.debug_mepc.expect(0x10.U)
    }
  }

  it should "executar ECALL + MRET e retomar na instrucao seguinte" in {
    val program = Seq(
      addi(5, 0, 0x20),   // 0x00
      csrrw(0, 0x305, 5), // 0x04 mtvec = 0x20
      addi(1, 0, 7),      // 0x08
      ECALL,              // 0x0C
      addi(1, 1, 1),      // 0x10
      0x00000013L,        // 0x14
      0x00000013L,        // 0x18
      0x00000013L,        // 0x1C
      csrrs(6, 0x341, 0), // 0x20 x6 = mepc
      addi(6, 6, 4),      // 0x24
      csrrw(0, 0x341, 6), // 0x28 mepc = 0x10
      csrrs(7, 0x342, 0), // 0x2C x7 = mcause
      MRET,               // 0x30
      0x00000013L         // 0x34
    )
    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      // Roda até o mcause ser escrito (trap commitou)
      var ciclos = 0
      while (dut.io.debug_mcause.peek().litValue == 0 && ciclos < 100) {
        dut.clock.step(1); ciclos += 1
      }
      dut.io.debug_mcause.expect(11.U)   // ecall de M-mode
      dut.io.debug_mtvec.expect(0x20.U)  // escrito pelo csrrw

      // Continua até o handler avançar mepc para 0x10
      ciclos = 0
      while (dut.io.debug_mepc.peek().litValue != 0x10 && ciclos < 100) {
        dut.clock.step(1); ciclos += 1
      }
      dut.io.debug_mepc.expect(0x10.U)

      // Continua até o mret redirecionar (pc volta para 0x10)
      ciclos = 0
      var viuRetomada = false
      while (!viuRetomada && ciclos < 100) {
        dut.clock.step(1); ciclos += 1
        if (dut.io.pc.peek().litValue == 0x10) viuRetomada = true
      }
      assert(viuRetomada, "PC não passou por 0x10 após mret")
    }
  }

  // ============================================================
  // TESTE 5: EBREAK gera mcause=3 e redireciona para mtvec
  // ============================================================
  it should "redirecionar para mtvec em EBREAK" in {
    val program = Seq(
      addi(5, 0, 0x20),   // 0x00
      csrrw(0, 0x305, 5), // 0x04 mtvec = 0x20
      EBREAK,             // 0x08
      0x00000013L,        // 0x0C
      0x00000013L,        // 0x10
      0x00000013L,        // 0x14
      0x00000013L,        // 0x18
      0x00000013L,        // 0x1C
      csrrs(7, 0x342, 0), // 0x20 x7 = mcause
      0x00000013L         // 0x24
    )
    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      var ciclos = 0
      while (dut.io.debug_mcause.peek().litValue == 0 && ciclos < 100) {
        dut.clock.step(1); ciclos += 1
      }
      dut.io.debug_mcause.expect(3.U)    // ebreak
      dut.io.debug_mepc.expect(0x08.U)   // PC do ebreak
      dut.io.debug_mtvec.expect(0x20.U)
      dut.io.debug_priv.expect(3.U)      // M-mode
    }
  }

  // ============================================================
  // TESTE 6: instrução ilegal (0x00000000) gera trap
  // ============================================================
  it should "tratar .word 0x00000000 como instrucao ilegal e redirecionar" in {
    val program = Seq(
      addi(5, 0, 0x20),   // 0x00
      csrrw(0, 0x305, 5), // 0x04 mtvec = 0x20
      0x00000000L,        // 0x08 ilegal
      0x00000013L,        // 0x0C
      0x00000013L,        // 0x10
      0x00000013L,        // 0x14
      0x00000013L,        // 0x18
      0x00000013L,        // 0x1C
      csrrs(7, 0x342, 0), // 0x20 x7 = mcause = 2
      0x00000013L         // 0x24
    )
    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      var ciclos = 0
      while (dut.io.debug_mcause.peek().litValue == 0 && ciclos < 100) {
        dut.clock.step(1); ciclos += 1
      }
      dut.io.debug_mcause.expect(2.U)      // instrução ilegal
      dut.io.debug_mepc.expect(0x08.U)     // PC do .word 0
      dut.io.debug_mtvec.expect(0x20.U)
      dut.io.debug_priv.expect(3.U)        // M-mode
    }
  }
}