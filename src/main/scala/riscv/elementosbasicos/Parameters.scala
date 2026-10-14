package riscv

import chisel3._

/** Parâmetros globais do processador RISC-V.
  *
  * Centraliza constantes usadas em vários módulos para evitar
  * literais mágicos espalhados pelo código.
  */
object Parameters {
  // ============================================================
  // LARGURAS DE DADOS E ENDEREÇOS
  // ============================================================
  val DataWidth    = 32  // Largura dos dados (RV32)
  val AddrWidth    = 32  // Largura dos endereços
  val InstrWidth   = 32  // Largura das instruções

  // ============================================================
  // TIMER
  // ============================================================
  // Valor padrão do limite do timer (~1s a 100MHz)
  val TimerDefaultLimit = 100000000

  val TimerBaseAddress = 0x80000000L
  val TimerAddressMask = 0x00000FFFL  // 4 KB de espaço

  // ============================================================
  // MMU (Sv32) - para uso futuro
  // ============================================================
  val PageSize  = 4096       // 4 KiB
  val PTESize   = 4          // 4 bytes por PTE em Sv32
  val VPNBits   = 10         // 10 bits por nível em Sv32
  val OffsetBits = 12        // 12 bits de offset em Sv32

  // ============================================================
  // INTERRUPÇÕES
  // ============================================================
  val InterruptFlagWidth = 8  // Largura do sinal interrupt_flag
}