package riscv

import chisel3._
import chisel3.util._
import riscv.elementosbasicos.Parameters

/** Timer periférico com MMIO e geração de interrupção configurável.
  *
  * Registradores de memória mapeada:
  *   - 0x4: limit  - Valor de limiar para interrupção (padrão: 100M ciclos)
  *   - 0x8: enable - Flag de habilitação da interrupção (padrão: true)
  *
  * Comportamento:
  *   - O contador incrementa a cada ciclo
  *   - O contador reinicia em 0 ao atingir o limite
  *   - A interrupção permanece ativa enquanto (count >= limit) e enabled
  */
class Timer extends Module {
  val io = IO(new Bundle {
    val bundle           = new RAMBundle
    val signal_interrupt = Output(Bool())

    // Sinais de debug
    val debug_limit   = Output(UInt(Parameters.DataWidth.W))
    val debug_enabled = Output(Bool())
  })

  val count   = RegInit(0.U(Parameters.DataWidth.W))
  val limit   = RegInit(Parameters.TimerDefaultLimit.U(Parameters.DataWidth.W))
  val enabled = RegInit(true.B)

  io.debug_limit   := limit
  io.debug_enabled := enabled

  // Endereços dos registradores MMIO (relativos ao endereço base)
  object DataAddr {
    val enable = 0x8.U(Parameters.AddrWidth.W)
    val limit  = 0x4.U(Parameters.AddrWidth.W)
  }

  // Interrupção: habilitada e contador >= limite
  io.signal_interrupt := enabled && (count >= limit)

  // Contador auto-recarregável
  count := Mux(count >= limit, 0.U, count + 1.U)

  // Leitura MMIO
  io.bundle.read_data := MuxLookup(io.bundle.address, 0.U)(
    IndexedSeq(
      DataAddr.enable -> enabled,
      DataAddr.limit  -> limit,
    )
  )

  // Escrita MMIO
  when(io.bundle.write_enable) {
    when(io.bundle.address === DataAddr.enable) {
      enabled := io.bundle.write_data
    } .elsewhen(io.bundle.address === DataAddr.limit) {
      limit := io.bundle.write_data
    }
  }
}