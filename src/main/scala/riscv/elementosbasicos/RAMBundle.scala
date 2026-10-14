package riscv

import chisel3._
import riscv.Parameters

/** Bundle padrão para periféricos de memória mapeada (MMIO). */
class RAMBundle extends Bundle {
  val address      = Input(UInt(Parameters.AddrWidth.W))
  val read_data    = Output(UInt(Parameters.DataWidth.W))
  val write_data   = Input(UInt(Parameters.DataWidth.W))
  val write_enable = Input(Bool())
}