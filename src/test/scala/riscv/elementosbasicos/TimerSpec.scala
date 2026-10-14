package riscv

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TimerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Timer"

  it should "gerar interrupção quando count >= limit" in {
  test(new Timer) { dut =>
    // Configura limit = 5
    dut.io.bundle.address.poke(0x4)
    dut.io.bundle.write_data.poke(5)
    dut.io.bundle.write_enable.poke(true)
    dut.clock.step(1)  // Ciclo 0: escrita do limit

    // Desabilita escrita
    dut.io.bundle.write_enable.poke(false)

    // Aguarda 4 ciclos (count = 1, 2, 3, 4, 5)
    for (_ <- 0 until 4) {
      dut.clock.step(1)  // Ciclos 1, 2, 3, 4
    }

    // Agora estamos no ciclo 4: count = 5
    dut.io.signal_interrupt.expect(true)

    // Aguarda 1 ciclo (count reinicia para 0)
    dut.clock.step(1)  // Ciclo 5: count = 0
    dut.io.signal_interrupt.expect(false)
  }
}

  it should "desabilitar interrupção quando enabled = false" in {
    test(new Timer) { dut =>
      // Desabilita
      dut.io.bundle.address.poke(0x8)
      dut.io.bundle.write_data.poke(0)
      dut.io.bundle.write_enable.poke(true)
      dut.clock.step(1)

      dut.io.bundle.write_enable.poke(false)

      // Aguarda
      for (_ <- 0 until 20) {
        dut.clock.step(1)
      }

      // Interrupção deve estar inativa
      dut.io.signal_interrupt.expect(false)
    }
  }

  it should "ler limit via MMIO" in {
    test(new Timer) { dut =>
      // Escreve limit = 42
      dut.io.bundle.address.poke(0x4)
      dut.io.bundle.write_data.poke(42)
      dut.io.bundle.write_enable.poke(true)
      dut.clock.step(1)

      // Lê limit
      dut.io.bundle.address.poke(0x4)
      dut.io.bundle.write_enable.poke(false)
      dut.clock.step(1)

      dut.io.bundle.read_data.expect(42)
    }
  }
}