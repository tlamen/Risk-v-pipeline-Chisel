package riscv.elementosbasicos

import chisel3._
import chisel3.util._
import riscv._
import riscv.elementosbasicos._

object PageFaultCode {
  val NONE                   = 0.U(4.W)
  val INSTRUCTION_PAGE_FAULT = 12.U(4.W)
  val LOAD_PAGE_FAULT        = 13.U(4.W)
  val STORE_PAGE_FAULT       = 15.U(4.W)
}

object AccessType {
  val FETCH = 0.U(2.W)
  val LOAD  = 1.U(2.W)
  val STORE = 2.U(2.W)
}

class MMU extends Module {
  val io = IO(new Bundle {
    val virtual_addr = Input(UInt(32.W))
    val satp         = Input(UInt(32.W))
    val sstatus      = Input(UInt(32.W))
    val current_priv = Input(UInt(2.W))
    val access_type  = Input(UInt(2.W))

    // Interface com a memória (page walker)
    val mem_read_addr = Output(UInt(32.W))
    val mem_read_data = Input(UInt(32.W))
    val mem_read_en   = Output(Bool())

    // Saídas
    val physical_addr = Output(UInt(32.W))
    val page_fault    = Output(Bool())
    val fault_addr    = Output(UInt(32.W))
    val fault_cause   = Output(UInt(4.W))
  })

  import Parameters._
  import RV32I._

  // ============================================================
  // 1. EXTRAIR CAMPOS DO satp
  // ============================================================
  val satp_mode = io.satp(31, 30)
  val satp_ppn  = io.satp(21, 0)
  val is_bare   = (satp_mode === 0.U)

  // ============================================================
  // 2. EXTRAIR CAMPOS DO ENDEREÇO VIRTUAL
  // ============================================================
  val vpn1   = io.virtual_addr(31, 22)
  val vpn0   = io.virtual_addr(21, 12)
  val offset = io.virtual_addr(11, 0)

  // ============================================================
  // 3. ESTADOS DO PAGE WALKER
  // ============================================================
  object State extends ChiselEnum {
    val IDLE, LEVEL1_REQ, LEVEL1_RESP, LEVEL2_REQ, LEVEL2_RESP, DONE, FAULT = Value
  }
  import State._

  val state       = RegInit(IDLE)
  val pte1_reg    = RegInit(0.U(32.W))
  val saved_vaddr = RegInit(0.U(32.W))
  val saved_offset = RegInit(0.U(12.W))

  // Saídas registradas
  val physical_addr_reg = RegInit(0.U(32.W))
  val page_fault_reg    = RegInit(false.B)
  val fault_addr_reg    = RegInit(0.U(32.W))
  val fault_cause_reg   = RegInit(PageFaultCode.NONE)
  val valid_reg         = RegInit(false.B)

  // ============================================================
  // 4. SINAIS DE CONTROLE DA MEMÓRIA (registrados)
  // ============================================================
  val mem_read_addr_wire = WireDefault(0.U(32.W))
  val mem_read_en_wire   = WireDefault(false.B)

  // Registrar as saídas para a memória (quebra o loop combinacional)
  io.mem_read_addr := mem_read_addr_wire
  io.mem_read_en   := mem_read_en_wire

  // ============================================================
  // 5. CÁLCULO DOS ENDEREÇOS DAS PTEs
  // ============================================================
  val pte1_addr = Cat(satp_ppn, 0.U(12.W)) + Cat(vpn1, 0.U(2.W))
  val pte1_ppn  = pte1_reg(31, 10)
  val pte2_addr = Cat(pte1_ppn, 0.U(12.W)) + Cat(vpn0, 0.U(2.W))

  // ============================================================
  // 6. EXTRAIR CAMPOS DA PTE
  // ============================================================
  def pte_valid(pte: UInt): Bool    = pte(0)
  def pte_read(pte: UInt): Bool     = pte(1)
  def pte_write(pte: UInt): Bool    = pte(2)
  def pte_exec(pte: UInt): Bool     = pte(3)
  def pte_user(pte: UInt): Bool     = pte(4)
  def pte_ppn(pte: UInt): UInt      = pte(31, 10)

  def is_leaf(pte: UInt): Bool = pte_read(pte) || pte_exec(pte)
  def is_pointer(pte: UInt): Bool = !pte_read(pte) && !pte_write(pte) && !pte_exec(pte)

  // ============================================================
  // 7. VERIFICAÇÃO DE PERMISSÕES
  // ============================================================
  val sum_bit = io.sstatus(18)
  val mxr_bit = io.sstatus(19)

  def check_permissions(pte: UInt): Bool = {
    val r = pte_read(pte)
    val w = pte_write(pte)
    val x = pte_exec(pte)
    val u = pte_user(pte)

    val u_ok = Mux(io.current_priv === PrivilegeMode.U,
                   u,
                   Mux(io.current_priv === PrivilegeMode.S,
                       u || sum_bit,
                       true.B))

    val access_ok = MuxLookup(io.access_type, false.B)(
      Seq(
        AccessType.FETCH -> x,
        AccessType.LOAD  -> (r || (mxr_bit && x)),
        AccessType.STORE -> w,
      )
    )

    u_ok && access_ok
  }

  def get_fault_code(): UInt = {
    MuxLookup(io.access_type, PageFaultCode.LOAD_PAGE_FAULT)(
      Seq(
        AccessType.FETCH -> PageFaultCode.INSTRUCTION_PAGE_FAULT,
        AccessType.LOAD  -> PageFaultCode.LOAD_PAGE_FAULT,
        AccessType.STORE -> PageFaultCode.STORE_PAGE_FAULT,
      )
    )
  }

  // ============================================================
  // 8. SAÍDAS COMBINACIONAIS
  // ============================================================
  io.physical_addr := Mux(is_bare, io.virtual_addr, physical_addr_reg)
  io.page_fault    := page_fault_reg && !is_bare
  io.fault_addr    := fault_addr_reg
  io.fault_cause   := fault_cause_reg

  // ============================================================
  // 9. MÁQUINA DE ESTADOS (com latência de memória)
  // ============================================================
  switch(state) {
    is(IDLE) {
      when(!is_bare) {
        // Salva o endereço virtual
        saved_vaddr := io.virtual_addr
        saved_offset := offset
        state := LEVEL1_REQ
      }
    }

    is(LEVEL1_REQ) {
      // Requisita a PTE de nível 1
      mem_read_addr_wire := pte1_addr
      mem_read_en_wire := true.B
      state := LEVEL1_RESP
    }

    is(LEVEL1_RESP) {
      // Usa o dado lido (disponível após 1 ciclo)
      when(pte_valid(io.mem_read_data)) {
        when(is_pointer(io.mem_read_data)) {
          pte1_reg := io.mem_read_data
          state := LEVEL2_REQ
        } .otherwise {
          // PTE folha no nível 1 → misaligned superpage
          page_fault_reg := true.B
          fault_addr_reg := saved_vaddr
          fault_cause_reg := get_fault_code()
          state := FAULT
        }
      } .otherwise {
        page_fault_reg := true.B
        fault_addr_reg := saved_vaddr
        fault_cause_reg := get_fault_code()
        state := FAULT
      }
    }

    is(LEVEL2_REQ) {
      // Requisita a PTE de nível 2
      mem_read_addr_wire := pte2_addr
      mem_read_en_wire := true.B
      state := LEVEL2_RESP
    }

    is(LEVEL2_RESP) {
      // Usa o dado lido
      when(pte_valid(io.mem_read_data)) {
        when(is_leaf(io.mem_read_data)) {
          when(check_permissions(io.mem_read_data)) {
            val ppn = pte_ppn(io.mem_read_data)
            physical_addr_reg := Cat(ppn, saved_offset)
            page_fault_reg := false.B
            state := DONE
          } .otherwise {
            page_fault_reg := true.B
            fault_addr_reg := saved_vaddr
            fault_cause_reg := get_fault_code()
            state := FAULT
          }
        } .otherwise {
          page_fault_reg := true.B
          fault_addr_reg := saved_vaddr
          fault_cause_reg := get_fault_code()
          state := FAULT
        }
      } .otherwise {
        page_fault_reg := true.B
        fault_addr_reg := saved_vaddr
        fault_cause_reg := get_fault_code()
        state := FAULT
      }
    }

    is(DONE) {
      state := IDLE
    }

    is(FAULT) {
      state := IDLE
      page_fault_reg := false.B
    }
  }
}