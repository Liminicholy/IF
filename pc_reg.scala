import chisel3._
import chisel3.util._

class PCRegIO extends Bundle {
  val clk               = Input(Clock())
  val rst               = Input(Bool())
  val pc_en             = Input(Bool())
  val M_except         = Input(Bool())
  val M_except_addr     = Input(UInt(32.W))
  val M_flush_all       = Input(Bool())
  val M_flush_all_addr  = Input(UInt(32.W))
  val E_bj             = Input(Bool())
  val E_bj_target      = Input(UInt(32.W))
  val D_bj             = Input(Bool())
  val D_bj_target      = Input(UInt(32.W))
  val D_fifo_full      = Input(Bool())
  val F_inst_data_ok1  = Input(Bool())
  val F_inst_data_ok2  = Input(Bool())

  val pc_next          = Output(UInt(32.W))
  val pc_curr          = Output(UInt(32.W))
}

class PCReg extends Module {
  val io = IO(new PCRegIO)

  val pc_reg = RegInit("hbfc00000".U(32.W)) // Reset to 0xbfc00000
  
  // PC update logic
  when(io.rst) {
    pc_reg := "hbfc00000".U
  }.elsewhen(io.M_except) {
    pc_reg := io.M_except_addr
  }.elsewhen(io.M_flush_all) {
    pc_reg := io.M_flush_all_addr
  }.elsewhen(io.E_bj) {
    pc_reg := io.E_bj_target
  }.elsewhen(io.D_bj) {
    pc_reg := io.D_bj_target
  }.elsewhen(io.D_fifo_full) {
    pc_reg := pc_reg // Hold current PC
  }.elsewhen(io.F_inst_data_ok1 && io.F_inst_data_ok2) {
    pc_reg := pc_reg + 8.U
  }.elsewhen(io.F_inst_data_ok1) {
    pc_reg := pc_reg + 4.U
  }.otherwise {
    pc_reg := pc_reg // Default case
  }

  // Output assignments
  io.pc_next := Mux(io.rst, "hbfc00000".U,
               Mux(io.M_except, io.M_except_addr,
               Mux(io.M_flush_all, io.M_flush_all_addr,
               Mux(io.E_bj, io.E_bj_target,
               Mux(io.D_bj, io.D_bj_target,
               Mux(io.D_fifo_full, pc_reg,
               Mux(io.F_inst_data_ok1 && io.F_inst_data_ok2, pc_reg + 8.U,
               Mux(io.F_inst_data_ok1, pc_reg + 4.U,
               pc_reg))))))))

  io.pc_curr := pc_reg
}