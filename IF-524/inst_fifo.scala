import chisel3._
import chisel3.util._

// 简化后的FIFO条目结构（移除TLB相关信号）
class FifoEntry extends Bundle {
  val addr = UInt(32.W)  // 指令地址(PC)
  val data = UInt(32.W)  // 指令数据
}

// 简化后的IO接口
class InstFifoIO extends Bundle {
  // 时钟和复位
  val clk       = Input(Clock())
  val rst       = Input(Bool())      // 系统复位
  val fifo_rst  = Input(Bool())      // FIFO专用复位
  val i_stall   = Input(Bool())      // 流水线暂停信号
  
  // 读取端口
  val read_en1      = Input(Bool())
  val read_en2      = Input(Bool())
  val read_data1    = Output(UInt(32.W))
  val read_data2    = Output(UInt(32.W))
  val read_address1 = Output(UInt(32.W))
  val read_address2 = Output(UInt(32.W))
  
  // 写入端口
  val write_en1     = Input(Bool())
  val write_en2     = Input(Bool())
  val write_address1 = Input(UInt(32.W))
  val write_address2 = Input(UInt(32.W))
  val write_data1   = Input(UInt(32.W))
  val write_data2   = Input(UInt(32.W))
  
  // 状态信号
  val empty         = Output(Bool())
  val almost_empty  = Output(Bool())
  val full          = Output(Bool())
}

class InstFifo extends Module {
  val io = IO(new InstFifoIO)
  
  // FIFO存储（16条目）
  val lines = Reg(Vec(16, new FifoEntry))
  
  // 指针和计数器
  val write_ptr = RegInit(0.U(4.W))
  val read_ptr  = RegInit(0.U(4.W))
  val count     = RegInit(0.U(4.W))
  
  //----------------------------------------
  // 写入逻辑
  //----------------------------------------
  when(io.write_en1) {
    lines(write_ptr).addr := io.write_address1
    lines(write_ptr).data := io.write_data1
  }
  
  when(io.write_en2) {
    lines(write_ptr +% 1.U).addr := io.write_address2  // 循环寻址
    lines(write_ptr +% 1.U).data := io.write_data2
  }

  // 写指针更新
  when(io.fifo_rst) {
    write_ptr := 0.U
  }.elsewhen(io.write_en1 && io.write_en2) {
    write_ptr := write_ptr + 2.U
  }.elsewhen(io.write_en1) {
    write_ptr := write_ptr + 1.U
  }
  
  //----------------------------------------
  // 读取逻辑
  //----------------------------------------
  // 组合逻辑输出
  io.read_data1    := lines(read_ptr).data
  io.read_data2    := Mux(count > 1.U, lines(read_ptr +% 1.U).data, 0.U)
  io.read_address1 := lines(read_ptr).addr
  io.read_address2 := Mux(count > 1.U, lines(read_ptr +% 1.U).addr, 0.U)

  // 读指针更新
  when(io.fifo_rst) {
    read_ptr := 0.U
  }.elsewhen(io.read_en1 && io.read_en2 && count > 1.U) {
    read_ptr := read_ptr + 2.U
  }.elsewhen(io.read_en1 && count > 0.U) {
    read_ptr := read_ptr + 1.U
  }
  
  //----------------------------------------
  // 计数器与状态
  //----------------------------------------
  // 计数逻辑优化版
  when(io.fifo_rst) {
    count := 0.U
  }.otherwise {
    val inc = Mux(io.write_en1 && io.write_en2, 2.U, 
               Mux(io.write_en1 || io.write_en2, 1.U, 0.U))
    val dec = Mux(io.read_en1 && io.read_en2 && count > 1.U, 2.U,
               Mux(io.read_en1 && count > 0.U, 1.U, 0.U))
    count := count + inc - dec
  }
  
  // 状态信号
  io.full         := (count === 15.U) || (count === 14.U && io.write_en1 && io.write_en2)
  io.empty        := (count === 0.U)
  io.almost_empty := (count === 1.U)
  
  //----------------------------------------
  // 可选：移除的性能计数器
  //----------------------------------------
  // val master_cnt = RegInit(0.U(64.W))  // 可移除
  // val slave_cnt  = RegInit(0.U(64.W))  // 可移除
}