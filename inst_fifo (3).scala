import chisel3._
import chisel3.util._

// FIFO条目结构（移除TLB相关信号）
class FifoEntry extends Bundle {
  val addr = UInt(32.W)  // 指令地址(PC)
  val data = UInt(32.W)  // 指令数据
}

// IO接口
class InstFifoIO extends Bundle {
  // 时钟和复位    删clk和rst，rst改为reset                                                             
  val fifo_rst  = Input(AsyncReset())      // FIFO专用复位（异步复位）cu
  
  val i_stall   = Input(Bool())      // 流水线暂停信号 cu
  
  // 读取端口
  val read_push      = Input(Bool()) //id提供一个push 即从指令被退回
  //val read_en2      = Input(Bool())
  val read_data1    = Output(UInt(32.W))
  val read_data2    = Output(UInt(32.W))
  val read_address1 = Output(UInt(32.W))
  val read_address2 = Output(UInt(32.W))
  
  // 写入端口
  //val write_en1     = Input(Bool())
  //val write_en2     = Input(Bool())
  val write_address1 = Input(UInt(32.W))
  val write_address2 = Input(UInt(32.W))
  val write_data1   = Input(UInt(32.W))
  val write_data2   = Input(UInt(32.W))
  //指令 对应数据
  
  // 状态信号
  val empty         = Output(Bool())
  val almost_empty  = Output(Bool())
  val full          = Output(Bool())
  val almost_full   = Output(Bool())
}

class InstFifo extends Module {
  val io = IO(new InstFifoIO)

  // 异步复位（立即清空）
  withReset(io.fifo_rst) {
    val write_ptr = RegInit(0.U(4.W))  //写指针
    val read_ptr  = RegInit(0.U(4.W))  //读指针
    val count    = RegInit(0.U(4.W))  //数据计数器
    val lines = Reg(Vec(16, new FifoEntry)) // 存储条目
  } 

 // 状态信号（组合逻辑）
  io.full         := (count === 15.U) // 无写使能|| (count === 14.U && io.write_en1 && io.write_en2)
  io.almost_full  := (count >= 14.U)
  io.empty        := (count === 0.U)
  io.almost_empty := (count <= 1.U)
  
  // 写入逻辑（同步）//写入和写指针是不受stall影响的 只和pc相关
when(!io.full && !io.almost_full){ //双写入
  lines(write_ptr) := Cat(io.write_address1, io.write_data1).asTypeOf(new FifoEntry)
  lines(write_ptr +% 1.U) := Cat(io.write_address2, io.write_data2).asTypeOf(new FifoEntry)
  write_ptr := write_ptr +% 2.U
  count := count +% 2.U
}.elsewhen(!io.full && io.almost_full){ //单写入
  lines(write_ptr) := Cat(io.write_address1, io.write_data1).asTypeOf(new FifoEntry)
  write_ptr := write_ptr +% 1.U
  count := count +% 1.U
}.elsewhen{
    write_ptr := write_ptr
    count := count
  }//满了 写指针停止
  
  // 读取逻辑（同步）
  // 组合逻辑输出（无时钟延迟）
  io.read_data1    := Mux(!io.empty, lines(read_ptr).data, 0.U)              //lines(read_ptr).data
  io.read_data2    := Mux(!io.almost_empty, lines(read_ptr +% 1.U).data, 0.U)  //是否almost_empty状态
  io.read_address1 := Mux(!io.empty, lines(read_ptr).addr, 0.U)                //lines(read_ptr).addr
  io.read_address2 := Mux(!io.almost_empty, lines(read_ptr +% 1.U).addr, 0.U)
 

  // 读指针更新（i_stall控制）,读使能 id提供一个 决定是否push（即从指令被退回作为下一周期的主指令）
  when(!io.i_stall) {  // i_stall为高时冻结读指针
    when(!io.read_push && !io.empty && !io.almost_empty) {
      read_ptr := read_ptr +% 2.U
      count := count - 2.U
    }.elsewhen(!io.empty && (io.almost_empty || io.read_push)) {
      read_ptr := read_ptr +% 1.U
      count := count - 1.U
    }.otherwise {
    read_ptr := read_ptr  // 显式保持 empty
    count := count
  }
}.otherwise {
  read_ptr := read_ptr    //显式保持 stall
  count := count
}
  
}
