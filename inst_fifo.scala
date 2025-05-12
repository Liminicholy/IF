import chisel3._
import chisel3.util._

class FifoEntry extends Bundle {
  val refill = Bool()
  val invalid = Bool()
  val addr = UInt(32.W)
  val data = UInt(32.W)
}

class InstFifoIO extends Bundle {
  val clk = Input(Clock())
  val rst = Input(Bool())
  val fifo_rst = Input(Bool())                 // fifo读写指针复位（重置指针）
  val flush_delay_slot = Input(Bool())
  val delay_sel_rst = Input(Bool())
  val D_delay_rst = Input(Bool())              // 标记下一条指令是否为延迟槽指令（需存储）
  val E_delay_rst = Input(Bool())              // 
  val D_ena = Input(Bool())
  val i_stall = Input(Bool())                  //流水线停滞信号
  val master_is_branch = Input(Bool())         // 主指令是否为分支指令（触发延迟槽）
  val master_is_in_delayslot_o = Output(Bool()) // 输出当前主指令是否在延迟槽中

  val read_en1 = Input(Bool())    
  val read_en2 = Input(Bool())    //读取使能                   
  val read_tlb_refill1 = Output(Bool())
  val read_tlb_refill2 = Output(Bool())
  val read_tlb_invalid1 = Output(Bool())
  val read_tlb_invalid2 = Output(Bool())
  val read_data1 = Output(UInt(32.W))  // instruction
  val read_data2 = Output(UInt(32.W))
  val read_address1 = Output(UInt(32.W)) // instruction address (PC)
  val read_address2 = Output(UInt(32.W)) 

  val write_en1 = Input(Bool()) 
  val write_en2 = Input(Bool())         // 写入使能
  val write_tlb_refill1 = Input(Bool())
  val write_tlb_refill2 = Input(Bool())  //tlb重填异常
  val write_tlb_invalid1 = Input(Bool())
  val write_tlb_invalid2 = Input(Bool()) //tlb无效异常
  val write_address1 = Input(UInt(32.W)) 
  val write_address2 = Input(UInt(32.W))  //指令地址（PC值）
  val write_data1 = Input(UInt(32.W))
  val write_data2 = Input(UInt(32.W))    //指令内容（32位） 
  
  val empty = Output(Bool())         //fifo为空
  val almost_empty = Output(Bool())  //fifo只剩一条指令
  val full = Output(Bool())          //fifo满（无法写入新指令）
}

class InstFifo extends Module {
  val io = IO(new InstFifoIO)

  // fifo structure
  val lines = Reg(Vec(16, new FifoEntry)) //16个深度的fifo，每个条目存储data addr tlb异常
  val read_line1 = Wire(new FifoEntry)
  val read_line2 = Wire(new FifoEntry)
  val write_line1 = Wire(new FifoEntry)
  val write_line2 = Wire(new FifoEntry)
  val delayslot_line = Reg(new FifoEntry) //临时保存延迟槽指令
  val delayslot_stall = RegInit(false.B) // 延迟槽数据未就绪时阻塞
  val delayslot_enable = RegInit(false.B) // 当前需要读取延迟槽指令

  // fifo control
  val write_pointer = RegInit(0.U(4.W))
  val read_pointer = RegInit(0.U(4.W))
  val data_count = RegInit(0.U(4.W))

  // INPUT and OUTPUT
  write_line1.refill := io.write_tlb_refill1
  write_line1.invalid := io.write_tlb_invalid1
  write_line1.addr := io.write_address1
  write_line1.data := io.write_data1

  write_line2.refill := io.write_tlb_refill2
  write_line2.invalid := io.write_tlb_invalid2
  write_line2.addr := io.write_address2
  write_line2.data := io.write_data2

  io.read_tlb_refill1 := read_line1.refill
  io.read_tlb_refill2 := read_line2.refill
  io.read_tlb_invalid1 := read_line1.invalid
  io.read_tlb_invalid2 := read_line2.invalid
  io.read_address1 := read_line1.addr
  io.read_address2 := read_line2.addr
  io.read_data1 := read_line1.data
  io.read_data2 := read_line2.data

  // fifo status
  io.full := data_count(3, 1).andR() || (write_pointer + 1.U === read_pointer) // 1110 data_count>=14,剩余空间不足2条指令，或指针即将重叠
  io.empty := data_count === 0.U               // 0000
  io.almost_empty := data_count === 1.U        // 0001

  // Delay slot judgment延迟槽处理
  when(io.rst || io.flush_delay_slot) {
    io.master_is_in_delayslot_o := false.B                   //当前在
  }.elsewhen(!io.read_en1) {
    io.master_is_in_delayslot_o := io.master_is_in_delayslot_o  
  }.elsewhen(io.master_is_branch && !io.read_en2) {
    io.master_is_in_delayslot_o := true.B                    //master is branch && slave未发射，下一条master是延迟槽指令
  }.otherwise {
    io.master_is_in_delayslot_o := false.B
  }

  // Delay slot read signal写入逻辑
  when(io.rst) {
    delayslot_stall := false.B
  }.elsewhen(io.fifo_rst && io.delay_sel_rst && !io.flush_delay_slot && io.i_stall && 
            (read_pointer + 1.U === write_pointer || read_pointer === write_pointer)) {
    delayslot_stall := true.B
  }.elsewhen(delayslot_stall && io.write_en1) {
    delayslot_stall := false.B
  }.otherwise {
    delayslot_stall := delayslot_stall
  }

  // Next instruction is in the delay slot that needs to be executed
  when(io.rst) {
    delayslot_enable := false.B
    delayslot_line := 0.U.asTypeOf(new FifoEntry)
  }.elsewhen(io.fifo_rst && !io.flush_delay_slot && io.delay_sel_rst) {
    when(io.E_delay_rst) {
      delayslot_enable := true.B
      delayslot_line := Mux(read_pointer === write_pointer, write_line1, lines(read_pointer))
    }.elsewhen(io.D_delay_rst) {
      delayslot_enable := true.B
      delayslot_line := Mux(read_pointer + 1.U === write_pointer, write_line1, lines(read_pointer + 1.U))
    }.otherwise {
      delayslot_enable := false.B
      delayslot_line := 0.U.asTypeOf(new FifoEntry)
    }
  }.elsewhen(!delayslot_stall && io.read_en1) {
    delayslot_enable := false.B
    delayslot_line := 0.U.asTypeOf(new FifoEntry)
  }

  // fifo read
  when(delayslot_enable) {
    read_line1 := delayslot_line
    read_line2 := 0.U.asTypeOf(new FifoEntry)
  }.elsewhen(io.empty) {
    read_line1 := 0.U.asTypeOf(new FifoEntry)
    read_line2 := 0.U.asTypeOf(new FifoEntry)
  }.elsewhen(io.almost_empty) {
    // can only read one piece of data
    read_line1 := lines(read_pointer)
    read_line2 := 0.U.asTypeOf(new FifoEntry)
  }.otherwise {
    // can read two pieces of data
    read_line1 := lines(read_pointer)
    read_line2 := lines(read_pointer + 1.U)
  }

  // fifo write
  when(io.write_en1) {
    lines(write_pointer) := write_line1
  }
  when(io.write_en2) {
    lines(write_pointer + 1.U) := write_line2
  }

  // Update write pointer
  when(io.fifo_rst) {
    write_pointer := 0.U
  }.elsewhen(io.write_en1 && io.write_en2) {
    write_pointer := write_pointer + 2.U
  }.elsewhen(io.write_en1) {
    write_pointer := write_pointer + 1.U
  }

  // Update read pointer
  when(io.fifo_rst) {
    read_pointer := 0.U
  }.elsewhen(io.empty || delayslot_enable) {
    read_pointer := read_pointer
  }.elsewhen(io.read_en1 && io.read_en2) {
    read_pointer := read_pointer + 2.U
  }.elsewhen(io.read_en1) {
    read_pointer := read_pointer + 1.U
  }

  // Update counter
  when(io.fifo_rst) {
    data_count := 0.U
  }.elsewhen(io.empty) {
    // only write, no read
    switch(Cat(io.write_en1, io.write_en2)) {
      is("b10".U) {
        data_count := data_count + 1.U
      }
      is("b11".U) {
        data_count := data_count + 2.U
      }
    }
  }.otherwise {
    // both write and read, write has priority
    switch(Cat(io.write_en1, io.write_en2, io.read_en1, io.read_en2)) {
      is("b1100".U) {
        data_count := data_count + 2.U
      }
      is("b1110".U, "b1000".U) {
        data_count := data_count + 1.U
      }
      is("b1011".U, "b0010".U) {
        data_count := data_count - 1.U
      }
      is("b0011".U) {
        data_count := Mux(data_count === 1.U, 0.U, data_count - 2.U)
      }
    }
  }

  // Statistics
  val slave_cnt = RegInit(0.U(65.W))
  val master_cnt = RegInit(0.U(65.W))
  
  when(io.rst) {
    master_cnt := 0.U
  }.elsewhen(io.read_en1 && (!io.empty || io.master_is_in_delayslot_o)) {
    master_cnt := master_cnt + 1.U
  }
  
  when(io.rst) {
    slave_cnt := 0.U
  }.elsewhen(io.read_en2 && (!io.empty && !io.master_is_branch && !io.almost_empty)) {
    slave_cnt := slave_cnt + 1.U
  }

  val total_cnt = master_cnt + slave_cnt
}
