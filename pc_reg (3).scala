import chisel3._
import chisel3.util._

// PCReg的IO接口定义
class PCRegIO extends Bundle {
  // 时钟和复位 删clk和rst
  
  // 控制信号
  //val pc_en             = Input(Bool())     // PC使能信号(当前代码未使用)
  
  // 异常处理
  //val M_except          = Input(Bool())     // 异常发生信号
  //val M_except_addr     = Input(UInt(32.W)) // 异常处理地址
  //val M_flush_all       = Input(Bool())     // 流水线刷新信号
  //val M_flush_all_addr  = Input(UInt(32.W)) // 刷新后地址
  
  // 分支预测和执行
  val E_bj             = Input(Bool())      // 执行阶段实际分支跳转信号
  val E_bj_correct     = Input(Bool())      //新增：分支预测是否正确
  val E_bj_target      = Input(UInt(32.W))  // 执行阶段分支目标地址
  val D_bj             = Input(Bool())      // 译码阶段预测分支信号
  val D_bj_target      = Input(UInt(32.W))  // 预测分支目标地址
  
  // 流控 fifo给信号
  val F_fifo_full      = Input(Bool())      // 指令FIFO满信号(反压)
  val F_fifo_almost_full = Input(Bool())    
  
  // 取指成功指示（默认 可以获取）
  //val F_inst_data_ok1  = Input(Bool())      // 第一条指令获取成功
  //val F_inst_data_ok2  = Input(Bool())      // 第二条指令获取成功
  
  // 输出
  val pc_next          = Output(UInt(32.W)) // 下一个PC值
  val pc_curr          = Output(UInt(32.W)) // 当前PC值
}

class PCReg extends Module {
  val io = IO(new PCRegIO)

  // PC寄存器，初始化为0xbfc00000(通常为MIPS架构的启动地址)
  val pc_reg = RegInit("hbfc00000".U(32.W))

  // 分支预测状态寄存器
  val predicted_branch = RegInit(false.B)
  val predicted_target = RegInit(0.U(32.W))
  
  // 更新预测状态
  when(io.D_bj) { //现在有一个未验证的分支预测在执行流水线中
    predicted_branch := true.B
    predicted_target := io.D_bj_target
  }.elsewhen(io.E_bj || !io.E_bj_correct) { //错误预测已处理 或正确预测已验证
    predicted_branch := false.B
  }
  
  // PC更新逻辑(优先级从高到低)
  when(reset) {
  // 1. 复位: 将PC设置为启动地址
    pc_reg := "hbfc00000".U
    predicted_branch := false.B  //清除
    predicted_target := 0.U
  }.elsewhen(io.E_bj && !io.E_bj_correct) {
  // 2. 执行阶段发现预测错误: 修正跳转（跳转到执行阶段计算出的正确目标地址）
    pc_reg := io.E_bj_target
    predicted_branch := false.B  //错误预测已经被处理
  }.elsewhen(io.D_bj && !predicted_branch) {
  // 3. 新分支预测: 只有当前没有未完成的分支预测时才接受新预测
    pc_reg := io.D_bj_target
  }.elsewhen(io.F_fifo_full) {
  // 4. FIFO满时保持PC不变(反压)
    pc_reg := pc_reg
  }.elsewhen(!predicted_branch) {
  // 5. 无分支预测时的正常PC递增
    when(!io.F_fifo_almost_full && !io.F_fifo_full) {
      pc_reg := pc_reg + 8.U  // 双指令
    }.elsewhen(io.F_fifo_almost_full) {
      pc_reg := pc_reg + 4.U  // 单指令
    }.otherwise {
      pc_reg := pc_reg
    }
  }.otherwise {
  // 6. 有未完成的分支预测+尚未执行验证，保持PC不变
    pc_reg := pc_reg
  }


  // 计算下一个PC值的组合逻辑
  io.pc_next := Mux(reset, "hbfc00000".U,
               Mux(io.E_bj && !io.E_bj_correct, io.E_bj_target,  //EX阶段确定需要跳转，预测错误 执行阶段计算出的正确目标地址
               Mux(io.D_bj && !predicted_branch, io.D_bj_target,  //ID阶段检测到分支指令，当前没有未完成的分支预测 预测的目标地址
               Mux(io.F_fifo_full, pc_reg,
               Mux(!predicted_branch, 
                 Mux(!io.F_fifo_almost_full && !io.F_fifo_full, pc_reg + 8.U,
                 Mux(io.F_fifo_almost_full, pc_reg + 4.U,
                 pc_reg)),
               pc_reg))))
  

  // 输出当前PC值
  io.pc_curr := pc_reg
}
