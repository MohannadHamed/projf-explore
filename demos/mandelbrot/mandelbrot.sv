import chisel3._
import _root_.circt.stage.ChiselStage
import chisel3.util._
import chisel3.ChiselEnum

class mandelbrot() extends Module {
  val fpWidth: Int = 25
  val fpInt: Int = 4
  val iterMax: Int = 255
  //val iterWidth = log2Ceil(iterMax + 1)
  val iterWidth = 8
  val io = IO(new Bundle {
    //val clk = Input(Clock())
    //val rst = Input(Bool())
    val start = Input(Bool())
    val re = Input(SInt(fpWidth.W))
    val im = Input(SInt(fpWidth.W))
    val iter = Output(UInt(iterWidth.W))
    val calculating = Output(Bool())
    val done = Output(Bool())
  })

  val x0 = Reg(SInt(fpWidth.W))
  val y0 = Reg(SInt(fpWidth.W))
  val x2 = Reg(SInt(fpWidth.W))
  val y2 = Reg(SInt(fpWidth.W))
  val x = Reg(SInt(fpWidth.W))
  val y = Reg(SInt(fpWidth.W))

  val mulA = Reg(SInt(fpWidth.W))
  val mulB = Reg(SInt(fpWidth.W))
  val mulVal = Wire(SInt(fpWidth.W))
  val mulValP = Reg(SInt(fpWidth.W))
  val mulStart = Reg(Bool())
  val mulDone = Wire(Bool())

  val mulModule = Module(new mul())
  //mulModule.io.clk := io.clk
  //mulModule.io.rst := io.rst
  mulModule.io.start := mulStart
  mulModule.io.a := mulA
  mulModule.io.b := mulB
  mulVal := mulModule.io.valOut
  mulDone := mulModule.io.done

  val xt = Reg(SInt(fpWidth.W))
  val xy2 = Reg(SInt(fpWidth.W))

  object State extends ChiselEnum {
    val idle, step1, step2, step2a, step3, step4  = Value
  }
  //val idle :: step1 :: step2 :: step2a :: step3 :: step4 :: Nil = Enum(6)
  val state = RegInit(State.idle)
  val iter = RegInit(0.U(iterWidth.W))
  val calculating = RegInit(false.B)
  val done = RegInit(false.B)
  io.iter := iter
  io.calculating := calculating
  io.done := done

  switch(state) {
    is(State.idle) {
      when(io.start) {
        state := State.step1
        calculating := true.B
        x0 := io.re
        y0 := io.im
        x := 0.S
        y := 0.S
        x2 := 0.S
        y2 := 0.S
        xy2 := 0.S
        xt := 0.S
        iter := 0.U
      }
    }
    is(State.step1) {
      done := false.B
      when((xy2(fpWidth - 1, fpWidth - fpInt) <= 4.U) && (iter < iterMax.U)) {
        state := State.step2
        mulA := x
        mulB := y
        mulStart := true.B
      } .otherwise {
        state := State.idle
        calculating := false.B
        done := true.B
      }
    }
    is(State.step2) {
      mulStart := false.B
      when(mulDone) {
        state := State.step2a
        mulValP := mulVal
        xt := x2 - y2 + x0
      }
    }
    is(State.step2a) {
      state := State.step3
      y := 2.S * mulValP + y0
      x := xt
      mulA := xt
      mulB := xt
      mulStart := true.B
    }
    is(State.step3) {
      mulStart := false.B
      when(mulDone) {
        state := State.step4
        x2 := mulVal
        mulA := y
        mulB := y
        mulStart := true.B
      }
    }
    is(State.step4) {
      mulStart := false.B
      when(mulDone) {
        state := State.step1
        y2 := mulVal
        xy2 := x2 + mulVal
        iter := iter + 1.U
      }
    }
  }
  /*when(reset.asBool) {
    state := State.idle
  }*/
}

object mandelbrot extends App {
  ChiselStage.emitSystemVerilogFile(
    new mandelbrot,
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}


