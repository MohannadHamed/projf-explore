// Generated by CIRCT firtool-1.62.0
// Standard header to adapt well known macros for register randomization.
`ifndef RANDOMIZE
  `ifdef RANDOMIZE_REG_INIT
    `define RANDOMIZE
  `endif // RANDOMIZE_REG_INIT
`endif // not def RANDOMIZE

// RANDOM may be set to an expression that produces a 32-bit random unsigned value.
`ifndef RANDOM
  `define RANDOM $random
`endif // not def RANDOM

// Users can define INIT_RANDOM as general code that gets injected into the
// initializer block for modules with registers.
`ifndef INIT_RANDOM
  `define INIT_RANDOM
`endif // not def INIT_RANDOM

// If using random initialization, you can also define RANDOMIZE_DELAY to
// customize the delay used, otherwise 0.002 is used.
`ifndef RANDOMIZE_DELAY
  `define RANDOMIZE_DELAY 0.002
`endif // not def RANDOMIZE_DELAY

// Define INIT_RANDOM_PROLOG_ for use in our modules below.
`ifndef INIT_RANDOM_PROLOG_
  `ifdef RANDOMIZE
    `ifdef VERILATOR
      `define INIT_RANDOM_PROLOG_ `INIT_RANDOM
    `else  // VERILATOR
      `define INIT_RANDOM_PROLOG_ `INIT_RANDOM #`RANDOMIZE_DELAY begin end
    `endif // VERILATOR
  `else  // RANDOMIZE
    `define INIT_RANDOM_PROLOG_
  `endif // RANDOMIZE
`endif // not def INIT_RANDOM_PROLOG_

// Include register initializers in init blocks unless synthesis is set
`ifndef SYNTHESIS
  `ifndef ENABLE_INITIAL_REG_
    `define ENABLE_INITIAL_REG_
  `endif // not def ENABLE_INITIAL_REG_
`endif // not def SYNTHESIS

// Include rmemory initializers in init blocks unless synthesis is set
`ifndef SYNTHESIS
  `ifndef ENABLE_INITIAL_MEM_
    `define ENABLE_INITIAL_MEM_
  `endif // not def ENABLE_INITIAL_MEM_
`endif // not def SYNTHESIS

module mul(	// \\src\\main\\scala\\mul.scala:11:7
  input         clock,	// \\src\\main\\scala\\mul.scala:11:7
                reset,	// \\src\\main\\scala\\mul.scala:11:7
                io_start,	// \\src\\main\\scala\\mul.scala:19:14
  output        io_done,	// \\src\\main\\scala\\mul.scala:19:14
  input  [24:0] io_a,	// \\src\\main\\scala\\mul.scala:19:14
                io_b,	// \\src\\main\\scala\\mul.scala:19:14
  output [24:0] io_valOut	// \\src\\main\\scala\\mul.scala:19:14
);

  reg  [24:0] a1;	// \\src\\main\\scala\\mul.scala:33:15
  reg  [24:0] b1;	// \\src\\main\\scala\\mul.scala:34:15
  reg  [24:0] prodT;	// \\src\\main\\scala\\mul.scala:35:18
  reg  [49:0] prod;	// \\src\\main\\scala\\mul.scala:36:17
  reg  [20:0] rbits;	// \\src\\main\\scala\\mul.scala:37:18
  reg         round;	// \\src\\main\\scala\\mul.scala:38:22
  reg         even;	// \\src\\main\\scala\\mul.scala:39:21
  reg  [1:0]  state;	// \\src\\main\\scala\\mul.scala:45:22
  wire        _GEN = state == 2'h0;	// \\src\\main\\scala\\mul.scala:45:22, :53:17, :75:13
  wire        _GEN_0 = state == 2'h1;	// \\src\\main\\scala\\mul.scala:45:22, :53:17, :80:68
  wire        _GEN_1 = state == 2'h2;	// \\src\\main\\scala\\mul.scala:45:22, :53:17, :64:13
  wire        _GEN_2 = _GEN | _GEN_0 | _GEN_1;	// \\src\\main\\scala\\mul.scala:47:11, :53:17
  always @(posedge clock) begin	// \\src\\main\\scala\\mul.scala:11:7
    automatic logic _GEN_3;	// \\src\\main\\scala\\mul.scala:35:18, :53:17
    _GEN_3 = _GEN | _GEN_0 | ~_GEN_1;	// \\src\\main\\scala\\mul.scala:35:18, :53:17
    if (_GEN & io_start) begin	// \\src\\main\\scala\\mul.scala:33:15, :53:17, :55:22, :57:12
      a1 <= io_a;	// \\src\\main\\scala\\mul.scala:33:15
      b1 <= io_b;	// \\src\\main\\scala\\mul.scala:34:15
    end
    if (_GEN_3) begin	// \\src\\main\\scala\\mul.scala:35:18, :53:17
    end
    else	// \\src\\main\\scala\\mul.scala:35:18, :53:17
      prodT <= prod[45:21];	// \\src\\main\\scala\\mul.scala:35:18, :36:17, :69:20
    if (_GEN | ~_GEN_0) begin	// \\src\\main\\scala\\mul.scala:36:17, :53:17
    end
    else	// \\src\\main\\scala\\mul.scala:36:17, :53:17
      prod <= {{25{a1[24]}}, a1} * {{25{b1[24]}}, b1};	// \\src\\main\\scala\\mul.scala:33:15, :34:15, :36:17, :65:18
    if (_GEN_3) begin	// \\src\\main\\scala\\mul.scala:35:18, :37:18, :53:17
    end
    else	// \\src\\main\\scala\\mul.scala:37:18, :53:17
      rbits <= prod[20:0];	// \\src\\main\\scala\\mul.scala:36:17, :37:18, :70:20
    if (reset) begin	// \\src\\main\\scala\\mul.scala:11:7
      round <= 1'h0;	// \\src\\main\\scala\\mul.scala:32:24, :38:22
      even <= 1'h0;	// \\src\\main\\scala\\mul.scala:32:24, :39:21
      state <= 2'h0;	// \\src\\main\\scala\\mul.scala:45:22, :75:13
    end
    else begin	// \\src\\main\\scala\\mul.scala:11:7
      automatic logic [3:0][1:0] _GEN_4;	// \\src\\main\\scala\\mul.scala:45:22, :53:17, :55:22, :64:13, :68:13, :75:13
      if (_GEN_3) begin	// \\src\\main\\scala\\mul.scala:35:18, :39:21, :53:17
      end
      else begin	// \\src\\main\\scala\\mul.scala:39:21, :53:17
        round <= prod[21];	// \\src\\main\\scala\\mul.scala:36:17, :38:22, :71:20
        even <= ~(prod[22]);	// \\src\\main\\scala\\mul.scala:36:17, :39:21, :72:{15,20}
      end
      _GEN_4 = {{2'h0}, {2'h3}, {2'h2}, {io_start ? 2'h1 : state}};	// \\src\\main\\scala\\mul.scala:45:22, :53:17, :55:22, :56:15, :64:13, :68:13, :75:13, :80:68
      state <= _GEN_4[state];	// \\src\\main\\scala\\mul.scala:45:22, :53:17, :55:22, :64:13, :68:13, :75:13
    end
  end // always @(posedge)
  `ifdef ENABLE_INITIAL_REG_	// \\src\\main\\scala\\mul.scala:11:7
    `ifdef FIRRTL_BEFORE_INITIAL	// \\src\\main\\scala\\mul.scala:11:7
      `FIRRTL_BEFORE_INITIAL	// \\src\\main\\scala\\mul.scala:11:7
    `endif // FIRRTL_BEFORE_INITIAL
    initial begin	// \\src\\main\\scala\\mul.scala:11:7
      automatic logic [31:0] _RANDOM[0:4];	// \\src\\main\\scala\\mul.scala:11:7
      `ifdef INIT_RANDOM_PROLOG_	// \\src\\main\\scala\\mul.scala:11:7
        `INIT_RANDOM_PROLOG_	// \\src\\main\\scala\\mul.scala:11:7
      `endif // INIT_RANDOM_PROLOG_
      `ifdef RANDOMIZE_REG_INIT	// \\src\\main\\scala\\mul.scala:11:7
        for (logic [2:0] i = 3'h0; i < 3'h5; i += 3'h1) begin
          _RANDOM[i] = `RANDOM;	// \\src\\main\\scala\\mul.scala:11:7
        end	// \\src\\main\\scala\\mul.scala:11:7
        a1 = _RANDOM[3'h0][25:1];	// \\src\\main\\scala\\mul.scala:11:7, :33:15
        b1 = {_RANDOM[3'h0][31:26], _RANDOM[3'h1][18:0]};	// \\src\\main\\scala\\mul.scala:11:7, :33:15, :34:15
        prodT = {_RANDOM[3'h1][31:19], _RANDOM[3'h2][11:0]};	// \\src\\main\\scala\\mul.scala:11:7, :34:15, :35:18
        prod = {_RANDOM[3'h2][31:12], _RANDOM[3'h3][29:0]};	// \\src\\main\\scala\\mul.scala:11:7, :35:18, :36:17
        rbits = {_RANDOM[3'h3][31:30], _RANDOM[3'h4][18:0]};	// \\src\\main\\scala\\mul.scala:11:7, :36:17, :37:18
        round = _RANDOM[3'h4][19];	// \\src\\main\\scala\\mul.scala:11:7, :37:18, :38:22
        even = _RANDOM[3'h4][20];	// \\src\\main\\scala\\mul.scala:11:7, :37:18, :39:21
        state = _RANDOM[3'h4][22:21];	// \\src\\main\\scala\\mul.scala:11:7, :37:18, :45:22
      `endif // RANDOMIZE_REG_INIT
    end // initial
    `ifdef FIRRTL_AFTER_INITIAL	// \\src\\main\\scala\\mul.scala:11:7
      `FIRRTL_AFTER_INITIAL	// \\src\\main\\scala\\mul.scala:11:7
    `endif // FIRRTL_AFTER_INITIAL
  `endif // ENABLE_INITIAL_REG_
  assign io_done = ~_GEN_2 & (&state);	// \\src\\main\\scala\\mul.scala:11:7, :45:22, :47:11, :53:17
  assign io_valOut =
    _GEN_2 | ~(&state)
      ? 25'h0
      : round & ~(even & rbits == 21'h100000) ? prodT + 25'h1 : prodT;	// \\src\\main\\scala\\mul.scala:11:7, :35:18, :37:18, :38:22, :39:21, :45:22, :47:11, :51:13, :53:17, :80:{23,30,33,40,49,68}
endmodule

module mandelbrot(	// \\src\\main\\scala\\mandelbrot.scala:6:7
  input         clock,	// \\src\\main\\scala\\mandelbrot.scala:6:7
                reset,	// \\src\\main\\scala\\mandelbrot.scala:6:7
                io_start,	// \\src\\main\\scala\\mandelbrot.scala:12:14
  input  [24:0] io_re,	// \\src\\main\\scala\\mandelbrot.scala:12:14
                io_im,	// \\src\\main\\scala\\mandelbrot.scala:12:14
  output [7:0]  io_iter,	// \\src\\main\\scala\\mandelbrot.scala:12:14
  output        io_calculating,	// \\src\\main\\scala\\mandelbrot.scala:12:14
                io_done	// \\src\\main\\scala\\mandelbrot.scala:12:14
);

  wire        _mulModule_io_done;	// \\src\\main\\scala\\mandelbrot.scala:38:25
  wire [24:0] _mulModule_io_valOut;	// \\src\\main\\scala\\mandelbrot.scala:38:25
  reg  [24:0] x0;	// \\src\\main\\scala\\mandelbrot.scala:22:15
  reg  [24:0] y0;	// \\src\\main\\scala\\mandelbrot.scala:23:15
  reg  [24:0] x2;	// \\src\\main\\scala\\mandelbrot.scala:24:15
  reg  [24:0] y2;	// \\src\\main\\scala\\mandelbrot.scala:25:15
  reg  [24:0] x;	// \\src\\main\\scala\\mandelbrot.scala:26:14
  reg  [24:0] y;	// \\src\\main\\scala\\mandelbrot.scala:27:14
  reg  [24:0] mulA;	// \\src\\main\\scala\\mandelbrot.scala:30:17
  reg  [24:0] mulB;	// \\src\\main\\scala\\mandelbrot.scala:31:17
  reg  [24:0] mulVal;	// \\src\\main\\scala\\mandelbrot.scala:32:19
  reg  [24:0] mulValP;	// \\src\\main\\scala\\mandelbrot.scala:33:20
  reg         mulStart;	// \\src\\main\\scala\\mandelbrot.scala:34:25
  reg  [24:0] xt;	// \\src\\main\\scala\\mandelbrot.scala:46:15
  reg  [24:0] xy2;	// \\src\\main\\scala\\mandelbrot.scala:47:16
  reg  [2:0]  state;	// \\src\\main\\scala\\mandelbrot.scala:53:22
  reg  [7:0]  iter;	// \\src\\main\\scala\\mandelbrot.scala:56:21
  reg         calculating;	// \\src\\main\\scala\\mandelbrot.scala:57:28
  reg         done;	// \\src\\main\\scala\\mandelbrot.scala:58:21
  always @(posedge clock) begin	// \\src\\main\\scala\\mandelbrot.scala:6:7
    automatic logic             _GEN;	// \\src\\main\\scala\\mandelbrot.scala:64:17
    automatic logic             _GEN_0;	// \\src\\main\\scala\\mandelbrot.scala:64:17
    automatic logic             _GEN_1;	// \\src\\main\\scala\\mandelbrot.scala:82:62
    automatic logic             _GEN_2;	// \\src\\main\\scala\\mandelbrot.scala:64:17
    automatic logic             _GEN_3;	// \\src\\main\\scala\\mandelbrot.scala:33:20, :64:17, :95:21, :97:17
    automatic logic             _GEN_4;	// \\src\\main\\scala\\mandelbrot.scala:64:17
    automatic logic             _GEN_5;	// \\src\\main\\scala\\mandelbrot.scala:64:17
    automatic logic             _GEN_6;	// \\src\\main\\scala\\mandelbrot.scala:64:17
    automatic logic             _GEN_7;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :121:21, :122:15
    automatic logic             _GEN_8;	// \\src\\main\\scala\\mandelbrot.scala:25:15, :64:17
    automatic logic [24:0]      _GEN_9;	// \\src\\main\\scala\\mandelbrot.scala:25:15, :64:17
    automatic logic [24:0]      _GEN_10;	// \\src\\main\\scala\\mandelbrot.scala:47:16, :64:17
    automatic logic [7:0][24:0] _GEN_11;	// \\src\\main\\scala\\mandelbrot.scala:25:15, :64:17, :66:22
    automatic logic [7:0][24:0] _GEN_12;	// \\src\\main\\scala\\mandelbrot.scala:47:16, :64:17, :66:22
    _GEN = state == 3'h0;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :88:15
    _GEN_0 = state == 3'h1;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :67:15
    _GEN_1 = xy2[24:21] < 4'h5 & iter != 8'hFF;	// \\src\\main\\scala\\mandelbrot.scala:6:7, :47:16, :56:21, :82:{16,54,62,71}
    _GEN_2 = state == 3'h2;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :103:16
    _GEN_3 = _GEN_2 & _mulModule_io_done;	// \\src\\main\\scala\\mandelbrot.scala:33:20, :38:25, :64:17, :95:21, :97:17
    _GEN_4 = state == 3'h3;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :96:15
    _GEN_5 = state == 3'h4;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :82:54
    _GEN_6 = state == 3'h5;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :112:15
    _GEN_7 = _GEN_6 & _mulModule_io_done;	// \\src\\main\\scala\\mandelbrot.scala:38:25, :53:22, :64:17, :121:21, :122:15
    _GEN_8 = _GEN_0 | _GEN_2 | _GEN_4 | _GEN_5 | ~_GEN_7;	// \\src\\main\\scala\\mandelbrot.scala:25:15, :53:22, :64:17, :121:21, :122:15
    _GEN_9 = _GEN_8 ? y2 : mulVal;	// \\src\\main\\scala\\mandelbrot.scala:25:15, :32:19, :64:17
    _GEN_10 = _GEN_8 ? xy2 : x2 + mulVal;	// \\src\\main\\scala\\mandelbrot.scala:24:15, :25:15, :32:19, :47:16, :64:17, :124:19
    if (_GEN & io_start) begin	// \\src\\main\\scala\\mandelbrot.scala:22:15, :64:17, :66:22, :69:12
      x0 <= io_re;	// \\src\\main\\scala\\mandelbrot.scala:22:15
      y0 <= io_im;	// \\src\\main\\scala\\mandelbrot.scala:23:15
    end
    if (_GEN) begin	// \\src\\main\\scala\\mandelbrot.scala:64:17
      if (io_start) begin	// \\src\\main\\scala\\mandelbrot.scala:12:14
        x2 <= 25'h0;	// \\src\\main\\scala\\mandelbrot.scala:24:15, :71:11
        x <= 25'h0;	// \\src\\main\\scala\\mandelbrot.scala:26:14, :71:11
        y <= 25'h0;	// \\src\\main\\scala\\mandelbrot.scala:27:14, :71:11
        xt <= 25'h0;	// \\src\\main\\scala\\mandelbrot.scala:46:15, :71:11
      end
    end
    else begin	// \\src\\main\\scala\\mandelbrot.scala:64:17
      automatic logic _GEN_13;	// \\src\\main\\scala\\mandelbrot.scala:24:15, :64:17, :111:21, :113:12
      _GEN_13 = _GEN_5 & _mulModule_io_done;	// \\src\\main\\scala\\mandelbrot.scala:24:15, :38:25, :64:17, :111:21, :113:12
      if (_GEN_0 | _GEN_2 | _GEN_4 | ~_GEN_13) begin	// \\src\\main\\scala\\mandelbrot.scala:24:15, :64:17, :111:21, :113:12
      end
      else	// \\src\\main\\scala\\mandelbrot.scala:24:15, :64:17
        x2 <= mulVal;	// \\src\\main\\scala\\mandelbrot.scala:24:15, :32:19
      if (_GEN_0 | _GEN_2 | ~_GEN_4) begin	// \\src\\main\\scala\\mandelbrot.scala:27:14, :64:17
      end
      else begin	// \\src\\main\\scala\\mandelbrot.scala:27:14, :64:17
        x <= xt;	// \\src\\main\\scala\\mandelbrot.scala:26:14, :46:15
        y <= {mulValP[23:0], 1'h0} + y0;	// \\src\\main\\scala\\mandelbrot.scala:23:15, :27:14, :33:20, :34:25, :103:26
      end
      if (_GEN_0) begin	// \\src\\main\\scala\\mandelbrot.scala:64:17
        if (_GEN_1) begin	// \\src\\main\\scala\\mandelbrot.scala:82:62
          mulA <= x;	// \\src\\main\\scala\\mandelbrot.scala:26:14, :30:17
          mulB <= y;	// \\src\\main\\scala\\mandelbrot.scala:27:14, :31:17
        end
      end
      else if (~_GEN_2) begin	// \\src\\main\\scala\\mandelbrot.scala:64:17, :94:16
        if (_GEN_4) begin	// \\src\\main\\scala\\mandelbrot.scala:64:17
          mulA <= xt;	// \\src\\main\\scala\\mandelbrot.scala:30:17, :46:15
          mulB <= xt;	// \\src\\main\\scala\\mandelbrot.scala:31:17, :46:15
        end
        else if (_GEN_13) begin	// \\src\\main\\scala\\mandelbrot.scala:24:15, :64:17, :111:21, :113:12
          mulA <= y;	// \\src\\main\\scala\\mandelbrot.scala:27:14, :30:17
          mulB <= y;	// \\src\\main\\scala\\mandelbrot.scala:27:14, :31:17
        end
      end
      if (_GEN_0 | ~_GEN_3) begin	// \\src\\main\\scala\\mandelbrot.scala:33:20, :46:15, :64:17, :95:21, :97:17
      end
      else	// \\src\\main\\scala\\mandelbrot.scala:46:15, :64:17
        xt <= x2 - y2 + x0;	// \\src\\main\\scala\\mandelbrot.scala:22:15, :24:15, :25:15, :46:15, :98:{18,23}
    end
    _GEN_11 =
      {{_GEN_9}, {_GEN_9}, {_GEN_9}, {y2}, {y2}, {y2}, {y2}, {io_start ? 25'h0 : y2}};	// \\src\\main\\scala\\mandelbrot.scala:25:15, :64:17, :66:22, :71:11, :74:12
    y2 <= _GEN_11[state];	// \\src\\main\\scala\\mandelbrot.scala:25:15, :53:22, :64:17, :66:22
    mulVal <= _mulModule_io_valOut;	// \\src\\main\\scala\\mandelbrot.scala:32:19, :38:25
    if (_GEN | _GEN_0 | ~_GEN_3) begin	// \\src\\main\\scala\\mandelbrot.scala:33:20, :64:17, :95:21, :97:17
    end
    else	// \\src\\main\\scala\\mandelbrot.scala:33:20, :64:17
      mulValP <= mulVal;	// \\src\\main\\scala\\mandelbrot.scala:32:19, :33:20
    _GEN_12 =
      {{_GEN_10},
       {_GEN_10},
       {_GEN_10},
       {xy2},
       {xy2},
       {xy2},
       {xy2},
       {io_start ? 25'h0 : xy2}};	// \\src\\main\\scala\\mandelbrot.scala:47:16, :64:17, :66:22, :71:11, :75:13
    xy2 <= _GEN_12[state];	// \\src\\main\\scala\\mandelbrot.scala:47:16, :53:22, :64:17, :66:22
    if (reset) begin	// \\src\\main\\scala\\mandelbrot.scala:6:7
      mulStart <= 1'h0;	// \\src\\main\\scala\\mandelbrot.scala:34:25
      state <= 3'h0;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :88:15
      iter <= 8'h0;	// \\src\\main\\scala\\mandelbrot.scala:56:21
      calculating <= 1'h0;	// \\src\\main\\scala\\mandelbrot.scala:34:25, :57:28
      done <= 1'h0;	// \\src\\main\\scala\\mandelbrot.scala:34:25, :58:21
    end
    else begin	// \\src\\main\\scala\\mandelbrot.scala:6:7
      automatic logic [7:0]      _GEN_14;	// \\src\\main\\scala\\mandelbrot.scala:56:21, :64:17
      automatic logic [7:0][7:0] _GEN_15;	// \\src\\main\\scala\\mandelbrot.scala:56:21, :64:17, :66:22
      _GEN_14 = _GEN_8 ? iter : iter + 8'h1;	// \\src\\main\\scala\\mandelbrot.scala:25:15, :56:21, :64:17, :125:22
      if (_GEN)	// \\src\\main\\scala\\mandelbrot.scala:64:17
        calculating <= io_start | calculating;	// \\src\\main\\scala\\mandelbrot.scala:57:28, :66:22, :68:21
      else begin	// \\src\\main\\scala\\mandelbrot.scala:64:17
        if (_GEN_0)	// \\src\\main\\scala\\mandelbrot.scala:64:17
          mulStart <= _GEN_1 | mulStart;	// \\src\\main\\scala\\mandelbrot.scala:34:25, :82:{62,85}, :86:18
        else	// \\src\\main\\scala\\mandelbrot.scala:64:17
          mulStart <=
            ~_GEN_2 & (_GEN_4 | (_GEN_5 ? _mulModule_io_done : ~_GEN_6 & mulStart));	// \\src\\main\\scala\\mandelbrot.scala:34:25, :38:25, :64:17, :94:16, :107:16, :111:21, :120:16
        calculating <= (~_GEN_0 | _GEN_1) & calculating;	// \\src\\main\\scala\\mandelbrot.scala:57:28, :64:17, :82:{62,85}
      end
      if (reset)	// \\src\\main\\scala\\mandelbrot.scala:6:7
        state <= 3'h0;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :88:15
      else begin	// \\src\\main\\scala\\mandelbrot.scala:6:7
        automatic logic [2:0]      _GEN_16;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :121:21, :122:15
        automatic logic [7:0][2:0] _GEN_17;	// \\src\\main\\scala\\mandelbrot.scala:64:17, :66:22, :82:85, :95:21, :102:13, :111:21
        _GEN_16 = _GEN_7 ? 3'h1 : state;	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :67:15, :121:21, :122:15
        _GEN_17 =
          {{_GEN_16},
           {_GEN_16},
           {_GEN_16},
           {_mulModule_io_done ? 3'h5 : state},
           {3'h4},
           {_mulModule_io_done ? 3'h3 : state},
           {{1'h0, _GEN_1, 1'h0}},
           {io_start ? 3'h1 : state}};	// \\src\\main\\scala\\mandelbrot.scala:34:25, :38:25, :53:22, :64:17, :66:22, :67:15, :82:{54,62,85}, :83:15, :88:15, :95:21, :96:15, :102:13, :111:21, :112:15, :121:21, :122:15
        state <= _GEN_17[state];	// \\src\\main\\scala\\mandelbrot.scala:53:22, :64:17, :66:22, :82:85, :95:21, :102:13, :111:21
      end
      _GEN_15 =
        {{_GEN_14},
         {_GEN_14},
         {_GEN_14},
         {iter},
         {iter},
         {iter},
         {iter},
         {io_start ? 8'h0 : iter}};	// \\src\\main\\scala\\mandelbrot.scala:56:21, :64:17, :66:22, :77:14
      iter <= _GEN_15[state];	// \\src\\main\\scala\\mandelbrot.scala:53:22, :56:21, :64:17, :66:22
      if (_GEN | ~_GEN_0) begin	// \\src\\main\\scala\\mandelbrot.scala:57:28, :58:21, :64:17, :82:85
      end
      else	// \\src\\main\\scala\\mandelbrot.scala:58:21, :64:17
        done <= ~_GEN_1;	// \\src\\main\\scala\\mandelbrot.scala:34:25, :58:21, :67:15, :81:12, :82:{62,85}, :90:14
    end
  end // always @(posedge)
  `ifdef ENABLE_INITIAL_REG_	// \\src\\main\\scala\\mandelbrot.scala:6:7
    `ifdef FIRRTL_BEFORE_INITIAL	// \\src\\main\\scala\\mandelbrot.scala:6:7
      `FIRRTL_BEFORE_INITIAL	// \\src\\main\\scala\\mandelbrot.scala:6:7
    `endif // FIRRTL_BEFORE_INITIAL
    initial begin	// \\src\\main\\scala\\mandelbrot.scala:6:7
      automatic logic [31:0] _RANDOM[0:9];	// \\src\\main\\scala\\mandelbrot.scala:6:7
      `ifdef INIT_RANDOM_PROLOG_	// \\src\\main\\scala\\mandelbrot.scala:6:7
        `INIT_RANDOM_PROLOG_	// \\src\\main\\scala\\mandelbrot.scala:6:7
      `endif // INIT_RANDOM_PROLOG_
      `ifdef RANDOMIZE_REG_INIT	// \\src\\main\\scala\\mandelbrot.scala:6:7
        for (logic [3:0] i = 4'h0; i < 4'hA; i += 4'h1) begin
          _RANDOM[i] = `RANDOM;	// \\src\\main\\scala\\mandelbrot.scala:6:7
        end	// \\src\\main\\scala\\mandelbrot.scala:6:7
        x0 = _RANDOM[4'h0][24:0];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :22:15
        y0 = {_RANDOM[4'h0][31:25], _RANDOM[4'h1][17:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :22:15, :23:15
        x2 = {_RANDOM[4'h1][31:18], _RANDOM[4'h2][10:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :23:15, :24:15
        y2 = {_RANDOM[4'h2][31:11], _RANDOM[4'h3][3:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :24:15, :25:15
        x = _RANDOM[4'h3][28:4];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :25:15, :26:14
        y = {_RANDOM[4'h3][31:29], _RANDOM[4'h4][21:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :25:15, :27:14
        mulA = {_RANDOM[4'h4][31:22], _RANDOM[4'h5][14:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :27:14, :30:17
        mulB = {_RANDOM[4'h5][31:15], _RANDOM[4'h6][7:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :30:17, :31:17
        mulVal = {_RANDOM[4'h6][31:8], _RANDOM[4'h7][0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :31:17, :32:19
        mulValP = _RANDOM[4'h7][25:1];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :32:19, :33:20
        mulStart = _RANDOM[4'h7][26];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :32:19, :34:25
        xt = {_RANDOM[4'h7][31:27], _RANDOM[4'h8][19:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :32:19, :46:15
        xy2 = {_RANDOM[4'h8][31:20], _RANDOM[4'h9][12:0]};	// \\src\\main\\scala\\mandelbrot.scala:6:7, :46:15, :47:16
        state = _RANDOM[4'h9][15:13];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :47:16, :53:22
        iter = _RANDOM[4'h9][23:16];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :47:16, :56:21
        calculating = _RANDOM[4'h9][24];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :47:16, :57:28
        done = _RANDOM[4'h9][25];	// \\src\\main\\scala\\mandelbrot.scala:6:7, :47:16, :58:21
      `endif // RANDOMIZE_REG_INIT
    end // initial
    `ifdef FIRRTL_AFTER_INITIAL	// \\src\\main\\scala\\mandelbrot.scala:6:7
      `FIRRTL_AFTER_INITIAL	// \\src\\main\\scala\\mandelbrot.scala:6:7
    `endif // FIRRTL_AFTER_INITIAL
  `endif // ENABLE_INITIAL_REG_
  mul mulModule (	// \\src\\main\\scala\\mandelbrot.scala:38:25
    .clock     (clock),
    .reset     (reset),
    .io_start  (mulStart),	// \\src\\main\\scala\\mandelbrot.scala:34:25
    .io_done   (_mulModule_io_done),
    .io_a      (mulA),	// \\src\\main\\scala\\mandelbrot.scala:30:17
    .io_b      (mulB),	// \\src\\main\\scala\\mandelbrot.scala:31:17
    .io_valOut (_mulModule_io_valOut)
  );
  assign io_iter = iter;	// \\src\\main\\scala\\mandelbrot.scala:6:7, :56:21
  assign io_calculating = calculating;	// \\src\\main\\scala\\mandelbrot.scala:6:7, :57:28
  assign io_done = done;	// \\src\\main\\scala\\mandelbrot.scala:6:7, :58:21
endmodule

