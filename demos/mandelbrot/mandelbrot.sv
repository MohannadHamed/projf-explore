// Generated by CIRCT firtool-1.62.0
module mul(
  input         clock,
                reset,
                io_start,
  output        io_done,
  input  [24:0] io_a,
                io_b,
  output [24:0] io_valOut
);

  reg  [24:0] a1;
  reg  [24:0] b1;
  reg  [24:0] prodT;
  reg  [49:0] prod;
  reg  [20:0] rbits;
  reg         round;
  reg         even;
  reg  [1:0]  state;
  wire        _GEN = state == 2'h0;
  wire        _GEN_0 = state == 2'h1;
  wire        _GEN_1 = state == 2'h2;
  wire        _GEN_2 = _GEN | _GEN_0 | _GEN_1;
  always @(posedge clock) begin
    automatic logic _GEN_3;
    _GEN_3 = _GEN | _GEN_0 | ~_GEN_1;
    if (_GEN & io_start) begin
      a1 <= io_a;
      b1 <= io_b;
    end
    if (_GEN_3) begin
    end
    else
      prodT <= prod[45:21];
    if (_GEN | ~_GEN_0) begin
    end
    else
      prod <= {{25{a1[24]}}, a1} * {{25{b1[24]}}, b1};
    if (_GEN_3) begin
    end
    else
      rbits <= prod[20:0];
    if (reset) begin
      round <= 1'h0;
      even <= 1'h0;
      state <= 2'h0;
    end
    else begin
      automatic logic [3:0][1:0] _GEN_4 =
        {{2'h0}, {2'h3}, {2'h2}, {io_start ? 2'h1 : state}};
      if (_GEN_3) begin
      end
      else begin
        round <= prod[21];
        even <= ~(prod[22]);
      end
      state <= _GEN_4[state];
    end
  end // always @(posedge)
  assign io_done = ~_GEN_2 & (&state);
  assign io_valOut =
    _GEN_2 | ~(&state)
      ? 25'h0
      : round & ~(even & rbits == 21'h100000) ? prodT + 25'h1 : prodT;
endmodule

module mandelbrot(
  input         clock,
                reset,
                io_start,
  input  [24:0] io_re,
                io_im,
  output [7:0]  io_iter,
  output        io_calculating,
                io_done
);

  wire        _mulModule_io_done;
  wire [24:0] _mulModule_io_valOut;
  reg  [24:0] x0;
  reg  [24:0] y0;
  reg  [24:0] x2;
  reg  [24:0] y2;
  reg  [24:0] x;
  reg  [24:0] y;
  reg  [24:0] mulA;
  reg  [24:0] mulB;
  reg  [24:0] mulValP;
  reg         mulStart;
  reg  [24:0] xt;
  reg  [24:0] xy2;
  reg  [2:0]  state;
  reg  [7:0]  iter;
  reg         calculating;
  reg         done;
  always @(posedge clock) begin
    automatic logic             _GEN;
    automatic logic             _GEN_0;
    automatic logic             _GEN_1;
    automatic logic             _GEN_2;
    automatic logic             _GEN_3;
    automatic logic             _GEN_4;
    automatic logic             _GEN_5;
    automatic logic             _GEN_6;
    automatic logic             _GEN_7;
    automatic logic             _GEN_8;
    automatic logic [24:0]      _GEN_9;
    automatic logic [24:0]      _GEN_10;
    automatic logic [7:0][24:0] _GEN_11;
    automatic logic [7:0][24:0] _GEN_12;
    _GEN = state == 3'h0;
    _GEN_0 = state == 3'h1;
    _GEN_1 = $signed(xy2[24:21]) < 4'sh5 & iter != 8'hFF;
    _GEN_2 = state == 3'h2;
    _GEN_3 = _GEN_2 & _mulModule_io_done;
    _GEN_4 = state == 3'h3;
    _GEN_5 = state == 3'h4;
    _GEN_6 = state == 3'h5;
    _GEN_7 = _GEN_6 & _mulModule_io_done;
    _GEN_8 = _GEN_0 | _GEN_2 | _GEN_4 | _GEN_5 | ~_GEN_7;
    _GEN_9 = _GEN_8 ? y2 : _mulModule_io_valOut;
    _GEN_10 = _GEN_8 ? xy2 : x2 + _mulModule_io_valOut;
    if (_GEN & io_start) begin
      x0 <= io_re;
      y0 <= io_im;
    end
    if (_GEN) begin
      if (io_start) begin
        x2 <= 25'h0;
        x <= 25'h0;
        y <= 25'h0;
        xt <= 25'h0;
      end
    end
    else begin
      automatic logic _GEN_13;
      _GEN_13 = _GEN_5 & _mulModule_io_done;
      if (_GEN_0 | _GEN_2 | _GEN_4 | ~_GEN_13) begin
      end
      else
        x2 <= _mulModule_io_valOut;
      if (_GEN_0 | _GEN_2 | ~_GEN_4) begin
      end
      else begin
        x <= xt;
        y <= {mulValP[23:0], 1'h0} + y0;
      end
      if (_GEN_0) begin
        if (_GEN_1) begin
          mulA <= x;
          mulB <= y;
        end
        mulStart <= _GEN_1 | mulStart;
      end
      else begin
        if (~_GEN_2) begin
          if (_GEN_4) begin
            mulA <= xt;
            mulB <= xt;
          end
          else if (_GEN_13) begin
            mulA <= y;
            mulB <= y;
          end
        end
        mulStart <=
          ~_GEN_2 & (_GEN_4 | (_GEN_5 ? _mulModule_io_done : ~_GEN_6 & mulStart));
      end
      if (_GEN_0 | ~_GEN_3) begin
      end
      else
        xt <= x2 - y2 + x0;
    end
    _GEN_11 =
      {{_GEN_9}, {_GEN_9}, {_GEN_9}, {y2}, {y2}, {y2}, {y2}, {io_start ? 25'h0 : y2}};
    y2 <= _GEN_11[state];
    if (_GEN | _GEN_0 | ~_GEN_3) begin
    end
    else
      mulValP <= _mulModule_io_valOut;
    _GEN_12 =
      {{_GEN_10},
       {_GEN_10},
       {_GEN_10},
       {xy2},
       {xy2},
       {xy2},
       {xy2},
       {io_start ? 25'h0 : xy2}};
    xy2 <= _GEN_12[state];
    if (reset) begin
      state <= 3'h0;
      iter <= 8'h0;
      calculating <= 1'h0;
      done <= 1'h0;
    end
    else begin
      automatic logic [2:0]      _GEN_14;
      automatic logic [7:0]      _GEN_15;
      automatic logic [7:0][2:0] _GEN_16;
      automatic logic [7:0][7:0] _GEN_17;
      _GEN_14 = _GEN_7 ? 3'h1 : state;
      _GEN_15 = _GEN_8 ? iter : iter + 8'h1;
      _GEN_16 =
        {{_GEN_14},
         {_GEN_14},
         {_GEN_14},
         {_mulModule_io_done ? 3'h5 : state},
         {3'h4},
         {_mulModule_io_done ? 3'h3 : state},
         {{1'h0, _GEN_1, 1'h0}},
         {io_start ? 3'h1 : state}};
      state <= _GEN_16[state];
      _GEN_17 =
        {{_GEN_15},
         {_GEN_15},
         {_GEN_15},
         {iter},
         {iter},
         {iter},
         {iter},
         {io_start ? 8'h0 : iter}};
      iter <= _GEN_17[state];
      if (_GEN)
        calculating <= io_start | calculating;
      else
        calculating <= (~_GEN_0 | _GEN_1) & calculating;
      if (_GEN | ~_GEN_0) begin
      end
      else
        done <= ~_GEN_1;
    end
  end // always @(posedge)
  mul mulModule (
    .clock     (clock),
    .reset     (reset),
    .io_start  (mulStart),
    .io_done   (_mulModule_io_done),
    .io_a      (mulA),
    .io_b      (mulB),
    .io_valOut (_mulModule_io_valOut)
  );
  assign io_iter = iter;
  assign io_calculating = calculating;
  assign io_done = done;
endmodule

