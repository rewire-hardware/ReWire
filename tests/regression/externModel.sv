module top_level (input logic [31:0] __in0,
  output logic [31:0] __out0);
  logic [31:0] zll_main_f_in;
  logic [31:0] main_andw32_in;
  logic [63:0] andw32_in;
  logic [31:0] extres;
  logic [31:0] notw32_in;
  logic [31:0] extresR1;
  logic [31:0] main_xorw32_in;
  logic [63:0] xorw32_in;
  logic [31:0] extresR2;
  logic [63:0] plusw32_in;
  logic [31:0] extresR3;
  assign zll_main_f_in = __in0;
  assign main_andw32_in = zll_main_f_in[31:0];
  assign andw32_in = {main_andw32_in[31:0], 32'hf0f0f0f0};
  andW32  inst (andw32_in[63:32], andw32_in[31:0], extres[31:0]);
  assign notw32_in = zll_main_f_in[31:0];
  notW32  instR1 (notw32_in[31:0], extresR1[31:0]);
  assign main_xorw32_in = extresR1;
  assign xorw32_in = {main_xorw32_in[31:0], 32'h12345678};
  xorW32  instR2 (xorw32_in[63:32], xorw32_in[31:0], extresR2[31:0]);
  assign plusw32_in = {extres, extresR2};
  plusW32  instR3 (plusw32_in[63:32], plusw32_in[31:0], extresR3[31:0]);
  assign __out0 = extresR3;
endmodule