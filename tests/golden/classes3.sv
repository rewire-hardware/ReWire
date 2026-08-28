module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [1:0] Main_$cunfrob$Bool$Bool$s32ddedfb_out;
  logic [1:0] Main_$cunfrob$Bool$Bool$s32ddedfb_outR1;
  logic [1:0] Main_$cunfrob$Bool$Bool$s32ddedfb_outR2;
  logic [1:0] Main_$cunfrob$Bool$Bool$s32ddedfb_outR3;
  logic [1:0] Main_$cfrob$Bool$Bool$s32ddedfb_out;
  logic [1:0] Main_$cfrob$Bool$Bool$s32ddedfb_outR1;
  logic [1:0] Main_$cfrob$Bool$Bool$s32ddedfb_outR2;
  logic [1:0] Main_$cfrob$Bool$Bool$s32ddedfb_outR3;
  logic [1:0] Main_$cunfrob$Bool$Bool$s32ddedfb_outR4;
  logic [1:0] Main_$cfrob$Bool$Bool$s32ddedfb_outR4;
  logic [1:0] Main_$cfrob$Bool$Bool$s32ddedfb_outR5;
  // combinational logic
  Main_$cunfrob$Bool$Bool$s32ddedfb  Zcunfrob$Bool$Bool$s32ddedfb_i ({__in0[0], __in0[1]}, Main_$cunfrob$Bool$Bool$s32ddedfb_out);
  Main_$cunfrob$Bool$Bool$s32ddedfb  Zcunfrob$Bool$Bool$s32ddedfb_iR1 ({__in0[2], __in0[3]}, Main_$cunfrob$Bool$Bool$s32ddedfb_outR1);
  Main_$cunfrob$Bool$Bool$s32ddedfb  Zcunfrob$Bool$Bool$s32ddedfb_iR2 ({__in0[4], __in0[5]}, Main_$cunfrob$Bool$Bool$s32ddedfb_outR2);
  Main_$cunfrob$Bool$Bool$s32ddedfb  Zcunfrob$Bool$Bool$s32ddedfb_iR3 ({__in0[6], __in0[7]}, Main_$cunfrob$Bool$Bool$s32ddedfb_outR3);
  wire [7:0] Zeta0 = {Main_$cunfrob$Bool$Bool$s32ddedfb_out, Main_$cunfrob$Bool$Bool$s32ddedfb_outR1, Main_$cunfrob$Bool$Bool$s32ddedfb_outR2, Main_$cunfrob$Bool$Bool$s32ddedfb_outR3};
  Main_$cfrob$Bool$Bool$s32ddedfb  Zcfrob$Bool$Bool$s32ddedfb_i (Zeta0[7:6], Main_$cfrob$Bool$Bool$s32ddedfb_out);
  Main_$cfrob$Bool$Bool$s32ddedfb  Zcfrob$Bool$Bool$s32ddedfb_iR1 (Zeta0[5:4], Main_$cfrob$Bool$Bool$s32ddedfb_outR1);
  Main_$cfrob$Bool$Bool$s32ddedfb  Zcfrob$Bool$Bool$s32ddedfb_iR2 (Zeta0[3:2], Main_$cfrob$Bool$Bool$s32ddedfb_outR2);
  Main_$cfrob$Bool$Bool$s32ddedfb  Zcfrob$Bool$Bool$s32ddedfb_iR3 (Zeta0[1:0], Main_$cfrob$Bool$Bool$s32ddedfb_outR3);
  wire [7:0] Zt0 = {Main_$cfrob$Bool$Bool$s32ddedfb_out, Main_$cfrob$Bool$Bool$s32ddedfb_outR1, Main_$cfrob$Bool$Bool$s32ddedfb_outR2, Main_$cfrob$Bool$Bool$s32ddedfb_outR3};
  wire [1:0] Zt1 = Zt0[7:6];
  wire [0:0] x = Zt1[1];
  wire [1:0] xR1 = {__in0[0], x};
  Main_$cunfrob$Bool$Bool$s32ddedfb  Zcunfrob$Bool$Bool$s32ddedfb_iR4 (xR1, Main_$cunfrob$Bool$Bool$s32ddedfb_outR4);
  Main_$cfrob$Bool$Bool$s32ddedfb  Zcfrob$Bool$Bool$s32ddedfb_iR4 (Main_$cunfrob$Bool$Bool$s32ddedfb_outR4, Main_$cfrob$Bool$Bool$s32ddedfb_outR4);
  wire [0:0] xR2 = Main_$cfrob$Bool$Bool$s32ddedfb_outR4[1];
  Main_$cfrob$Bool$Bool$s32ddedfb  Zcfrob$Bool$Bool$s32ddedfb_iR5 ({__in0[1], __in0[7]}, Main_$cfrob$Bool$Bool$s32ddedfb_outR5);
  wire [0:0] y = Main_$cfrob$Bool$Bool$s32ddedfb_outR5[0];
  wire [0:0] Zt3 = xR2 ? 1'h1 : y;
  wire [7:0] Za = (~Zt3) ? __in0 : (__in0 ^ 8'h55);
  // outputs
  assign __out0 = Za;
endmodule

// Main.$cunfrob
module Main_$cunfrob (input logic [0:0] b,
  output logic [0:0] res);
  assign res = b;
endmodule

// Main.$cfrob$Bool$Bool$s32ddedfb
// specialized from 'Main.$cfrob' at Bool, Bool
// also: Main.$cfrob$Bool$Bool$sda47482e, Main.$csole$Bool$Bool$sa0d0e4b4
module Main_$cfrob$Bool$Bool$s32ddedfb (input logic [1:0] Zds,
  output logic [1:0] res);
  wire [0:0] a = Zds[1];
  wire [0:0] b = Zds[0];
  assign res = {~a, ~b};
endmodule

// Main.$cunfrob$Bool$Bool$s32ddedfb
// specialized from 'Main.$cunfrob' at Bool, Bool
// also: Main.$cunfrob$Bool$Bool$sda47482e
module Main_$cunfrob$Bool$Bool$s32ddedfb (input logic [1:0] Zds,
  output logic [1:0] res);
  logic [0:0] Main_$cunfrob_out;
  logic [0:0] Main_$cunfrob_outR1;
  wire [0:0] a = Zds[1];
  wire [0:0] b = Zds[0];
  Main_$cunfrob  Zcunfrob_i (a, Main_$cunfrob_out);
  Main_$cunfrob  Zcunfrob_iR1 (b, Main_$cunfrob_outR1);
  assign res = {Main_$cunfrob_out, Main_$cunfrob_outR1};
endmodule