module top_level (input logic [1023:0] __in0,
  input logic [31:0] __in1,
  output logic [63:0] __out0);
  logic [7:0] zll_main_compute3_out;
  logic [7:0] zll_main_compute3_outR1;
  logic [7:0] zll_main_compute3_outR2;
  logic [7:0] zll_main_compute3_outR3;
  logic [7:0] zll_main_compute3_outR4;
  logic [7:0] zll_main_compute3_outR5;
  logic [7:0] zll_main_compute3_outR6;
  logic [7:0] zll_main_compute3_outR7;
  logic [64:0] zi5;
  logic [63:0] zi6;
  logic [64:0] zres;
  ZLL_Main_compute3  inst (__in1, __in0[1023:960], zll_main_compute3_out);
  ZLL_Main_compute3  instR1 (__in1, __in0[959:896], zll_main_compute3_outR1);
  ZLL_Main_compute3  instR2 (__in1, __in0[895:832], zll_main_compute3_outR2);
  ZLL_Main_compute3  instR3 (__in1, __in0[831:768], zll_main_compute3_outR3);
  ZLL_Main_compute3  instR4 (__in1, __in0[767:704], zll_main_compute3_outR4);
  ZLL_Main_compute3  instR5 (__in1, __in0[703:640], zll_main_compute3_outR5);
  ZLL_Main_compute3  instR6 (__in1, __in0[639:576], zll_main_compute3_outR6);
  ZLL_Main_compute3  instR7 (__in1, __in0[575:512], zll_main_compute3_outR7);
  assign zi5 = {1'h0, zll_main_compute3_out, zll_main_compute3_outR1, zll_main_compute3_outR2, zll_main_compute3_outR3, zll_main_compute3_outR4, zll_main_compute3_outR5, zll_main_compute3_outR6, zll_main_compute3_outR7};
  assign zi6 = zi5[63:0];
  assign zres = {1'h1, zi6};
  assign __out0 = zres[63:0];
endmodule

module ZLL_Main_compute3 (input logic [31:0] arg0,
  input logic [63:0] arg1,
  output logic [7:0] res);
  assign res = arg0[7:0] + arg1[39:32];
endmodule