module top_level (input logic [1023:0] __in0,
  input logic [31:0] __in1,
  output logic [63:0] __out0);
  logic [7:0] zll_main_compute1_out;
  logic [7:0] zll_main_compute1_outR1;
  logic [7:0] zll_main_compute1_outR2;
  logic [7:0] zll_main_compute1_outR3;
  logic [7:0] zll_main_compute1_outR4;
  logic [7:0] zll_main_compute1_outR5;
  logic [7:0] zll_main_compute1_outR6;
  logic [7:0] zll_main_compute1_outR7;
  logic [63:0] zi4;
  logic [63:0] zres;
  ZLL_Main_compute1  inst (__in1, __in0[1023:960], zll_main_compute1_out);
  ZLL_Main_compute1  instR1 (__in1, __in0[959:896], zll_main_compute1_outR1);
  ZLL_Main_compute1  instR2 (__in1, __in0[895:832], zll_main_compute1_outR2);
  ZLL_Main_compute1  instR3 (__in1, __in0[831:768], zll_main_compute1_outR3);
  ZLL_Main_compute1  instR4 (__in1, __in0[767:704], zll_main_compute1_outR4);
  ZLL_Main_compute1  instR5 (__in1, __in0[703:640], zll_main_compute1_outR5);
  ZLL_Main_compute1  instR6 (__in1, __in0[639:576], zll_main_compute1_outR6);
  ZLL_Main_compute1  instR7 (__in1, __in0[575:512], zll_main_compute1_outR7);
  assign zi4 = {zll_main_compute1_out, zll_main_compute1_outR1, zll_main_compute1_outR2, zll_main_compute1_outR3, zll_main_compute1_outR4, zll_main_compute1_outR5, zll_main_compute1_outR6, zll_main_compute1_outR7};
  assign zres = zi4;
  assign __out0 = zres;
endmodule

module ZLL_Main_compute1 (input logic [31:0] arg0,
  input logic [63:0] arg1,
  output logic [7:0] res);
  assign res = arg0[7:0] + arg1[39:32];
endmodule