module top_level (input logic [1023:0] __in0,
  input logic [31:0] __in1,
  output logic [63:0] __out0);
  logic [1055:0] zin;
  logic [1023:0] zi0;
  logic [31:0] zi1;
  logic [1055:0] zi2;
  logic [1023:0] zi3;
  logic [31:0] zi4;
  logic [511:0] zi5;
  logic [7:0] zll_main_compute_out;
  logic [511:0] zi6;
  logic [7:0] zll_main_compute_outR1;
  logic [511:0] zi7;
  logic [7:0] zll_main_compute_outR2;
  logic [511:0] zi8;
  logic [7:0] zll_main_compute_outR3;
  logic [511:0] zi9;
  logic [7:0] zll_main_compute_outR4;
  logic [511:0] zi10;
  logic [7:0] zll_main_compute_outR5;
  logic [511:0] zi11;
  logic [7:0] zll_main_compute_outR6;
  logic [511:0] zi12;
  logic [7:0] zll_main_compute_outR7;
  logic [64:0] zi13;
  logic [63:0] zi14;
  logic [64:0] zres;
  assign zin = {__in0, __in1};
  assign zi0 = zin[1055:32];
  assign zi1 = zin[31:0];
  assign zi2 = {zi0, zi1};
  assign zi3 = zi2[1055:32];
  assign zi4 = zi2[31:0];
  assign zi5 = zi3[1023:512];
  ZLL_Main_compute  inst (zi4, zi5[511:448], zll_main_compute_out);
  assign zi6 = zi3[1023:512];
  ZLL_Main_compute  instR1 (zi4, zi6[447:384], zll_main_compute_outR1);
  assign zi7 = zi3[1023:512];
  ZLL_Main_compute  instR2 (zi4, zi7[383:320], zll_main_compute_outR2);
  assign zi8 = zi3[1023:512];
  ZLL_Main_compute  instR3 (zi4, zi8[319:256], zll_main_compute_outR3);
  assign zi9 = zi3[1023:512];
  ZLL_Main_compute  instR4 (zi4, zi9[255:192], zll_main_compute_outR4);
  assign zi10 = zi3[1023:512];
  ZLL_Main_compute  instR5 (zi4, zi10[191:128], zll_main_compute_outR5);
  assign zi11 = zi3[1023:512];
  ZLL_Main_compute  instR6 (zi4, zi11[127:64], zll_main_compute_outR6);
  assign zi12 = zi3[1023:512];
  ZLL_Main_compute  instR7 (zi4, zi12[63:0], zll_main_compute_outR7);
  assign zi13 = {1'h0, {zll_main_compute_out, zll_main_compute_outR1, zll_main_compute_outR2, zll_main_compute_outR3, zll_main_compute_outR4, zll_main_compute_outR5, zll_main_compute_outR6, zll_main_compute_outR7}};
  assign zi14 = zi13[63:0];
  assign zres = {1'h1, zi14};
  assign __out0 = zres[63:0];
endmodule

module ZLL_Main_compute (input logic [31:0] arg0,
  input logic [63:0] arg1,
  output logic [7:0] res);
  assign res = arg0[7:0] + arg1[39:32];
endmodule