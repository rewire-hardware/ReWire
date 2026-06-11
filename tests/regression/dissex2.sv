module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  logic [16:0] zin;
  logic [15:0] zi0;
  logic [0:0] zi1;
  logic [16:0] zi2;
  logic [0:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [16:0] zi6;
  logic [7:0] zi7;
  logic [7:0] zi8;
  logic [25:0] zll_main_incr12_out;
  logic [25:0] zi9;
  logic [7:0] zi10;
  logic [7:0] zi11;
  logic [7:0] zi12;
  logic [25:0] zll_main_incr12_outR1;
  logic [25:0] zi13;
  logic [33:0] zi14;
  logic [7:0] zi15;
  logic [7:0] zi16;
  logic [7:0] zi17;
  logic [7:0] zi18;
  logic [25:0] zll_main_incr22_out;
  logic [25:0] zi19;
  logic [41:0] zi20;
  logic [7:0] zi21;
  logic [7:0] zi22;
  logic [7:0] zi23;
  logic [7:0] zi24;
  logic [25:0] zll_main_incr22_outR1;
  logic [25:0] zi25;
  logic [7:0] zi26;
  logic [7:0] zi27;
  logic [25:0] main_sig_out;
  logic [16:0] zi28;
  logic [7:0] zi29;
  logic [7:0] zi30;
  logic [25:0] main_sig_outR1;
  logic [25:0] zres;
  assign zin = {__st0, __st1, __in0};
  assign zi0 = zin[16:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[16];
  assign zi4 = zi2[15:8];
  assign zi5 = zi2[7:0];
  assign zi6 = {zi4, zi5, zi3};
  assign zi7 = zi6[16:9];
  assign zi8 = zi6[8:1];
  ZLL_Main_incr12  inst ({zi7, zi7, zi8}, zll_main_incr12_out);
  assign zi9 = zll_main_incr12_out;
  assign zi10 = zi9[23:16];
  assign zi11 = zi9[15:8];
  assign zi12 = zi9[7:0];
  ZLL_Main_incr12  instR1 ({zi12, zi11, zi12}, zll_main_incr12_outR1);
  assign zi13 = zll_main_incr12_outR1;
  assign zi14 = {zi10, zi13};
  assign zi15 = zi14[33:26];
  assign zi16 = zi14[23:16];
  assign zi17 = zi14[15:8];
  assign zi18 = zi14[7:0];
  ZLL_Main_incr22  instR2 ({zi16, zi18}, zll_main_incr22_out);
  assign zi19 = zll_main_incr22_out;
  assign zi20 = {zi16, zi15, zi19};
  assign zi21 = zi20[41:34];
  assign zi22 = zi20[33:26];
  assign zi23 = zi20[15:8];
  assign zi24 = zi20[7:0];
  ZLL_Main_incr22  instR3 ({zi23, zi22 + zi21}, zll_main_incr22_outR1);
  assign zi25 = zll_main_incr22_outR1;
  assign zi26 = zi25[15:8];
  assign zi27 = zi25[7:0];
  Main_sig  instR4 (zi26, zi27, main_sig_out);
  assign zi28 = {zi5, zi4, zi3};
  assign zi29 = zi28[16:9];
  assign zi30 = zi28[8:1];
  Main_sig  instR5 (zi30, zi29, main_sig_outR1);
  assign zres = (zi6[0] == 1'h1) ? main_sig_out : main_sig_outR1;
  assign __st0_next = zres[15:8];
  assign __st1_next = zres[7:0];
  assign __out0 = zres[23:16];
  initial {__st0, __st1} = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h1;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_incr22 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  assign zi0 = arg0[15:8];
  assign zi1 = arg0[7:0];
  assign res = {10'h100, zi0, zi1};
endmodule

module Main_sig (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [25:0] zll_main_incr12_out;
  logic [25:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  ZLL_Main_incr12  inst ({arg0, arg0, arg1}, zll_main_incr12_out);
  assign zi0 = zll_main_incr12_out;
  assign zi1 = zi0[23:16];
  assign zi2 = zi0[15:8];
  assign zi3 = zi0[7:0];
  assign res = {2'h2, zi1, zi2, zi3};
endmodule

module ZLL_Main_incr12 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  assign zi0 = arg0[23:16];
  assign zi1 = arg0[15:8];
  assign zi2 = arg0[7:0];
  assign res = {2'h0, zi0, zi1, zi2};
endmodule