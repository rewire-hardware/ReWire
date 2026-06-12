module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  logic [25:0] zll_main_incr16_out;
  logic [25:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [25:0] zll_main_incr16_outR1;
  logic [25:0] zi7;
  logic [7:0] zi8;
  logic [15:0] zi17;
  logic [7:0] zi18;
  logic [7:0] zi19;
  logic [25:0] zi20;
  logic [7:0] zi21;
  logic [7:0] zi22;
  logic [25:0] main_sig_out;
  logic [25:0] main_sig_outR1;
  logic [25:0] zres;
  ZLL_Main_incr16  inst ({__st0, __st0, __st1}, zll_main_incr16_out);
  assign zi3 = zll_main_incr16_out;
  assign zi4 = zi3[23:16];
  assign zi5 = zi3[15:8];
  assign zi6 = zi3[7:0];
  ZLL_Main_incr16  instR1 ({zi6, zi5, zi6}, zll_main_incr16_outR1);
  assign zi7 = zll_main_incr16_outR1;
  assign zi8 = zi7[23:16];
  assign zi17 = {zi8, zi4 + zi8};
  assign zi18 = zi17[15:8];
  assign zi19 = zi17[7:0];
  assign zi20 = {10'h100, zi18, zi19};
  assign zi21 = zi20[15:8];
  assign zi22 = zi20[7:0];
  Main_sig  instR2 (zi21, zi22, main_sig_out);
  Main_sig  instR3 (__st0, __st1, main_sig_outR1);
  assign zres = (__in0 == 1'h1) ? main_sig_out : main_sig_outR1;
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

module Main_sig (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  assign res = {2'h2, arg0, arg0, arg1};
endmodule

module ZLL_Main_incr16 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  assign zi0 = arg0[23:16];
  assign zi1 = arg0[15:8];
  assign zi2 = arg0[7:0];
  assign res = {2'h0, zi0, zi1, zi2};
endmodule