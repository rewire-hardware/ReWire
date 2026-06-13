module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  logic [25:0] zll_main_incr4_out;
  logic [25:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi6;
  logic [25:0] zll_main_incr20_out;
  logic [25:0] zi15;
  logic [7:0] zi16;
  logic [25:0] zll_main_incr20_outR1;
  logic [25:0] zi18;
  logic [7:0] zi19;
  logic [7:0] zi20;
  logic [25:0] main_sig_out;
  logic [25:0] main_sig_outR1;
  logic [25:0] zres;
  ZLL_Main_incr4  inst ({__st0, __st0, __st1}, zll_main_incr4_out);
  assign zi3 = zll_main_incr4_out;
  assign zi4 = zi3[23:16];
  assign zi6 = zi3[7:0];
  ZLL_Main_incr20  instR1 ({zi6, zi6}, zll_main_incr20_out);
  assign zi15 = zll_main_incr20_out;
  assign zi16 = zi15[15:8];
  ZLL_Main_incr20  instR2 ({zi16, zi4 + zi6}, zll_main_incr20_outR1);
  assign zi18 = zll_main_incr20_outR1;
  assign zi19 = zi18[15:8];
  assign zi20 = zi18[7:0];
  Main_sig  instR3 (zi19, zi20, main_sig_out);
  Main_sig  instR4 (__st0, __st1, main_sig_outR1);
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

module ZLL_Main_incr20 (input logic [15:0] arg0,
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
  logic [25:0] zll_main_incr4_out;
  logic [25:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  ZLL_Main_incr4  inst ({arg0, arg0, arg1}, zll_main_incr4_out);
  assign zi0 = zll_main_incr4_out;
  assign zi1 = zi0[23:16];
  assign zi2 = zi0[15:8];
  assign zi3 = zi0[7:0];
  assign res = {2'h2, zi1, zi2, zi3};
endmodule

module ZLL_Main_incr4 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  assign zi0 = arg0[23:16];
  assign zi1 = arg0[15:8];
  assign zi2 = arg0[7:0];
  assign res = {2'h0, zi0, zi1, zi2};
endmodule