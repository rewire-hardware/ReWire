module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  logic [25:0] zll_main_begin6_out;
  logic [23:0] main_first_out;
  logic [25:0] zll_main_sig8_out;
  logic [25:0] zi0;
  logic [7:0] zi1;
  logic [15:0] zi2;
  logic [15:0] slice_in;
  logic [25:0] zll_main_sig8_outR1;
  logic [25:0] zi6;
  logic [7:0] zi7;
  logic [15:0] zi9;
  logic [25:0] zi10;
  logic [15:0] zi11;
  logic [25:0] zll_main_begin6_outR1;
  logic [25:0] zres;
  ZLL_Main_begin6  inst (__st0, zll_main_begin6_out);
  Main_first  instR1 (__st0, main_first_out);
  ZLL_Main_sig8  instR2 (main_first_out, zll_main_sig8_out);
  assign zi0 = zll_main_sig8_out;
  assign zi1 = zi0[23:16];
  assign zi2 = zi0[15:0];
  assign slice_in = zi2 >> {8'h80{1'h0}};
  ZLL_Main_sig8  instR3 ({slice_in[7:0], zi2}, zll_main_sig8_outR1);
  assign zi6 = zll_main_sig8_outR1;
  assign zi7 = zi6[23:16];
  assign zi9 = {zi7, zi1 + zi7};
  assign zi10 = {10'h100, zi9};
  assign zi11 = zi10[15:0];
  ZLL_Main_begin6  instR4 (zi11, zll_main_begin6_outR1);
  assign zres = (__in0 == 1'h1) ? zll_main_begin6_out : zll_main_begin6_outR1;
  assign __st0_next = zres[15:0];
  assign __out0 = zres[23:16];
  initial __st0 = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_begin6 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [23:0] main_first_out;
  logic [25:0] zll_main_sig8_out;
  logic [25:0] zi0;
  logic [7:0] zi1;
  logic [15:0] zi2;
  Main_first  inst (arg0, main_first_out);
  ZLL_Main_sig8  instR1 (main_first_out, zll_main_sig8_out);
  assign zi0 = zll_main_sig8_out;
  assign zi1 = zi0[23:16];
  assign zi2 = zi0[15:0];
  assign res = {2'h2, zi1, zi2};
endmodule

module ZLL_Main_sig8 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [7:0] zi0;
  logic [15:0] zi1;
  assign zi0 = arg0[23:16];
  assign zi1 = arg0[15:0];
  assign res = {2'h0, zi0, zi1};
endmodule

module Main_first (input logic [15:0] arg0,
  output logic [23:0] res);
  assign res = {arg0[15:8], arg0};
endmodule