module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  logic [16:0] zin;
  logic [15:0] zi0;
  logic [0:0] zi1;
  logic [16:0] zi2;
  logic [0:0] zi3;
  logic [15:0] zi4;
  logic [16:0] zi5;
  logic [15:0] zi6;
  logic [25:0] main_sig_out;
  logic [16:0] zi7;
  logic [15:0] zi8;
  logic [23:0] main_first_out;
  logic [25:0] zll_main_incr16_out;
  logic [25:0] zi9;
  logic [7:0] zi10;
  logic [15:0] zi11;
  logic [31:0] zi12;
  logic [15:0] zi13;
  logic [15:0] zi14;
  logic [15:0] slice_in;
  logic [25:0] zll_main_incr16_outR1;
  logic [25:0] zi15;
  logic [33:0] zi16;
  logic [7:0] zi17;
  logic [7:0] zi18;
  logic [15:0] zi19;
  logic [15:0] zi20;
  logic [25:0] zi21;
  logic [15:0] zi22;
  logic [25:0] main_sig_outR1;
  logic [25:0] zres;
  assign zin = {__st0, __in0};
  assign zi0 = zin[16:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[16];
  assign zi4 = zi2[15:0];
  assign zi5 = {zi4, zi3};
  assign zi6 = zi5[16:1];
  Main_sig  inst (zi6, main_sig_out);
  assign zi7 = {zi4, zi3};
  assign zi8 = zi7[16:1];
  Main_first  instR1 (zi8, main_first_out);
  ZLL_Main_incr16  instR2 (main_first_out, zll_main_incr16_out);
  assign zi9 = zll_main_incr16_out;
  assign zi10 = zi9[23:16];
  assign zi11 = zi9[15:0];
  assign zi12 = {zi11, zi11};
  assign zi13 = zi12[31:16];
  assign zi14 = zi12[15:0];
  assign slice_in = zi13 >> (((128'h2 - {{7'h7f{1'h0}}, 1'h1}) - 128'h1) * 128'h8);
  ZLL_Main_incr16  instR3 ({slice_in[7:0], zi14}, zll_main_incr16_outR1);
  assign zi15 = zll_main_incr16_outR1;
  assign zi16 = {zi10, zi15};
  assign zi17 = zi16[33:26];
  assign zi18 = zi16[23:16];
  assign zi19 = zi16[15:0];
  assign zi20 = {zi18, zi17 + zi18};
  assign zi21 = {10'h100, zi20};
  assign zi22 = zi21[15:0];
  Main_sig  instR4 (zi22, main_sig_outR1);
  assign zres = (zi5[0] == 1'h1) ? main_sig_out : main_sig_outR1;
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

module ZLL_Main_incr16 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [7:0] zi0;
  logic [15:0] zi1;
  assign zi0 = arg0[23:16];
  assign zi1 = arg0[15:0];
  assign res = {2'h0, zi0, zi1};
endmodule

module Main_sig (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [23:0] main_first_out;
  logic [25:0] zll_main_incr16_out;
  logic [25:0] zi0;
  logic [7:0] zi1;
  logic [15:0] zi2;
  Main_first  inst (arg0, main_first_out);
  ZLL_Main_incr16  instR1 (main_first_out, zll_main_incr16_out);
  assign zi0 = zll_main_incr16_out;
  assign zi1 = zi0[23:16];
  assign zi2 = zi0[15:0];
  assign res = {2'h2, zi1, zi2};
endmodule

module Main_first (input logic [15:0] arg0,
  output logic [23:0] res);
  logic [31:0] zi0;
  logic [15:0] zi1;
  logic [15:0] zi2;
  assign zi0 = {arg0, arg0};
  assign zi1 = zi0[31:16];
  assign zi2 = zi0[15:0];
  assign res = {zi1[15:8], zi2};
endmodule