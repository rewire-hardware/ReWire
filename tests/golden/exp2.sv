module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [99:0] __in0,
  output logic [99:0] __out0);
  logic [999:0] __resumption_tag;
  logic [999:0] __resumption_tag_next;
  logic [999:0] __st0;
  logic [999:0] __st0_next;
  logic [999:0] zll_main_ss$2_out;
  logic [999:0] zi5;
  logic [99:0] main_x2_out;
  logic [999:0] zll_main_ss$2_outR1;
  logic [999:0] zi6;
  logic [99:0] main_x2_outR1;
  logic [999:0] zll_main_ss$2_outR2;
  logic [999:0] zi7;
  logic [99:0] main_x2_outR2;
  logic [999:0] zll_main_ss$2_outR3;
  logic [999:0] zi8;
  logic [99:0] main_x2_outR3;
  logic [999:0] zll_main_ss$2_outR4;
  logic [999:0] zi9;
  logic [99:0] main_x2_outR4;
  logic [999:0] zll_main_ss$2_outR5;
  logic [999:0] zi10;
  logic [99:0] main_x2_outR5;
  logic [999:0] zll_main_ss$2_outR6;
  logic [999:0] zi11;
  logic [99:0] main_x2_outR6;
  logic [999:0] zll_main_ss$2_outR7;
  logic [999:0] zi12;
  logic [99:0] main_x2_outR7;
  logic [999:0] zll_main_ss$2_outR8;
  logic [999:0] zi13;
  logic [99:0] main_x2_outR8;
  logic [999:0] zll_main_ss$2_outR9;
  logic [999:0] zi14;
  logic [99:0] main_x2_outR9;
  logic [999:0] zi15;
  logic [2099:0] zres;
  ZLL_Main_ss$2  inst (__resumption_tag[899:0], __in0, zll_main_ss$2_out);
  assign zi5 = zll_main_ss$2_out;
  Main_x2  instR1 (zi5[999:900], main_x2_out);
  ZLL_Main_ss$2  instR2 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR1);
  assign zi6 = zll_main_ss$2_outR1;
  Main_x2  instR3 (zi6[899:800], main_x2_outR1);
  ZLL_Main_ss$2  instR4 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR2);
  assign zi7 = zll_main_ss$2_outR2;
  Main_x2  instR5 (zi7[799:700], main_x2_outR2);
  ZLL_Main_ss$2  instR6 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR3);
  assign zi8 = zll_main_ss$2_outR3;
  Main_x2  instR7 (zi8[699:600], main_x2_outR3);
  ZLL_Main_ss$2  instR8 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR4);
  assign zi9 = zll_main_ss$2_outR4;
  Main_x2  instR9 (zi9[599:500], main_x2_outR4);
  ZLL_Main_ss$2  instR10 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR5);
  assign zi10 = zll_main_ss$2_outR5;
  Main_x2  instR11 (zi10[499:400], main_x2_outR5);
  ZLL_Main_ss$2  instR12 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR6);
  assign zi11 = zll_main_ss$2_outR6;
  Main_x2  instR13 (zi11[399:300], main_x2_outR6);
  ZLL_Main_ss$2  instR14 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR7);
  assign zi12 = zll_main_ss$2_outR7;
  Main_x2  instR15 (zi12[299:200], main_x2_outR7);
  ZLL_Main_ss$2  instR16 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR8);
  assign zi13 = zll_main_ss$2_outR8;
  Main_x2  instR17 (zi13[199:100], main_x2_outR8);
  ZLL_Main_ss$2  instR18 (__resumption_tag[899:0], __in0, zll_main_ss$2_outR9);
  assign zi14 = zll_main_ss$2_outR9;
  Main_x2  instR19 (zi14[99:0], main_x2_outR9);
  assign zi15 = {main_x2_out, main_x2_outR1, main_x2_outR2, main_x2_outR3, main_x2_outR4, main_x2_outR5, main_x2_outR6, main_x2_outR7, main_x2_outR8, main_x2_outR9};
  assign zres = {zi15[999:900], zi15, zi15};
  assign __resumption_tag_next = zres[1999:1000];
  assign __st0_next = zres[999:0];
  assign __out0 = zres[2099:2000];
  initial {__resumption_tag, __st0} = {11'h7d0{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {11'h7d0{1'h0}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_Main_ss$2 (input logic [899:0] arg0,
  input logic [99:0] arg1,
  output logic [999:0] res);
  assign res = {arg0, arg1};
endmodule

module Main_x2 (input logic [99:0] arg0,
  output logic [99:0] res);
  assign res = arg0 * 100'h2;
endmodule