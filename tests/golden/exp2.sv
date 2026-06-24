module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [99:0] __in0,
  output logic [99:0] __out0);
  logic [999:0] __resumption_tag;
  logic [999:0] __resumption_tag_next;
  logic [999:0] __st0;
  logic [999:0] __st0_next;
  logic [999:0] zll_main_ss$3_out;
  logic [999:0] zi6;
  logic [99:0] main_x2_out;
  logic [999:0] zll_main_ss$3_outR1;
  logic [999:0] zi7;
  logic [99:0] main_x2_outR1;
  logic [999:0] zll_main_ss$3_outR2;
  logic [999:0] zi8;
  logic [99:0] main_x2_outR2;
  logic [999:0] zll_main_ss$3_outR3;
  logic [999:0] zi9;
  logic [99:0] main_x2_outR3;
  logic [999:0] zll_main_ss$3_outR4;
  logic [999:0] zi10;
  logic [99:0] main_x2_outR4;
  logic [999:0] zll_main_ss$3_outR5;
  logic [999:0] zi11;
  logic [99:0] main_x2_outR5;
  logic [999:0] zll_main_ss$3_outR6;
  logic [999:0] zi12;
  logic [99:0] main_x2_outR6;
  logic [999:0] zll_main_ss$3_outR7;
  logic [999:0] zi13;
  logic [99:0] main_x2_outR7;
  logic [999:0] zll_main_ss$3_outR8;
  logic [999:0] zi14;
  logic [99:0] main_x2_outR8;
  logic [999:0] zll_main_ss$3_outR9;
  logic [999:0] zi15;
  logic [99:0] main_x2_outR9;
  logic [999:0] zi16;
  logic [2100:0] zi17;
  logic [999:0] zi18;
  logic [2100:0] zres;
  ZLL_Main_ss$3  inst (__resumption_tag[899:0], __in0, zll_main_ss$3_out);
  assign zi6 = zll_main_ss$3_out;
  Main_x2  instR1 (zi6[999:900], main_x2_out);
  ZLL_Main_ss$3  instR2 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR1);
  assign zi7 = zll_main_ss$3_outR1;
  Main_x2  instR3 (zi7[899:800], main_x2_outR1);
  ZLL_Main_ss$3  instR4 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR2);
  assign zi8 = zll_main_ss$3_outR2;
  Main_x2  instR5 (zi8[799:700], main_x2_outR2);
  ZLL_Main_ss$3  instR6 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR3);
  assign zi9 = zll_main_ss$3_outR3;
  Main_x2  instR7 (zi9[699:600], main_x2_outR3);
  ZLL_Main_ss$3  instR8 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR4);
  assign zi10 = zll_main_ss$3_outR4;
  Main_x2  instR9 (zi10[599:500], main_x2_outR4);
  ZLL_Main_ss$3  instR10 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR5);
  assign zi11 = zll_main_ss$3_outR5;
  Main_x2  instR11 (zi11[499:400], main_x2_outR5);
  ZLL_Main_ss$3  instR12 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR6);
  assign zi12 = zll_main_ss$3_outR6;
  Main_x2  instR13 (zi12[399:300], main_x2_outR6);
  ZLL_Main_ss$3  instR14 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR7);
  assign zi13 = zll_main_ss$3_outR7;
  Main_x2  instR15 (zi13[299:200], main_x2_outR7);
  ZLL_Main_ss$3  instR16 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR8);
  assign zi14 = zll_main_ss$3_outR8;
  Main_x2  instR17 (zi14[199:100], main_x2_outR8);
  ZLL_Main_ss$3  instR18 (__resumption_tag[899:0], __in0, zll_main_ss$3_outR9);
  assign zi15 = zll_main_ss$3_outR9;
  Main_x2  instR19 (zi15[99:0], main_x2_outR9);
  assign zi16 = {main_x2_out, main_x2_outR1, main_x2_outR2, main_x2_outR3, main_x2_outR4, main_x2_outR5, main_x2_outR6, main_x2_outR7, main_x2_outR8, main_x2_outR9};
  assign zi17 = {{101'h1, {10'h3e8{1'h0}}}, zi16};
  assign zi18 = zi17[999:0];
  assign zres = {1'h1, zi17[999:900], zi18, zi18};
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

module ZLL_Main_ss$3 (input logic [899:0] arg0,
  input logic [99:0] arg1,
  output logic [999:0] res);
  assign res = {arg0, arg1};
endmodule

module Main_x2 (input logic [99:0] arg0,
  output logic [99:0] res);
  assign res = arg0 * 100'h2;
endmodule