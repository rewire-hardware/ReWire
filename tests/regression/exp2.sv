module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [99:0] __in0,
  output logic [99:0] __out0);
  logic [999:0] __resumption_tag;
  logic [999:0] __resumption_tag_next;
  logic [999:0] __st0;
  logic [999:0] __st0_next;
  logic [2099:0] zin;
  logic [1999:0] zi0;
  logic [99:0] zi1;
  logic [2099:0] zi2;
  logic [99:0] zi3;
  logic [999:0] zi4;
  logic [999:0] zi5;
  logic [1099:0] zi6;
  logic [999:0] zi7;
  logic [99:0] zi8;
  logic [999:0] zll_main_ss$3_out;
  logic [999:0] zi9;
  logic [99:0] main_x2_out;
  logic [999:0] zll_main_ss$3_outR1;
  logic [999:0] zi10;
  logic [99:0] main_x2_outR1;
  logic [999:0] zll_main_ss$3_outR2;
  logic [999:0] zi11;
  logic [99:0] main_x2_outR2;
  logic [999:0] zll_main_ss$3_outR3;
  logic [999:0] zi12;
  logic [99:0] main_x2_outR3;
  logic [999:0] zll_main_ss$3_outR4;
  logic [999:0] zi13;
  logic [99:0] main_x2_outR4;
  logic [999:0] zll_main_ss$3_outR5;
  logic [999:0] zi14;
  logic [99:0] main_x2_outR5;
  logic [999:0] zll_main_ss$3_outR6;
  logic [999:0] zi15;
  logic [99:0] main_x2_outR6;
  logic [999:0] zll_main_ss$3_outR7;
  logic [999:0] zi16;
  logic [99:0] main_x2_outR7;
  logic [999:0] zll_main_ss$3_outR8;
  logic [999:0] zi17;
  logic [99:0] main_x2_outR8;
  logic [999:0] zll_main_ss$3_outR9;
  logic [999:0] zi18;
  logic [99:0] main_x2_outR9;
  logic [999:0] zi19;
  logic [2100:0] zi20;
  logic [999:0] zi21;
  logic [1999:0] zi22;
  logic [999:0] zi23;
  logic [999:0] zi24;
  logic [2100:0] zi25;
  logic [999:0] zi26;
  logic [999:0] zi27;
  logic [2100:0] zres;
  assign zin = {__resumption_tag, __st0, __in0};
  assign zi0 = zin[2099:100];
  assign zi1 = zin[99:0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[2099:2000];
  assign zi4 = zi2[1999:1000];
  assign zi5 = zi2[999:0];
  assign zi6 = {zi4, zi3};
  assign zi7 = zi6[1099:100];
  assign zi8 = zi6[99:0];
  ZLL_Main_ss$3  inst (zi7[899:0], zi8, zll_main_ss$3_out);
  assign zi9 = zll_main_ss$3_out;
  Main_x2  instR1 (zi9[999:900], main_x2_out);
  ZLL_Main_ss$3  instR2 (zi7[899:0], zi8, zll_main_ss$3_outR1);
  assign zi10 = zll_main_ss$3_outR1;
  Main_x2  instR3 (zi10[899:800], main_x2_outR1);
  ZLL_Main_ss$3  instR4 (zi7[899:0], zi8, zll_main_ss$3_outR2);
  assign zi11 = zll_main_ss$3_outR2;
  Main_x2  instR5 (zi11[799:700], main_x2_outR2);
  ZLL_Main_ss$3  instR6 (zi7[899:0], zi8, zll_main_ss$3_outR3);
  assign zi12 = zll_main_ss$3_outR3;
  Main_x2  instR7 (zi12[699:600], main_x2_outR3);
  ZLL_Main_ss$3  instR8 (zi7[899:0], zi8, zll_main_ss$3_outR4);
  assign zi13 = zll_main_ss$3_outR4;
  Main_x2  instR9 (zi13[599:500], main_x2_outR4);
  ZLL_Main_ss$3  instR10 (zi7[899:0], zi8, zll_main_ss$3_outR5);
  assign zi14 = zll_main_ss$3_outR5;
  Main_x2  instR11 (zi14[499:400], main_x2_outR5);
  ZLL_Main_ss$3  instR12 (zi7[899:0], zi8, zll_main_ss$3_outR6);
  assign zi15 = zll_main_ss$3_outR6;
  Main_x2  instR13 (zi15[399:300], main_x2_outR6);
  ZLL_Main_ss$3  instR14 (zi7[899:0], zi8, zll_main_ss$3_outR7);
  assign zi16 = zll_main_ss$3_outR7;
  Main_x2  instR15 (zi16[299:200], main_x2_outR7);
  ZLL_Main_ss$3  instR16 (zi7[899:0], zi8, zll_main_ss$3_outR8);
  assign zi17 = zll_main_ss$3_outR8;
  Main_x2  instR17 (zi17[199:100], main_x2_outR8);
  ZLL_Main_ss$3  instR18 (zi7[899:0], zi8, zll_main_ss$3_outR9);
  assign zi18 = zll_main_ss$3_outR9;
  Main_x2  instR19 (zi18[99:0], main_x2_outR9);
  assign zi19 = {main_x2_out, main_x2_outR1, main_x2_outR2, main_x2_outR3, main_x2_outR4, main_x2_outR5, main_x2_outR6, main_x2_outR7, main_x2_outR8, main_x2_outR9};
  assign zi20 = {{101'h1, {10'h3e8{1'h0}}}, zi19};
  assign zi21 = zi20[999:0];
  assign zi22 = {zi21, zi21};
  assign zi23 = zi22[1999:1000];
  assign zi24 = zi22[999:0];
  assign zi25 = {{7'h65{1'h0}}, zi23, zi24};
  assign zi26 = zi25[1999:1000];
  assign zi27 = zi25[999:0];
  assign zres = {1'h1, zi26[999:900], zi26, zi27};
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
  logic [999:0] zi0;
  logic [899:0] zi1;
  logic [99:0] zi2;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[999:100];
  assign zi2 = zi0[99:0];
  assign res = {zi1, zi2};
endmodule

module Main_x2 (input logic [99:0] arg0,
  output logic [99:0] res);
  assign res = arg0 * 100'h2;
endmodule