module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [99:0] __in0,
  output logic [99:0] __out0);
  logic [999:0] __resumption_tag;
  logic [999:0] __resumption_tag_next;
  logic [999:0] __st0;
  logic [999:0] __st0_next;
  logic [99:0] main_x2_out;
  logic [99:0] main_x2_outR1;
  logic [99:0] main_x2_outR2;
  logic [99:0] main_x2_outR3;
  logic [99:0] main_x2_outR4;
  logic [99:0] main_x2_outR5;
  logic [99:0] main_x2_outR6;
  logic [99:0] main_x2_outR7;
  logic [99:0] main_x2_outR8;
  logic [99:0] main_x2_outR9;
  logic [999:0] zi3;
  logic [2099:0] zres;
  Main_x2  inst (__resumption_tag[899:800], main_x2_out);
  Main_x2  instR1 (__resumption_tag[799:700], main_x2_outR1);
  Main_x2  instR2 (__resumption_tag[699:600], main_x2_outR2);
  Main_x2  instR3 (__resumption_tag[599:500], main_x2_outR3);
  Main_x2  instR4 (__resumption_tag[499:400], main_x2_outR4);
  Main_x2  instR5 (__resumption_tag[399:300], main_x2_outR5);
  Main_x2  instR6 (__resumption_tag[299:200], main_x2_outR6);
  Main_x2  instR7 (__resumption_tag[199:100], main_x2_outR7);
  Main_x2  instR8 (__resumption_tag[99:0], main_x2_outR8);
  Main_x2  instR9 (__in0, main_x2_outR9);
  assign zi3 = {main_x2_out, main_x2_outR1, main_x2_outR2, main_x2_outR3, main_x2_outR4, main_x2_outR5, main_x2_outR6, main_x2_outR7, main_x2_outR8, main_x2_outR9};
  assign zres = {zi3[999:900], zi3, zi3};
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

module Main_x2 (input logic [99:0] arg0,
  output logic [99:0] res);
  assign res = arg0 * 100'h2;
endmodule