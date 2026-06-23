module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [18:0] zll_pure_dispatch1_out;
  logic [18:0] zll_main_loop13_out;
  logic [18:0] zll_pure_dispatch1_outR1;
  logic [18:0] zres;
  ZLL_Pure_dispatch1  inst (__in0, __st0, zll_pure_dispatch1_out);
  ZLL_Main_loop13  instR1 ({11'h200, __in0}, zll_main_loop13_out);
  ZLL_Pure_dispatch1  instR2 (__in0, __st0, zll_pure_dispatch1_outR1);
  assign zres = (__resumption_tag == 2'h1) ? zll_pure_dispatch1_out : ((__resumption_tag == 2'h2) ? zll_main_loop13_out : zll_pure_dispatch1_outR1);
  assign __resumption_tag_next = zres[9:8];
  assign __st0_next = zres[7:0];
  assign __out0 = zres[17:10];
  initial {__resumption_tag, __st0} = 10'h200;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 10'h200;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_Main_loop21 (input logic [7:0] arg0,
  output logic [0:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  assign zi0 = arg0[0];
  assign zi1 = zi0;
  assign res = (zi1 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_loop13 (input logic [18:0] arg0,
  output logic [18:0] res);
  logic [7:0] zi0;
  logic [0:0] zll_main_loop21_out;
  logic [8:0] zi7;
  logic [7:0] zi8;
  logic [0:0] zll_main_loop21_outR1;
  logic [8:0] zi9;
  logic [7:0] zi10;
  assign zi0 = arg0[7:0];
  ZLL_Main_loop21  inst (zi0, zll_main_loop21_out);
  assign zi7 = {zi0, zll_main_loop21_out};
  assign zi8 = zi7[8:1];
  ZLL_Main_loop21  instR1 (zi0, zll_main_loop21_outR1);
  assign zi9 = {zi0, zll_main_loop21_outR1};
  assign zi10 = zi9[8:1];
  assign res = (zi7[0] == 1'h1) ? {11'h409, zi8} : {11'h408, zi10};
endmodule

module ZLL_Pure_dispatch1 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [18:0] res);
  logic [18:0] zll_main_loop13_out;
  ZLL_Main_loop13  inst ({11'h200, arg1}, zll_main_loop13_out);
  assign res = zll_main_loop13_out;
endmodule