module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  logic [5:0] zin;
  logic [4:0] zi0;
  logic [0:0] zi1;
  logic [5:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  logic [0:0] zi6;
  logic [1:0] zi7;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [6:0] zi10;
  logic [0:0] zi11;
  logic [0:0] zi12;
  logic [5:0] zi13;
  logic [0:0] zi14;
  logic [0:0] zi15;
  logic [0:0] zi16;
  logic [3:0] zi17;
  logic [0:0] zi18;
  logic [0:0] zi19;
  logic [0:0] zi20;
  logic [3:0] zi21;
  logic [0:0] zi22;
  logic [0:0] zi23;
  logic [0:0] zi24;
  logic [2:0] zi25;
  logic [0:0] zi26;
  logic [0:0] zi27;
  logic [0:0] zi28;
  logic [6:0] zi29;
  logic [7:0] zi30;
  logic [0:0] zi31;
  logic [0:0] zi32;
  logic [0:0] zi33;
  logic [0:0] zi34;
  logic [6:0] zll_main_go28_out;
  logic [5:0] zi35;
  logic [0:0] zi36;
  logic [0:0] zi37;
  logic [0:0] zi38;
  logic [0:0] zi39;
  logic [6:0] zll_main_go28_outR1;
  logic [6:0] zres;
  assign zin = {__resumption_tag, __st0, __st1, __in0};
  assign zi0 = zin[5:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[5];
  assign zi4 = zi2[2];
  assign zi5 = zi2[1];
  assign zi6 = zi2[0];
  assign zi7 = {zi5, zi4};
  assign zi8 = zi7[1];
  assign zi9 = zi7[0];
  assign zi10 = {5'h2, zi8, zi9};
  assign zi11 = zi10[1];
  assign zi12 = zi10[0];
  assign zi13 = {zi1, zi0};
  assign zi14 = zi13[5];
  assign zi15 = zi13[1];
  assign zi16 = zi13[0];
  assign zi17 = {zi16, zi15, zi14, zi14};
  assign zi18 = zi17[3];
  assign zi19 = zi17[2];
  assign zi20 = zi17[1];
  assign zi21 = {zi16, zi15, zi14, zi14};
  assign zi22 = zi21[3];
  assign zi23 = zi21[2];
  assign zi24 = zi21[1];
  assign zi25 = {zi23, zi23, zi22};
  assign zi26 = zi25[2];
  assign zi27 = zi25[1];
  assign zi28 = zi25[0];
  assign zi29 = {4'h0, zi26, zi27, zi28};
  assign zi30 = {zi24, zi29};
  assign zi31 = zi30[7];
  assign zi32 = zi30[2];
  assign zi33 = zi30[1];
  assign zi34 = zi30[0];
  ZLL_Main_go28  inst (zi31, zi32, zi33, zi34, zll_main_go28_out);
  assign zi35 = {zi1, zi0};
  assign zi36 = zi35[5];
  assign zi37 = zi35[2];
  assign zi38 = zi35[1];
  assign zi39 = zi35[0];
  ZLL_Main_go28  instR1 (zi37, zi36, zi38, zi39, zll_main_go28_outR1);
  assign zres = (zi2[4:3] == 2'h1) ? {5'h14, zi11, zi12} : ((zi13[4:3] == 2'h2) ? ((zi17[0] == 1'h1) ? {4'h8, zi20, zi19, zi18} : zll_main_go28_out) : zll_main_go28_outR1);
  assign __resumption_tag_next = zres[4:2];
  assign __st0_next = zres[1];
  assign __st1_next = zres[0];
  assign __out0 = zres[5];
  initial {__resumption_tag, __st0, __st1} = 5'h11;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h11;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_go28 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [6:0] res);
  logic [1:0] zi0;
  logic [0:0] zll_main_go23_out;
  logic [1:0] zi1;
  logic [0:0] zi2;
  logic [0:0] rewirezupreludezuzazazuout;
  logic [0:0] rewirezupreludezuzazazuoutR1;
  assign zi0 = {arg0, arg1};
  ZLL_Main_go23  inst (zi0[1], zll_main_go23_out);
  assign zi1 = {arg0, arg1};
  assign zi2 = zi1[1];
  ReWirezuPreludezuzaza  instR1 (zi2, zi2, rewirezupreludezuzazazuout);
  ReWirezuPreludezuzaza  instR2 ((zi0[0] == 1'h1) ? zll_main_go23_out : rewirezupreludezuzazazuout, arg1, rewirezupreludezuzazazuoutR1);
  assign res = {1'h1, rewirezupreludezuzazazuoutR1, 2'h1, arg1, arg2, arg3};
endmodule

module ZLL_Main_go23 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [1:0] zi0;
  logic [0:0] zll_main_go23_out;
  logic [1:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {arg0, arg1};
  ZLL_Main_go23  inst (zi0[0], zll_main_go23_out);
  assign zi1 = {arg0, arg1};
  assign zi2 = zi1[0];
  assign res = (zi0[1] == 1'h1) ? zll_main_go23_out : 1'h0;
endmodule