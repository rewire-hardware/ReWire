module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  logic [2:0] zin;
  logic [1:0] zi0;
  logic [0:0] zi1;
  logic [2:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  logic [0:0] rewire_prelude_not_out;
  logic [2:0] zi6;
  logic [0:0] zi7;
  logic [0:0] zi8;
  logic [2:0] zi9;
  logic [0:0] zi10;
  logic [0:0] zi11;
  logic [0:0] zi12;
  logic [4:0] zi13;
  logic [0:0] zi14;
  logic [0:0] zi15;
  logic [0:0] zi16;
  logic [4:0] zll_main_sig35_out;
  logic [4:0] zi17;
  logic [5:0] zi18;
  logic [0:0] zi19;
  logic [0:0] zi20;
  logic [0:0] zi21;
  logic [0:0] zi22;
  logic [4:0] zll_main_sig15_out;
  logic [4:0] zi23;
  logic [6:0] zi24;
  logic [0:0] zi25;
  logic [0:0] zi26;
  logic [0:0] zi27;
  logic [0:0] zi28;
  logic [1:0] zi29;
  logic [0:0] zi30;
  logic [0:0] zi31;
  logic [0:0] zi32;
  logic [4:0] zll_main_sig15_outR1;
  logic [4:0] zll_main_sig24_out;
  logic [0:0] rewire_prelude_not_outR1;
  logic [2:0] zi33;
  logic [0:0] zi34;
  logic [0:0] zi35;
  logic [4:0] zll_main_sig24_outR1;
  logic [4:0] zres;
  assign zin = {__st0, __st1, __in0};
  assign zi0 = zin[2:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[2];
  assign zi4 = zi2[1];
  assign zi5 = zi2[0];
  ReWire_Prelude_not  inst (zi3, rewire_prelude_not_out);
  assign zi6 = {zi5, zi4, rewire_prelude_not_out};
  assign zi7 = zi6[2];
  assign zi8 = zi6[1];
  assign zi9 = {zi8, zi8, zi7};
  assign zi10 = zi9[2];
  assign zi11 = zi9[1];
  assign zi12 = zi9[0];
  assign zi13 = {2'h0, zi10, zi11, zi12};
  assign zi14 = zi13[2];
  assign zi15 = zi13[1];
  assign zi16 = zi13[0];
  ZLL_Main_sig35  instR1 ({zi16, zi15, zi16}, zll_main_sig35_out);
  assign zi17 = zll_main_sig35_out;
  assign zi18 = {zi14, zi17};
  assign zi19 = zi18[5];
  assign zi20 = zi18[2];
  assign zi21 = zi18[1];
  assign zi22 = zi18[0];
  ZLL_Main_sig15  instR2 ({zi20, zi22}, zll_main_sig15_out);
  assign zi23 = zll_main_sig15_out;
  assign zi24 = {zi20, zi19, zi23};
  assign zi25 = zi24[6];
  assign zi26 = zi24[5];
  assign zi27 = zi24[1];
  assign zi28 = zi24[0];
  assign zi29 = {zi26, zi25};
  assign zi30 = zi29[1];
  assign zi31 = zi29[0];
  assign zi32 = zi30 ^ zi31;
  ZLL_Main_sig15  instR3 ({zi27, zi32}, zll_main_sig15_outR1);
  ZLL_Main_sig24  instR4 (zll_main_sig15_outR1, zll_main_sig24_out);
  ReWire_Prelude_not  instR5 (zi3, rewire_prelude_not_outR1);
  assign zi33 = {zi5, zi4, rewire_prelude_not_outR1};
  assign zi34 = zi33[2];
  assign zi35 = zi33[1];
  ZLL_Main_sig24  instR6 ({3'h2, zi35, zi34}, zll_main_sig24_outR1);
  assign zres = (zi6[0] == 1'h1) ? zll_main_sig24_out : zll_main_sig24_outR1;
  assign __st0_next = zres[1];
  assign __st1_next = zres[0];
  assign __out0 = zres[2];
  initial {__st0, __st1} = 2'h3;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 2'h3;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ReWire_Prelude_not (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_sig35 (input logic [2:0] arg0,
  output logic [4:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = arg0[2];
  assign zi1 = arg0[1];
  assign zi2 = arg0[0];
  assign res = {2'h0, zi0, zi1, zi2};
endmodule

module ZLL_Main_sig24 (input logic [4:0] arg0,
  output logic [4:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [4:0] zll_main_sig35_out;
  logic [4:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  assign zi0 = arg0[1];
  assign zi1 = arg0[0];
  ZLL_Main_sig35  inst ({zi0, zi0, zi1}, zll_main_sig35_out);
  assign zi2 = zll_main_sig35_out;
  assign zi3 = zi2[2];
  assign zi4 = zi2[1];
  assign zi5 = zi2[0];
  assign res = {2'h2, zi3, zi4, zi5};
endmodule

module ZLL_Main_sig15 (input logic [1:0] arg0,
  output logic [4:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  assign zi0 = arg0[1];
  assign zi1 = arg0[0];
  assign res = {3'h2, zi0, zi1};
endmodule