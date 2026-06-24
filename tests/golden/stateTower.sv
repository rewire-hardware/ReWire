module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  logic [0:0] rewire_prelude_not_out;
  logic [2:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  logic [4:0] zll_main_sig30_out;
  logic [4:0] zi6;
  logic [0:0] zi7;
  logic [0:0] zi9;
  logic [4:0] zll_main_sig37_out;
  logic [4:0] zi18;
  logic [0:0] zi19;
  logic [0:0] zi24;
  logic [4:0] zll_main_sig37_outR1;
  logic [4:0] zll_main_sig49_out;
  logic [0:0] rewire_prelude_not_outR1;
  logic [2:0] zi25;
  logic [0:0] zi26;
  logic [0:0] zi27;
  logic [4:0] zll_main_sig49_outR1;
  logic [4:0] zres;
  ReWire_Prelude_not  inst (__in0, rewire_prelude_not_out);
  assign zi3 = {__st1, __st0, rewire_prelude_not_out};
  assign zi4 = zi3[2];
  assign zi5 = zi3[1];
  ZLL_Main_sig30  instR1 ({zi5, zi5, zi4}, zll_main_sig30_out);
  assign zi6 = zll_main_sig30_out;
  assign zi7 = zi6[2];
  assign zi9 = zi6[0];
  ZLL_Main_sig37  instR2 ({zi9, zi9}, zll_main_sig37_out);
  assign zi18 = zll_main_sig37_out;
  assign zi19 = zi18[1];
  assign zi24 = zi7 ^ zi9;
  ZLL_Main_sig37  instR3 ({zi19, zi24}, zll_main_sig37_outR1);
  ZLL_Main_sig49  instR4 (zll_main_sig37_outR1, zll_main_sig49_out);
  ReWire_Prelude_not  instR5 (__in0, rewire_prelude_not_outR1);
  assign zi25 = {__st1, __st0, rewire_prelude_not_outR1};
  assign zi26 = zi25[2];
  assign zi27 = zi25[1];
  ZLL_Main_sig49  instR6 ({3'h2, zi27, zi26}, zll_main_sig49_outR1);
  assign zres = (zi3[0] == 1'h1) ? zll_main_sig49_out : zll_main_sig49_outR1;
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

module ZLL_Main_sig49 (input logic [4:0] arg0,
  output logic [4:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [4:0] zll_main_sig30_out;
  logic [4:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  assign zi0 = arg0[1];
  assign zi1 = arg0[0];
  ZLL_Main_sig30  inst ({zi0, zi0, zi1}, zll_main_sig30_out);
  assign zi2 = zll_main_sig30_out;
  assign zi3 = zi2[2];
  assign zi4 = zi2[1];
  assign zi5 = zi2[0];
  assign res = {2'h2, zi3, zi4, zi5};
endmodule

module ReWire_Prelude_not (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_sig37 (input logic [1:0] arg0,
  output logic [4:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  assign zi0 = arg0[1];
  assign zi1 = arg0[0];
  assign res = {3'h2, zi0, zi1};
endmodule

module ZLL_Main_sig30 (input logic [2:0] arg0,
  output logic [4:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = arg0[2];
  assign zi1 = arg0[1];
  assign zi2 = arg0[0];
  assign res = {2'h0, zi0, zi1, zi2};
endmodule