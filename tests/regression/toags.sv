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
  logic [6:0] zll_main_go1_out;
  logic [0:0] zi11;
  logic [6:0] zll_main_go1_outR1;
  logic [0:0] zi14;
  logic [6:0] zres;
  ZLL_Main_go1  inst (__in0, __st0, __st0, __st1, zll_main_go1_out);
  assign zi11 = __resumption_tag[0];
  ZLL_Main_go1  instR1 (zi11, __in0, __st0, __st1, zll_main_go1_outR1);
  assign zi14 = __resumption_tag[0];
  assign zres = (__resumption_tag[2:1] == 2'h1) ? ((__in0 == 1'h1) ? {4'ha, __in0, __st0, __st1} : zll_main_go1_out) : ((__resumption_tag[2:1] == 2'h2) ? zll_main_go1_outR1 : {5'h12, __st0, zi14});
  assign __resumption_tag_next = zres[4:2];
  assign __st0_next = zres[1];
  assign __st1_next = zres[0];
  assign __out0 = zres[5];
  initial {__resumption_tag, __st0, __st1} = 5'h9;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h9;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_go20 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [0:0] zll_main_go20_out;
  ZLL_Main_go20  inst (arg1, zll_main_go20_out);
  assign res = (arg0 == 1'h1) ? zll_main_go20_out : 1'h0;
endmodule

module ZLL_Main_go1 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [6:0] res);
  logic [0:0] zll_main_go20_out;
  logic [0:0] rewirezupreludezuzazazuout;
  logic [0:0] rewirezupreludezuzazazuoutR1;
  ZLL_Main_go20  inst (arg0, zll_main_go20_out);
  ReWirezuPreludezuzaza  instR1 (arg0, arg0, rewirezupreludezuzazazuout);
  ReWirezuPreludezuzaza  instR2 ((arg1 == 1'h1) ? zll_main_go20_out : rewirezupreludezuzazazuout, arg1, rewirezupreludezuzazazuoutR1);
  assign res = {1'h1, rewirezupreludezuzazazuoutR1, 2'h0, arg1, arg2, arg3};
endmodule