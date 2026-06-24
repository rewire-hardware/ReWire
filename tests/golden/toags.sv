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
  logic [0:0] zi1;
  logic [6:0] zll_main_go9_out;
  logic [0:0] zi4;
  logic [6:0] zll_main_go9_outR1;
  logic [6:0] zres;
  assign zi1 = __resumption_tag[0];
  ZLL_Main_go9  inst (zi1, __in0, __st0, __st1, zll_main_go9_out);
  assign zi4 = __resumption_tag[0];
  ZLL_Main_go9  instR1 (__in0, __st0, __st0, __st1, zll_main_go9_outR1);
  assign zres = (__resumption_tag[2:1] == 2'h1) ? zll_main_go9_out : ((__resumption_tag[2:1] == 2'h2) ? {5'h10, __st0, zi4} : ((__in0 == 1'h1) ? {4'h9, __in0, __st0, __st1} : zll_main_go9_outR1));
  assign __resumption_tag_next = zres[4:2];
  assign __st0_next = zres[1];
  assign __st1_next = zres[0];
  assign __out0 = zres[5];
  initial {__resumption_tag, __st0, __st1} = 5'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h1;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZZLLzuReWirezuPreludezuzaza3 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [0:0] zzllzurewirezupreludezuzaza3zuout;
  ZZLLzuReWirezuPreludezuzaza3  inst (arg1, zzllzurewirezupreludezuzaza3zuout);
  assign res = (arg0 == 1'h1) ? zzllzurewirezupreludezuzaza3zuout : 1'h0;
endmodule

module ZLL_Main_go9 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [6:0] res);
  logic [0:0] zzllzurewirezupreludezuzaza3zuout;
  logic [0:0] rewirezupreludezuzazazuout;
  logic [0:0] rewirezupreludezuzazazuoutR1;
  ZZLLzuReWirezuPreludezuzaza3  inst (arg0, zzllzurewirezupreludezuzaza3zuout);
  ReWirezuPreludezuzaza  instR1 (arg0, arg0, rewirezupreludezuzazazuout);
  ReWirezuPreludezuzaza  instR2 ((arg1 == 1'h1) ? zzllzurewirezupreludezuzaza3zuout : rewirezupreludezuzazazuout, arg1, rewirezupreludezuzazazuoutR1);
  assign res = {1'h1, rewirezupreludezuzazazuoutR1, 2'h2, arg1, arg2, arg3};
endmodule