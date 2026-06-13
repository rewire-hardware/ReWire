module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [3:0] zll_pure_dispatch5_out;
  logic [3:0] zll_pure_dispatch5_outR1;
  logic [3:0] zres;
  ZLL_Pure_dispatch5  inst (__in0, __resumption_tag[0], zll_pure_dispatch5_out);
  ZLL_Pure_dispatch5  instR1 (__in0, __resumption_tag[0], zll_pure_dispatch5_outR1);
  assign zres = (__resumption_tag[2:1] == 2'h1) ? ((__in0 == 1'h1) ? {3'h3, __in0} : {__in0, 2'h0, __in0}) : ((__resumption_tag[2:1] == 2'h2) ? 4'h2 : ((__resumption_tag[2:1] == 2'h3) ? zll_pure_dispatch5_out : zll_pure_dispatch5_outR1));
  assign __resumption_tag_next = zres[2:0];
  assign __out0 = zres[3];
  initial __resumption_tag = 3'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Pure_dispatch5 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [0:0] zzllzurewirezupreludezuzaza2zuout;
  logic [0:0] rewirezupreludezuzazazuout;
  logic [0:0] rewirezupreludezuzazazuoutR1;
  ZZLLzuReWirezuPreludezuzaza2  inst (arg1, zzllzurewirezupreludezuzaza2zuout);
  ReWirezuPreludezuzaza  instR1 (arg1, arg1, rewirezupreludezuzazazuout);
  ReWirezuPreludezuzaza  instR2 ((arg0 == 1'h1) ? zzllzurewirezupreludezuzaza2zuout : rewirezupreludezuzazazuout, arg0, rewirezupreludezuzazazuoutR1);
  assign res = {rewirezupreludezuzazazuoutR1, 3'h4};
endmodule

module ZZLLzuReWirezuPreludezuzaza2 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [0:0] zzllzurewirezupreludezuzaza2zuout;
  ZZLLzuReWirezuPreludezuzaza2  inst (arg1, zzllzurewirezupreludezuzaza2zuout);
  assign res = (arg0 == 1'h1) ? zzllzurewirezupreludezuzaza2zuout : 1'h0;
endmodule