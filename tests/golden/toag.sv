module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [0:0] zi0;
  logic [3:0] zll_main_go9_out;
  logic [0:0] zi1;
  logic [3:0] zll_main_go9_outR1;
  logic [3:0] zres;
  assign zi0 = __resumption_tag[0];
  ZLL_Main_go9  inst (zi0, __in0, zll_main_go9_out);
  assign zi1 = __resumption_tag[0];
  ZLL_Main_go9  instR1 (zi1, __in0, zll_main_go9_outR1);
  assign zres = (__resumption_tag[2:1] == 2'h1) ? 4'h6 : ((__resumption_tag[2:1] == 2'h2) ? zll_main_go9_out : ((__resumption_tag[2:1] == 2'h3) ? ((__in0 == 1'h0) ? {__in0, 2'h2, __in0} : {3'h0, __in0}) : zll_main_go9_outR1));
  assign __resumption_tag_next = zres[2:0];
  assign __out0 = zres[3];
  initial __resumption_tag = 3'h6;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h6;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_go9 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [0:0] zll_main_go5_out;
  logic [0:0] zll_main_go5_outR1;
  ZLL_Main_go5  inst (arg0, arg0, zll_main_go5_out);
  ZLL_Main_go5  instR1 ((arg1 == 1'h0) ? zll_main_go5_out : arg0, arg1, zll_main_go5_outR1);
  assign res = {zll_main_go5_outR1, 3'h2};
endmodule

module ZLL_Main_go5 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? arg1 : 1'h0;
endmodule