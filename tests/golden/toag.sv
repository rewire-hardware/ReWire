module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [0:0] zi0;
  logic [0:0] zll_main_go2_out;
  logic [0:0] zll_main_go2_outR1;
  logic [3:0] zres;
  assign zi0 = __resumption_tag[0];
  ZLL_Main_go2  inst (zi0, zi0, zll_main_go2_out);
  ZLL_Main_go2  instR1 ((__in0 == 1'h0) ? zll_main_go2_out : zi0, __in0, zll_main_go2_outR1);
  assign zres = (__resumption_tag[2:1] == 2'h1) ? {zll_main_go2_outR1, 3'h0} : ((__resumption_tag[2:1] == 2'h2) ? ((__in0 == 1'h0) ? {__in0, 2'h1, __in0} : {3'h1, __in0}) : 4'h4);
  assign __resumption_tag_next = zres[2:0];
  assign __out0 = zres[3];
  initial __resumption_tag = 3'h4;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h4;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_go2 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? arg1 : 1'h0;
endmodule