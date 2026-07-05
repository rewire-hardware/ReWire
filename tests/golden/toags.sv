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
  logic [5:0] zll_main_go4_out;
  logic [0:0] zi3;
  logic [0:0] zi6;
  logic [5:0] zll_main_go4_outR1;
  logic [5:0] zres;
  ZLL_Main_go4  inst (__in0, __st0, __st0, __st1, zll_main_go4_out);
  assign zi3 = __resumption_tag[0];
  assign zi6 = __resumption_tag[0];
  ZLL_Main_go4  instR1 (zi6, __in0, __st0, __st1, zll_main_go4_outR1);
  assign zres = (__resumption_tag[2:1] == 2'h1) ? ((__in0 == 1'h0) ? zll_main_go4_out : {3'h0, __in0, __st0, __st1}) : ((__resumption_tag[2:1] == 2'h2) ? {4'h2, __st0, zi3} : zll_main_go4_outR1);
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

module ZLL_Main_go8 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  assign res = (arg1 == 1'h1) ? arg0 : 1'h0;
endmodule

module ZLL_Main_go4 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [5:0] res);
  logic [0:0] zll_main_go8_out;
  logic [0:0] zll_main_go8_outR1;
  ZLL_Main_go8  inst (arg0, arg0, zll_main_go8_out);
  ZLL_Main_go8  instR1 (arg1, (arg1 == 1'h0) ? zll_main_go8_out : arg0, zll_main_go8_outR1);
  assign res = {zll_main_go8_outR1, 2'h2, arg1, arg2, arg3};
endmodule