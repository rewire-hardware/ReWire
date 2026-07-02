module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  logic [0:0] zi0;
  logic [2:0] zll_main_loop5_out;
  logic [2:0] zll_main_loop5_outR1;
  logic [2:0] zres;
  assign zi0 = __resumption_tag[0];
  ZLL_Main_loop5  inst (zi0, __in0, zll_main_loop5_out);
  ZLL_Main_loop5  instR1 (1'h0, __in0, zll_main_loop5_outR1);
  assign zres = (__resumption_tag[1] == 1'h1) ? zll_main_loop5_out : zll_main_loop5_outR1;
  assign __resumption_tag_next = zres[1:0];
  assign __out0 = zres[2];
  initial __resumption_tag = 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_loop5 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [2:0] res);
  logic [0:0] zi0;
  logic [0:0] main_zookus_out;
  logic [0:0] main_zookus_outR1;
  assign zi0 = (arg0 == 1'h1) ? 1'h1 : 1'h0;
  Main_zookus  inst (zi0, main_zookus_out);
  Main_zookus  instR1 (main_zookus_out, main_zookus_outR1);
  assign res = {main_zookus_outR1, 1'h1, zi0};
endmodule

module Main_zookus (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule