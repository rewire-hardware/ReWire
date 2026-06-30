module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zll_main_go17_out;
  logic [1:0] zi3;
  logic [0:0] zll_main_go12_out;
  logic [0:0] zll_main_go12_outR1;
  logic [0:0] zi5;
  logic [2:0] zi6;
  logic [0:0] zi7;
  logic [2:0] zres;
  ZLL_Main_go17  inst (__st0, zll_main_go17_out);
  assign zi3 = {__st0, zll_main_go17_out};
  ZLL_Main_go12  instR1 (zi3[1], zll_main_go12_out);
  ZLL_Main_go12  instR2 (__st0, zll_main_go12_outR1);
  assign zi5 = (zi3[0] == 1'h1) ? zll_main_go12_out : zll_main_go12_outR1;
  assign zi6 = {2'h0, zi5};
  assign zi7 = zi6[0];
  assign zres = {2'h2, zi7};
  assign __st0_next = zres[0];
  assign __out0 = zres[1];
  initial __st0 = 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_go17 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_go12 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule