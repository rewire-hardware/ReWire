module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [2:0] zll_main_go1_out;
  logic [0:0] zi0;
  logic [1:0] zi1;
  logic [0:0] zi2;
  logic [0:0] zi3;
  logic [2:0] zi4;
  logic [0:0] zi5;
  logic [0:0] zi6;
  logic [2:0] zll_main_go1_outR1;
  logic [2:0] zres;
  ZLL_Main_go1  inst (__st0, __st0, zll_main_go1_out);
  assign zi0 = (__st0 == 1'h0) ? 1'h1 : 1'h0;
  assign zi1 = {zi0, zi0};
  assign zi2 = zi1[1];
  assign zi3 = zi1[0];
  assign zi4 = {1'h0, zi2, zi3};
  assign zi5 = zi4[1];
  assign zi6 = zi4[0];
  ZLL_Main_go1  instR1 (zi5, zi6, zll_main_go1_outR1);
  assign zres = (__in0 == 1'h0) ? zll_main_go1_out : zll_main_go1_outR1;
  assign __st0_next = zres[0];
  assign __out0 = zres[1];
  initial __st0 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_go1 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [2:0] res);
  assign res = {1'h1, arg0, arg0};
endmodule