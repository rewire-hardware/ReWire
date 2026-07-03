module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [2:0] zll_main_dev6_out;
  logic [2:0] zres;
  ZLL_Main_dev6  inst (__st0, zll_main_dev6_out);
  assign zres = (__in0 == 1'h0) ? zll_main_dev6_out : 3'h6;
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

module ZLL_Main_dev6 (input logic [0:0] arg0,
  output logic [2:0] res);
  logic [0:0] zi0;
  logic [2:0] zi1;
  logic [0:0] zi2;
  logic [0:0] zi3;
  logic [2:0] zi4;
  logic [0:0] zi5;
  assign zi0 = (arg0 == 1'h0) ? arg0 : 1'h0;
  assign zi1 = {2'h0, zi0};
  assign zi2 = zi1[0];
  assign zi3 = (arg0 == 1'h0) ? 1'h1 : zi2;
  assign zi4 = {2'h0, zi3};
  assign zi5 = zi4[0];
  assign res = {1'h1, arg0, zi5};
endmodule