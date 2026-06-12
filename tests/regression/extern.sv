module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] zx2_out;
  mymod  zx2 (clk, rst, __in0, zx2_out);
  assign __out0 = zx2_out;
endmodule