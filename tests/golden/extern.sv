module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] mymod$x1_out;
  // instances
  mymod  mymod$x1 (clk, rst, __in0, mymod$x1_out);
  // outputs
  assign __out0 = mymod$x1_out;
endmodule