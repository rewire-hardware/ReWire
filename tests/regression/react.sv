module top_level (input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] zi0;
  logic [0:0] zi1;
  logic [1:0] zres;
  assign zi0 = {1'h0, (__in0 == 1'h1) ? 1'h0 : 1'h1};
  assign zi1 = zi0[0];
  assign zres = {1'h1, zi1};
  assign __out0 = zres[0];
endmodule