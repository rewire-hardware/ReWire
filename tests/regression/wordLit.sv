module top_level (input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [16:0] zi0;
  logic [15:0] zi1;
  logic [16:0] zres;
  assign zi0 = {1'h0, {8'h0, __in0} ^ 16'h1};
  assign zi1 = zi0[15:0];
  assign zres = {1'h1, zi1};
  assign __out0 = zres[15:0];
endmodule