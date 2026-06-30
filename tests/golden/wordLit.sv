module top_level (input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [15:0] zi0;
  logic [15:0] zres;
  assign zi0 = {8'h0, __in0} ^ 16'h1;
  assign zres = zi0;
  assign __out0 = zres;
endmodule