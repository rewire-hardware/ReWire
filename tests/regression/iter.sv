module top_level (input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] zres;
  assign zres = (__in0 == 1'h1) ? 1'h0 : 1'h1;
  assign __out0 = zres;
endmodule