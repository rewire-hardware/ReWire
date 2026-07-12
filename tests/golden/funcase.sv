module top_level (input logic [1:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] zi0;
  logic [0:0] zres;
  assign zi0 = (__in0 == 2'h0) ? 1'h0 : ((__in0 == 2'h1) ? 1'h0 : (~(__in0 == 2'h2)));
  assign zres = zi0;
  assign __out0 = zres;
endmodule