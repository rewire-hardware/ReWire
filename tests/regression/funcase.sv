module top_level (input logic [1:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] zin;
  logic [0:0] zres;
  assign zin = __in0;
  assign zres = (zin == 2'h0) ? 1'h0 : ((zin == 2'h1) ? 1'h0 : ((zin == 2'h2) ? 1'h0 : 1'h1));
  assign __out0 = zres;
endmodule