module top_level (input logic [6:0] __in0,
  output logic [6:0] __out0);
  logic [6:0] zin;
  logic [6:0] zi0;
  logic [6:0] zres;
  assign zin = __in0;
  assign zi0 = (7'h64 == 7'h0) ? (zin + 7'h1) : ((zin + 7'h1) % 7'h64);
  assign zres = zi0;
  assign __out0 = zres;
endmodule