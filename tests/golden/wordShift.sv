module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zres;
  assign zi0 = (__in0 << 8'h5) | (__in0 >> 8'h3);
  assign zi1 = (zi0 << 8'h3) | $unsigned($signed(zi0) >>> 8'h5);
  assign zres = zi1;
  assign __out0 = zres;
endmodule