module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] zi4;
  logic [7:0] zi9;
  logic [7:0] zres;
  assign zi4 = (__in0 << 8'h5) | (__in0 >> 8'h3);
  assign zi9 = (zi4 << 8'h3) | $unsigned($signed(zi4) >>> 8'h5);
  assign zres = zi9;
  assign __out0 = zres;
endmodule