module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] zi4;
  logic [8:0] zi9;
  logic [7:0] zi10;
  logic [8:0] zres;
  assign zi4 = (__in0 << 8'h5) | (__in0 >> 8'h3);
  assign zi9 = {1'h0, (zi4 << 8'h3) | $unsigned($signed(zi4) >>> 8'h5)};
  assign zi10 = zi9[7:0];
  assign zres = {1'h1, zi10};
  assign __out0 = zres[7:0];
endmodule