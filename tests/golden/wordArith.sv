module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] zi0;
  logic [7:0] zres;
  assign zi0 = ((__in0 + 8'h1) == 8'h0) ? ((8'h3 == 8'h0) ? 8'hff : ((((__in0 + 8'h1) ** 8'h2) * (__in0 - 8'h2)) / 8'h3)) : (((8'h3 == 8'h0) ? 8'hff : ((((__in0 + 8'h1) ** 8'h2) * (__in0 - 8'h2)) / 8'h3)) % (__in0 + 8'h1));
  assign zres = zi0;
  assign __out0 = zres;
endmodule