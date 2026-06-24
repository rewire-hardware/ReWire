module top_level (input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [7:0] zi0;
  logic [15:0] zres;
  assign zi0 = __in0 & 8'h3;
  assign zres = {(zi0 == 8'h0) ? 8'hff : (__in0 / zi0), ((zi0 == 8'h0) ? __in0 : (__in0 % zi0)) ^ 8'hff};
  assign __out0 = zres;
endmodule