module top_level (input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [7:0] zin;
  logic [7:0] zi0;
  logic [15:0] zres;
  assign zin = __in0;
  assign zi0 = (8'h0 == 8'h0) ? (zin & 8'h3) : ((zin & 8'h3) % 8'h0);
  assign zres = {(zi0 == 8'h0) ? 8'hff : (zin / zi0), ((zi0 == 8'h0) ? zin : (zin % zi0)) ^ 8'hff};
  assign __out0 = zres;
endmodule