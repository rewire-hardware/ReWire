module top_level (input logic [6:0] __in0,
  output logic [4:0] __out0);
  logic [6:0] slice_in;
  logic [127:0] slice_inR1;
  logic [4:0] zi0;
  logic [4:0] zres;
  assign slice_in = (7'h14 == 7'h0) ? __in0 : (__in0 % 7'h14);
  assign slice_inR1 = (128'h14 == {8'h80{1'h0}}) ? ({{7'h7b{1'h0}}, slice_in[4:0]} + 128'h6) : (({{7'h7b{1'h0}}, slice_in[4:0]} + 128'h6) % 128'h14);
  assign zi0 = slice_inR1[4:0];
  assign zres = zi0;
  assign __out0 = zres;
endmodule