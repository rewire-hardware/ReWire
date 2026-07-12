module top_level (input logic [6:0] __in0,
  output logic [4:0] __out0);
  // combinational logic
  wire [6:0] slice_in = (7'h14 == 7'h0) ? __in0 : (__in0 % 7'h14);
  wire [127:0] slice_inR1 = (128'h14 == {8'h80{1'h0}}) ? ({{7'h7b{1'h0}}, slice_in[4:0]} + 128'h6) : (({{7'h7b{1'h0}}, slice_in[4:0]} + 128'h6) % 128'h14);
  wire [4:0] Za = slice_inR1[4:0];
  // outputs
  assign __out0 = Za;
endmodule