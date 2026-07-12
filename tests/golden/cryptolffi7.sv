module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] cry$mix$8_8_bump$0_out;
  logic [7:0] cry$mix$8_8_scale$3_out;
  logic [7:0] cry$mix$8_8_scale$3_outR1;
  logic [7:0] cry$mix$8_8_bump$0_outR1;
  // combinational logic
  cry$mix$8__8_bump$0  bump$0_i (__in0, cry$mix$8_8_bump$0_out);
  cry$mix$8__8_scale$3  scale$3_i (cry$mix$8_8_bump$0_out, cry$mix$8_8_scale$3_out);
  cry$mix$8__8_scale$3  scale$3_iR1 (__in0, cry$mix$8_8_scale$3_outR1);
  cry$mix$8__8_bump$0  bump$0_iR1 (cry$mix$8_8_scale$3_outR1, cry$mix$8_8_bump$0_outR1);
  wire [7:0] Za = cry$mix$8_8_scale$3_out + cry$mix$8_8_bump$0_outR1;
  // outputs
  assign __out0 = Za;
endmodule

// cry$mix$8_8.bump$0
module cry$mix$8__8_bump$0 (input logic [7:0] x$0,
  output logic [7:0] res);
  assign res = x$0 + 8'h1;
endmodule

// cry$mix$8_8.scale$3
module cry$mix$8__8_scale$3 (input logic [7:0] x$0,
  output logic [7:0] res);
  assign res = x$0 * 8'h3;
endmodule