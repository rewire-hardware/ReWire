module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  output logic [7:0] __out0);
  logic [7:0] cry$hof$8_8_inc$1_out;
  logic [7:0] cry$hof$8_8_inc$1_outR1;
  logic [7:0] cry$hof$8_8_inc$1_outR2;
  // combinational logic
  cry$hof$8__8_inc$1  inc$1_i (__in1, cry$hof$8_8_inc$1_out);
  cry$hof$8__8_inc$1  inc$1_iR1 (cry$hof$8_8_inc$1_out, cry$hof$8_8_inc$1_outR1);
  wire [0:0] Zeta0 = __in0 == __in1;
  wire [7:0] Zeta1 = __in0 * (__in0 * (__in0 * (__in0 * (__in0 * 8'h1))));
  cry$hof$8__8_inc$1  inc$1_iR2 (Zeta1, cry$hof$8_8_inc$1_outR2);
  wire [7:0] Za = ((((__in1 + __in0) + __in0) + ((cry$hof$8_8_inc$1_outR1 + ((__in1 * 8'h2) * 8'h2)) + __in1)) + (__in0 + __in1)) + (Zeta0 ? cry$hof$8_8_inc$1_outR2 : (~Zeta1));
  // outputs
  assign __out0 = Za;
endmodule

// cry$hof$8_8.inc$1
// also: cry$pick$Bit_8_8.inc$0
module cry$hof$8__8_inc$1 (input logic [7:0] v$0,
  output logic [7:0] res);
  assign res = v$0 + 8'h1;
endmodule