module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  output logic [7:0] __out0);
  logic [7:0] cry$hof$8_8_inc$1_out;
  logic [7:0] cry$hof$8_8_inc$1_outR1;
  logic [0:0] zi5;
  logic [7:0] zi6;
  logic [7:0] cry$hof$8_8_inc$1_outR2;
  logic [7:0] zi7;
  logic [7:0] zres;
  cry$hof$8__8_inc$1  inst (__in1, cry$hof$8_8_inc$1_out);
  cry$hof$8__8_inc$1  instR1 (cry$hof$8_8_inc$1_out, cry$hof$8_8_inc$1_outR1);
  assign zi5 = __in0 == __in1;
  assign zi6 = __in0 * (__in0 * (__in0 * (__in0 * (__in0 * 8'h1))));
  cry$hof$8__8_inc$1  instR2 (zi6, cry$hof$8_8_inc$1_outR2);
  assign zi7 = ((((__in1 + __in0) + __in0) + ((cry$hof$8_8_inc$1_outR1 + ((__in1 * 8'h2) * 8'h2)) + __in1)) + (__in0 + __in1)) + (zi5 ? cry$hof$8_8_inc$1_outR2 : (~zi6));
  assign zres = zi7;
  assign __out0 = zres;
endmodule

module cry$hof$8__8_inc$1 (input logic [7:0] arg0,
  output logic [7:0] res);
  assign res = arg0 + 8'h1;
endmodule