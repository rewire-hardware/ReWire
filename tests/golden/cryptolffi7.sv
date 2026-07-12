module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] cry$mix$0_bump$0_out;
  logic [7:0] cry$mix$0_scale$3_out;
  logic [7:0] cry$mix$0_scale$3_outR1;
  logic [7:0] cry$mix$0_bump$0_outR1;
  logic [7:0] zi0;
  logic [7:0] zres;
  cry$mix$0_bump$0  inst (__in0, cry$mix$0_bump$0_out);
  cry$mix$0_scale$3  instR1 (cry$mix$0_bump$0_out, cry$mix$0_scale$3_out);
  cry$mix$0_scale$3  instR2 (__in0, cry$mix$0_scale$3_outR1);
  cry$mix$0_bump$0  instR3 (cry$mix$0_scale$3_outR1, cry$mix$0_bump$0_outR1);
  assign zi0 = cry$mix$0_scale$3_out + cry$mix$0_bump$0_outR1;
  assign zres = zi0;
  assign __out0 = zres;
endmodule

module cry$mix$0_bump$0 (input logic [7:0] arg0,
  output logic [7:0] res);
  assign res = arg0 + 8'h1;
endmodule

module cry$mix$0_scale$3 (input logic [7:0] arg0,
  output logic [7:0] res);
  assign res = arg0 * 8'h3;
endmodule