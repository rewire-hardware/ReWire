module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [7:0] zi3;
  logic [7:0] zres;
  assign zi3 = ((__in0 & __in1) ^ __in2) | (~(((~__in0) & (~__in1)) ^ __in2));
  assign zres = zi3;
  assign __out0 = zres;
endmodule