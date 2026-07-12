module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  // combinational logic
  wire [7:0] Za = ((__in0 & __in1) ^ __in2) | (~(((~__in0) & (~__in1)) ^ __in2));
  // outputs
  assign __out0 = Za;
endmodule