module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0);
  // combinational logic
  wire [63:0] Za = {__in0[39:32], __in0[47:40], __in0[55:48], __in0[63:56], 8'h0, __in1[23:0]};
  // outputs
  assign __out0 = Za;
endmodule