module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  // combinational logic
  wire [63:0] slice_in = __in1 >> {8'h80{1'h0}};
  wire [127:0] Za = {__in0[55:0], slice_in[7:0], __in0[63:56], __in1[63:8]};
  // outputs
  assign __out0 = Za[127:64];
  assign __out1 = Za[63:0];
endmodule