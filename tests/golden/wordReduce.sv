module top_level (input logic [7:0] __in0,
  output logic [5:0] __out0);
  // combinational logic
  wire [5:0] Za = {&__in0, ~(&__in0), |__in0, ~(|__in0), ^__in0, ~(^__in0)};
  // outputs
  assign __out0 = Za;
endmodule