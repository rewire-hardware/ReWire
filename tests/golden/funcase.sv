module top_level (input logic [1:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] Za;
  // combinational logic
  always_comb case (__in0)
    2'h0: Za = 1'h0;
    2'h1: Za = 1'h0;
    default: Za = ~(__in0 == 2'h2);
  endcase
  // outputs
  assign __out0 = Za;
endmodule