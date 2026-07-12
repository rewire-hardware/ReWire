module top_level (input logic [6:0] __in0,
  output logic [6:0] __out0);
  // combinational logic
  wire [6:0] a = (7'h64 == 7'h0) ? (__in0 + 7'h1) : ((__in0 + 7'h1) % 7'h64);
  // outputs
  assign __out0 = a;
endmodule