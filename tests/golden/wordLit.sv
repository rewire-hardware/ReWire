module top_level (input logic [7:0] __in0,
  output logic [15:0] __out0);
  // combinational logic
  wire [15:0] Zx = {8'h0, __in0} ^ 16'h1;
  // outputs
  assign __out0 = Zx;
endmodule