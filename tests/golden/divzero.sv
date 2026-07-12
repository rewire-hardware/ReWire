module top_level (input logic [7:0] __in0,
  output logic [15:0] __out0);
  // combinational logic
  wire [7:0] b = __in0 & 8'h3;
  wire [15:0] Za = {(b == 8'h0) ? 8'hff : (__in0 / b), ((b == 8'h0) ? __in0 : (__in0 % b)) ^ 8'hff};
  // outputs
  assign __out0 = Za;
endmodule