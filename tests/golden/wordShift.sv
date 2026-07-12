module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  // combinational logic
  wire [7:0] Za = (__in0 << 8'h5) | (__in0 >> 8'h3);
  wire [7:0] Za1 = (Za << 8'h3) | $unsigned($signed(Za) >>> 8'h5);
  // outputs
  assign __out0 = Za1;
endmodule