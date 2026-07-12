module top_level (input logic [15:0] __in0,
  output logic [7:0] __out0);
  // combinational logic
  wire [0:0] Zt1 = __in0[15];
  wire [0:0] Zt2 = Zt1 ? __in0[8] : 1'h0;
  wire [7:0] Za = (~Zt2) ? __in0[15:8] : __in0[7:0];
  // outputs
  assign __out0 = Za;
endmodule