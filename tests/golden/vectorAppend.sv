module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0);
  logic [63:0] zi0;
  logic [63:0] zres;
  assign zi0 = {__in0[39:32], __in0[47:40], __in0[55:48], __in0[63:56], 8'h0, __in1[23:0]};
  assign zres = zi0;
  assign __out0 = zres;
endmodule