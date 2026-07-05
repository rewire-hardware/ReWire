module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [63:0] slice_in;
  logic [127:0] zi1;
  logic [127:0] zres;
  assign slice_in = __in1 >> {8'h80{1'h0}};
  assign zi1 = {__in0[55:0], slice_in[7:0], __in0[63:56], __in1[63:8]};
  assign zres = zi1;
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule