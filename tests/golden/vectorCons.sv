module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [7:0] zi2;
  logic [55:0] zi3;
  logic [63:0] slice_in;
  logic [7:0] zi4;
  logic [55:0] zi5;
  logic [127:0] zi12;
  logic [127:0] zres;
  assign zi2 = __in0[63:56];
  assign zi3 = __in0[55:0];
  assign slice_in = __in1 >> {8'h80{1'h0}};
  assign zi4 = slice_in[7:0];
  assign zi5 = __in1[63:8];
  assign zi12 = {zi3, zi4, zi2, zi5};
  assign zres = zi12;
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule