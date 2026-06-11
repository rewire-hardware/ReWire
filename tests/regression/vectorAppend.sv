module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0);
  logic [127:0] zin;
  logic [63:0] zi0;
  logic [63:0] zi1;
  logic [31:0] zi2;
  logic [64:0] zi3;
  logic [63:0] zi4;
  logic [64:0] zres;
  assign zin = {__in0, __in1};
  assign zi0 = zin[127:64];
  assign zi1 = zin[63:0];
  assign zi2 = zi0[63:32];
  assign zi3 = {1'h0, {{zi2[7:0], zi2[15:8], zi2[23:16], zi2[31:24]}, 8'h0, zi1[23:0]}};
  assign zi4 = zi3[63:0];
  assign zres = {1'h1, zi4};
  assign __out0 = zres[63:0];
endmodule