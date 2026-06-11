module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [23:0] zin;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [8:0] zi3;
  logic [7:0] zi4;
  logic [8:0] zres;
  assign zin = {__in0, __in1, __in2};
  assign zi0 = zin[23:16];
  assign zi1 = zin[15:8];
  assign zi2 = zin[7:0];
  assign zi3 = {1'h0, ((zi0 & zi1) ^ zi2) | (~(((~zi0) & (~zi1)) ^ zi2))};
  assign zi4 = zi3[7:0];
  assign zres = {1'h1, zi4};
  assign __out0 = zres[7:0];
endmodule