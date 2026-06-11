module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [127:0] zin;
  logic [63:0] zi0;
  logic [63:0] zi1;
  logic [7:0] zi2;
  logic [55:0] zi3;
  logic [63:0] slice_in;
  logic [7:0] zi4;
  logic [55:0] zi5;
  logic [63:0] zi6;
  logic [55:0] zi7;
  logic [7:0] zi8;
  logic [63:0] zi9;
  logic [7:0] zi10;
  logic [55:0] zi11;
  logic [128:0] zi12;
  logic [127:0] zi13;
  logic [128:0] zres;
  assign zin = {__in0, __in1};
  assign zi0 = zin[127:64];
  assign zi1 = zin[63:0];
  assign zi2 = zi0[63:56];
  assign zi3 = zi0[55:0];
  assign slice_in = zi1 >> (((128'h8 - {{7'h7d{1'h0}}, 3'h7}) - 128'h1) * 128'h8);
  assign zi4 = slice_in[7:0];
  assign zi5 = zi1[63:8];
  assign zi6 = {zi3, zi4};
  assign zi7 = zi6[63:8];
  assign zi8 = zi6[7:0];
  assign zi9 = {zi2, zi5};
  assign zi10 = zi9[63:56];
  assign zi11 = zi9[55:0];
  assign zi12 = {1'h0, {{zi7, zi8}, {zi10, zi11}}};
  assign zi13 = zi12[127:0];
  assign zres = {1'h1, zi13};
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule