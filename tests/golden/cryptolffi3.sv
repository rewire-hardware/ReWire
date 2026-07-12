module top_level (input logic [1:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [7:0] zi5;
  logic [15:0] zi6;
  logic [7:0] zi8;
  logic [7:0] zi9;
  logic [7:0] zi10;
  logic [7:0] zi15;
  logic [7:0] zi16;
  logic [9:0] zi17;
  logic [7:0] zi18;
  logic [7:0] zres;
  assign zi5 = __in1 + __in2;
  assign zi6 = {zi5, __in1};
  assign zi8 = {__in1[3:0] + ((__in1[7:4] == 4'hf) ? 4'h1 : 4'h0), __in1[7:4] + 4'h1};
  assign zi9 = (zi6[15:8] ^ zi6[7:0]) ^ {zi8[3:0], zi8[7:4]};
  assign zi10 = {__in1[0], __in1[1], __in1[2], __in1[3], __in1[4], __in1[5], __in1[6], __in1[7]};
  assign zi15 = __in2 - zi10;
  assign zi16 = zi9 ^ zi15;
  assign zi17 = (__in0 == 2'h0) ? 10'h0 : ((__in0 == 2'h1) ? 10'h100 : {2'h2, zi16});
  assign zi18 = (zi17[9:8] == 2'h0) ? (zi9 + zi15) : ((zi17[9:8] == 2'h1) ? (zi9 - zi15) : ((zi17[9:8] == 2'h2) ? zi17[7:0] : 8'h0));
  assign zres = zi18;
  assign __out0 = zres;
endmodule