module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] zx2_out;
  logic [15:0] zin;
  logic [15:0] zh0;
  logic [15:0] zh1;
  logic [8:0] zi0;
  logic [7:0] zi1;
  logic [8:0] zres;
  assign zin = __in0;
  assign zh0 = zin;
  assign zh1 = zh0;
  assign zi0 = {1'h0, zx2_out};
  assign zi1 = zi0[7:0];
  assign zres = {1'h1, zi1};
  mymod  zx2 (clk, rst, zh1, zx2_out);
  assign __out0 = zres[7:0];
endmodule