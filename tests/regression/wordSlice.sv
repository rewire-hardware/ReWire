module top_level (input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] zin;
  logic [0:0] rewirezupreludezuzazazuout;
  logic [0:0] zi0;
  logic [16:0] zi1;
  logic [15:0] zi2;
  logic [0:0] rewirezupreludezuzazazuoutR1;
  logic [0:0] zi3;
  logic [16:0] zi4;
  logic [15:0] zi5;
  logic [8:0] zi6;
  logic [7:0] zi7;
  logic [8:0] zres;
  assign zin = __in0;
  ReWirezuPreludezuzaza  inst (zin[15], zin[8], rewirezupreludezuzazazuout);
  assign zi0 = rewirezupreludezuzazazuout;
  assign zi1 = {zin, zi0};
  assign zi2 = zi1[16:1];
  ReWirezuPreludezuzaza  instR1 (zin[15], zin[8], rewirezupreludezuzazazuoutR1);
  assign zi3 = rewirezupreludezuzazazuoutR1;
  assign zi4 = {zin, zi3};
  assign zi5 = zi4[16:1];
  assign zi6 = {1'h0, (zi1[0] == 1'h1) ? zi2[7:0] : zi5[15:8]};
  assign zi7 = zi6[7:0];
  assign zres = {1'h1, zi7};
  assign __out0 = zres[7:0];
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [1:0] zi0;
  logic [0:0] zi1;
  logic [1:0] zi2;
  logic [0:0] zi3;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[0];
  assign zi2 = {arg0, arg1};
  assign zi3 = zi2[0];
  assign res = (zi0[1] == 1'h1) ? zi1 : 1'h0;
endmodule