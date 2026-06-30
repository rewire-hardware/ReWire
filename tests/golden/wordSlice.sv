module top_level (input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [0:0] rewirezupreludezuzazazuout;
  logic [0:0] zi0;
  logic [7:0] zi2;
  logic [7:0] zres;
  ReWirezuPreludezuzaza  inst (__in0[15], __in0[8], rewirezupreludezuzazazuout);
  assign zi0 = rewirezupreludezuzazazuout;
  assign zi2 = (zi0 == 1'h1) ? __in0[7:0] : __in0[15:8];
  assign zres = zi2;
  assign __out0 = zres;
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? arg1 : 1'h0;
endmodule