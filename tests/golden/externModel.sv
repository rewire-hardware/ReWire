module top_level (input logic [31:0] __in0,
  output logic [31:0] __out0);
  logic [31:0] extres;
  logic [31:0] zi0;
  logic [31:0] extresR1;
  logic [31:0] zi1;
  logic [31:0] extresR2;
  logic [31:0] zi2;
  logic [31:0] extresR3;
  logic [31:0] zi3;
  logic [31:0] zres;
  andW32  inst (__in0, 32'hf0f0f0f0, extres[31:0]);
  assign zi0 = extres;
  notW32  instR1 (__in0, extresR1[31:0]);
  assign zi1 = extresR1;
  xorW32  instR2 (zi1, 32'h12345678, extresR2[31:0]);
  assign zi2 = extresR2;
  plusW32  instR3 (zi0, zi2, extresR3[31:0]);
  assign zi3 = extresR3;
  assign zres = zi3;
  assign __out0 = zres;
endmodule

module Main_andModel (input logic [31:0] arg0,
  input logic [31:0] arg1,
  output logic [31:0] res);
  assign res = arg0 & arg1;
endmodule

module Main_xorModel (input logic [31:0] arg0,
  input logic [31:0] arg1,
  output logic [31:0] res);
  assign res = arg0 ^ arg1;
endmodule

module Main_notModel (input logic [31:0] arg0,
  output logic [31:0] res);
  assign res = ~arg0;
endmodule

module Main_plusModel (input logic [31:0] arg0,
  input logic [31:0] arg1,
  output logic [31:0] res);
  assign res = arg0 + arg1;
endmodule