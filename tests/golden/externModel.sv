module top_level (input logic [31:0] __in0,
  output logic [31:0] __out0);
  logic [31:0] extres;
  logic [31:0] extresR1;
  logic [31:0] extresR2;
  logic [31:0] extresR3;
  // combinational logic
  andW32  andW32_i (__in0, 32'hf0f0f0f0, extres[31:0]);
  notW32  notW32_i (__in0, extresR1[31:0]);
  xorW32  xorW32_i (extresR1, 32'h12345678, extresR2[31:0]);
  plusW32  plusW32_i (extres, extresR2, extresR3[31:0]);
  // outputs
  assign __out0 = extresR3;
endmodule

// Main.andModel
module Main_andModel (input logic [31:0] a,
  input logic [31:0] b,
  output logic [31:0] res);
  assign res = a & b;
endmodule

// Main.xorModel
module Main_xorModel (input logic [31:0] a,
  input logic [31:0] b,
  output logic [31:0] res);
  assign res = a ^ b;
endmodule

// Main.notModel
module Main_notModel (input logic [31:0] Zeta0,
  output logic [31:0] res);
  assign res = ~Zeta0;
endmodule

// Main.plusModel
module Main_plusModel (input logic [31:0] a,
  input logic [31:0] b,
  output logic [31:0] res);
  assign res = a + b;
endmodule