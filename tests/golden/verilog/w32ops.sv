module plusW32 (input logic [31:0] a, input logic [31:0] b, output logic [31:0] out);
      assign out = a + b;
endmodule

module andW32 (input logic [31:0] a, input logic [31:0] b, output logic [31:0] out);
      assign out = a & b;
endmodule

module xorW32 (input logic [31:0] a, input logic [31:0] b, output logic [31:0] out);
      assign out = a ^ b;
endmodule

module notW32 (input logic [31:0] a, output logic [31:0] out);
      assign out = ~a;
endmodule
