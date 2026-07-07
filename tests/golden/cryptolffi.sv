module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  output logic [7:0] __out0);
  logic [3:0] zi2;
  logic [3:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [7:0] zres;
  assign zi2 = __in1[7:4];
  assign zi3 = __in1[3:0];
  assign zi4 = {zi3, zi2};
  assign zi5 = (zi4 > __in0) ? __in0 : zi4;
  assign zres = zi5;
  assign __out0 = zres;
endmodule