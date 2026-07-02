module top_level (input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [7:0] zi3;
  logic [7:0] zres;
  assign zi0 = __in0[15];
  assign zi1 = __in0[8];
  assign zi2 = (zi0 == 1'h1) ? zi1 : 1'h0;
  assign zi3 = (zi2 == 1'h0) ? __in0[15:8] : __in0[7:0];
  assign zres = zi3;
  assign __out0 = zres;
endmodule