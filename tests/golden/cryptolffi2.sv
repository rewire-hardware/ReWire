module top_level (input logic [15:0] __in0,
  output logic [15:0] __out0,
  output logic [3:0] __out1);
  logic [3:0] cry$bytemap$0_subst$1_out;
  logic [3:0] cry$bytemap$0_subst$1_outR1;
  logic [3:0] cry$bytemap$0_subst$1_outR2;
  logic [3:0] cry$bytemap$0_subst$1_outR3;
  logic [15:0] zi0;
  logic [3:0] zi1;
  logic [19:0] zi2;
  logic [19:0] zres;
  cry$bytemap$0_subst$1  inst (__in0[15:12], cry$bytemap$0_subst$1_out);
  cry$bytemap$0_subst$1  instR1 (__in0[11:8], cry$bytemap$0_subst$1_outR1);
  cry$bytemap$0_subst$1  instR2 (__in0[7:4], cry$bytemap$0_subst$1_outR2);
  cry$bytemap$0_subst$1  instR3 (__in0[3:0], cry$bytemap$0_subst$1_outR3);
  assign zi0 = {cry$bytemap$0_subst$1_out, cry$bytemap$0_subst$1_outR1, cry$bytemap$0_subst$1_outR2, cry$bytemap$0_subst$1_outR3};
  assign zi1 = (((4'h0 ^ __in0[15:12]) ^ __in0[11:8]) ^ __in0[7:4]) ^ __in0[3:0];
  assign zi2 = {{zi0[7:0], zi0[15:8]}, {zi1[1:0], zi1[3:2]}};
  assign zres = zi2;
  assign __out0 = zres[19:4];
  assign __out1 = zres[3:0];
endmodule

module cry$bytemap$0_subst$1 (input logic [3:0] arg0,
  output logic [3:0] res);
  logic [63:0] slice_in;
  assign slice_in = 64'he4d12fb83a6c5907 << ({2'h0, arg0} * 6'h4);
  assign res = slice_in[63:60];
endmodule