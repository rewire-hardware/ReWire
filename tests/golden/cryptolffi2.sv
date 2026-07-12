module top_level (input logic [15:0] __in0,
  output logic [15:0] __out0,
  output logic [3:0] __out1);
  logic [3:0] cry$bytemap$16_16_subst$1_out;
  logic [3:0] cry$bytemap$16_16_subst$1_outR1;
  logic [3:0] cry$bytemap$16_16_subst$1_outR2;
  logic [3:0] cry$bytemap$16_16_subst$1_outR3;
  // combinational logic
  cry$bytemap$16__16_subst$1  subst$1_i (__in0[15:12], cry$bytemap$16_16_subst$1_out);
  cry$bytemap$16__16_subst$1  subst$1_iR1 (__in0[11:8], cry$bytemap$16_16_subst$1_outR1);
  cry$bytemap$16__16_subst$1  subst$1_iR2 (__in0[7:4], cry$bytemap$16_16_subst$1_outR2);
  cry$bytemap$16__16_subst$1  subst$1_iR3 (__in0[3:0], cry$bytemap$16_16_subst$1_outR3);
  wire [15:0] Zeta0 = {cry$bytemap$16_16_subst$1_out, cry$bytemap$16_16_subst$1_outR1, cry$bytemap$16_16_subst$1_outR2, cry$bytemap$16_16_subst$1_outR3};
  wire [3:0] Zeta0R1 = (((4'h0 ^ __in0[15:12]) ^ __in0[11:8]) ^ __in0[7:4]) ^ __in0[3:0];
  wire [19:0] Za = {{Zeta0[7:0], Zeta0[15:8]}, {Zeta0R1[1:0], Zeta0R1[3:2]}};
  // outputs
  assign __out0 = Za[19:4];
  assign __out1 = Za[3:0];
endmodule

// cry$bytemap$16_16.subst$1
module cry$bytemap$16__16_subst$1 (input logic [3:0] i$0,
  output logic [3:0] res);
  wire [63:0] slice_in = 64'he4d12fb83a6c5907 << ({2'h0, i$0} * 6'h4);
  assign res = slice_in[63:60];
endmodule