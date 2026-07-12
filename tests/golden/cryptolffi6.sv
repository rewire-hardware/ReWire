module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  output logic [7:0] __out0);
  // combinational logic
  wire [7:0] za$4698 = (8'hfb == 8'h0) ? __in0 : (__in0 % 8'hfb);
  wire [7:0] zb$4701 = (8'hfb == 8'h0) ? __in1 : (__in1 % 8'hfb);
  wire [15:0] slice_in = (16'hfb == 16'h0) ? ({8'h0, za$4698} * {8'h0, za$4698}) : (({8'h0, za$4698} * {8'h0, za$4698}) % 16'hfb);
  wire [8:0] slice_inR1 = (9'hfb == 9'h0) ? ({1'h0, slice_in[7:0]} + {1'h0, zb$4701}) : (({1'h0, slice_in[7:0]} + {1'h0, zb$4701}) % 9'hfb);
  wire [15:0] Zeta0 = {__in0, __in1};
  wire [15:0] slice_inR2 = (16'h101 == 16'h0) ? Zeta0 : (Zeta0 % 16'h101);
  wire [16:0] slice_inR3 = (17'h101 == 17'h0) ? (17'h0 * {8'h0, slice_inR2[8:0]}) : ((17'h0 * {8'h0, slice_inR2[8:0]}) % 17'h101);
  wire [9:0] slice_inR4 = (10'h101 == 10'h0) ? ({1'h0, slice_inR3[8:0]} + 10'h1) : (({1'h0, slice_inR3[8:0]} + 10'h1) % 10'h101);
  wire [15:0] slice_inR5 = (16'h101 == 16'h0) ? Zeta0 : (Zeta0 % 16'h101);
  wire [16:0] slice_inR6 = (17'h101 == 17'h0) ? ({8'h0, slice_inR4[8:0]} * {8'h0, slice_inR5[8:0]}) : (({8'h0, slice_inR4[8:0]} * {8'h0, slice_inR5[8:0]}) % 17'h101);
  wire [9:0] slice_inR7 = (10'h101 == 10'h0) ? ({1'h0, slice_inR6[8:0]} + 10'h2) : (({1'h0, slice_inR6[8:0]} + 10'h2) % 10'h101);
  wire [15:0] slice_inR8 = (16'h101 == 16'h0) ? Zeta0 : (Zeta0 % 16'h101);
  wire [16:0] slice_inR9 = (17'h101 == 17'h0) ? ({8'h0, slice_inR7[8:0]} * {8'h0, slice_inR8[8:0]}) : (({8'h0, slice_inR7[8:0]} * {8'h0, slice_inR8[8:0]}) % 17'h101);
  wire [9:0] slice_inR10 = (10'h101 == 10'h0) ? ({1'h0, slice_inR9[8:0]} + 10'h3) : (({1'h0, slice_inR9[8:0]} + 10'h3) % 10'h101);
  wire [15:0] slice_inR11 = (16'h101 == 16'h0) ? Zeta0 : (Zeta0 % 16'h101);
  wire [16:0] slice_inR12 = (17'h101 == 17'h0) ? ({8'h0, slice_inR10[8:0]} * {8'h0, slice_inR11[8:0]}) : (({8'h0, slice_inR10[8:0]} * {8'h0, slice_inR11[8:0]}) % 17'h101);
  wire [9:0] slice_inR13 = (10'h101 == 10'h0) ? ({1'h0, slice_inR12[8:0]} + 10'h4) : (({1'h0, slice_inR12[8:0]} + 10'h4) % 10'h101);
  wire [15:0] slice_inR14 = {7'h0, slice_inR13[8:0]};
  wire [7:0] Za = slice_inR1[7:0] + slice_inR14[7:0];
  // outputs
  assign __out0 = Za;
endmodule