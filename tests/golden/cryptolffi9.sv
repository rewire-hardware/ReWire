module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  output logic [7:0] __out0);
  // combinational logic
  wire [39:0] slice_in = {__in0, __in0 + 8'h1, __in0 + 8'h2, __in0 + 8'h3, __in0 + 8'h4};
  wire [7:0] iter$0$1 = (__in0 * 8'h3) + 8'h1;
  wire [7:0] iter$0$2 = (iter$0$1 * 8'h3) + 8'h1;
  wire [7:0] iter$0$3 = (iter$0$2 * 8'h3) + 8'h1;
  wire [7:0] iter$0$4 = (iter$0$3 * 8'h3) + 8'h1;
  wire [23:0] slice_inR1 = {__in1 + (8'h2 * ((__in1 + 8'h2) - __in1)), __in1 + (8'h3 * ((__in1 + 8'h2) - __in1)), __in1 + (8'h4 * ((__in1 + 8'h2) - __in1))};
  wire [7:0] s$4714$1 = (__in1 << 1'h1) ^ (__in1[7] ? 8'h1d : 8'h0);
  wire [7:0] s$4714$2 = (s$4714$1 << 1'h1) ^ (s$4714$1[7] ? 8'h1d : 8'h0);
  wire [7:0] s$4714$3 = (s$4714$2 << 1'h1) ^ (s$4714$2[7] ? 8'h1d : 8'h0);
  wire [7:0] s$4714$4 = (s$4714$3 << 1'h1) ^ (s$4714$3[7] ? 8'h1d : 8'h0);
  wire [7:0] s$4714$5 = (s$4714$4 << 1'h1) ^ (s$4714$4[7] ? 8'h1d : 8'h0);
  wire [47:0] slice_inR2 = {__in1, s$4714$1, s$4714$2, s$4714$3, s$4714$4, s$4714$5};
  wire [31:0] Zeta0 = {24'h0, __in0};
  wire [31:0] slice_inR3 = {Zeta0[31:24] + 8'h0, Zeta0[23:16] + 8'h1, Zeta0[15:8] + 8'h2, Zeta0[7:0] + 8'h3};
  wire [31:0] slice_inR4 = {__in1, __in1, __in1, __in1};
  wire [7:0] Za = ((((slice_in[7:0] + iter$0$4) + slice_inR1[7:0]) + slice_inR2[7:0]) + slice_inR3[7:0]) + slice_inR4[7:0];
  // outputs
  assign __out0 = Za;
endmodule