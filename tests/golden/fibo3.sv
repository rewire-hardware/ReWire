module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  logic [23:0] main_first_out;
  logic [23:0] zi0;
  logic [7:0] zi1;
  logic [15:0] zi2;
  logic [24:0] zi3;
  logic [7:0] zi4;
  logic [15:0] zi5;
  logic [15:0] slice_in;
  logic [23:0] zi6;
  logic [7:0] zi7;
  logic [15:0] zi8;
  logic [24:0] zi9;
  logic [7:0] zi10;
  logic [15:0] zi12;
  logic [24:0] main_sig_out;
  logic [24:0] main_sig_outR1;
  logic [24:0] zres;
  Main_first  inst (__st0, main_first_out);
  assign zi0 = main_first_out;
  assign zi1 = zi0[23:16];
  assign zi2 = zi0[15:0];
  assign zi3 = {1'h0, zi1, zi2};
  assign zi4 = zi3[23:16];
  assign zi5 = zi3[15:0];
  assign slice_in = zi5 >> {8'h80{1'h0}};
  assign zi6 = {slice_in[7:0], zi5};
  assign zi7 = zi6[23:16];
  assign zi8 = zi6[15:0];
  assign zi9 = {1'h0, zi7, zi8};
  assign zi10 = zi9[23:16];
  assign zi12 = {zi10, zi4 + zi10};
  Main_sig  instR1 (zi12, main_sig_out);
  Main_sig  instR2 (__st0, main_sig_outR1);
  assign zres = (__in0 == 1'h0) ? main_sig_out : main_sig_outR1;
  assign __st0_next = zres[15:0];
  assign __out0 = zres[23:16];
  initial __st0 = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module Main_sig (input logic [15:0] arg0,
  output logic [24:0] res);
  logic [23:0] main_first_out;
  logic [23:0] zt0;
  logic [7:0] v;
  logic [15:0] s0;
  logic [24:0] zt1;
  logic [7:0] vR1;
  logic [15:0] s0R1;
  Main_first  inst (arg0, main_first_out);
  assign zt0 = main_first_out;
  assign v = zt0[23:16];
  assign s0 = zt0[15:0];
  assign zt1 = {1'h0, v, s0};
  assign vR1 = zt1[23:16];
  assign s0R1 = zt1[15:0];
  assign res = {1'h1, vR1, s0R1};
endmodule

module Main_first (input logic [15:0] arg0,
  output logic [23:0] res);
  assign res = {arg0[15:8], arg0};
endmodule