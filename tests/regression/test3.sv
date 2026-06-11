module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [8:0] zin;
  logic [7:0] zi0;
  logic [0:0] zi1;
  logic [8:0] zi2;
  logic [0:0] zi3;
  logic [7:0] zi4;
  logic [8:0] zi5;
  logic [16:0] zll_main_go8_out;
  logic [8:0] zi6;
  logic [16:0] zll_main_go8_outR1;
  logic [16:0] zres;
  assign zin = {__st0, __in0};
  assign zi0 = zin[8:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[8];
  assign zi4 = zi2[7:0];
  assign zi5 = {zi4, zi3};
  ZLL_Main_go8  inst (zi5[8:1], zll_main_go8_out);
  assign zi6 = {zi4, zi3};
  ZLL_Main_go8  instR1 (zi6[8:1], zll_main_go8_outR1);
  assign zres = (zi5[0] == 1'h1) ? zll_main_go8_out : zll_main_go8_outR1;
  assign __st0_next = zres[7:0];
  assign __out0 = zres[15:8];
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_go8 (input logic [7:0] arg0,
  output logic [16:0] res);
  logic [15:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [16:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  assign zi0 = {arg0, arg0};
  assign zi1 = zi0[15:8];
  assign zi2 = zi0[7:0];
  assign zi3 = {1'h0, zi1, zi2};
  assign zi4 = zi3[15:8];
  assign zi5 = zi3[7:0];
  assign res = {1'h1, zi4, zi5};
endmodule