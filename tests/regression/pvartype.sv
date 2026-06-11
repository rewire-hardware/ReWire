module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [1:0] zin;
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [1:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [1:0] zi5;
  logic [0:0] zi6;
  logic [0:0] zi7;
  logic [0:0] zll_main_go12_out;
  logic [1:0] zi8;
  logic [0:0] zll_main_go12_outR1;
  logic [0:0] zll_main_go12_outR2;
  logic [0:0] zi9;
  logic [1:0] zi10;
  logic [0:0] zll_main_go12_outR3;
  logic [0:0] zi11;
  logic [2:0] zi12;
  logic [0:0] zi13;
  logic [2:0] zres;
  assign zin = {__st0, __in0};
  assign zi0 = zin[1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[1];
  assign zi4 = zi2[0];
  assign zi5 = {zi4, zi4};
  assign zi6 = zi5[1];
  assign zi7 = zi5[0];
  ZLL_Main_go12  inst (zi6, zll_main_go12_out);
  assign zi8 = {zi7, zll_main_go12_out};
  ZLL_Main_go12  instR1 (zi8[1], zll_main_go12_outR1);
  ZLL_Main_go12  instR2 (zi6, zll_main_go12_outR2);
  assign zi9 = zll_main_go12_outR2;
  assign zi10 = {zi7, zi9};
  ZLL_Main_go12  instR3 (zi10[1], zll_main_go12_outR3);
  assign zi11 = (zi8[0] == 1'h1) ? zll_main_go12_outR1 : zll_main_go12_outR3;
  assign zi12 = {2'h0, zi11};
  assign zi13 = zi12[0];
  assign zres = {2'h2, zi13};
  assign __st0_next = zres[0];
  assign __out0 = zres[1];
  initial __st0 = 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_go12 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule