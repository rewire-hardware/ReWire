module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  logic [16:0] zin;
  logic [15:0] zi0;
  logic [0:0] zi1;
  logic [16:0] zi2;
  logic [0:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [16:0] zi6;
  logic [7:0] zi7;
  logic [7:0] zi8;
  logic [23:0] main_loop_out;
  logic [16:0] zi9;
  logic [7:0] zi10;
  logic [7:0] zi11;
  logic [23:0] main_loop_outR1;
  logic [23:0] zres;
  assign zin = {__resumption_tag, __in0};
  assign zi0 = zin[16:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[16];
  assign zi4 = zi2[15:8];
  assign zi5 = zi2[7:0];
  assign zi6 = {zi4, zi5, zi3};
  assign zi7 = zi6[16:9];
  assign zi8 = zi6[8:1];
  Main_loop  inst (zi8, zi7, main_loop_out);
  assign zi9 = {zi4, zi5, zi3};
  assign zi10 = zi9[16:9];
  assign zi11 = zi9[8:1];
  Main_loop  instR1 (zi10, zi11 + zi10, main_loop_outR1);
  assign zres = (zi6[0] == 1'h1) ? main_loop_out : main_loop_outR1;
  assign __resumption_tag_next = zres[15:0];
  assign __out0 = zres[23:16];
  initial __resumption_tag = 16'h100;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h100;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_loop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [23:0] res);
  logic [15:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[15:8];
  assign zi2 = zi0[7:0];
  assign res = {zi1, zi2, zi1};
endmodule