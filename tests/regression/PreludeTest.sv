module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [1:0] zin;
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [1:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [1:0] zi5;
  logic [0:0] zi6;
  logic [1:0] zi7;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [0:0] main_zookus_out;
  logic [0:0] main_zookus_outR1;
  logic [1:0] zres;
  assign zin = {__resumption_tag, __in0};
  assign zi0 = zin[1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[1];
  assign zi4 = zi2[0];
  assign zi5 = {zi4, 1'h1};
  assign zi6 = zi5[0];
  assign zi7 = {zi4, 1'h1};
  assign zi8 = zi7[0];
  assign zi9 = (zi5[1] == 1'h1) ? zi6 : 1'h0;
  Main_zookus  inst (zi9, main_zookus_out);
  Main_zookus  instR1 (main_zookus_out, main_zookus_outR1);
  assign zres = {main_zookus_outR1, zi9};
  assign __resumption_tag_next = zres[0];
  assign __out0 = zres[1];
  initial __resumption_tag = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_zookus (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {arg0, arg0};
  assign zi1 = zi0[1];
  assign zi2 = zi0[0];
  assign res = zi1;
endmodule