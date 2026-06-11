module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [2:0] zin;
  logic [1:0] zi0;
  logic [0:0] zi1;
  logic [2:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  logic [3:0] zi6;
  logic [4:0] zi7;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [3:0] zres;
  assign zin = {__resumption_tag, __st0, __in0};
  assign zi0 = zin[2:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[2];
  assign zi4 = zi2[1];
  assign zi5 = zi2[0];
  assign zi6 = {3'h0, zi4};
  assign zi7 = {zi4, zi6};
  assign zi8 = zi7[4];
  assign zi9 = zi7[0];
  assign zres = {1'h1, zi8, zi8, zi9};
  assign __resumption_tag_next = zres[1];
  assign __st0_next = zres[0];
  assign __out0 = zres[2];
  initial {__resumption_tag, __st0} = 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule