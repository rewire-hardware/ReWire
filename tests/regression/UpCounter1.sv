module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [7:0] __resumption_tag;
  logic [7:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [15:0] zin;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [24:0] zi3;
  logic [7:0] zi4;
  logic [15:0] zi5;
  logic [7:0] zi6;
  logic [7:0] zi7;
  logic [24:0] zi8;
  logic [7:0] zi9;
  logic [7:0] zi10;
  logic [24:0] zres;
  assign zin = {__resumption_tag, __st0};
  assign zi0 = zin[15:8];
  assign zi1 = zin[7:0];
  assign zi2 = zi0 + 8'h1;
  assign zi3 = {17'h100, zi2};
  assign zi4 = zi3[7:0];
  assign zi5 = {zi4, zi4};
  assign zi6 = zi5[15:8];
  assign zi7 = zi5[7:0];
  assign zi8 = {9'h0, zi6, zi7};
  assign zi9 = zi8[15:8];
  assign zi10 = zi8[7:0];
  assign zres = {1'h1, zi9, zi9, zi10};
  assign __resumption_tag_next = zres[15:8];
  assign __st0_next = zres[7:0];
  assign __out0 = zres[23:16];
  initial {__resumption_tag, __st0} = 16'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 16'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule