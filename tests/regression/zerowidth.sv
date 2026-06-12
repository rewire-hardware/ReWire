module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] zin;
  logic [18:0] zi0;
  logic [7:0] zi1;
  logic [15:0] zi2;
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [18:0] zi5;
  logic [7:0] zi6;
  logic [7:0] zi7;
  logic [7:0] zi8;
  logic [18:0] zi9;
  logic [26:0] zi10;
  logic [7:0] zi11;
  logic [7:0] zi12;
  logic [18:0] zi13;
  logic [26:0] zi14;
  logic [7:0] zi15;
  logic [7:0] zi16;
  logic [18:0] zres;
  assign zin = __st0;
  assign zi0 = {11'h0, zin};
  assign zi1 = zi0[7:0];
  assign zi2 = {zi1, zi1};
  assign zi3 = zi2[15:8];
  assign zi4 = zi2[7:0];
  assign zi5 = {3'h1, zi3, zi4};
  assign zi6 = zi5[15:8];
  assign zi7 = zi5[7:0];
  assign zi8 = zi6 + 8'h1;
  assign zi9 = {11'h200, zi8};
  assign zi10 = {zi6, zi9};
  assign zi11 = zi10[26:19];
  assign zi12 = zi10[7:0];
  assign zi13 = {11'h200, zi12};
  assign zi14 = {zi11, zi13};
  assign zi15 = zi14[26:19];
  assign zi16 = zi14[7:0];
  assign zres = {3'h4, zi15, zi16};
  assign __st0_next = zres[7:0];
  assign __out0 = zres[15:8];
  initial __st0 = 8'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule