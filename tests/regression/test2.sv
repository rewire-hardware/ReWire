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
  logic [15:0] zi5;
  logic [7:0] zi6;
  logic [7:0] zi7;
  logic [17:0] zi8;
  logic [7:0] zi9;
  logic [7:0] zi10;
  logic [17:0] zi11;
  logic [25:0] zi12;
  logic [7:0] zi13;
  logic [7:0] zi14;
  logic [17:0] zres;
  assign zin = {__st0, __in0};
  assign zi0 = zin[8:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[8];
  assign zi4 = zi2[7:0];
  assign zi5 = {zi4, zi4};
  assign zi6 = zi5[15:8];
  assign zi7 = zi5[7:0];
  assign zi8 = {2'h0, zi6, zi7};
  assign zi9 = zi8[15:8];
  assign zi10 = zi8[7:0];
  assign zi11 = {10'h100, zi9};
  assign zi12 = {zi9, zi11};
  assign zi13 = zi12[25:18];
  assign zi14 = zi12[7:0];
  assign zres = {2'h2, zi13, zi14};
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