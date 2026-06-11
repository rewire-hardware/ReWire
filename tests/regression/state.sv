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
  logic [2:0] zi6;
  logic [0:0] zi7;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [1:0] zi10;
  logic [0:0] zi11;
  logic [0:0] zi12;
  logic [0:0] zi13;
  logic [0:0] zi14;
  logic [1:0] zi15;
  logic [0:0] zi16;
  logic [0:0] zi17;
  logic [1:0] zi18;
  logic [0:0] zi19;
  logic [0:0] zi20;
  logic [2:0] zi21;
  logic [0:0] zi22;
  logic [0:0] zi23;
  logic [2:0] zres;
  assign zin = {__st0, __in0};
  assign zi0 = zin[1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[1];
  assign zi4 = zi2[0];
  assign zi5 = {zi4, zi4};
  assign zi6 = {zi3, zi5};
  assign zi7 = zi6[2];
  assign zi8 = zi6[1];
  assign zi9 = zi6[0];
  assign zi10 = {zi7, zi8};
  assign zi11 = zi10[1];
  assign zi12 = zi10[0];
  assign zi13 = zi11 ^ zi12;
  assign zi14 = zi13;
  assign zi15 = {zi8, zi14};
  assign zi16 = zi15[1];
  assign zi17 = zi15[0];
  assign zi18 = {zi16, zi17};
  assign zi19 = zi18[1];
  assign zi20 = zi18[0];
  assign zi21 = {1'h0, zi19, zi20};
  assign zi22 = zi21[1];
  assign zi23 = zi21[0];
  assign zres = {1'h1, zi22, zi23};
  assign __st0_next = zres[0];
  assign __out0 = zres[1];
  initial __st0 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule