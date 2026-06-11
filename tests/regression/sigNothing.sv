module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [0:0] __out0,
  output logic [7:0] __out1);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [15:0] zin;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [15:0] zi2;
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [16:0] zres;
  assign zin = {__st0, __in0};
  assign zi0 = zin[15:8];
  assign zi1 = zin[7:0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[15:8];
  assign zi4 = zi2[7:0];
  assign zres = {9'h0, zi4};
  assign __st0_next = zres[7:0];
  assign __out0 = zres[16];
  assign __out1 = zres[15:8];
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule