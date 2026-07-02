module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zi0;
  logic [2:0] zi1;
  logic [1:0] zi2;
  logic [0:0] zi3;
  logic [3:0] zi4;
  logic [0:0] zi7;
  logic [0:0] zi8;
  logic [3:0] zres;
  assign zi0 = __st0 ^ __in0;
  assign zi1 = {zi0, __st0, __st0};
  assign zi2 = zi1[2:1];
  assign zi3 = zi1[0];
  assign zi4 = {1'h0, zi2, zi3};
  assign zi7 = zi4[2];
  assign zi8 = zi4[1];
  assign zres = {2'h2, zi7, zi8};
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