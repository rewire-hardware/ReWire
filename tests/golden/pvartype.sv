module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zi1;
  logic [2:0] zi2;
  logic [0:0] zi3;
  logic [2:0] zres;
  assign zi1 = (__st0 == 1'h0) ? __st0 : __st0;
  assign zi2 = {2'h0, zi1};
  assign zi3 = zi2[0];
  assign zres = {2'h2, zi3};
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