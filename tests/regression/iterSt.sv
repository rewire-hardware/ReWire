module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zi9;
  logic [2:0] zi10;
  logic [1:0] zi11;
  logic [0:0] zi12;
  logic [4:0] zi13;
  logic [0:0] zi16;
  logic [0:0] zi17;
  logic [4:0] zres;
  assign zi9 = __st0 ^ __in0;
  assign zi10 = {zi9, __st0, __st0};
  assign zi11 = zi10[2:1];
  assign zi12 = zi10[0];
  assign zi13 = {2'h0, zi11, zi12};
  assign zi16 = zi13[2];
  assign zi17 = zi13[1];
  assign zres = {3'h4, zi16, zi17};
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