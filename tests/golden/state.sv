module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zi6;
  logic [0:0] zi7;
  logic [1:0] zi8;
  logic [0:0] zi9;
  logic [0:0] zi10;
  logic [2:0] zi11;
  logic [0:0] zi12;
  logic [0:0] zi13;
  logic [2:0] zres;
  assign zi6 = __in0 ^ __st0;
  assign zi7 = zi6;
  assign zi8 = {__st0, zi7};
  assign zi9 = zi8[1];
  assign zi10 = zi8[0];
  assign zi11 = {1'h0, zi9, zi10};
  assign zi12 = zi11[1];
  assign zi13 = zi11[0];
  assign zres = {1'h1, zi12, zi13};
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