module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [3:0] __in0,
  output logic [15:0] __out0);
  logic [3:0] __st0;
  logic [3:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  logic [15:0] __st2;
  logic [15:0] __st2_next;
  logic [7:0] zi4;
  logic [15:0] zi5;
  logic [43:0] zres;
  assign zi4 = {4'h0, __st0} + __st1;
  assign zi5 = {8'h0, __st1} + __st2;
  assign zres = {{12'h0, __st0} + __st2, __in0, zi4, zi5};
  assign __st0_next = zres[27:24];
  assign __st1_next = zres[23:16];
  assign __st2_next = zres[15:0];
  assign __out0 = zres[43:28];
  initial {__st0, __st1, __st2} = 28'h30005;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1, __st2} <= 28'h30005;
    end else begin
      {__st0, __st1, __st2} <= {__st0_next, __st1_next, __st2_next};
    end
  end
endmodule