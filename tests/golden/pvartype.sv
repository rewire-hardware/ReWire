module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zll_main_go1_out;
  logic [0:0] zi0;
  logic [0:0] zi2;
  logic [2:0] zi3;
  logic [0:0] zi4;
  logic [2:0] zres;
  ZLL_Main_go1  inst (__st0, zll_main_go1_out);
  assign zi0 = zll_main_go1_out;
  assign zi2 = (zi0 == 1'h1) ? __st0 : __st0;
  assign zi3 = {2'h0, zi2};
  assign zi4 = zi3[0];
  assign zres = {2'h2, zi4};
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

module ZLL_Main_go1 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule