module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [15:0] zll_main_go3_out;
  logic [15:0] zll_main_go3_outR1;
  logic [15:0] zres;
  ZLL_Main_go3  inst (__st0 + 8'h1, zll_main_go3_out);
  ZLL_Main_go3  instR1 ((__st0 << 8'h1) | {7'h0, __st0[7]}, zll_main_go3_outR1);
  assign zres = (__in0 == 1'h0) ? zll_main_go3_out : zll_main_go3_outR1;
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

module ZLL_Main_go3 (input logic [7:0] arg0,
  output logic [15:0] res);
  assign res = {arg0, arg0};
endmodule