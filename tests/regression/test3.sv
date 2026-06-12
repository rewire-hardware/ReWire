module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [16:0] zll_main_go7_out;
  logic [16:0] zll_main_go7_outR1;
  logic [16:0] zres;
  ZLL_Main_go7  inst (__st0, zll_main_go7_out);
  ZLL_Main_go7  instR1 (__st0, zll_main_go7_outR1);
  assign zres = (__in0 == 1'h1) ? zll_main_go7_out : zll_main_go7_outR1;
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

module ZLL_Main_go7 (input logic [7:0] arg0,
  output logic [16:0] res);
  assign res = {1'h1, arg0, arg0};
endmodule