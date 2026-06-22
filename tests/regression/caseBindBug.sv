module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zi3;
  logic [3:0] zll_main_go24_out;
  logic [3:0] zll_main_go34_out;
  logic [3:0] zll_main_go24_outR1;
  logic [3:0] zll_main_go34_outR1;
  logic [3:0] zres;
  assign zi3 = (__st0 == 1'h0) ? 1'h1 : 1'h0;
  ZLL_Main_go24  inst ({zi3, zi3}, zll_main_go24_out);
  ZLL_Main_go34  instR1 (zll_main_go24_out, zll_main_go34_out);
  ZLL_Main_go24  instR2 ({__st0, __st0}, zll_main_go24_outR1);
  ZLL_Main_go34  instR3 (zll_main_go24_outR1, zll_main_go34_outR1);
  assign zres = (__in0 == 1'h1) ? zll_main_go34_out : zll_main_go34_outR1;
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

module ZLL_Main_go34 (input logic [3:0] arg0,
  output logic [3:0] res);
  logic [0:0] zi0;
  assign zi0 = arg0[1];
  assign res = {2'h2, zi0, zi0};
endmodule

module ZLL_Main_go24 (input logic [1:0] arg0,
  output logic [3:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  assign zi0 = arg0[1];
  assign zi1 = arg0[0];
  assign res = {2'h0, zi0, zi1};
endmodule