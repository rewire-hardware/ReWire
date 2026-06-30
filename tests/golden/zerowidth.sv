module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [18:0] zll_main_loop30_out;
  logic [18:0] zi8;
  logic [7:0] zi9;
  logic [18:0] zll_main_loop30_outR1;
  logic [18:0] zi10;
  logic [7:0] zi11;
  logic [18:0] zres;
  ZLL_Main_loop30  inst (__st0 + 8'h1, zll_main_loop30_out);
  assign zi8 = zll_main_loop30_out;
  assign zi9 = zi8[7:0];
  ZLL_Main_loop30  instR1 (zi9, zll_main_loop30_outR1);
  assign zi10 = zll_main_loop30_outR1;
  assign zi11 = zi10[7:0];
  assign zres = {3'h4, __st0, zi11};
  assign __st0_next = zres[7:0];
  assign __out0 = zres[15:8];
  initial __st0 = 8'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_loop30 (input logic [7:0] arg0,
  output logic [18:0] res);
  assign res = {11'h200, arg0};
endmodule