module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [17:0] zll_main_go23_out;
  logic [17:0] zi0;
  logic [7:0] zi1;
  logic [0:0] zi3;
  logic [17:0] zll_main_go29_out;
  logic [17:0] zll_main_go21_out;
  logic [17:0] zll_main_go23_outR1;
  logic [17:0] zi4;
  logic [7:0] zi5;
  logic [17:0] zll_main_go29_outR1;
  logic [17:0] zll_main_go21_outR1;
  logic [17:0] zres;
  ZLL_Main_go23  inst ({__st0, __st0}, zll_main_go23_out);
  assign zi0 = zll_main_go23_out;
  assign zi1 = zi0[15:8];
  assign zi3 = zi0[15];
  ZLL_Main_go29  instR1 ((zi1 << 8'h1) | {7'h0, zi3}, zll_main_go29_out);
  ZLL_Main_go21  instR2 (zll_main_go29_out, zll_main_go21_out);
  ZLL_Main_go23  instR3 ({__st0, __st0}, zll_main_go23_outR1);
  assign zi4 = zll_main_go23_outR1;
  assign zi5 = zi4[15:8];
  ZLL_Main_go29  instR4 (zi5 + 8'h1, zll_main_go29_outR1);
  ZLL_Main_go21  instR5 (zll_main_go29_outR1, zll_main_go21_outR1);
  assign zres = (__in0 == 1'h1) ? zll_main_go21_out : zll_main_go21_outR1;
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

module ZLL_Main_go29 (input logic [7:0] arg0,
  output logic [17:0] res);
  assign res = {10'h100, arg0};
endmodule

module ZLL_Main_go23 (input logic [15:0] arg0,
  output logic [17:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  assign zi0 = arg0[15:8];
  assign zi1 = arg0[7:0];
  assign res = {2'h0, zi0, zi1};
endmodule

module ZLL_Main_go21 (input logic [17:0] arg0,
  output logic [17:0] res);
  logic [7:0] zi0;
  logic [17:0] zll_main_go23_out;
  logic [17:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  assign zi0 = arg0[7:0];
  ZLL_Main_go23  inst ({zi0, zi0}, zll_main_go23_out);
  assign zi1 = zll_main_go23_out;
  assign zi2 = zi1[15:8];
  assign zi3 = zi1[7:0];
  assign res = {2'h2, zi2, zi3};
endmodule