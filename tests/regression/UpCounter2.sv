module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [8:0] zin;
  logic [7:0] zi0;
  logic [0:0] zi1;
  logic [8:0] zi2;
  logic [0:0] zi3;
  logic [7:0] zi4;
  logic [8:0] zi5;
  logic [7:0] zi6;
  logic [17:0] zll_main_go28_out;
  logic [17:0] zi7;
  logic [7:0] zi8;
  logic [7:0] zi9;
  logic [0:0] zi10;
  logic [17:0] zll_main_go27_out;
  logic [17:0] zll_main_go33_out;
  logic [8:0] zi11;
  logic [7:0] zi12;
  logic [17:0] zll_main_go28_outR1;
  logic [17:0] zi13;
  logic [7:0] zi14;
  logic [7:0] zi15;
  logic [17:0] zll_main_go27_outR1;
  logic [17:0] zll_main_go33_outR1;
  logic [17:0] zres;
  assign zin = {__st0, __in0};
  assign zi0 = zin[8:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[8];
  assign zi4 = zi2[7:0];
  assign zi5 = {zi4, zi3};
  assign zi6 = zi5[8:1];
  ZLL_Main_go28  inst ({zi6, zi6}, zll_main_go28_out);
  assign zi7 = zll_main_go28_out;
  assign zi8 = zi7[15:8];
  assign zi9 = zi7[7:0];
  assign zi10 = zi8[7];
  ZLL_Main_go27  instR1 ((zi8 << 8'h1) | {7'h0, zi10}, zll_main_go27_out);
  ZLL_Main_go33  instR2 (zll_main_go27_out, zll_main_go33_out);
  assign zi11 = {zi4, zi3};
  assign zi12 = zi11[8:1];
  ZLL_Main_go28  instR3 ({zi12, zi12}, zll_main_go28_outR1);
  assign zi13 = zll_main_go28_outR1;
  assign zi14 = zi13[15:8];
  assign zi15 = zi13[7:0];
  ZLL_Main_go27  instR4 (zi14 + 8'h1, zll_main_go27_outR1);
  ZLL_Main_go33  instR5 (zll_main_go27_outR1, zll_main_go33_outR1);
  assign zres = (zi5[0] == 1'h1) ? zll_main_go33_out : zll_main_go33_outR1;
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

module ZLL_Main_go33 (input logic [17:0] arg0,
  output logic [17:0] res);
  logic [7:0] zi0;
  logic [17:0] zll_main_go28_out;
  logic [17:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  assign zi0 = arg0[7:0];
  ZLL_Main_go28  inst ({zi0, zi0}, zll_main_go28_out);
  assign zi1 = zll_main_go28_out;
  assign zi2 = zi1[15:8];
  assign zi3 = zi1[7:0];
  assign res = {2'h2, zi2, zi3};
endmodule

module ZLL_Main_go28 (input logic [15:0] arg0,
  output logic [17:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  assign zi0 = arg0[15:8];
  assign zi1 = arg0[7:0];
  assign res = {2'h0, zi0, zi1};
endmodule

module ZLL_Main_go27 (input logic [7:0] arg0,
  output logic [17:0] res);
  assign res = {10'h100, arg0};
endmodule