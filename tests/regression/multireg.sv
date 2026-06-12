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
  logic [46:0] zll_main_loop70_out;
  logic [46:0] zi34;
  logic [3:0] zi35;
  logic [15:0] zi37;
  logic [46:0] zll_main_loop70_outR1;
  logic [46:0] zi38;
  logic [3:0] zi39;
  logic [7:0] zi40;
  logic [27:0] zi42;
  logic [3:0] zi43;
  logic [7:0] zi44;
  logic [15:0] zi45;
  logic [46:0] zi46;
  logic [3:0] zi47;
  logic [7:0] zi48;
  logic [15:0] zi49;
  logic [46:0] zres;
  ZLL_Main_loop70  inst ({__in0, __st1, __st2}, zll_main_loop70_out);
  assign zi34 = zll_main_loop70_out;
  assign zi35 = zi34[27:24];
  assign zi37 = zi34[15:0];
  ZLL_Main_loop70  instR1 ({zi35, {4'h0, __st0} + __st1, zi37}, zll_main_loop70_outR1);
  assign zi38 = zll_main_loop70_outR1;
  assign zi39 = zi38[27:24];
  assign zi40 = zi38[23:16];
  assign zi42 = {zi39, zi40, {8'h0, __st1} + __st2};
  assign zi43 = zi42[27:24];
  assign zi44 = zi42[23:16];
  assign zi45 = zi42[15:0];
  assign zi46 = {19'h30000, zi43, zi44, zi45};
  assign zi47 = zi46[27:24];
  assign zi48 = zi46[23:16];
  assign zi49 = zi46[15:0];
  assign zres = {3'h4, {12'h0, __st0} + __st2, zi47, zi48, zi49};
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

module ZLL_Main_loop70 (input logic [27:0] arg0,
  output logic [46:0] res);
  logic [3:0] zi0;
  logic [7:0] zi1;
  logic [15:0] zi2;
  assign zi0 = arg0[27:24];
  assign zi1 = arg0[23:16];
  assign zi2 = arg0[15:0];
  assign res = {19'h30000, zi0, zi1, zi2};
endmodule