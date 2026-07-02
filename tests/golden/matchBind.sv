module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [17:0] zll_main_loop5_out;
  logic [17:0] zll_main_reset1_out;
  logic [17:0] zll_main_loop5_outR1;
  logic [17:0] zres;
  ZLL_Main_loop5  inst (__in0, __st0, zll_main_loop5_out);
  ZLL_Main_reset1  instR1 (__in0, zll_main_reset1_out);
  ZLL_Main_loop5  instR2 (__in0, __st0, zll_main_loop5_outR1);
  assign zres = (__resumption_tag == 2'h1) ? zll_main_loop5_out : ((__resumption_tag == 2'h2) ? zll_main_reset1_out : zll_main_loop5_outR1);
  assign __resumption_tag_next = zres[9:8];
  assign __st0_next = zres[7:0];
  assign __out0 = zres[17:10];
  initial {__resumption_tag, __st0} = 10'h200;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 10'h200;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_Main_loop5 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [17:0] res);
  logic [17:0] zll_main_reset1_out;
  ZLL_Main_reset1  inst (arg1, zll_main_reset1_out);
  assign res = zll_main_reset1_out;
endmodule

module ZLL_Main_reset1 (input logic [7:0] arg0,
  output logic [17:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = arg0[0];
  assign zi1 = zi0;
  assign zi2 = (zi1 == 1'h1) ? 1'h0 : 1'h1;
  assign res = (zi2 == 1'h0) ? {10'h9, arg0} : {10'h8, arg0};
endmodule