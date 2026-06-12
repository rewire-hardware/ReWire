module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] zll_main_sig4_in;
  logic [17:0] zll_main_sig10_in;
  logic [16:0] zll_main_sig9_in;
  logic [16:0] zll_main_sig8_in;
  logic [15:0] main_incr_in;
  logic [15:0] main_first_in;
  logic [23:0] main_first_out;
  logic [23:0] zll_main_incr16_in;
  logic [25:0] zll_main_incr16_out;
  logic [25:0] zll_main_incr10_in;
  logic [25:0] zll_main_incr4_in;
  logic [23:0] zll_main_incr_in;
  logic [15:0] main_second_in;
  logic [31:0] zll_main_second5_in;
  logic [31:0] zll_main_second6_in;
  logic [15:0] zll_main_second2_in;
  logic [15:0] resize_in;
  logic [255:0] binop_in;
  logic [127:0] resize_inR1;
  logic [23:0] zll_main_incr16_inR1;
  logic [25:0] zll_main_incr16_outR1;
  logic [33:0] zll_main_incr9_in;
  logic [33:0] zll_main_incr13_in;
  logic [31:0] zll_main_incr3_in;
  logic [15:0] binop_inR1;
  logic [15:0] zll_main_incr20_in;
  logic [25:0] zll_main_incr15_in;
  logic [25:0] zll_main_begin6_in;
  logic [25:0] zll_main_begin6_out;
  logic [16:0] zll_main_begin6_inR1;
  logic [25:0] zll_main_begin6_outR1;
  logic [1:0] __padding;
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  assign zll_main_sig4_in = {__in0, __st0};
  assign zll_main_sig10_in = {zll_main_sig4_in[16], zll_main_sig4_in[16], zll_main_sig4_in[15:0]};
  assign zll_main_sig9_in = {zll_main_sig10_in[17], zll_main_sig10_in[15:0]};
  assign zll_main_sig8_in = {zll_main_sig9_in[15:0], zll_main_sig9_in[16]};
  assign main_incr_in = zll_main_sig8_in[16:1];
  assign main_first_in = main_incr_in[15:0];
  Main_first  inst (main_first_in[15:0], main_first_out);
  assign zll_main_incr16_in = main_first_out;
  ZLL_Main_incr16  instR1 (zll_main_incr16_in[23:0], zll_main_incr16_out);
  assign zll_main_incr10_in = zll_main_incr16_out;
  assign zll_main_incr4_in = zll_main_incr10_in[25:0];
  assign zll_main_incr_in = {zll_main_incr4_in[23:16], zll_main_incr4_in[15:0]};
  assign main_second_in = zll_main_incr_in[15:0];
  assign zll_main_second5_in = {main_second_in[15:0], main_second_in[15:0]};
  assign zll_main_second6_in = zll_main_second5_in[31:0];
  assign zll_main_second2_in = zll_main_second6_in[31:16];
  assign resize_in = zll_main_second2_in[15:0];
  assign binop_in = {128'(resize_in[15:0]), {8'h80{1'h0}}};
  assign resize_inR1 = binop_in[255:128] >> binop_in[127:0];
  assign zll_main_incr16_inR1 = {resize_inR1[7:0], zll_main_second6_in[15:0]};
  ZLL_Main_incr16  instR2 (zll_main_incr16_inR1[23:0], zll_main_incr16_outR1);
  assign zll_main_incr9_in = {zll_main_incr_in[23:16], zll_main_incr16_outR1};
  assign zll_main_incr13_in = {zll_main_incr9_in[33:26], zll_main_incr9_in[25:0]};
  assign zll_main_incr3_in = {zll_main_incr13_in[33:26], zll_main_incr13_in[23:16], zll_main_incr13_in[15:0]};
  assign binop_inR1 = {zll_main_incr3_in[31:24], zll_main_incr3_in[23:16]};
  assign zll_main_incr20_in = {zll_main_incr3_in[23:16], binop_inR1[15:8] + binop_inR1[7:0]};
  assign zll_main_incr15_in = {10'h100, zll_main_incr20_in[15:0]};
  assign zll_main_begin6_in = zll_main_incr15_in[25:0];
  ZLL_Main_begin6  instR3 (zll_main_begin6_in[15:0], zll_main_begin6_out);
  assign zll_main_begin6_inR1 = {zll_main_sig10_in[15:0], zll_main_sig10_in[16]};
  ZLL_Main_begin6  instR4 (zll_main_begin6_inR1[16:1], zll_main_begin6_outR1);
  assign {__padding, __out0, __st0_next} = (zll_main_begin6_inR1[0] == 1'h1) ? zll_main_begin6_outR1 : zll_main_begin6_out;
  initial __st0 = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_begin6 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [15:0] main_sig_in;
  logic [15:0] main_first_in;
  logic [23:0] main_first_out;
  logic [23:0] zll_main_incr16_in;
  logic [25:0] zll_main_incr16_out;
  logic [25:0] zll_main_sig1_in;
  logic [25:0] zll_main_sig5_in;
  logic [23:0] zll_main_sig2_in;
  assign main_sig_in = arg0;
  assign main_first_in = main_sig_in[15:0];
  Main_first  inst (main_first_in[15:0], main_first_out);
  assign zll_main_incr16_in = main_first_out;
  ZLL_Main_incr16  instR1 (zll_main_incr16_in[23:0], zll_main_incr16_out);
  assign zll_main_sig1_in = zll_main_incr16_out;
  assign zll_main_sig5_in = zll_main_sig1_in[25:0];
  assign zll_main_sig2_in = {zll_main_sig5_in[23:16], zll_main_sig5_in[15:0]};
  assign res = {2'h2, zll_main_sig2_in[23:16], zll_main_sig2_in[15:0]};
endmodule

module ZLL_Main_incr16 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [23:0] zll_main_incr19_in;
  assign zll_main_incr19_in = arg0;
  assign res = {2'h0, zll_main_incr19_in[23:16], zll_main_incr19_in[15:0]};
endmodule

module Main_first (input logic [15:0] arg0,
  output logic [23:0] res);
  logic [31:0] zll_main_first2_in;
  logic [31:0] zll_main_first1_in;
  logic [15:0] zll_main_first3_in;
  logic [15:0] id_in;
  assign zll_main_first2_in = {arg0, arg0};
  assign zll_main_first1_in = zll_main_first2_in[31:0];
  assign zll_main_first3_in = zll_main_first1_in[31:16];
  assign id_in = zll_main_first3_in[15:0];
  assign res = {id_in[15:8], zll_main_first1_in[15:0]};
endmodule