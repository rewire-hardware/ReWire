module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zi0;
  logic [0:0] zll_l_main_dev138_out;
  logic [0:0] zll_l_main_dev138_outR1;
  logic [0:0] zll_l_main_dev138_outR2;
  logic [0:0] zll_l_main_dev138_outR3;
  logic [0:0] zll_l_main_dev138_outR4;
  logic [0:0] zll_l_main_dev138_outR5;
  logic [0:0] zll_l_main_dev138_outR6;
  logic [0:0] zll_l_main_dev138_outR7;
  logic [7:0] zi1;
  logic [0:0] zll_l_main_dev13_out;
  logic [0:0] zll_l_main_dev13_outR1;
  logic [0:0] zll_l_main_dev13_outR2;
  logic [0:0] zll_l_main_dev13_outR3;
  logic [0:0] zll_l_main_dev13_outR4;
  logic [0:0] zll_l_main_dev13_outR5;
  logic [0:0] zll_l_main_dev13_outR6;
  logic [0:0] zll_l_main_dev13_outR7;
  logic [7:0] zi5;
  logic [0:0] zll_l_main_dev136_out;
  logic [0:0] zll_l_main_dev136_outR1;
  logic [0:0] zll_l_main_dev136_outR2;
  logic [0:0] zll_l_main_dev136_outR3;
  logic [0:0] zll_l_main_dev136_outR4;
  logic [0:0] zll_l_main_dev136_outR5;
  logic [0:0] zll_l_main_dev136_outR6;
  logic [0:0] zll_l_main_dev136_outR7;
  logic [7:0] zi9;
  logic [0:0] zll_l_main_dev124_out;
  logic [0:0] zll_l_main_dev124_outR1;
  logic [0:0] zll_l_main_dev124_outR2;
  logic [0:0] zll_l_main_dev124_outR3;
  logic [0:0] zll_l_main_dev124_outR4;
  logic [0:0] zll_l_main_dev124_outR5;
  logic [0:0] zll_l_main_dev124_outR6;
  logic [0:0] zll_l_main_dev124_outR7;
  logic [7:0] zi13;
  logic [31:0] zi14;
  logic [31:0] zres;
  assign zi0 = __in0 * 8'h2;
  ZLL_L_Main_dev138  inst (zi0, __in0, 3'h2, 3'h0, zll_l_main_dev138_out);
  ZLL_L_Main_dev138  instR1 (zi0, __in0, 3'h2, 3'h1, zll_l_main_dev138_outR1);
  ZLL_L_Main_dev138  instR2 (zi0, __in0, 3'h2, 3'h2, zll_l_main_dev138_outR2);
  ZLL_L_Main_dev138  instR3 (zi0, __in0, 3'h2, 3'h3, zll_l_main_dev138_outR3);
  ZLL_L_Main_dev138  instR4 (zi0, __in0, 3'h2, 3'h4, zll_l_main_dev138_outR4);
  ZLL_L_Main_dev138  instR5 (zi0, __in0, 3'h2, 3'h5, zll_l_main_dev138_outR5);
  ZLL_L_Main_dev138  instR6 (zi0, __in0, 3'h2, 3'h6, zll_l_main_dev138_outR6);
  ZLL_L_Main_dev138  instR7 (zi0, __in0, 3'h2, 3'h7, zll_l_main_dev138_outR7);
  assign zi1 = {zll_l_main_dev138_out, zll_l_main_dev138_outR1, zll_l_main_dev138_outR2, zll_l_main_dev138_outR3, zll_l_main_dev138_outR4, zll_l_main_dev138_outR5, zll_l_main_dev138_outR6, zll_l_main_dev138_outR7};
  ZLL_L_Main_dev13  instR8 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h0, zll_l_main_dev13_out);
  ZLL_L_Main_dev13  instR9 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h1, zll_l_main_dev13_outR1);
  ZLL_L_Main_dev13  instR10 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h2, zll_l_main_dev13_outR2);
  ZLL_L_Main_dev13  instR11 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h3, zll_l_main_dev13_outR3);
  ZLL_L_Main_dev13  instR12 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h4, zll_l_main_dev13_outR4);
  ZLL_L_Main_dev13  instR13 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h5, zll_l_main_dev13_outR5);
  ZLL_L_Main_dev13  instR14 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h6, zll_l_main_dev13_outR6);
  ZLL_L_Main_dev13  instR15 (zi0, __in0, 3'h2, 3'h4, 3'h1, 3'h7, zll_l_main_dev13_outR7);
  assign zi5 = {zll_l_main_dev13_out, zll_l_main_dev13_outR1, zll_l_main_dev13_outR2, zll_l_main_dev13_outR3, zll_l_main_dev13_outR4, zll_l_main_dev13_outR5, zll_l_main_dev13_outR6, zll_l_main_dev13_outR7};
  ZLL_L_Main_dev136  instR16 (3'h2, zi0, __in0, 3'h4, 3'h0, zll_l_main_dev136_out);
  ZLL_L_Main_dev136  instR17 (3'h2, zi0, __in0, 3'h4, 3'h1, zll_l_main_dev136_outR1);
  ZLL_L_Main_dev136  instR18 (3'h2, zi0, __in0, 3'h4, 3'h2, zll_l_main_dev136_outR2);
  ZLL_L_Main_dev136  instR19 (3'h2, zi0, __in0, 3'h4, 3'h3, zll_l_main_dev136_outR3);
  ZLL_L_Main_dev136  instR20 (3'h2, zi0, __in0, 3'h4, 3'h4, zll_l_main_dev136_outR4);
  ZLL_L_Main_dev136  instR21 (3'h2, zi0, __in0, 3'h4, 3'h5, zll_l_main_dev136_outR5);
  ZLL_L_Main_dev136  instR22 (3'h2, zi0, __in0, 3'h4, 3'h6, zll_l_main_dev136_outR6);
  ZLL_L_Main_dev136  instR23 (3'h2, zi0, __in0, 3'h4, 3'h7, zll_l_main_dev136_outR7);
  assign zi9 = {zll_l_main_dev136_out, zll_l_main_dev136_outR1, zll_l_main_dev136_outR2, zll_l_main_dev136_outR3, zll_l_main_dev136_outR4, zll_l_main_dev136_outR5, zll_l_main_dev136_outR6, zll_l_main_dev136_outR7};
  ZLL_L_Main_dev124  instR24 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h0, zll_l_main_dev124_out);
  ZLL_L_Main_dev124  instR25 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h1, zll_l_main_dev124_outR1);
  ZLL_L_Main_dev124  instR26 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h2, zll_l_main_dev124_outR2);
  ZLL_L_Main_dev124  instR27 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h3, zll_l_main_dev124_outR3);
  ZLL_L_Main_dev124  instR28 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h4, zll_l_main_dev124_outR4);
  ZLL_L_Main_dev124  instR29 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h5, zll_l_main_dev124_outR5);
  ZLL_L_Main_dev124  instR30 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h6, zll_l_main_dev124_outR6);
  ZLL_L_Main_dev124  instR31 (3'h1, zi0, __in0, 3'h4, 3'h2, 3'h7, zll_l_main_dev124_outR7);
  assign zi13 = {zll_l_main_dev124_out, zll_l_main_dev124_outR1, zll_l_main_dev124_outR2, zll_l_main_dev124_outR3, zll_l_main_dev124_outR4, zll_l_main_dev124_outR5, zll_l_main_dev124_outR6, zll_l_main_dev124_outR7};
  assign zi14 = {zi1, zi5, zi9, zi13};
  assign zres = zi14;
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_L_Main_dev158 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_L_Main_dev155 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module ZLL_L_Main_dev138 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  output logic [0:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zi0;
  logic [0:0] zll_l_main_dev158_out;
  logic [0:0] zi1;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [7:0] slice_inR3;
  logic [0:0] zll_l_main_dev155_out;
  logic [127:0] slice_inR4;
  logic [7:0] slice_inR5;
  logic [0:0] zll_l_main_dev155_outR1;
  logic [0:0] zll_l_main_dev155_outR2;
  assign slice_in = {{7'h7d{1'h0}}, arg3};
  assign zt0 = slice_in[0];
  assign zi0 = zt0;
  ZLL_L_Main_dev158  inst (zi0, zll_l_main_dev158_out);
  assign zi1 = zll_l_main_dev158_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg3} - 128'h1) : (({{7'h7d{1'h0}}, arg3} - 128'h1) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR3 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  instR1 (slice_inR3[0], zll_l_main_dev155_out);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR5 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR4[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  instR2 (slice_inR5[0], zll_l_main_dev155_outR1);
  ZLL_L_Main_dev155  instR3 ((zi1 == 1'h0) ? zll_l_main_dev155_out : zll_l_main_dev155_outR1, zll_l_main_dev155_outR2);
  assign res = zll_l_main_dev155_outR2;
endmodule

module ZLL_L_Main_dev136 (input logic [2:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [7:0] slice_inR2;
  logic [0:0] zll_l_main_dev155_out;
  logic [127:0] slice_inR3;
  logic [7:0] slice_inR4;
  logic [0:0] zll_l_main_dev155_outR1;
  logic [0:0] zll_l_main_dev155_outR2;
  assign zi0 = {{7'h7d{1'h0}}, arg4} < {{7'h7d{1'h0}}, arg3};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR2 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR1[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  inst (slice_inR2[0], zll_l_main_dev155_out);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR4 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  instR1 (slice_inR4[0], zll_l_main_dev155_outR1);
  ZLL_L_Main_dev155  instR2 ((zi0 == 1'h0) ? zll_l_main_dev155_out : zll_l_main_dev155_outR1, zll_l_main_dev155_outR2);
  assign res = zll_l_main_dev155_outR2;
endmodule

module ZLL_L_Main_dev124 (input logic [2:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [7:0] slice_inR3;
  logic [0:0] zll_l_main_dev155_out;
  logic [127:0] slice_inR4;
  logic [127:0] slice_inR5;
  logic [7:0] slice_inR6;
  logic [0:0] zll_l_main_dev155_outR1;
  logic [0:0] zll_l_main_dev155_outR2;
  assign zi0 = {{7'h7d{1'h0}}, arg5} < {{7'h7d{1'h0}}, arg3};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR3 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  inst (slice_inR3[0], zll_l_main_dev155_out);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR6 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR5[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  instR1 (slice_inR6[0], zll_l_main_dev155_outR1);
  ZLL_L_Main_dev155  instR2 ((zi0 == 1'h0) ? zll_l_main_dev155_out : zll_l_main_dev155_outR1, zll_l_main_dev155_outR2);
  assign res = zll_l_main_dev155_outR2;
endmodule

module ZLL_L_Main_dev13 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zi0;
  logic [0:0] zll_l_main_dev158_out;
  logic [0:0] zi1;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [127:0] slice_inR3;
  logic [7:0] slice_inR4;
  logic [0:0] zll_l_main_dev155_out;
  logic [127:0] slice_inR5;
  logic [127:0] slice_inR6;
  logic [7:0] slice_inR7;
  logic [0:0] zll_l_main_dev155_outR1;
  logic [0:0] zll_l_main_dev155_outR2;
  assign slice_in = {{7'h7d{1'h0}}, arg5};
  assign zt0 = slice_in[0];
  assign zi0 = zt0;
  ZLL_L_Main_dev158  inst (zi0, zll_l_main_dev158_out);
  assign zi1 = zll_l_main_dev158_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg3} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) : (({{7'h7d{1'h0}}, arg3} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) % 128'h8);
  assign slice_inR4 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  instR1 (slice_inR4[0], zll_l_main_dev155_out);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR6 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg3} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) : (({{7'h7d{1'h0}}, arg3} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) % 128'h8);
  assign slice_inR7 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR6[2:0]}) - 128'h1) * 128'h1);
  ZLL_L_Main_dev155  instR2 (slice_inR7[0], zll_l_main_dev155_outR1);
  ZLL_L_Main_dev155  instR3 ((zi1 == 1'h0) ? zll_l_main_dev155_out : zll_l_main_dev155_outR1, zll_l_main_dev155_outR2);
  assign res = zll_l_main_dev155_outR2;
endmodule