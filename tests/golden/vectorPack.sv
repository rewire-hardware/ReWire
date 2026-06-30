module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [2:0] zll_main_compute239_out;
  logic [2:0] zll_main_compute179_out;
  logic [2:0] zi5;
  logic [2:0] zll_main_compute239_outR1;
  logic [0:0] zll_main_compute215_out;
  logic [0:0] zi6;
  logic [2:0] zll_main_compute239_outR2;
  logic [0:0] zll_main_compute215_outR1;
  logic [2:0] zll_main_compute229_out;
  logic [2:0] zi7;
  logic [7:0] zll_main_compute53_out;
  logic [7:0] zll_main_compute53_outR1;
  logic [7:0] zll_main_compute53_outR2;
  logic [7:0] zll_main_compute53_outR3;
  logic [7:0] zll_main_compute53_outR4;
  logic [7:0] zll_main_compute53_outR5;
  logic [7:0] zll_main_compute53_outR6;
  logic [7:0] zll_main_compute53_outR7;
  logic [2:0] zll_main_compute239_outR3;
  logic [2:0] zll_main_compute179_outR1;
  logic [2:0] zi11;
  logic [2:0] zll_main_compute239_outR4;
  logic [0:0] zll_main_compute215_outR2;
  logic [0:0] zi12;
  logic [2:0] zll_main_compute218_out;
  logic [2:0] zi14;
  logic [7:0] zll_main_compute39_out;
  logic [7:0] zll_main_compute39_outR1;
  logic [7:0] zll_main_compute39_outR2;
  logic [7:0] zll_main_compute39_outR3;
  logic [7:0] zll_main_compute39_outR4;
  logic [7:0] zll_main_compute39_outR5;
  logic [7:0] zll_main_compute39_outR6;
  logic [7:0] zll_main_compute39_outR7;
  logic [127:0] zi15;
  logic [63:0] zi16;
  logic [63:0] zi17;
  logic [2:0] zll_main_compute239_outR5;
  logic [2:0] zll_main_compute179_outR2;
  logic [2:0] zi21;
  logic [2:0] zll_main_compute239_outR6;
  logic [0:0] zll_main_compute215_outR3;
  logic [0:0] zi22;
  logic [2:0] zll_main_compute239_outR7;
  logic [0:0] zll_main_compute215_outR4;
  logic [2:0] zll_main_compute229_outR1;
  logic [2:0] zi23;
  logic [7:0] zll_main_compute83_out;
  logic [7:0] zll_main_compute83_outR1;
  logic [7:0] zll_main_compute83_outR2;
  logic [7:0] zll_main_compute83_outR3;
  logic [7:0] zll_main_compute83_outR4;
  logic [7:0] zll_main_compute83_outR5;
  logic [7:0] zll_main_compute83_outR6;
  logic [7:0] zll_main_compute83_outR7;
  logic [2:0] zll_main_compute239_outR8;
  logic [2:0] zll_main_compute179_outR3;
  logic [2:0] zi27;
  logic [2:0] zll_main_compute239_outR9;
  logic [0:0] zll_main_compute215_outR5;
  logic [0:0] zi28;
  logic [2:0] zll_main_compute218_outR1;
  logic [2:0] zi30;
  logic [7:0] zll_main_compute200_out;
  logic [7:0] zll_main_compute200_outR1;
  logic [7:0] zll_main_compute200_outR2;
  logic [7:0] zll_main_compute200_outR3;
  logic [7:0] zll_main_compute200_outR4;
  logic [7:0] zll_main_compute200_outR5;
  logic [7:0] zll_main_compute200_outR6;
  logic [7:0] zll_main_compute200_outR7;
  logic [127:0] zi31;
  logic [63:0] zi32;
  logic [63:0] zi33;
  logic [7:0] zll_main_compute114_out;
  logic [7:0] zll_main_compute114_outR1;
  logic [7:0] zll_main_compute114_outR2;
  logic [7:0] zll_main_compute114_outR3;
  logic [7:0] zll_main_compute114_outR4;
  logic [7:0] zll_main_compute114_outR5;
  logic [7:0] zll_main_compute114_outR6;
  logic [7:0] zll_main_compute114_outR7;
  logic [2:0] zll_main_compute239_outR10;
  logic [2:0] zll_main_compute179_outR4;
  logic [2:0] zi40;
  logic [2:0] zll_main_compute239_outR11;
  logic [0:0] zll_main_compute215_outR6;
  logic [0:0] zi41;
  logic [2:0] zll_main_compute218_outR2;
  logic [2:0] zi43;
  logic [7:0] zll_main_compute34_out;
  logic [7:0] zll_main_compute34_outR1;
  logic [7:0] zll_main_compute34_outR2;
  logic [7:0] zll_main_compute34_outR3;
  logic [7:0] zll_main_compute34_outR4;
  logic [7:0] zll_main_compute34_outR5;
  logic [7:0] zll_main_compute34_outR6;
  logic [7:0] zll_main_compute34_outR7;
  logic [127:0] zi44;
  logic [63:0] zi45;
  logic [63:0] zi46;
  logic [127:0] zi47;
  logic [127:0] zres;
  ZLL_Main_compute239  inst (__in0, zll_main_compute239_out);
  ZLL_Main_compute179  instR1 (zll_main_compute239_out, 3'h2, zll_main_compute179_out);
  assign zi5 = zll_main_compute179_out;
  ZLL_Main_compute239  instR2 (__in0, zll_main_compute239_outR1);
  ZLL_Main_compute215  instR3 (zll_main_compute239_outR1, zll_main_compute215_out);
  assign zi6 = zll_main_compute215_out;
  ZLL_Main_compute239  instR4 (__in0, zll_main_compute239_outR2);
  ZLL_Main_compute215  instR5 (zll_main_compute239_outR2, zll_main_compute215_outR1);
  ZLL_Main_compute229  instR6 (zi5, 3'h1, zll_main_compute215_outR1, zll_main_compute229_out);
  assign zi7 = (zi6 == 1'h1) ? zi5 : zll_main_compute229_out;
  ZLL_Main_compute53  instR7 (__in0, 3'h2, __in1, zi7, 3'h0, zll_main_compute53_out);
  ZLL_Main_compute53  instR8 (__in0, 3'h2, __in1, zi7, 3'h1, zll_main_compute53_outR1);
  ZLL_Main_compute53  instR9 (__in0, 3'h2, __in1, zi7, 3'h2, zll_main_compute53_outR2);
  ZLL_Main_compute53  instR10 (__in0, 3'h2, __in1, zi7, 3'h3, zll_main_compute53_outR3);
  ZLL_Main_compute53  instR11 (__in0, 3'h2, __in1, zi7, 3'h4, zll_main_compute53_outR4);
  ZLL_Main_compute53  instR12 (__in0, 3'h2, __in1, zi7, 3'h5, zll_main_compute53_outR5);
  ZLL_Main_compute53  instR13 (__in0, 3'h2, __in1, zi7, 3'h6, zll_main_compute53_outR6);
  ZLL_Main_compute53  instR14 (__in0, 3'h2, __in1, zi7, 3'h7, zll_main_compute53_outR7);
  ZLL_Main_compute239  instR15 (__in0, zll_main_compute239_outR3);
  ZLL_Main_compute179  instR16 (zll_main_compute239_outR3, 3'h2, zll_main_compute179_outR1);
  assign zi11 = zll_main_compute179_outR1;
  ZLL_Main_compute239  instR17 (__in0, zll_main_compute239_outR4);
  ZLL_Main_compute215  instR18 (zll_main_compute239_outR4, zll_main_compute215_outR2);
  assign zi12 = zll_main_compute215_outR2;
  ZLL_Main_compute218  instR19 (zi11, 3'h1, zll_main_compute218_out);
  assign zi14 = (zi12 == 1'h1) ? zi11 : zll_main_compute218_out;
  ZLL_Main_compute39  instR20 (3'h2, zi14, __in0, __in1, 3'h1, 3'h0, zll_main_compute39_out);
  ZLL_Main_compute39  instR21 (3'h2, zi14, __in0, __in1, 3'h1, 3'h1, zll_main_compute39_outR1);
  ZLL_Main_compute39  instR22 (3'h2, zi14, __in0, __in1, 3'h1, 3'h2, zll_main_compute39_outR2);
  ZLL_Main_compute39  instR23 (3'h2, zi14, __in0, __in1, 3'h1, 3'h3, zll_main_compute39_outR3);
  ZLL_Main_compute39  instR24 (3'h2, zi14, __in0, __in1, 3'h1, 3'h4, zll_main_compute39_outR4);
  ZLL_Main_compute39  instR25 (3'h2, zi14, __in0, __in1, 3'h1, 3'h5, zll_main_compute39_outR5);
  ZLL_Main_compute39  instR26 (3'h2, zi14, __in0, __in1, 3'h1, 3'h6, zll_main_compute39_outR6);
  ZLL_Main_compute39  instR27 (3'h2, zi14, __in0, __in1, 3'h1, 3'h7, zll_main_compute39_outR7);
  assign zi15 = {{zll_main_compute53_out, zll_main_compute53_outR1, zll_main_compute53_outR2, zll_main_compute53_outR3, zll_main_compute53_outR4, zll_main_compute53_outR5, zll_main_compute53_outR6, zll_main_compute53_outR7}, {zll_main_compute39_out, zll_main_compute39_outR1, zll_main_compute39_outR2, zll_main_compute39_outR3, zll_main_compute39_outR4, zll_main_compute39_outR5, zll_main_compute39_outR6, zll_main_compute39_outR7}};
  assign zi16 = zi15[127:64];
  assign zi17 = zi15[63:0];
  ZLL_Main_compute239  instR28 (zi17, zll_main_compute239_outR5);
  ZLL_Main_compute179  instR29 (zll_main_compute239_outR5, 3'h2, zll_main_compute179_outR2);
  assign zi21 = zll_main_compute179_outR2;
  ZLL_Main_compute239  instR30 (zi17, zll_main_compute239_outR6);
  ZLL_Main_compute215  instR31 (zll_main_compute239_outR6, zll_main_compute215_outR3);
  assign zi22 = zll_main_compute215_outR3;
  ZLL_Main_compute239  instR32 (zi17, zll_main_compute239_outR7);
  ZLL_Main_compute215  instR33 (zll_main_compute239_outR7, zll_main_compute215_outR4);
  ZLL_Main_compute229  instR34 (zi21, 3'h1, zll_main_compute215_outR4, zll_main_compute229_outR1);
  assign zi23 = (zi22 == 1'h1) ? zi21 : zll_main_compute229_outR1;
  ZLL_Main_compute83  instR35 (3'h2, zi16, zi23, zi17, 3'h0, zll_main_compute83_out);
  ZLL_Main_compute83  instR36 (3'h2, zi16, zi23, zi17, 3'h1, zll_main_compute83_outR1);
  ZLL_Main_compute83  instR37 (3'h2, zi16, zi23, zi17, 3'h2, zll_main_compute83_outR2);
  ZLL_Main_compute83  instR38 (3'h2, zi16, zi23, zi17, 3'h3, zll_main_compute83_outR3);
  ZLL_Main_compute83  instR39 (3'h2, zi16, zi23, zi17, 3'h4, zll_main_compute83_outR4);
  ZLL_Main_compute83  instR40 (3'h2, zi16, zi23, zi17, 3'h5, zll_main_compute83_outR5);
  ZLL_Main_compute83  instR41 (3'h2, zi16, zi23, zi17, 3'h6, zll_main_compute83_outR6);
  ZLL_Main_compute83  instR42 (3'h2, zi16, zi23, zi17, 3'h7, zll_main_compute83_outR7);
  ZLL_Main_compute239  instR43 (zi17, zll_main_compute239_outR8);
  ZLL_Main_compute179  instR44 (zll_main_compute239_outR8, 3'h2, zll_main_compute179_outR3);
  assign zi27 = zll_main_compute179_outR3;
  ZLL_Main_compute239  instR45 (zi17, zll_main_compute239_outR9);
  ZLL_Main_compute215  instR46 (zll_main_compute239_outR9, zll_main_compute215_outR5);
  assign zi28 = zll_main_compute215_outR5;
  ZLL_Main_compute218  instR47 (zi27, 3'h1, zll_main_compute218_outR1);
  assign zi30 = (zi28 == 1'h1) ? zi27 : zll_main_compute218_outR1;
  ZLL_Main_compute200  instR48 (3'h2, 3'h1, zi17, zi16, zi30, 3'h0, zll_main_compute200_out);
  ZLL_Main_compute200  instR49 (3'h2, 3'h1, zi17, zi16, zi30, 3'h1, zll_main_compute200_outR1);
  ZLL_Main_compute200  instR50 (3'h2, 3'h1, zi17, zi16, zi30, 3'h2, zll_main_compute200_outR2);
  ZLL_Main_compute200  instR51 (3'h2, 3'h1, zi17, zi16, zi30, 3'h3, zll_main_compute200_outR3);
  ZLL_Main_compute200  instR52 (3'h2, 3'h1, zi17, zi16, zi30, 3'h4, zll_main_compute200_outR4);
  ZLL_Main_compute200  instR53 (3'h2, 3'h1, zi17, zi16, zi30, 3'h5, zll_main_compute200_outR5);
  ZLL_Main_compute200  instR54 (3'h2, 3'h1, zi17, zi16, zi30, 3'h6, zll_main_compute200_outR6);
  ZLL_Main_compute200  instR55 (3'h2, 3'h1, zi17, zi16, zi30, 3'h7, zll_main_compute200_outR7);
  assign zi31 = {{zll_main_compute83_out, zll_main_compute83_outR1, zll_main_compute83_outR2, zll_main_compute83_outR3, zll_main_compute83_outR4, zll_main_compute83_outR5, zll_main_compute83_outR6, zll_main_compute83_outR7}, {zll_main_compute200_out, zll_main_compute200_outR1, zll_main_compute200_outR2, zll_main_compute200_outR3, zll_main_compute200_outR4, zll_main_compute200_outR5, zll_main_compute200_outR6, zll_main_compute200_outR7}};
  assign zi32 = zi31[127:64];
  assign zi33 = zi31[63:0];
  ZLL_Main_compute114  instR56 (3'h2, zi33, 3'h1, zi32, 3'h0, zll_main_compute114_out);
  ZLL_Main_compute114  instR57 (3'h2, zi33, 3'h1, zi32, 3'h1, zll_main_compute114_outR1);
  ZLL_Main_compute114  instR58 (3'h2, zi33, 3'h1, zi32, 3'h2, zll_main_compute114_outR2);
  ZLL_Main_compute114  instR59 (3'h2, zi33, 3'h1, zi32, 3'h3, zll_main_compute114_outR3);
  ZLL_Main_compute114  instR60 (3'h2, zi33, 3'h1, zi32, 3'h4, zll_main_compute114_outR4);
  ZLL_Main_compute114  instR61 (3'h2, zi33, 3'h1, zi32, 3'h5, zll_main_compute114_outR5);
  ZLL_Main_compute114  instR62 (3'h2, zi33, 3'h1, zi32, 3'h6, zll_main_compute114_outR6);
  ZLL_Main_compute114  instR63 (3'h2, zi33, 3'h1, zi32, 3'h7, zll_main_compute114_outR7);
  ZLL_Main_compute239  instR64 (zi32, zll_main_compute239_outR10);
  ZLL_Main_compute179  instR65 (zll_main_compute239_outR10, 3'h2, zll_main_compute179_outR4);
  assign zi40 = zll_main_compute179_outR4;
  ZLL_Main_compute239  instR66 (zi32, zll_main_compute239_outR11);
  ZLL_Main_compute215  instR67 (zll_main_compute239_outR11, zll_main_compute215_outR6);
  assign zi41 = zll_main_compute215_outR6;
  ZLL_Main_compute218  instR68 (zi40, 3'h1, zll_main_compute218_outR2);
  assign zi43 = (zi41 == 1'h1) ? zi40 : zll_main_compute218_outR2;
  ZLL_Main_compute34  instR69 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h0, zll_main_compute34_out);
  ZLL_Main_compute34  instR70 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h1, zll_main_compute34_outR1);
  ZLL_Main_compute34  instR71 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h2, zll_main_compute34_outR2);
  ZLL_Main_compute34  instR72 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h3, zll_main_compute34_outR3);
  ZLL_Main_compute34  instR73 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h4, zll_main_compute34_outR4);
  ZLL_Main_compute34  instR74 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h5, zll_main_compute34_outR5);
  ZLL_Main_compute34  instR75 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h6, zll_main_compute34_outR6);
  ZLL_Main_compute34  instR76 (zi43, 3'h2, 3'h1, zi33, zi32, 3'h7, zll_main_compute34_outR7);
  assign zi44 = {zll_main_compute114_out, zll_main_compute114_outR1, zll_main_compute114_outR2, zll_main_compute114_outR3, zll_main_compute114_outR4, zll_main_compute114_outR5, zll_main_compute114_outR6, zll_main_compute114_outR7, {zll_main_compute34_out, zll_main_compute34_outR1, zll_main_compute34_outR2, zll_main_compute34_outR3, zll_main_compute34_outR4, zll_main_compute34_outR5, zll_main_compute34_outR6, zll_main_compute34_outR7}};
  assign zi45 = zi44[127:64];
  assign zi46 = zi44[63:0];
  assign zi47 = {zi45, zi46};
  assign zres = zi47;
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute239 (input logic [63:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_compute234 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_compute229 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [2:0] zll_main_compute218_out;
  ZLL_Main_compute218  inst (arg0, arg1, zll_main_compute218_out);
  assign res = zll_main_compute218_out;
endmodule

module ZLL_Main_compute219 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute218 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute215 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute200 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute234_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute219_out;
  logic [2:0] zll_main_compute218_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute190_out;
  logic [2:0] zll_main_compute219_outR1;
  logic [2:0] zll_main_compute218_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute234  inst (arg5, arg4, zll_main_compute234_out);
  assign zi0 = zll_main_compute234_out;
  ZLL_Main_compute219  instR1 (arg5, arg0, zll_main_compute219_out);
  ZLL_Main_compute218  instR2 (zll_main_compute219_out, arg1, zll_main_compute218_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute218_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute190  instR3 (arg5, arg4, zll_main_compute190_out);
  ZLL_Main_compute219  instR4 (zll_main_compute190_out, arg0, zll_main_compute219_outR1);
  ZLL_Main_compute218  instR5 (zll_main_compute219_outR1, arg1, zll_main_compute218_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute218_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute190 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute179 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute114 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute215_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute179_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute190_out;
  logic [2:0] zll_main_compute179_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute215  inst (arg4, zll_main_compute215_out);
  assign zi0 = zll_main_compute215_out;
  ZLL_Main_compute179  instR1 (arg4, arg0, zll_main_compute179_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute179_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute190  instR2 (arg4, arg2, zll_main_compute190_out);
  ZLL_Main_compute179  instR3 (zll_main_compute190_out, arg0, zll_main_compute179_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute179_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute83 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute234_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute219_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute190_out;
  logic [2:0] zll_main_compute219_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute234  inst (arg4, arg2, zll_main_compute234_out);
  assign zi0 = zll_main_compute234_out;
  ZLL_Main_compute219  instR1 (arg4, arg0, zll_main_compute219_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute219_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute190  instR2 (arg4, arg2, zll_main_compute190_out);
  ZLL_Main_compute219  instR3 (zll_main_compute190_out, arg0, zll_main_compute219_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute219_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute53 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute234_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute219_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute190_out;
  logic [2:0] zll_main_compute219_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute234  inst (arg4, arg3, zll_main_compute234_out);
  assign zi0 = zll_main_compute234_out;
  ZLL_Main_compute219  instR1 (arg4, arg1, zll_main_compute219_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute219_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute190  instR2 (arg4, arg3, zll_main_compute190_out);
  ZLL_Main_compute219  instR3 (zll_main_compute190_out, arg1, zll_main_compute219_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute219_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute39 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute234_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute219_out;
  logic [2:0] zll_main_compute218_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute190_out;
  logic [2:0] zll_main_compute219_outR1;
  logic [2:0] zll_main_compute218_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute234  inst (arg5, arg1, zll_main_compute234_out);
  assign zi0 = zll_main_compute234_out;
  ZLL_Main_compute219  instR1 (arg5, arg0, zll_main_compute219_out);
  ZLL_Main_compute218  instR2 (zll_main_compute219_out, arg4, zll_main_compute218_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute218_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute190  instR3 (arg5, arg1, zll_main_compute190_out);
  ZLL_Main_compute219  instR4 (zll_main_compute190_out, arg0, zll_main_compute219_outR1);
  ZLL_Main_compute218  instR5 (zll_main_compute219_outR1, arg4, zll_main_compute218_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute218_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute34 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute215_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute179_out;
  logic [2:0] zll_main_compute218_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute190_out;
  logic [2:0] zll_main_compute179_outR1;
  logic [2:0] zll_main_compute218_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute215  inst (arg5, zll_main_compute215_out);
  assign zi0 = zll_main_compute215_out;
  ZLL_Main_compute179  instR1 (arg5, arg1, zll_main_compute179_out);
  ZLL_Main_compute218  instR2 (arg0, zll_main_compute179_out, zll_main_compute218_out);
  assign slice_in = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute218_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute190  instR3 (arg5, arg2, zll_main_compute190_out);
  ZLL_Main_compute179  instR4 (zll_main_compute190_out, arg1, zll_main_compute179_outR1);
  ZLL_Main_compute218  instR5 (arg0, zll_main_compute179_outR1, zll_main_compute218_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute218_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule