module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [2:0] zll_main_compute444_out;
  logic [2:0] zll_main_compute436_out;
  logic [2:0] zi5;
  logic [2:0] zll_main_compute444_outR1;
  logic [0:0] zll_main_compute379_out;
  logic [0:0] zi6;
  logic [2:0] zll_main_compute409_out;
  logic [2:0] zll_main_compute444_outR2;
  logic [0:0] zll_main_compute379_outR1;
  logic [2:0] zll_main_compute187_out;
  logic [2:0] zi7;
  logic [7:0] zll_main_compute415_out;
  logic [7:0] zll_main_compute415_outR1;
  logic [7:0] zll_main_compute415_outR2;
  logic [7:0] zll_main_compute415_outR3;
  logic [7:0] zll_main_compute415_outR4;
  logic [7:0] zll_main_compute415_outR5;
  logic [7:0] zll_main_compute415_outR6;
  logic [7:0] zll_main_compute415_outR7;
  logic [2:0] zll_main_compute444_outR3;
  logic [2:0] zll_main_compute436_outR1;
  logic [2:0] zi11;
  logic [2:0] zll_main_compute444_outR4;
  logic [0:0] zll_main_compute379_outR2;
  logic [0:0] zi12;
  logic [2:0] zll_main_compute409_outR1;
  logic [2:0] zll_main_compute444_outR5;
  logic [0:0] zll_main_compute379_outR3;
  logic [2:0] zll_main_compute187_outR1;
  logic [2:0] zi13;
  logic [7:0] zll_main_compute312_out;
  logic [7:0] zll_main_compute312_outR1;
  logic [7:0] zll_main_compute312_outR2;
  logic [7:0] zll_main_compute312_outR3;
  logic [7:0] zll_main_compute312_outR4;
  logic [7:0] zll_main_compute312_outR5;
  logic [7:0] zll_main_compute312_outR6;
  logic [7:0] zll_main_compute312_outR7;
  logic [127:0] zi14;
  logic [63:0] zi15;
  logic [63:0] zi16;
  logic [2:0] zll_main_compute444_outR6;
  logic [2:0] zll_main_compute436_outR2;
  logic [2:0] zi20;
  logic [2:0] zll_main_compute444_outR7;
  logic [0:0] zll_main_compute379_outR4;
  logic [0:0] zi21;
  logic [2:0] zll_main_compute409_outR2;
  logic [2:0] zll_main_compute444_outR8;
  logic [0:0] zll_main_compute379_outR5;
  logic [2:0] zll_main_compute357_out;
  logic [2:0] zi22;
  logic [7:0] zll_main_compute393_out;
  logic [7:0] zll_main_compute393_outR1;
  logic [7:0] zll_main_compute393_outR2;
  logic [7:0] zll_main_compute393_outR3;
  logic [7:0] zll_main_compute393_outR4;
  logic [7:0] zll_main_compute393_outR5;
  logic [7:0] zll_main_compute393_outR6;
  logic [7:0] zll_main_compute393_outR7;
  logic [2:0] zll_main_compute444_outR9;
  logic [2:0] zll_main_compute436_outR3;
  logic [2:0] zi26;
  logic [2:0] zll_main_compute444_outR10;
  logic [0:0] zll_main_compute379_outR6;
  logic [0:0] zi27;
  logic [2:0] zll_main_compute409_outR3;
  logic [2:0] zll_main_compute444_outR11;
  logic [0:0] zll_main_compute379_outR7;
  logic [2:0] zll_main_compute357_outR1;
  logic [2:0] zi28;
  logic [7:0] zll_main_compute254_out;
  logic [7:0] zll_main_compute254_outR1;
  logic [7:0] zll_main_compute254_outR2;
  logic [7:0] zll_main_compute254_outR3;
  logic [7:0] zll_main_compute254_outR4;
  logic [7:0] zll_main_compute254_outR5;
  logic [7:0] zll_main_compute254_outR6;
  logic [7:0] zll_main_compute254_outR7;
  logic [127:0] zi29;
  logic [63:0] zi30;
  logic [63:0] zi31;
  logic [7:0] zll_main_compute201_out;
  logic [7:0] zll_main_compute201_outR1;
  logic [7:0] zll_main_compute201_outR2;
  logic [7:0] zll_main_compute201_outR3;
  logic [7:0] zll_main_compute201_outR4;
  logic [7:0] zll_main_compute201_outR5;
  logic [7:0] zll_main_compute201_outR6;
  logic [7:0] zll_main_compute201_outR7;
  logic [2:0] zll_main_compute444_outR12;
  logic [2:0] zll_main_compute436_outR4;
  logic [2:0] zi38;
  logic [2:0] zll_main_compute444_outR13;
  logic [0:0] zll_main_compute379_outR8;
  logic [0:0] zi39;
  logic [2:0] zll_main_compute409_outR4;
  logic [2:0] zll_main_compute444_outR14;
  logic [0:0] zll_main_compute379_outR9;
  logic [2:0] zll_main_compute187_outR2;
  logic [2:0] zi40;
  logic [7:0] zll_main_compute372_out;
  logic [7:0] zll_main_compute372_outR1;
  logic [7:0] zll_main_compute372_outR2;
  logic [7:0] zll_main_compute372_outR3;
  logic [7:0] zll_main_compute372_outR4;
  logic [7:0] zll_main_compute372_outR5;
  logic [7:0] zll_main_compute372_outR6;
  logic [7:0] zll_main_compute372_outR7;
  logic [127:0] zi41;
  logic [63:0] zi42;
  logic [63:0] zi43;
  logic [128:0] zi44;
  logic [127:0] zi45;
  logic [128:0] zres;
  ZLL_Main_compute444  inst (__in0, zll_main_compute444_out);
  ZLL_Main_compute436  instR1 (zll_main_compute444_out, 3'h2, zll_main_compute436_out);
  assign zi5 = zll_main_compute436_out;
  ZLL_Main_compute444  instR2 (__in0, zll_main_compute444_outR1);
  ZLL_Main_compute379  instR3 (zll_main_compute444_outR1, zll_main_compute379_out);
  assign zi6 = zll_main_compute379_out;
  ZLL_Main_compute409  instR4 (zi5, zll_main_compute409_out);
  ZLL_Main_compute444  instR5 (__in0, zll_main_compute444_outR2);
  ZLL_Main_compute379  instR6 (zll_main_compute444_outR2, zll_main_compute379_outR1);
  ZLL_Main_compute187  instR7 (3'h1, zi5, zll_main_compute379_outR1, zll_main_compute187_out);
  assign zi7 = (zi6 == 1'h1) ? zll_main_compute409_out : zll_main_compute187_out;
  ZLL_Main_compute415  instR8 (__in1, __in0, 3'h2, zi7, 3'h0, zll_main_compute415_out);
  ZLL_Main_compute415  instR9 (__in1, __in0, 3'h2, zi7, 3'h1, zll_main_compute415_outR1);
  ZLL_Main_compute415  instR10 (__in1, __in0, 3'h2, zi7, 3'h2, zll_main_compute415_outR2);
  ZLL_Main_compute415  instR11 (__in1, __in0, 3'h2, zi7, 3'h3, zll_main_compute415_outR3);
  ZLL_Main_compute415  instR12 (__in1, __in0, 3'h2, zi7, 3'h4, zll_main_compute415_outR4);
  ZLL_Main_compute415  instR13 (__in1, __in0, 3'h2, zi7, 3'h5, zll_main_compute415_outR5);
  ZLL_Main_compute415  instR14 (__in1, __in0, 3'h2, zi7, 3'h6, zll_main_compute415_outR6);
  ZLL_Main_compute415  instR15 (__in1, __in0, 3'h2, zi7, 3'h7, zll_main_compute415_outR7);
  ZLL_Main_compute444  instR16 (__in0, zll_main_compute444_outR3);
  ZLL_Main_compute436  instR17 (zll_main_compute444_outR3, 3'h2, zll_main_compute436_outR1);
  assign zi11 = zll_main_compute436_outR1;
  ZLL_Main_compute444  instR18 (__in0, zll_main_compute444_outR4);
  ZLL_Main_compute379  instR19 (zll_main_compute444_outR4, zll_main_compute379_outR2);
  assign zi12 = zll_main_compute379_outR2;
  ZLL_Main_compute409  instR20 (zi11, zll_main_compute409_outR1);
  ZLL_Main_compute444  instR21 (__in0, zll_main_compute444_outR5);
  ZLL_Main_compute379  instR22 (zll_main_compute444_outR5, zll_main_compute379_outR3);
  ZLL_Main_compute187  instR23 (3'h1, zi11, zll_main_compute379_outR3, zll_main_compute187_outR1);
  assign zi13 = (zi12 == 1'h1) ? zll_main_compute409_outR1 : zll_main_compute187_outR1;
  ZLL_Main_compute312  instR24 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h0, zll_main_compute312_out);
  ZLL_Main_compute312  instR25 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h1, zll_main_compute312_outR1);
  ZLL_Main_compute312  instR26 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h2, zll_main_compute312_outR2);
  ZLL_Main_compute312  instR27 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h3, zll_main_compute312_outR3);
  ZLL_Main_compute312  instR28 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h4, zll_main_compute312_outR4);
  ZLL_Main_compute312  instR29 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h5, zll_main_compute312_outR5);
  ZLL_Main_compute312  instR30 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h6, zll_main_compute312_outR6);
  ZLL_Main_compute312  instR31 (__in1, 3'h1, __in0, zi13, 3'h2, 3'h7, zll_main_compute312_outR7);
  assign zi14 = {{zll_main_compute415_out, zll_main_compute415_outR1, zll_main_compute415_outR2, zll_main_compute415_outR3, zll_main_compute415_outR4, zll_main_compute415_outR5, zll_main_compute415_outR6, zll_main_compute415_outR7}, {zll_main_compute312_out, zll_main_compute312_outR1, zll_main_compute312_outR2, zll_main_compute312_outR3, zll_main_compute312_outR4, zll_main_compute312_outR5, zll_main_compute312_outR6, zll_main_compute312_outR7}};
  assign zi15 = zi14[127:64];
  assign zi16 = zi14[63:0];
  ZLL_Main_compute444  instR32 (zi16, zll_main_compute444_outR6);
  ZLL_Main_compute436  instR33 (zll_main_compute444_outR6, 3'h2, zll_main_compute436_outR2);
  assign zi20 = zll_main_compute436_outR2;
  ZLL_Main_compute444  instR34 (zi16, zll_main_compute444_outR7);
  ZLL_Main_compute379  instR35 (zll_main_compute444_outR7, zll_main_compute379_outR4);
  assign zi21 = zll_main_compute379_outR4;
  ZLL_Main_compute409  instR36 (zi20, zll_main_compute409_outR2);
  ZLL_Main_compute444  instR37 (zi16, zll_main_compute444_outR8);
  ZLL_Main_compute379  instR38 (zll_main_compute444_outR8, zll_main_compute379_outR5);
  ZLL_Main_compute357  instR39 (zi20, 3'h1, zll_main_compute379_outR5, zll_main_compute357_out);
  assign zi22 = (zi21 == 1'h1) ? zll_main_compute409_outR2 : zll_main_compute357_out;
  ZLL_Main_compute393  instR40 (zi22, 3'h2, zi15, zi16, 3'h0, zll_main_compute393_out);
  ZLL_Main_compute393  instR41 (zi22, 3'h2, zi15, zi16, 3'h1, zll_main_compute393_outR1);
  ZLL_Main_compute393  instR42 (zi22, 3'h2, zi15, zi16, 3'h2, zll_main_compute393_outR2);
  ZLL_Main_compute393  instR43 (zi22, 3'h2, zi15, zi16, 3'h3, zll_main_compute393_outR3);
  ZLL_Main_compute393  instR44 (zi22, 3'h2, zi15, zi16, 3'h4, zll_main_compute393_outR4);
  ZLL_Main_compute393  instR45 (zi22, 3'h2, zi15, zi16, 3'h5, zll_main_compute393_outR5);
  ZLL_Main_compute393  instR46 (zi22, 3'h2, zi15, zi16, 3'h6, zll_main_compute393_outR6);
  ZLL_Main_compute393  instR47 (zi22, 3'h2, zi15, zi16, 3'h7, zll_main_compute393_outR7);
  ZLL_Main_compute444  instR48 (zi16, zll_main_compute444_outR9);
  ZLL_Main_compute436  instR49 (zll_main_compute444_outR9, 3'h2, zll_main_compute436_outR3);
  assign zi26 = zll_main_compute436_outR3;
  ZLL_Main_compute444  instR50 (zi16, zll_main_compute444_outR10);
  ZLL_Main_compute379  instR51 (zll_main_compute444_outR10, zll_main_compute379_outR6);
  assign zi27 = zll_main_compute379_outR6;
  ZLL_Main_compute409  instR52 (zi26, zll_main_compute409_outR3);
  ZLL_Main_compute444  instR53 (zi16, zll_main_compute444_outR11);
  ZLL_Main_compute379  instR54 (zll_main_compute444_outR11, zll_main_compute379_outR7);
  ZLL_Main_compute357  instR55 (zi26, 3'h1, zll_main_compute379_outR7, zll_main_compute357_outR1);
  assign zi28 = (zi27 == 1'h1) ? zll_main_compute409_outR3 : zll_main_compute357_outR1;
  ZLL_Main_compute254  instR56 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h0, zll_main_compute254_out);
  ZLL_Main_compute254  instR57 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h1, zll_main_compute254_outR1);
  ZLL_Main_compute254  instR58 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h2, zll_main_compute254_outR2);
  ZLL_Main_compute254  instR59 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h3, zll_main_compute254_outR3);
  ZLL_Main_compute254  instR60 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h4, zll_main_compute254_outR4);
  ZLL_Main_compute254  instR61 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h5, zll_main_compute254_outR5);
  ZLL_Main_compute254  instR62 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h6, zll_main_compute254_outR6);
  ZLL_Main_compute254  instR63 (zi28, 3'h2, 3'h1, zi16, zi15, 3'h7, zll_main_compute254_outR7);
  assign zi29 = {{zll_main_compute393_out, zll_main_compute393_outR1, zll_main_compute393_outR2, zll_main_compute393_outR3, zll_main_compute393_outR4, zll_main_compute393_outR5, zll_main_compute393_outR6, zll_main_compute393_outR7}, {zll_main_compute254_out, zll_main_compute254_outR1, zll_main_compute254_outR2, zll_main_compute254_outR3, zll_main_compute254_outR4, zll_main_compute254_outR5, zll_main_compute254_outR6, zll_main_compute254_outR7}};
  assign zi30 = zi29[127:64];
  assign zi31 = zi29[63:0];
  ZLL_Main_compute201  instR64 (zi31, 3'h2, zi30, 3'h1, 3'h0, zll_main_compute201_out);
  ZLL_Main_compute201  instR65 (zi31, 3'h2, zi30, 3'h1, 3'h1, zll_main_compute201_outR1);
  ZLL_Main_compute201  instR66 (zi31, 3'h2, zi30, 3'h1, 3'h2, zll_main_compute201_outR2);
  ZLL_Main_compute201  instR67 (zi31, 3'h2, zi30, 3'h1, 3'h3, zll_main_compute201_outR3);
  ZLL_Main_compute201  instR68 (zi31, 3'h2, zi30, 3'h1, 3'h4, zll_main_compute201_outR4);
  ZLL_Main_compute201  instR69 (zi31, 3'h2, zi30, 3'h1, 3'h5, zll_main_compute201_outR5);
  ZLL_Main_compute201  instR70 (zi31, 3'h2, zi30, 3'h1, 3'h6, zll_main_compute201_outR6);
  ZLL_Main_compute201  instR71 (zi31, 3'h2, zi30, 3'h1, 3'h7, zll_main_compute201_outR7);
  ZLL_Main_compute444  instR72 (zi30, zll_main_compute444_outR12);
  ZLL_Main_compute436  instR73 (zll_main_compute444_outR12, 3'h2, zll_main_compute436_outR4);
  assign zi38 = zll_main_compute436_outR4;
  ZLL_Main_compute444  instR74 (zi30, zll_main_compute444_outR13);
  ZLL_Main_compute379  instR75 (zll_main_compute444_outR13, zll_main_compute379_outR8);
  assign zi39 = zll_main_compute379_outR8;
  ZLL_Main_compute409  instR76 (zi38, zll_main_compute409_outR4);
  ZLL_Main_compute444  instR77 (zi30, zll_main_compute444_outR14);
  ZLL_Main_compute379  instR78 (zll_main_compute444_outR14, zll_main_compute379_outR9);
  ZLL_Main_compute187  instR79 (3'h1, zi38, zll_main_compute379_outR9, zll_main_compute187_outR2);
  assign zi40 = (zi39 == 1'h1) ? zll_main_compute409_outR4 : zll_main_compute187_outR2;
  ZLL_Main_compute372  instR80 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h0, zll_main_compute372_out);
  ZLL_Main_compute372  instR81 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h1, zll_main_compute372_outR1);
  ZLL_Main_compute372  instR82 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h2, zll_main_compute372_outR2);
  ZLL_Main_compute372  instR83 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h3, zll_main_compute372_outR3);
  ZLL_Main_compute372  instR84 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h4, zll_main_compute372_outR4);
  ZLL_Main_compute372  instR85 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h5, zll_main_compute372_outR5);
  ZLL_Main_compute372  instR86 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h6, zll_main_compute372_outR6);
  ZLL_Main_compute372  instR87 (zi40, zi31, 3'h2, 3'h1, zi30, 3'h7, zll_main_compute372_outR7);
  assign zi41 = {zll_main_compute201_out, zll_main_compute201_outR1, zll_main_compute201_outR2, zll_main_compute201_outR3, zll_main_compute201_outR4, zll_main_compute201_outR5, zll_main_compute201_outR6, zll_main_compute201_outR7, {zll_main_compute372_out, zll_main_compute372_outR1, zll_main_compute372_outR2, zll_main_compute372_outR3, zll_main_compute372_outR4, zll_main_compute372_outR5, zll_main_compute372_outR6, zll_main_compute372_outR7}};
  assign zi42 = zi41[127:64];
  assign zi43 = zi41[63:0];
  assign zi44 = {1'h0, {zi42, zi43}};
  assign zi45 = zi44[127:0];
  assign zres = {1'h1, zi45};
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute444 (input logic [63:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_compute436 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute429 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute424 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute418 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute415 (input logic [63:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute302_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute418_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute424_out;
  logic [2:0] zll_main_compute418_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute302  inst (arg4, arg3, zll_main_compute302_out);
  assign zi0 = zll_main_compute302_out;
  ZLL_Main_compute418  instR1 (arg4, arg2, zll_main_compute418_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute418_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute424  instR2 (arg4, arg3, zll_main_compute424_out);
  ZLL_Main_compute418  instR3 (zll_main_compute424_out, arg2, zll_main_compute418_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute418_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute409 (input logic [2:0] arg0,
  output logic [2:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_compute393 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute302_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute418_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute424_out;
  logic [2:0] zll_main_compute418_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute302  inst (arg4, arg0, zll_main_compute302_out);
  assign zi0 = zll_main_compute302_out;
  ZLL_Main_compute418  instR1 (arg4, arg1, zll_main_compute418_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute418_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute424  instR2 (arg4, arg0, zll_main_compute424_out);
  ZLL_Main_compute418  instR3 (zll_main_compute424_out, arg1, zll_main_compute418_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute418_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute379 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute372 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute379_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute436_out;
  logic [2:0] zll_main_compute429_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute424_out;
  logic [2:0] zll_main_compute436_outR1;
  logic [2:0] zll_main_compute429_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute379  inst (arg5, zll_main_compute379_out);
  assign zi0 = zll_main_compute379_out;
  ZLL_Main_compute436  instR1 (arg5, arg2, zll_main_compute436_out);
  ZLL_Main_compute429  instR2 (arg0, zll_main_compute436_out, zll_main_compute429_out);
  assign slice_in = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute429_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute424  instR3 (arg5, arg3, zll_main_compute424_out);
  ZLL_Main_compute436  instR4 (zll_main_compute424_out, arg2, zll_main_compute436_outR1);
  ZLL_Main_compute429  instR5 (arg0, zll_main_compute436_outR1, zll_main_compute429_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute429_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute357 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [2:0] zll_main_compute429_out;
  ZLL_Main_compute429  inst (arg0, arg1, zll_main_compute429_out);
  assign res = zll_main_compute429_out;
endmodule

module ZLL_Main_compute312 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute302_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute418_out;
  logic [2:0] zll_main_compute429_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute424_out;
  logic [2:0] zll_main_compute418_outR1;
  logic [2:0] zll_main_compute429_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute302  inst (arg5, arg3, zll_main_compute302_out);
  assign zi0 = zll_main_compute302_out;
  ZLL_Main_compute418  instR1 (arg5, arg4, zll_main_compute418_out);
  ZLL_Main_compute429  instR2 (zll_main_compute418_out, arg1, zll_main_compute429_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute429_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute424  instR3 (arg5, arg3, zll_main_compute424_out);
  ZLL_Main_compute418  instR4 (zll_main_compute424_out, arg4, zll_main_compute418_outR1);
  ZLL_Main_compute429  instR5 (zll_main_compute418_outR1, arg1, zll_main_compute429_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute429_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute302 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_compute254 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute302_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute418_out;
  logic [2:0] zll_main_compute429_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute424_out;
  logic [2:0] zll_main_compute418_outR1;
  logic [2:0] zll_main_compute429_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute302  inst (arg5, arg0, zll_main_compute302_out);
  assign zi0 = zll_main_compute302_out;
  ZLL_Main_compute418  instR1 (arg5, arg1, zll_main_compute418_out);
  ZLL_Main_compute429  instR2 (zll_main_compute418_out, arg2, zll_main_compute429_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute429_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute424  instR3 (arg5, arg0, zll_main_compute424_out);
  ZLL_Main_compute418  instR4 (zll_main_compute424_out, arg1, zll_main_compute418_outR1);
  ZLL_Main_compute429  instR5 (zll_main_compute418_outR1, arg2, zll_main_compute429_outR1);
  assign slice_inR1 = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute429_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute201 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute379_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute436_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute424_out;
  logic [2:0] zll_main_compute436_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute379  inst (arg4, zll_main_compute379_out);
  assign zi0 = zll_main_compute379_out;
  ZLL_Main_compute436  instR1 (arg4, arg1, zll_main_compute436_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute436_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute424  instR2 (arg4, arg3, zll_main_compute424_out);
  ZLL_Main_compute436  instR3 (zll_main_compute424_out, arg1, zll_main_compute436_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute436_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute187 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [2:0] zll_main_compute429_out;
  ZLL_Main_compute429  inst (arg1, arg0, zll_main_compute429_out);
  assign res = zll_main_compute429_out;
endmodule