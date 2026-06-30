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
  logic [2:0] zll_main_compute203_out;
  logic [2:0] zi8;
  logic [7:0] zll_main_compute415_out;
  logic [7:0] zll_main_compute415_outR1;
  logic [7:0] zll_main_compute415_outR2;
  logic [7:0] zll_main_compute415_outR3;
  logic [7:0] zll_main_compute415_outR4;
  logic [7:0] zll_main_compute415_outR5;
  logic [7:0] zll_main_compute415_outR6;
  logic [7:0] zll_main_compute415_outR7;
  logic [2:0] zll_main_compute444_outR2;
  logic [2:0] zll_main_compute436_outR1;
  logic [2:0] zi12;
  logic [2:0] zll_main_compute444_outR3;
  logic [0:0] zll_main_compute379_outR1;
  logic [0:0] zi13;
  logic [2:0] zll_main_compute409_outR1;
  logic [2:0] zll_main_compute203_outR1;
  logic [2:0] zi15;
  logic [7:0] zll_main_compute312_out;
  logic [7:0] zll_main_compute312_outR1;
  logic [7:0] zll_main_compute312_outR2;
  logic [7:0] zll_main_compute312_outR3;
  logic [7:0] zll_main_compute312_outR4;
  logic [7:0] zll_main_compute312_outR5;
  logic [7:0] zll_main_compute312_outR6;
  logic [7:0] zll_main_compute312_outR7;
  logic [127:0] zi16;
  logic [63:0] zi17;
  logic [63:0] zi18;
  logic [2:0] zll_main_compute444_outR4;
  logic [2:0] zll_main_compute436_outR2;
  logic [2:0] zi22;
  logic [2:0] zll_main_compute444_outR5;
  logic [0:0] zll_main_compute379_outR2;
  logic [0:0] zi23;
  logic [2:0] zll_main_compute409_outR2;
  logic [2:0] zll_main_compute395_out;
  logic [2:0] zi25;
  logic [7:0] zll_main_compute393_out;
  logic [7:0] zll_main_compute393_outR1;
  logic [7:0] zll_main_compute393_outR2;
  logic [7:0] zll_main_compute393_outR3;
  logic [7:0] zll_main_compute393_outR4;
  logic [7:0] zll_main_compute393_outR5;
  logic [7:0] zll_main_compute393_outR6;
  logic [7:0] zll_main_compute393_outR7;
  logic [2:0] zll_main_compute444_outR6;
  logic [2:0] zll_main_compute436_outR3;
  logic [2:0] zi29;
  logic [2:0] zll_main_compute444_outR7;
  logic [0:0] zll_main_compute379_outR3;
  logic [0:0] zi30;
  logic [2:0] zll_main_compute409_outR3;
  logic [2:0] zll_main_compute395_outR1;
  logic [2:0] zi32;
  logic [7:0] zll_main_compute254_out;
  logic [7:0] zll_main_compute254_outR1;
  logic [7:0] zll_main_compute254_outR2;
  logic [7:0] zll_main_compute254_outR3;
  logic [7:0] zll_main_compute254_outR4;
  logic [7:0] zll_main_compute254_outR5;
  logic [7:0] zll_main_compute254_outR6;
  logic [7:0] zll_main_compute254_outR7;
  logic [127:0] zi33;
  logic [63:0] zi34;
  logic [63:0] zi35;
  logic [7:0] zll_main_compute201_out;
  logic [7:0] zll_main_compute201_outR1;
  logic [7:0] zll_main_compute201_outR2;
  logic [7:0] zll_main_compute201_outR3;
  logic [7:0] zll_main_compute201_outR4;
  logic [7:0] zll_main_compute201_outR5;
  logic [7:0] zll_main_compute201_outR6;
  logic [7:0] zll_main_compute201_outR7;
  logic [2:0] zll_main_compute444_outR8;
  logic [2:0] zll_main_compute436_outR4;
  logic [2:0] zi42;
  logic [2:0] zll_main_compute444_outR9;
  logic [0:0] zll_main_compute379_outR4;
  logic [0:0] zi43;
  logic [2:0] zll_main_compute409_outR4;
  logic [2:0] zll_main_compute203_outR2;
  logic [2:0] zi45;
  logic [7:0] zll_main_compute372_out;
  logic [7:0] zll_main_compute372_outR1;
  logic [7:0] zll_main_compute372_outR2;
  logic [7:0] zll_main_compute372_outR3;
  logic [7:0] zll_main_compute372_outR4;
  logic [7:0] zll_main_compute372_outR5;
  logic [7:0] zll_main_compute372_outR6;
  logic [7:0] zll_main_compute372_outR7;
  logic [127:0] zi46;
  logic [63:0] zi47;
  logic [63:0] zi48;
  logic [128:0] zi49;
  logic [127:0] zi50;
  logic [128:0] zres;
  ZLL_Main_compute444  inst (__in0, zll_main_compute444_out);
  ZLL_Main_compute436  instR1 (zll_main_compute444_out, 3'h2, zll_main_compute436_out);
  assign zi5 = zll_main_compute436_out;
  ZLL_Main_compute444  instR2 (__in0, zll_main_compute444_outR1);
  ZLL_Main_compute379  instR3 (zll_main_compute444_outR1, zll_main_compute379_out);
  assign zi6 = zll_main_compute379_out;
  ZLL_Main_compute409  instR4 (zi5, zll_main_compute409_out);
  ZLL_Main_compute203  instR5 (3'h1, zi5, zll_main_compute203_out);
  assign zi8 = (zi6 == 1'h1) ? zll_main_compute409_out : zll_main_compute203_out;
  ZLL_Main_compute415  instR6 (__in1, __in0, 3'h2, zi8, 3'h0, zll_main_compute415_out);
  ZLL_Main_compute415  instR7 (__in1, __in0, 3'h2, zi8, 3'h1, zll_main_compute415_outR1);
  ZLL_Main_compute415  instR8 (__in1, __in0, 3'h2, zi8, 3'h2, zll_main_compute415_outR2);
  ZLL_Main_compute415  instR9 (__in1, __in0, 3'h2, zi8, 3'h3, zll_main_compute415_outR3);
  ZLL_Main_compute415  instR10 (__in1, __in0, 3'h2, zi8, 3'h4, zll_main_compute415_outR4);
  ZLL_Main_compute415  instR11 (__in1, __in0, 3'h2, zi8, 3'h5, zll_main_compute415_outR5);
  ZLL_Main_compute415  instR12 (__in1, __in0, 3'h2, zi8, 3'h6, zll_main_compute415_outR6);
  ZLL_Main_compute415  instR13 (__in1, __in0, 3'h2, zi8, 3'h7, zll_main_compute415_outR7);
  ZLL_Main_compute444  instR14 (__in0, zll_main_compute444_outR2);
  ZLL_Main_compute436  instR15 (zll_main_compute444_outR2, 3'h2, zll_main_compute436_outR1);
  assign zi12 = zll_main_compute436_outR1;
  ZLL_Main_compute444  instR16 (__in0, zll_main_compute444_outR3);
  ZLL_Main_compute379  instR17 (zll_main_compute444_outR3, zll_main_compute379_outR1);
  assign zi13 = zll_main_compute379_outR1;
  ZLL_Main_compute409  instR18 (zi12, zll_main_compute409_outR1);
  ZLL_Main_compute203  instR19 (3'h1, zi12, zll_main_compute203_outR1);
  assign zi15 = (zi13 == 1'h1) ? zll_main_compute409_outR1 : zll_main_compute203_outR1;
  ZLL_Main_compute312  instR20 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h0, zll_main_compute312_out);
  ZLL_Main_compute312  instR21 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h1, zll_main_compute312_outR1);
  ZLL_Main_compute312  instR22 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h2, zll_main_compute312_outR2);
  ZLL_Main_compute312  instR23 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h3, zll_main_compute312_outR3);
  ZLL_Main_compute312  instR24 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h4, zll_main_compute312_outR4);
  ZLL_Main_compute312  instR25 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h5, zll_main_compute312_outR5);
  ZLL_Main_compute312  instR26 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h6, zll_main_compute312_outR6);
  ZLL_Main_compute312  instR27 (__in1, 3'h1, __in0, zi15, 3'h2, 3'h7, zll_main_compute312_outR7);
  assign zi16 = {{zll_main_compute415_out, zll_main_compute415_outR1, zll_main_compute415_outR2, zll_main_compute415_outR3, zll_main_compute415_outR4, zll_main_compute415_outR5, zll_main_compute415_outR6, zll_main_compute415_outR7}, {zll_main_compute312_out, zll_main_compute312_outR1, zll_main_compute312_outR2, zll_main_compute312_outR3, zll_main_compute312_outR4, zll_main_compute312_outR5, zll_main_compute312_outR6, zll_main_compute312_outR7}};
  assign zi17 = zi16[127:64];
  assign zi18 = zi16[63:0];
  ZLL_Main_compute444  instR28 (zi18, zll_main_compute444_outR4);
  ZLL_Main_compute436  instR29 (zll_main_compute444_outR4, 3'h2, zll_main_compute436_outR2);
  assign zi22 = zll_main_compute436_outR2;
  ZLL_Main_compute444  instR30 (zi18, zll_main_compute444_outR5);
  ZLL_Main_compute379  instR31 (zll_main_compute444_outR5, zll_main_compute379_outR2);
  assign zi23 = zll_main_compute379_outR2;
  ZLL_Main_compute409  instR32 (zi22, zll_main_compute409_outR2);
  ZLL_Main_compute395  instR33 (zi22, 3'h1, zll_main_compute395_out);
  assign zi25 = (zi23 == 1'h1) ? zll_main_compute409_outR2 : zll_main_compute395_out;
  ZLL_Main_compute393  instR34 (zi25, 3'h2, zi17, zi18, 3'h0, zll_main_compute393_out);
  ZLL_Main_compute393  instR35 (zi25, 3'h2, zi17, zi18, 3'h1, zll_main_compute393_outR1);
  ZLL_Main_compute393  instR36 (zi25, 3'h2, zi17, zi18, 3'h2, zll_main_compute393_outR2);
  ZLL_Main_compute393  instR37 (zi25, 3'h2, zi17, zi18, 3'h3, zll_main_compute393_outR3);
  ZLL_Main_compute393  instR38 (zi25, 3'h2, zi17, zi18, 3'h4, zll_main_compute393_outR4);
  ZLL_Main_compute393  instR39 (zi25, 3'h2, zi17, zi18, 3'h5, zll_main_compute393_outR5);
  ZLL_Main_compute393  instR40 (zi25, 3'h2, zi17, zi18, 3'h6, zll_main_compute393_outR6);
  ZLL_Main_compute393  instR41 (zi25, 3'h2, zi17, zi18, 3'h7, zll_main_compute393_outR7);
  ZLL_Main_compute444  instR42 (zi18, zll_main_compute444_outR6);
  ZLL_Main_compute436  instR43 (zll_main_compute444_outR6, 3'h2, zll_main_compute436_outR3);
  assign zi29 = zll_main_compute436_outR3;
  ZLL_Main_compute444  instR44 (zi18, zll_main_compute444_outR7);
  ZLL_Main_compute379  instR45 (zll_main_compute444_outR7, zll_main_compute379_outR3);
  assign zi30 = zll_main_compute379_outR3;
  ZLL_Main_compute409  instR46 (zi29, zll_main_compute409_outR3);
  ZLL_Main_compute395  instR47 (zi29, 3'h1, zll_main_compute395_outR1);
  assign zi32 = (zi30 == 1'h1) ? zll_main_compute409_outR3 : zll_main_compute395_outR1;
  ZLL_Main_compute254  instR48 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h0, zll_main_compute254_out);
  ZLL_Main_compute254  instR49 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h1, zll_main_compute254_outR1);
  ZLL_Main_compute254  instR50 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h2, zll_main_compute254_outR2);
  ZLL_Main_compute254  instR51 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h3, zll_main_compute254_outR3);
  ZLL_Main_compute254  instR52 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h4, zll_main_compute254_outR4);
  ZLL_Main_compute254  instR53 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h5, zll_main_compute254_outR5);
  ZLL_Main_compute254  instR54 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h6, zll_main_compute254_outR6);
  ZLL_Main_compute254  instR55 (zi32, 3'h2, 3'h1, zi18, zi17, 3'h7, zll_main_compute254_outR7);
  assign zi33 = {{zll_main_compute393_out, zll_main_compute393_outR1, zll_main_compute393_outR2, zll_main_compute393_outR3, zll_main_compute393_outR4, zll_main_compute393_outR5, zll_main_compute393_outR6, zll_main_compute393_outR7}, {zll_main_compute254_out, zll_main_compute254_outR1, zll_main_compute254_outR2, zll_main_compute254_outR3, zll_main_compute254_outR4, zll_main_compute254_outR5, zll_main_compute254_outR6, zll_main_compute254_outR7}};
  assign zi34 = zi33[127:64];
  assign zi35 = zi33[63:0];
  ZLL_Main_compute201  instR56 (zi35, 3'h2, zi34, 3'h1, 3'h0, zll_main_compute201_out);
  ZLL_Main_compute201  instR57 (zi35, 3'h2, zi34, 3'h1, 3'h1, zll_main_compute201_outR1);
  ZLL_Main_compute201  instR58 (zi35, 3'h2, zi34, 3'h1, 3'h2, zll_main_compute201_outR2);
  ZLL_Main_compute201  instR59 (zi35, 3'h2, zi34, 3'h1, 3'h3, zll_main_compute201_outR3);
  ZLL_Main_compute201  instR60 (zi35, 3'h2, zi34, 3'h1, 3'h4, zll_main_compute201_outR4);
  ZLL_Main_compute201  instR61 (zi35, 3'h2, zi34, 3'h1, 3'h5, zll_main_compute201_outR5);
  ZLL_Main_compute201  instR62 (zi35, 3'h2, zi34, 3'h1, 3'h6, zll_main_compute201_outR6);
  ZLL_Main_compute201  instR63 (zi35, 3'h2, zi34, 3'h1, 3'h7, zll_main_compute201_outR7);
  ZLL_Main_compute444  instR64 (zi34, zll_main_compute444_outR8);
  ZLL_Main_compute436  instR65 (zll_main_compute444_outR8, 3'h2, zll_main_compute436_outR4);
  assign zi42 = zll_main_compute436_outR4;
  ZLL_Main_compute444  instR66 (zi34, zll_main_compute444_outR9);
  ZLL_Main_compute379  instR67 (zll_main_compute444_outR9, zll_main_compute379_outR4);
  assign zi43 = zll_main_compute379_outR4;
  ZLL_Main_compute409  instR68 (zi42, zll_main_compute409_outR4);
  ZLL_Main_compute203  instR69 (3'h1, zi42, zll_main_compute203_outR2);
  assign zi45 = (zi43 == 1'h1) ? zll_main_compute409_outR4 : zll_main_compute203_outR2;
  ZLL_Main_compute372  instR70 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h0, zll_main_compute372_out);
  ZLL_Main_compute372  instR71 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h1, zll_main_compute372_outR1);
  ZLL_Main_compute372  instR72 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h2, zll_main_compute372_outR2);
  ZLL_Main_compute372  instR73 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h3, zll_main_compute372_outR3);
  ZLL_Main_compute372  instR74 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h4, zll_main_compute372_outR4);
  ZLL_Main_compute372  instR75 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h5, zll_main_compute372_outR5);
  ZLL_Main_compute372  instR76 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h6, zll_main_compute372_outR6);
  ZLL_Main_compute372  instR77 (zi45, zi35, 3'h2, 3'h1, zi34, 3'h7, zll_main_compute372_outR7);
  assign zi46 = {zll_main_compute201_out, zll_main_compute201_outR1, zll_main_compute201_outR2, zll_main_compute201_outR3, zll_main_compute201_outR4, zll_main_compute201_outR5, zll_main_compute201_outR6, zll_main_compute201_outR7, {zll_main_compute372_out, zll_main_compute372_outR1, zll_main_compute372_outR2, zll_main_compute372_outR3, zll_main_compute372_outR4, zll_main_compute372_outR5, zll_main_compute372_outR6, zll_main_compute372_outR7}};
  assign zi47 = zi46[127:64];
  assign zi48 = zi46[63:0];
  assign zi49 = {1'h0, {zi47, zi48}};
  assign zi50 = zi49[127:0];
  assign zres = {1'h1, zi50};
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

module ZLL_Main_compute395 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_compute429_out;
  ZLL_Main_compute429  inst (arg0, arg1, zll_main_compute429_out);
  assign res = zll_main_compute429_out;
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

module ZLL_Main_compute203 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_compute429_out;
  ZLL_Main_compute429  inst (arg1, arg0, zll_main_compute429_out);
  assign res = zll_main_compute429_out;
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