module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [127:0] zin;
  logic [63:0] zi0;
  logic [63:0] zi1;
  logic [127:0] zi2;
  logic [63:0] zi3;
  logic [63:0] zi4;
  logic [2:0] zll_main_compute445_out;
  logic [2:0] zll_main_compute386_out;
  logic [2:0] zi5;
  logic [2:0] zll_main_compute445_outR1;
  logic [0:0] zll_main_compute433_out;
  logic [2:0] zll_main_compute244_out;
  logic [2:0] zi6;
  logic [7:0] zll_main_compute209_out;
  logic [7:0] zll_main_compute209_outR1;
  logic [7:0] zll_main_compute209_outR2;
  logic [7:0] zll_main_compute209_outR3;
  logic [7:0] zll_main_compute209_outR4;
  logic [7:0] zll_main_compute209_outR5;
  logic [7:0] zll_main_compute209_outR6;
  logic [7:0] zll_main_compute209_outR7;
  logic [127:0] zi7;
  logic [63:0] zi8;
  logic [63:0] zi9;
  logic [2:0] zll_main_compute445_outR2;
  logic [2:0] zll_main_compute386_outR1;
  logic [2:0] zi10;
  logic [2:0] zll_main_compute445_outR3;
  logic [0:0] zll_main_compute433_outR1;
  logic [0:0] zi11;
  logic [3:0] zi12;
  logic [2:0] zll_main_compute334_out;
  logic [2:0] zll_main_compute445_outR4;
  logic [0:0] zll_main_compute433_outR2;
  logic [2:0] zll_main_compute360_out;
  logic [2:0] zi13;
  logic [7:0] zll_main_compute419_out;
  logic [7:0] zll_main_compute419_outR1;
  logic [7:0] zll_main_compute419_outR2;
  logic [7:0] zll_main_compute419_outR3;
  logic [7:0] zll_main_compute419_outR4;
  logic [7:0] zll_main_compute419_outR5;
  logic [7:0] zll_main_compute419_outR6;
  logic [7:0] zll_main_compute419_outR7;
  logic [127:0] zi14;
  logic [63:0] zi15;
  logic [63:0] zi16;
  logic [127:0] zi17;
  logic [63:0] zi18;
  logic [63:0] zi19;
  logic [2:0] zll_main_compute445_outR5;
  logic [2:0] zll_main_compute386_outR2;
  logic [2:0] zi20;
  logic [2:0] zll_main_compute445_outR6;
  logic [0:0] zll_main_compute433_outR3;
  logic [2:0] zll_main_compute244_outR1;
  logic [2:0] zi21;
  logic [7:0] zll_main_compute399_out;
  logic [7:0] zll_main_compute399_outR1;
  logic [7:0] zll_main_compute399_outR2;
  logic [7:0] zll_main_compute399_outR3;
  logic [7:0] zll_main_compute399_outR4;
  logic [7:0] zll_main_compute399_outR5;
  logic [7:0] zll_main_compute399_outR6;
  logic [7:0] zll_main_compute399_outR7;
  logic [127:0] zi22;
  logic [63:0] zi23;
  logic [63:0] zi24;
  logic [2:0] zll_main_compute445_outR7;
  logic [2:0] zll_main_compute386_outR3;
  logic [2:0] zi25;
  logic [2:0] zll_main_compute445_outR8;
  logic [0:0] zll_main_compute433_outR4;
  logic [0:0] zi26;
  logic [3:0] zi27;
  logic [2:0] zll_main_compute334_outR1;
  logic [2:0] zll_main_compute445_outR9;
  logic [0:0] zll_main_compute433_outR5;
  logic [2:0] zll_main_compute360_outR1;
  logic [2:0] zi28;
  logic [7:0] zll_main_compute447_out;
  logic [7:0] zll_main_compute447_outR1;
  logic [7:0] zll_main_compute447_outR2;
  logic [7:0] zll_main_compute447_outR3;
  logic [7:0] zll_main_compute447_outR4;
  logic [7:0] zll_main_compute447_outR5;
  logic [7:0] zll_main_compute447_outR6;
  logic [7:0] zll_main_compute447_outR7;
  logic [127:0] zi29;
  logic [63:0] zi30;
  logic [63:0] zi31;
  logic [127:0] zi32;
  logic [63:0] zi33;
  logic [63:0] zi34;
  logic [7:0] zll_main_compute13_out;
  logic [7:0] zll_main_compute13_outR1;
  logic [7:0] zll_main_compute13_outR2;
  logic [7:0] zll_main_compute13_outR3;
  logic [7:0] zll_main_compute13_outR4;
  logic [7:0] zll_main_compute13_outR5;
  logic [7:0] zll_main_compute13_outR6;
  logic [7:0] zll_main_compute13_outR7;
  logic [127:0] zi35;
  logic [63:0] zi36;
  logic [63:0] zi37;
  logic [2:0] zll_main_compute445_outR10;
  logic [2:0] zll_main_compute386_outR4;
  logic [2:0] zi38;
  logic [2:0] zll_main_compute445_outR11;
  logic [0:0] zll_main_compute433_outR6;
  logic [0:0] zi39;
  logic [3:0] zi40;
  logic [2:0] zll_main_compute334_outR2;
  logic [2:0] zll_main_compute445_outR12;
  logic [0:0] zll_main_compute433_outR7;
  logic [2:0] zll_main_compute416_out;
  logic [2:0] zi41;
  logic [7:0] zll_main_compute145_out;
  logic [7:0] zll_main_compute145_outR1;
  logic [7:0] zll_main_compute145_outR2;
  logic [7:0] zll_main_compute145_outR3;
  logic [7:0] zll_main_compute145_outR4;
  logic [7:0] zll_main_compute145_outR5;
  logic [7:0] zll_main_compute145_outR6;
  logic [7:0] zll_main_compute145_outR7;
  logic [127:0] zi42;
  logic [63:0] zi43;
  logic [63:0] zi44;
  logic [128:0] zi45;
  logic [127:0] zi46;
  logic [128:0] zres;
  assign zin = {__in0, __in1};
  assign zi0 = zin[127:64];
  assign zi1 = zin[63:0];
  assign zi2 = {zi0, zi1};
  assign zi3 = zi2[127:64];
  assign zi4 = zi2[63:0];
  ZLL_Main_compute445  inst (zi3, zll_main_compute445_out);
  ZLL_Main_compute386  instR1 (zll_main_compute445_out, 3'h2, zll_main_compute386_out);
  assign zi5 = zll_main_compute386_out;
  ZLL_Main_compute445  instR2 (zi3, zll_main_compute445_outR1);
  ZLL_Main_compute433  instR3 (zll_main_compute445_outR1, zll_main_compute433_out);
  ZLL_Main_compute244  instR4 (zi3, zi5, 3'h1, zll_main_compute433_out, zll_main_compute244_out);
  assign zi6 = zll_main_compute244_out;
  ZLL_Main_compute209  instR5 (zi4, zi3, zi6, 3'h2, 3'h0, zll_main_compute209_out);
  ZLL_Main_compute209  instR6 (zi4, zi3, zi6, 3'h2, 3'h1, zll_main_compute209_outR1);
  ZLL_Main_compute209  instR7 (zi4, zi3, zi6, 3'h2, 3'h2, zll_main_compute209_outR2);
  ZLL_Main_compute209  instR8 (zi4, zi3, zi6, 3'h2, 3'h3, zll_main_compute209_outR3);
  ZLL_Main_compute209  instR9 (zi4, zi3, zi6, 3'h2, 3'h4, zll_main_compute209_outR4);
  ZLL_Main_compute209  instR10 (zi4, zi3, zi6, 3'h2, 3'h5, zll_main_compute209_outR5);
  ZLL_Main_compute209  instR11 (zi4, zi3, zi6, 3'h2, 3'h6, zll_main_compute209_outR6);
  ZLL_Main_compute209  instR12 (zi4, zi3, zi6, 3'h2, 3'h7, zll_main_compute209_outR7);
  assign zi7 = {zi0, zi1};
  assign zi8 = zi7[127:64];
  assign zi9 = zi7[63:0];
  ZLL_Main_compute445  instR13 (zi8, zll_main_compute445_outR2);
  ZLL_Main_compute386  instR14 (zll_main_compute445_outR2, 3'h2, zll_main_compute386_outR1);
  assign zi10 = zll_main_compute386_outR1;
  ZLL_Main_compute445  instR15 (zi8, zll_main_compute445_outR3);
  ZLL_Main_compute433  instR16 (zll_main_compute445_outR3, zll_main_compute433_outR1);
  assign zi11 = zll_main_compute433_outR1;
  assign zi12 = {zi10, zi11};
  ZLL_Main_compute334  instR17 (zi12[3:1], zll_main_compute334_out);
  ZLL_Main_compute445  instR18 (zi8, zll_main_compute445_outR4);
  ZLL_Main_compute433  instR19 (zll_main_compute445_outR4, zll_main_compute433_outR2);
  ZLL_Main_compute360  instR20 (3'h1, zi10, zll_main_compute433_outR2, zll_main_compute360_out);
  assign zi13 = (zi12[0] == 1'h1) ? zll_main_compute334_out : zll_main_compute360_out;
  ZLL_Main_compute419  instR21 (3'h1, zi8, 3'h2, zi13, zi9, 3'h0, zll_main_compute419_out);
  ZLL_Main_compute419  instR22 (3'h1, zi8, 3'h2, zi13, zi9, 3'h1, zll_main_compute419_outR1);
  ZLL_Main_compute419  instR23 (3'h1, zi8, 3'h2, zi13, zi9, 3'h2, zll_main_compute419_outR2);
  ZLL_Main_compute419  instR24 (3'h1, zi8, 3'h2, zi13, zi9, 3'h3, zll_main_compute419_outR3);
  ZLL_Main_compute419  instR25 (3'h1, zi8, 3'h2, zi13, zi9, 3'h4, zll_main_compute419_outR4);
  ZLL_Main_compute419  instR26 (3'h1, zi8, 3'h2, zi13, zi9, 3'h5, zll_main_compute419_outR5);
  ZLL_Main_compute419  instR27 (3'h1, zi8, 3'h2, zi13, zi9, 3'h6, zll_main_compute419_outR6);
  ZLL_Main_compute419  instR28 (3'h1, zi8, 3'h2, zi13, zi9, 3'h7, zll_main_compute419_outR7);
  assign zi14 = {{zll_main_compute209_out, zll_main_compute209_outR1, zll_main_compute209_outR2, zll_main_compute209_outR3, zll_main_compute209_outR4, zll_main_compute209_outR5, zll_main_compute209_outR6, zll_main_compute209_outR7}, {zll_main_compute419_out, zll_main_compute419_outR1, zll_main_compute419_outR2, zll_main_compute419_outR3, zll_main_compute419_outR4, zll_main_compute419_outR5, zll_main_compute419_outR6, zll_main_compute419_outR7}};
  assign zi15 = zi14[127:64];
  assign zi16 = zi14[63:0];
  assign zi17 = {zi16, zi15};
  assign zi18 = zi17[127:64];
  assign zi19 = zi17[63:0];
  ZLL_Main_compute445  instR29 (zi18, zll_main_compute445_outR5);
  ZLL_Main_compute386  instR30 (zll_main_compute445_outR5, 3'h2, zll_main_compute386_outR2);
  assign zi20 = zll_main_compute386_outR2;
  ZLL_Main_compute445  instR31 (zi18, zll_main_compute445_outR6);
  ZLL_Main_compute433  instR32 (zll_main_compute445_outR6, zll_main_compute433_outR3);
  ZLL_Main_compute244  instR33 (zi18, zi20, 3'h1, zll_main_compute433_outR3, zll_main_compute244_outR1);
  assign zi21 = zll_main_compute244_outR1;
  ZLL_Main_compute399  instR34 (zi18, zi19, zi21, 3'h2, 3'h0, zll_main_compute399_out);
  ZLL_Main_compute399  instR35 (zi18, zi19, zi21, 3'h2, 3'h1, zll_main_compute399_outR1);
  ZLL_Main_compute399  instR36 (zi18, zi19, zi21, 3'h2, 3'h2, zll_main_compute399_outR2);
  ZLL_Main_compute399  instR37 (zi18, zi19, zi21, 3'h2, 3'h3, zll_main_compute399_outR3);
  ZLL_Main_compute399  instR38 (zi18, zi19, zi21, 3'h2, 3'h4, zll_main_compute399_outR4);
  ZLL_Main_compute399  instR39 (zi18, zi19, zi21, 3'h2, 3'h5, zll_main_compute399_outR5);
  ZLL_Main_compute399  instR40 (zi18, zi19, zi21, 3'h2, 3'h6, zll_main_compute399_outR6);
  ZLL_Main_compute399  instR41 (zi18, zi19, zi21, 3'h2, 3'h7, zll_main_compute399_outR7);
  assign zi22 = {zi16, zi15};
  assign zi23 = zi22[127:64];
  assign zi24 = zi22[63:0];
  ZLL_Main_compute445  instR42 (zi23, zll_main_compute445_outR7);
  ZLL_Main_compute386  instR43 (zll_main_compute445_outR7, 3'h2, zll_main_compute386_outR3);
  assign zi25 = zll_main_compute386_outR3;
  ZLL_Main_compute445  instR44 (zi23, zll_main_compute445_outR8);
  ZLL_Main_compute433  instR45 (zll_main_compute445_outR8, zll_main_compute433_outR4);
  assign zi26 = zll_main_compute433_outR4;
  assign zi27 = {zi25, zi26};
  ZLL_Main_compute334  instR46 (zi27[3:1], zll_main_compute334_outR1);
  ZLL_Main_compute445  instR47 (zi23, zll_main_compute445_outR9);
  ZLL_Main_compute433  instR48 (zll_main_compute445_outR9, zll_main_compute433_outR5);
  ZLL_Main_compute360  instR49 (3'h1, zi25, zll_main_compute433_outR5, zll_main_compute360_outR1);
  assign zi28 = (zi27[0] == 1'h1) ? zll_main_compute334_outR1 : zll_main_compute360_outR1;
  ZLL_Main_compute447  instR50 (3'h2, zi24, zi28, 3'h1, zi23, 3'h0, zll_main_compute447_out);
  ZLL_Main_compute447  instR51 (3'h2, zi24, zi28, 3'h1, zi23, 3'h1, zll_main_compute447_outR1);
  ZLL_Main_compute447  instR52 (3'h2, zi24, zi28, 3'h1, zi23, 3'h2, zll_main_compute447_outR2);
  ZLL_Main_compute447  instR53 (3'h2, zi24, zi28, 3'h1, zi23, 3'h3, zll_main_compute447_outR3);
  ZLL_Main_compute447  instR54 (3'h2, zi24, zi28, 3'h1, zi23, 3'h4, zll_main_compute447_outR4);
  ZLL_Main_compute447  instR55 (3'h2, zi24, zi28, 3'h1, zi23, 3'h5, zll_main_compute447_outR5);
  ZLL_Main_compute447  instR56 (3'h2, zi24, zi28, 3'h1, zi23, 3'h6, zll_main_compute447_outR6);
  ZLL_Main_compute447  instR57 (3'h2, zi24, zi28, 3'h1, zi23, 3'h7, zll_main_compute447_outR7);
  assign zi29 = {{zll_main_compute399_out, zll_main_compute399_outR1, zll_main_compute399_outR2, zll_main_compute399_outR3, zll_main_compute399_outR4, zll_main_compute399_outR5, zll_main_compute399_outR6, zll_main_compute399_outR7}, {zll_main_compute447_out, zll_main_compute447_outR1, zll_main_compute447_outR2, zll_main_compute447_outR3, zll_main_compute447_outR4, zll_main_compute447_outR5, zll_main_compute447_outR6, zll_main_compute447_outR7}};
  assign zi30 = zi29[127:64];
  assign zi31 = zi29[63:0];
  assign zi32 = {zi30, zi31};
  assign zi33 = zi32[127:64];
  assign zi34 = zi32[63:0];
  ZLL_Main_compute13  instR58 (zi33, zi34, 3'h1, 3'h2, 3'h0, zll_main_compute13_out);
  ZLL_Main_compute13  instR59 (zi33, zi34, 3'h1, 3'h2, 3'h1, zll_main_compute13_outR1);
  ZLL_Main_compute13  instR60 (zi33, zi34, 3'h1, 3'h2, 3'h2, zll_main_compute13_outR2);
  ZLL_Main_compute13  instR61 (zi33, zi34, 3'h1, 3'h2, 3'h3, zll_main_compute13_outR3);
  ZLL_Main_compute13  instR62 (zi33, zi34, 3'h1, 3'h2, 3'h4, zll_main_compute13_outR4);
  ZLL_Main_compute13  instR63 (zi33, zi34, 3'h1, 3'h2, 3'h5, zll_main_compute13_outR5);
  ZLL_Main_compute13  instR64 (zi33, zi34, 3'h1, 3'h2, 3'h6, zll_main_compute13_outR6);
  ZLL_Main_compute13  instR65 (zi33, zi34, 3'h1, 3'h2, 3'h7, zll_main_compute13_outR7);
  assign zi35 = {zi30, zi31};
  assign zi36 = zi35[127:64];
  assign zi37 = zi35[63:0];
  ZLL_Main_compute445  instR66 (zi36, zll_main_compute445_outR10);
  ZLL_Main_compute386  instR67 (zll_main_compute445_outR10, 3'h2, zll_main_compute386_outR4);
  assign zi38 = zll_main_compute386_outR4;
  ZLL_Main_compute445  instR68 (zi36, zll_main_compute445_outR11);
  ZLL_Main_compute433  instR69 (zll_main_compute445_outR11, zll_main_compute433_outR6);
  assign zi39 = zll_main_compute433_outR6;
  assign zi40 = {zi38, zi39};
  ZLL_Main_compute334  instR70 (zi40[3:1], zll_main_compute334_outR2);
  ZLL_Main_compute445  instR71 (zi36, zll_main_compute445_outR12);
  ZLL_Main_compute433  instR72 (zll_main_compute445_outR12, zll_main_compute433_outR7);
  ZLL_Main_compute416  instR73 (zi38, 3'h1, zll_main_compute433_outR7, zll_main_compute416_out);
  assign zi41 = (zi40[0] == 1'h1) ? zll_main_compute334_outR2 : zll_main_compute416_out;
  ZLL_Main_compute145  instR74 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h0, zll_main_compute145_out);
  ZLL_Main_compute145  instR75 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h1, zll_main_compute145_outR1);
  ZLL_Main_compute145  instR76 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h2, zll_main_compute145_outR2);
  ZLL_Main_compute145  instR77 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h3, zll_main_compute145_outR3);
  ZLL_Main_compute145  instR78 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h4, zll_main_compute145_outR4);
  ZLL_Main_compute145  instR79 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h5, zll_main_compute145_outR5);
  ZLL_Main_compute145  instR80 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h6, zll_main_compute145_outR6);
  ZLL_Main_compute145  instR81 (zi41, 3'h2, zi36, zi37, 3'h1, 3'h7, zll_main_compute145_outR7);
  assign zi42 = {{zll_main_compute13_out, zll_main_compute13_outR1, zll_main_compute13_outR2, zll_main_compute13_outR3, zll_main_compute13_outR4, zll_main_compute13_outR5, zll_main_compute13_outR6, zll_main_compute13_outR7}, {zll_main_compute145_out, zll_main_compute145_outR1, zll_main_compute145_outR2, zll_main_compute145_outR3, zll_main_compute145_outR4, zll_main_compute145_outR5, zll_main_compute145_outR6, zll_main_compute145_outR7}};
  assign zi43 = zi42[127:64];
  assign zi44 = zi42[63:0];
  assign zi45 = {1'h0, {zi43, zi44}};
  assign zi46 = zi45[127:0];
  assign zres = {1'h1, zi46};
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute451 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [5:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zi2;
  logic [127:0] slice_in;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[5:3];
  assign zi2 = zi0[2:0];
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, zi1} + {{7'h7d{1'h0}}, zi2}) : (({{7'h7d{1'h0}}, zi1} + {{7'h7d{1'h0}}, zi2}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute447 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute409_out;
  logic [0:0] zi0;
  logic [73:0] zi1;
  logic [2:0] zi2;
  logic [2:0] zi3;
  logic [2:0] zi4;
  logic [63:0] zi5;
  logic [2:0] zll_main_compute430_out;
  logic [2:0] zll_main_compute451_out;
  logic [63:0] slice_in;
  logic [0:0] zll_main_compute409_outR1;
  logic [0:0] zi6;
  logic [76:0] zi7;
  logic [2:0] zi8;
  logic [2:0] zi9;
  logic [63:0] zi10;
  logic [2:0] zi11;
  logic [2:0] zi12;
  logic [2:0] zll_main_compute441_out;
  logic [2:0] zll_main_compute430_outR1;
  logic [2:0] zll_main_compute451_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute409  inst (arg5, arg2, zll_main_compute409_out);
  assign zi0 = zll_main_compute409_out;
  assign zi1 = {arg5, arg0, arg3, arg4, zi0};
  assign zi2 = zi1[73:71];
  assign zi3 = zi1[70:68];
  assign zi4 = zi1[67:65];
  assign zi5 = zi1[64:1];
  ZLL_Main_compute430  instR1 (zi2, zi3, zll_main_compute430_out);
  ZLL_Main_compute451  instR2 (zll_main_compute430_out, zi4, zll_main_compute451_out);
  assign slice_in = zi5 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute451_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute409  instR3 (arg5, arg2, zll_main_compute409_outR1);
  assign zi6 = zll_main_compute409_outR1;
  assign zi7 = {arg5, arg0, arg1, arg2, arg3, zi6};
  assign zi8 = zi7[76:74];
  assign zi9 = zi7[73:71];
  assign zi10 = zi7[70:7];
  assign zi11 = zi7[6:4];
  assign zi12 = zi7[3:1];
  ZLL_Main_compute441  instR4 (zi8, zi11, zll_main_compute441_out);
  ZLL_Main_compute430  instR5 (zll_main_compute441_out, zi9, zll_main_compute430_outR1);
  ZLL_Main_compute451  instR6 (zll_main_compute430_outR1, zi12, zll_main_compute451_outR1);
  assign slice_inR1 = zi10 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute451_outR1}) - 128'h1) * 128'h8);
  assign res = (zi1[0] == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute445 (input logic [63:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_compute441 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [5:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zi2;
  logic [127:0] slice_in;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[5:3];
  assign zi2 = zi0[2:0];
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, zi1} - {{7'h7d{1'h0}}, zi2}) : (({{7'h7d{1'h0}}, zi1} - {{7'h7d{1'h0}}, zi2}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute433 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute430 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [5:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zi2;
  logic [127:0] slice_in;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[5:3];
  assign zi2 = zi0[2:0];
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, zi1} * {{7'h7d{1'h0}}, zi2}) : (({{7'h7d{1'h0}}, zi1} * {{7'h7d{1'h0}}, zi2}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute419 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute409_out;
  logic [0:0] zi0;
  logic [73:0] zi1;
  logic [2:0] zi2;
  logic [63:0] zi3;
  logic [2:0] zi4;
  logic [2:0] zi5;
  logic [2:0] zll_main_compute430_out;
  logic [2:0] zll_main_compute451_out;
  logic [63:0] slice_in;
  logic [0:0] zll_main_compute409_outR1;
  logic [0:0] zi6;
  logic [76:0] zi7;
  logic [2:0] zi8;
  logic [2:0] zi9;
  logic [2:0] zi10;
  logic [2:0] zi11;
  logic [63:0] zi12;
  logic [2:0] zll_main_compute441_out;
  logic [2:0] zll_main_compute430_outR1;
  logic [2:0] zll_main_compute451_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute409  inst (arg5, arg3, zll_main_compute409_out);
  assign zi0 = zll_main_compute409_out;
  assign zi1 = {arg0, arg1, arg2, arg5, zi0};
  assign zi2 = zi1[73:71];
  assign zi3 = zi1[70:7];
  assign zi4 = zi1[6:4];
  assign zi5 = zi1[3:1];
  ZLL_Main_compute430  instR1 (zi5, zi4, zll_main_compute430_out);
  ZLL_Main_compute451  instR2 (zll_main_compute430_out, zi2, zll_main_compute451_out);
  assign slice_in = zi3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute451_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute409  instR3 (arg5, arg3, zll_main_compute409_outR1);
  assign zi6 = zll_main_compute409_outR1;
  assign zi7 = {arg0, arg2, arg5, arg3, arg4, zi6};
  assign zi8 = zi7[76:74];
  assign zi9 = zi7[73:71];
  assign zi10 = zi7[70:68];
  assign zi11 = zi7[67:65];
  assign zi12 = zi7[64:1];
  ZLL_Main_compute441  instR4 (zi10, zi11, zll_main_compute441_out);
  ZLL_Main_compute430  instR5 (zll_main_compute441_out, zi9, zll_main_compute430_outR1);
  ZLL_Main_compute451  instR6 (zll_main_compute430_outR1, zi8, zll_main_compute451_outR1);
  assign slice_inR1 = zi12 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute451_outR1}) - 128'h1) * 128'h8);
  assign res = (zi1[0] == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute416 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [6:0] zt0;
  logic [2:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zll_main_compute451_out;
  assign zt0 = {arg0, arg1, arg2};
  assign zi0 = zt0[6:4];
  assign zi1 = zt0[3:1];
  ZLL_Main_compute451  inst (zi0, zi1, zll_main_compute451_out);
  assign res = zll_main_compute451_out;
endmodule

module ZLL_Main_compute409 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  logic [5:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zi2;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[5:3];
  assign zi2 = zi0[2:0];
  assign res = {{7'h7d{1'h0}}, zi1} < {{7'h7d{1'h0}}, zi2};
endmodule

module ZLL_Main_compute399 (input logic [63:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute409_out;
  logic [0:0] zi0;
  logic [70:0] zi1;
  logic [7:0] zll_main_compute187_out;
  logic [0:0] zll_main_compute409_outR1;
  logic [0:0] zi2;
  logic [73:0] zi3;
  logic [63:0] zi4;
  logic [2:0] zi5;
  logic [2:0] zi6;
  logic [2:0] zi7;
  logic [2:0] zll_main_compute441_out;
  logic [2:0] zll_main_compute430_out;
  logic [63:0] slice_in;
  ZLL_Main_compute409  inst (arg4, arg2, zll_main_compute409_out);
  assign zi0 = zll_main_compute409_out;
  assign zi1 = {arg0, arg4, arg3, zi0};
  ZLL_Main_compute187  instR1 (zi1[70:7], zi1[6:4], zi1[3:1], zll_main_compute187_out);
  ZLL_Main_compute409  instR2 (arg4, arg2, zll_main_compute409_outR1);
  assign zi2 = zll_main_compute409_outR1;
  assign zi3 = {arg1, arg4, arg2, arg3, zi2};
  assign zi4 = zi3[73:10];
  assign zi5 = zi3[9:7];
  assign zi6 = zi3[6:4];
  assign zi7 = zi3[3:1];
  ZLL_Main_compute441  instR3 (zi5, zi6, zll_main_compute441_out);
  ZLL_Main_compute430  instR4 (zll_main_compute441_out, zi7, zll_main_compute430_out);
  assign slice_in = zi4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute430_out}) - 128'h1) * 128'h8);
  assign res = (zi1[0] == 1'h1) ? zll_main_compute187_out : slice_in[7:0];
endmodule

module ZLL_Main_compute386 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [5:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zi2;
  logic [127:0] slice_in;
  assign zi0 = {arg0, arg1};
  assign zi1 = zi0[5:3];
  assign zi2 = zi0[2:0];
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, zi2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, zi1} / {{7'h7d{1'h0}}, zi2})) : ((({{7'h7d{1'h0}}, zi2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, zi1} / {{7'h7d{1'h0}}, zi2})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute360 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [6:0] zt0;
  logic [2:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zll_main_compute451_out;
  assign zt0 = {arg0, arg1, arg2};
  assign zi0 = zt0[6:4];
  assign zi1 = zt0[3:1];
  ZLL_Main_compute451  inst (zi1, zi0, zll_main_compute451_out);
  assign res = zll_main_compute451_out;
endmodule

module ZLL_Main_compute334 (input logic [2:0] arg0,
  output logic [2:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_compute244 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [0:0] arg3,
  output logic [2:0] res);
  logic [3:0] zt0;
  logic [2:0] zll_main_compute334_out;
  logic [2:0] zll_main_compute445_out;
  logic [0:0] zll_main_compute433_out;
  logic [2:0] zll_main_compute416_out;
  assign zt0 = {arg1, arg3};
  ZLL_Main_compute334  inst (zt0[3:1], zll_main_compute334_out);
  ZLL_Main_compute445  instR1 (arg0, zll_main_compute445_out);
  ZLL_Main_compute433  instR2 (zll_main_compute445_out, zll_main_compute433_out);
  ZLL_Main_compute416  instR3 (arg1, arg2, zll_main_compute433_out, zll_main_compute416_out);
  assign res = (zt0[0] == 1'h1) ? zll_main_compute334_out : zll_main_compute416_out;
endmodule

module ZLL_Main_compute209 (input logic [63:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute409_out;
  logic [0:0] zi0;
  logic [70:0] zi1;
  logic [7:0] zll_main_compute187_out;
  logic [0:0] zll_main_compute409_outR1;
  logic [0:0] zi2;
  logic [73:0] zi3;
  logic [63:0] zi4;
  logic [2:0] zi5;
  logic [2:0] zi6;
  logic [2:0] zi7;
  logic [2:0] zll_main_compute441_out;
  logic [2:0] zll_main_compute430_out;
  logic [63:0] slice_in;
  ZLL_Main_compute409  inst (arg4, arg2, zll_main_compute409_out);
  assign zi0 = zll_main_compute409_out;
  assign zi1 = {arg1, arg4, arg3, zi0};
  ZLL_Main_compute187  instR1 (zi1[70:7], zi1[6:4], zi1[3:1], zll_main_compute187_out);
  ZLL_Main_compute409  instR2 (arg4, arg2, zll_main_compute409_outR1);
  assign zi2 = zll_main_compute409_outR1;
  assign zi3 = {arg0, arg2, arg4, arg3, zi2};
  assign zi4 = zi3[73:10];
  assign zi5 = zi3[9:7];
  assign zi6 = zi3[6:4];
  assign zi7 = zi3[3:1];
  ZLL_Main_compute441  instR3 (zi6, zi5, zll_main_compute441_out);
  ZLL_Main_compute430  instR4 (zll_main_compute441_out, zi7, zll_main_compute430_out);
  assign slice_in = zi4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute430_out}) - 128'h1) * 128'h8);
  assign res = (zi1[0] == 1'h1) ? zll_main_compute187_out : slice_in[7:0];
endmodule

module ZLL_Main_compute187 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  output logic [7:0] res);
  logic [2:0] zll_main_compute430_out;
  logic [63:0] slice_in;
  ZLL_Main_compute430  inst (arg1, arg2, zll_main_compute430_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute430_out}) - 128'h1) * 128'h8);
  assign res = slice_in[7:0];
endmodule

module ZLL_Main_compute145 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute433_out;
  logic [0:0] zi0;
  logic [73:0] zi1;
  logic [2:0] zi2;
  logic [2:0] zi3;
  logic [63:0] zi4;
  logic [2:0] zi5;
  logic [2:0] zll_main_compute386_out;
  logic [2:0] zll_main_compute451_out;
  logic [63:0] slice_in;
  logic [0:0] zll_main_compute433_outR1;
  logic [0:0] zi6;
  logic [76:0] zi7;
  logic [2:0] zi8;
  logic [2:0] zi9;
  logic [2:0] zi10;
  logic [63:0] zi11;
  logic [2:0] zi12;
  logic [2:0] zll_main_compute441_out;
  logic [2:0] zll_main_compute386_outR1;
  logic [2:0] zll_main_compute451_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute433  inst (arg5, zll_main_compute433_out);
  assign zi0 = zll_main_compute433_out;
  assign zi1 = {arg0, arg1, arg2, arg5, zi0};
  assign zi2 = zi1[73:71];
  assign zi3 = zi1[70:68];
  assign zi4 = zi1[67:4];
  assign zi5 = zi1[3:1];
  ZLL_Main_compute386  instR1 (zi5, zi3, zll_main_compute386_out);
  ZLL_Main_compute451  instR2 (zi2, zll_main_compute386_out, zll_main_compute451_out);
  assign slice_in = zi4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute451_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute433  instR3 (arg5, zll_main_compute433_outR1);
  assign zi6 = zll_main_compute433_outR1;
  assign zi7 = {arg0, arg1, arg5, arg3, arg4, zi6};
  assign zi8 = zi7[76:74];
  assign zi9 = zi7[73:71];
  assign zi10 = zi7[70:68];
  assign zi11 = zi7[67:4];
  assign zi12 = zi7[3:1];
  ZLL_Main_compute441  instR4 (zi10, zi12, zll_main_compute441_out);
  ZLL_Main_compute386  instR5 (zll_main_compute441_out, zi9, zll_main_compute386_outR1);
  ZLL_Main_compute451  instR6 (zi8, zll_main_compute386_outR1, zll_main_compute451_outR1);
  assign slice_inR1 = zi11 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute451_outR1}) - 128'h1) * 128'h8);
  assign res = (zi1[0] == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute13 (input logic [63:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute433_out;
  logic [0:0] zi0;
  logic [70:0] zi1;
  logic [63:0] zi2;
  logic [2:0] zi3;
  logic [2:0] zi4;
  logic [2:0] zll_main_compute386_out;
  logic [63:0] slice_in;
  logic [0:0] zll_main_compute433_outR1;
  logic [0:0] zi5;
  logic [73:0] zi6;
  logic [2:0] zi7;
  logic [63:0] zi8;
  logic [2:0] zi9;
  logic [2:0] zi10;
  logic [2:0] zll_main_compute441_out;
  logic [2:0] zll_main_compute386_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute433  inst (arg4, zll_main_compute433_out);
  assign zi0 = zll_main_compute433_out;
  assign zi1 = {arg0, arg4, arg3, zi0};
  assign zi2 = zi1[70:7];
  assign zi3 = zi1[6:4];
  assign zi4 = zi1[3:1];
  ZLL_Main_compute386  instR1 (zi3, zi4, zll_main_compute386_out);
  assign slice_in = zi2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute386_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute433  instR2 (arg4, zll_main_compute433_outR1);
  assign zi5 = zll_main_compute433_outR1;
  assign zi6 = {arg4, arg1, arg2, arg3, zi5};
  assign zi7 = zi6[73:71];
  assign zi8 = zi6[70:7];
  assign zi9 = zi6[6:4];
  assign zi10 = zi6[3:1];
  ZLL_Main_compute441  instR3 (zi7, zi9, zll_main_compute441_out);
  ZLL_Main_compute386  instR4 (zll_main_compute441_out, zi10, zll_main_compute386_outR1);
  assign slice_inR1 = zi8 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute386_outR1}) - 128'h1) * 128'h8);
  assign res = (zi1[0] == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule