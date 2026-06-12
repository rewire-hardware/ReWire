module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [2:0] zll_main_compute437_out;
  logic [2:0] zll_main_compute396_out;
  logic [2:0] zi5;
  logic [2:0] zll_main_compute437_outR1;
  logic [0:0] zll_main_compute403_out;
  logic [2:0] zll_main_compute282_out;
  logic [2:0] zi6;
  logic [7:0] zll_main_compute85_out;
  logic [7:0] zll_main_compute85_outR1;
  logic [7:0] zll_main_compute85_outR2;
  logic [7:0] zll_main_compute85_outR3;
  logic [7:0] zll_main_compute85_outR4;
  logic [7:0] zll_main_compute85_outR5;
  logic [7:0] zll_main_compute85_outR6;
  logic [7:0] zll_main_compute85_outR7;
  logic [2:0] zll_main_compute437_outR2;
  logic [2:0] zll_main_compute396_outR1;
  logic [2:0] zi10;
  logic [2:0] zll_main_compute437_outR3;
  logic [0:0] zll_main_compute403_outR1;
  logic [0:0] zi11;
  logic [2:0] zll_main_compute417_out;
  logic [2:0] zll_main_compute437_outR4;
  logic [0:0] zll_main_compute403_outR2;
  logic [2:0] zll_main_compute391_out;
  logic [2:0] zi12;
  logic [7:0] zll_main_compute346_out;
  logic [7:0] zll_main_compute346_outR1;
  logic [7:0] zll_main_compute346_outR2;
  logic [7:0] zll_main_compute346_outR3;
  logic [7:0] zll_main_compute346_outR4;
  logic [7:0] zll_main_compute346_outR5;
  logic [7:0] zll_main_compute346_outR6;
  logic [7:0] zll_main_compute346_outR7;
  logic [127:0] zi13;
  logic [63:0] zi14;
  logic [63:0] zi15;
  logic [2:0] zll_main_compute437_outR5;
  logic [2:0] zll_main_compute396_outR2;
  logic [2:0] zi19;
  logic [2:0] zll_main_compute437_outR6;
  logic [0:0] zll_main_compute403_outR3;
  logic [0:0] zi20;
  logic [2:0] zll_main_compute417_outR1;
  logic [2:0] zll_main_compute437_outR7;
  logic [0:0] zll_main_compute403_outR4;
  logic [2:0] zll_main_compute391_outR1;
  logic [2:0] zi21;
  logic [7:0] zll_main_compute190_out;
  logic [7:0] zll_main_compute190_outR1;
  logic [7:0] zll_main_compute190_outR2;
  logic [7:0] zll_main_compute190_outR3;
  logic [7:0] zll_main_compute190_outR4;
  logic [7:0] zll_main_compute190_outR5;
  logic [7:0] zll_main_compute190_outR6;
  logic [7:0] zll_main_compute190_outR7;
  logic [2:0] zll_main_compute437_outR8;
  logic [2:0] zll_main_compute396_outR3;
  logic [2:0] zi25;
  logic [2:0] zll_main_compute437_outR9;
  logic [0:0] zll_main_compute403_outR5;
  logic [2:0] zll_main_compute282_outR1;
  logic [2:0] zi26;
  logic [7:0] zll_main_compute53_out;
  logic [7:0] zll_main_compute53_outR1;
  logic [7:0] zll_main_compute53_outR2;
  logic [7:0] zll_main_compute53_outR3;
  logic [7:0] zll_main_compute53_outR4;
  logic [7:0] zll_main_compute53_outR5;
  logic [7:0] zll_main_compute53_outR6;
  logic [7:0] zll_main_compute53_outR7;
  logic [127:0] zi27;
  logic [63:0] zi28;
  logic [63:0] zi29;
  logic [7:0] zll_main_compute175_out;
  logic [7:0] zll_main_compute175_outR1;
  logic [7:0] zll_main_compute175_outR2;
  logic [7:0] zll_main_compute175_outR3;
  logic [7:0] zll_main_compute175_outR4;
  logic [7:0] zll_main_compute175_outR5;
  logic [7:0] zll_main_compute175_outR6;
  logic [7:0] zll_main_compute175_outR7;
  logic [2:0] zll_main_compute437_outR10;
  logic [2:0] zll_main_compute396_outR4;
  logic [2:0] zi36;
  logic [2:0] zll_main_compute437_outR11;
  logic [0:0] zll_main_compute403_outR6;
  logic [0:0] zi37;
  logic [2:0] zll_main_compute417_outR2;
  logic [2:0] zll_main_compute437_outR12;
  logic [0:0] zll_main_compute403_outR7;
  logic [2:0] zll_main_compute391_outR2;
  logic [2:0] zi38;
  logic [7:0] zll_main_compute452_out;
  logic [7:0] zll_main_compute452_outR1;
  logic [7:0] zll_main_compute452_outR2;
  logic [7:0] zll_main_compute452_outR3;
  logic [7:0] zll_main_compute452_outR4;
  logic [7:0] zll_main_compute452_outR5;
  logic [7:0] zll_main_compute452_outR6;
  logic [7:0] zll_main_compute452_outR7;
  logic [127:0] zi39;
  logic [63:0] zi40;
  logic [63:0] zi41;
  logic [128:0] zi42;
  logic [127:0] zi43;
  logic [128:0] zres;
  ZLL_Main_compute437  inst (__in0, zll_main_compute437_out);
  ZLL_Main_compute396  instR1 (zll_main_compute437_out, 3'h2, zll_main_compute396_out);
  assign zi5 = zll_main_compute396_out;
  ZLL_Main_compute437  instR2 (__in0, zll_main_compute437_outR1);
  ZLL_Main_compute403  instR3 (zll_main_compute437_outR1, zll_main_compute403_out);
  ZLL_Main_compute282  instR4 (zi5, __in0, 3'h1, zll_main_compute403_out, zll_main_compute282_out);
  assign zi6 = zll_main_compute282_out;
  ZLL_Main_compute85  instR5 (zi6, __in1, 3'h2, __in0, 3'h0, zll_main_compute85_out);
  ZLL_Main_compute85  instR6 (zi6, __in1, 3'h2, __in0, 3'h1, zll_main_compute85_outR1);
  ZLL_Main_compute85  instR7 (zi6, __in1, 3'h2, __in0, 3'h2, zll_main_compute85_outR2);
  ZLL_Main_compute85  instR8 (zi6, __in1, 3'h2, __in0, 3'h3, zll_main_compute85_outR3);
  ZLL_Main_compute85  instR9 (zi6, __in1, 3'h2, __in0, 3'h4, zll_main_compute85_outR4);
  ZLL_Main_compute85  instR10 (zi6, __in1, 3'h2, __in0, 3'h5, zll_main_compute85_outR5);
  ZLL_Main_compute85  instR11 (zi6, __in1, 3'h2, __in0, 3'h6, zll_main_compute85_outR6);
  ZLL_Main_compute85  instR12 (zi6, __in1, 3'h2, __in0, 3'h7, zll_main_compute85_outR7);
  ZLL_Main_compute437  instR13 (__in0, zll_main_compute437_outR2);
  ZLL_Main_compute396  instR14 (zll_main_compute437_outR2, 3'h2, zll_main_compute396_outR1);
  assign zi10 = zll_main_compute396_outR1;
  ZLL_Main_compute437  instR15 (__in0, zll_main_compute437_outR3);
  ZLL_Main_compute403  instR16 (zll_main_compute437_outR3, zll_main_compute403_outR1);
  assign zi11 = zll_main_compute403_outR1;
  ZLL_Main_compute417  instR17 (zi10, zll_main_compute417_out);
  ZLL_Main_compute437  instR18 (__in0, zll_main_compute437_outR4);
  ZLL_Main_compute403  instR19 (zll_main_compute437_outR4, zll_main_compute403_outR2);
  ZLL_Main_compute391  instR20 (3'h1, zi10, zll_main_compute403_outR2, zll_main_compute391_out);
  assign zi12 = (zi11 == 1'h1) ? zll_main_compute417_out : zll_main_compute391_out;
  ZLL_Main_compute346  instR21 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h0, zll_main_compute346_out);
  ZLL_Main_compute346  instR22 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h1, zll_main_compute346_outR1);
  ZLL_Main_compute346  instR23 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h2, zll_main_compute346_outR2);
  ZLL_Main_compute346  instR24 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h3, zll_main_compute346_outR3);
  ZLL_Main_compute346  instR25 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h4, zll_main_compute346_outR4);
  ZLL_Main_compute346  instR26 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h5, zll_main_compute346_outR5);
  ZLL_Main_compute346  instR27 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h6, zll_main_compute346_outR6);
  ZLL_Main_compute346  instR28 (__in0, __in1, 3'h1, zi12, 3'h2, 3'h7, zll_main_compute346_outR7);
  assign zi13 = {{zll_main_compute85_out, zll_main_compute85_outR1, zll_main_compute85_outR2, zll_main_compute85_outR3, zll_main_compute85_outR4, zll_main_compute85_outR5, zll_main_compute85_outR6, zll_main_compute85_outR7}, {zll_main_compute346_out, zll_main_compute346_outR1, zll_main_compute346_outR2, zll_main_compute346_outR3, zll_main_compute346_outR4, zll_main_compute346_outR5, zll_main_compute346_outR6, zll_main_compute346_outR7}};
  assign zi14 = zi13[127:64];
  assign zi15 = zi13[63:0];
  ZLL_Main_compute437  instR29 (zi15, zll_main_compute437_outR5);
  ZLL_Main_compute396  instR30 (zll_main_compute437_outR5, 3'h2, zll_main_compute396_outR2);
  assign zi19 = zll_main_compute396_outR2;
  ZLL_Main_compute437  instR31 (zi15, zll_main_compute437_outR6);
  ZLL_Main_compute403  instR32 (zll_main_compute437_outR6, zll_main_compute403_outR3);
  assign zi20 = zll_main_compute403_outR3;
  ZLL_Main_compute417  instR33 (zi19, zll_main_compute417_outR1);
  ZLL_Main_compute437  instR34 (zi15, zll_main_compute437_outR7);
  ZLL_Main_compute403  instR35 (zll_main_compute437_outR7, zll_main_compute403_outR4);
  ZLL_Main_compute391  instR36 (3'h1, zi19, zll_main_compute403_outR4, zll_main_compute391_outR1);
  assign zi21 = (zi20 == 1'h1) ? zll_main_compute417_outR1 : zll_main_compute391_outR1;
  ZLL_Main_compute190  instR37 (zi21, 3'h2, zi14, zi15, 3'h0, zll_main_compute190_out);
  ZLL_Main_compute190  instR38 (zi21, 3'h2, zi14, zi15, 3'h1, zll_main_compute190_outR1);
  ZLL_Main_compute190  instR39 (zi21, 3'h2, zi14, zi15, 3'h2, zll_main_compute190_outR2);
  ZLL_Main_compute190  instR40 (zi21, 3'h2, zi14, zi15, 3'h3, zll_main_compute190_outR3);
  ZLL_Main_compute190  instR41 (zi21, 3'h2, zi14, zi15, 3'h4, zll_main_compute190_outR4);
  ZLL_Main_compute190  instR42 (zi21, 3'h2, zi14, zi15, 3'h5, zll_main_compute190_outR5);
  ZLL_Main_compute190  instR43 (zi21, 3'h2, zi14, zi15, 3'h6, zll_main_compute190_outR6);
  ZLL_Main_compute190  instR44 (zi21, 3'h2, zi14, zi15, 3'h7, zll_main_compute190_outR7);
  ZLL_Main_compute437  instR45 (zi15, zll_main_compute437_outR8);
  ZLL_Main_compute396  instR46 (zll_main_compute437_outR8, 3'h2, zll_main_compute396_outR3);
  assign zi25 = zll_main_compute396_outR3;
  ZLL_Main_compute437  instR47 (zi15, zll_main_compute437_outR9);
  ZLL_Main_compute403  instR48 (zll_main_compute437_outR9, zll_main_compute403_outR5);
  ZLL_Main_compute282  instR49 (zi25, zi15, 3'h1, zll_main_compute403_outR5, zll_main_compute282_outR1);
  assign zi26 = zll_main_compute282_outR1;
  ZLL_Main_compute53  instR50 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h0, zll_main_compute53_out);
  ZLL_Main_compute53  instR51 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h1, zll_main_compute53_outR1);
  ZLL_Main_compute53  instR52 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h2, zll_main_compute53_outR2);
  ZLL_Main_compute53  instR53 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h3, zll_main_compute53_outR3);
  ZLL_Main_compute53  instR54 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h4, zll_main_compute53_outR4);
  ZLL_Main_compute53  instR55 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h5, zll_main_compute53_outR5);
  ZLL_Main_compute53  instR56 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h6, zll_main_compute53_outR6);
  ZLL_Main_compute53  instR57 (zi26, zi15, 3'h1, 3'h2, zi14, 3'h7, zll_main_compute53_outR7);
  assign zi27 = {{zll_main_compute190_out, zll_main_compute190_outR1, zll_main_compute190_outR2, zll_main_compute190_outR3, zll_main_compute190_outR4, zll_main_compute190_outR5, zll_main_compute190_outR6, zll_main_compute190_outR7}, {zll_main_compute53_out, zll_main_compute53_outR1, zll_main_compute53_outR2, zll_main_compute53_outR3, zll_main_compute53_outR4, zll_main_compute53_outR5, zll_main_compute53_outR6, zll_main_compute53_outR7}};
  assign zi28 = zi27[127:64];
  assign zi29 = zi27[63:0];
  ZLL_Main_compute175  instR58 (zi29, 3'h2, zi28, 3'h1, 3'h0, zll_main_compute175_out);
  ZLL_Main_compute175  instR59 (zi29, 3'h2, zi28, 3'h1, 3'h1, zll_main_compute175_outR1);
  ZLL_Main_compute175  instR60 (zi29, 3'h2, zi28, 3'h1, 3'h2, zll_main_compute175_outR2);
  ZLL_Main_compute175  instR61 (zi29, 3'h2, zi28, 3'h1, 3'h3, zll_main_compute175_outR3);
  ZLL_Main_compute175  instR62 (zi29, 3'h2, zi28, 3'h1, 3'h4, zll_main_compute175_outR4);
  ZLL_Main_compute175  instR63 (zi29, 3'h2, zi28, 3'h1, 3'h5, zll_main_compute175_outR5);
  ZLL_Main_compute175  instR64 (zi29, 3'h2, zi28, 3'h1, 3'h6, zll_main_compute175_outR6);
  ZLL_Main_compute175  instR65 (zi29, 3'h2, zi28, 3'h1, 3'h7, zll_main_compute175_outR7);
  ZLL_Main_compute437  instR66 (zi28, zll_main_compute437_outR10);
  ZLL_Main_compute396  instR67 (zll_main_compute437_outR10, 3'h2, zll_main_compute396_outR4);
  assign zi36 = zll_main_compute396_outR4;
  ZLL_Main_compute437  instR68 (zi28, zll_main_compute437_outR11);
  ZLL_Main_compute403  instR69 (zll_main_compute437_outR11, zll_main_compute403_outR6);
  assign zi37 = zll_main_compute403_outR6;
  ZLL_Main_compute417  instR70 (zi36, zll_main_compute417_outR2);
  ZLL_Main_compute437  instR71 (zi28, zll_main_compute437_outR12);
  ZLL_Main_compute403  instR72 (zll_main_compute437_outR12, zll_main_compute403_outR7);
  ZLL_Main_compute391  instR73 (3'h1, zi36, zll_main_compute403_outR7, zll_main_compute391_outR2);
  assign zi38 = (zi37 == 1'h1) ? zll_main_compute417_outR2 : zll_main_compute391_outR2;
  ZLL_Main_compute452  instR74 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h0, zll_main_compute452_out);
  ZLL_Main_compute452  instR75 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h1, zll_main_compute452_outR1);
  ZLL_Main_compute452  instR76 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h2, zll_main_compute452_outR2);
  ZLL_Main_compute452  instR77 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h3, zll_main_compute452_outR3);
  ZLL_Main_compute452  instR78 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h4, zll_main_compute452_outR4);
  ZLL_Main_compute452  instR79 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h5, zll_main_compute452_outR5);
  ZLL_Main_compute452  instR80 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h6, zll_main_compute452_outR6);
  ZLL_Main_compute452  instR81 (zi29, 3'h1, zi38, 3'h2, zi28, 3'h7, zll_main_compute452_outR7);
  assign zi39 = {zll_main_compute175_out, zll_main_compute175_outR1, zll_main_compute175_outR2, zll_main_compute175_outR3, zll_main_compute175_outR4, zll_main_compute175_outR5, zll_main_compute175_outR6, zll_main_compute175_outR7, {zll_main_compute452_out, zll_main_compute452_outR1, zll_main_compute452_outR2, zll_main_compute452_outR3, zll_main_compute452_outR4, zll_main_compute452_outR5, zll_main_compute452_outR6, zll_main_compute452_outR7}};
  assign zi40 = zi39[127:64];
  assign zi41 = zi39[63:0];
  assign zi42 = {1'h0, {zi40, zi41}};
  assign zi43 = zi42[127:0];
  assign zres = {1'h1, zi43};
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute452 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute403_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute396_out;
  logic [2:0] zll_main_compute401_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute352_out;
  logic [2:0] zll_main_compute396_outR1;
  logic [2:0] zll_main_compute401_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute403  inst (arg5, zll_main_compute403_out);
  assign zi0 = zll_main_compute403_out;
  ZLL_Main_compute396  instR1 (arg5, arg3, zll_main_compute396_out);
  ZLL_Main_compute401  instR2 (arg2, zll_main_compute396_out, zll_main_compute401_out);
  assign slice_in = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute401_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute352  instR3 (arg5, arg1, zll_main_compute352_out);
  ZLL_Main_compute396  instR4 (zll_main_compute352_out, arg3, zll_main_compute396_outR1);
  ZLL_Main_compute401  instR5 (arg2, zll_main_compute396_outR1, zll_main_compute401_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute401_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute437 (input logic [63:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_compute435 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute417 (input logic [2:0] arg0,
  output logic [2:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_compute403 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute401 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute396 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute391 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [2:0] zll_main_compute401_out;
  ZLL_Main_compute401  inst (arg1, arg0, zll_main_compute401_out);
  assign res = zll_main_compute401_out;
endmodule

module ZLL_Main_compute352 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute346 (input logic [63:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute307_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute435_out;
  logic [2:0] zll_main_compute401_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute352_out;
  logic [2:0] zll_main_compute435_outR1;
  logic [2:0] zll_main_compute401_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute307  inst (arg5, arg3, zll_main_compute307_out);
  assign zi0 = zll_main_compute307_out;
  ZLL_Main_compute435  instR1 (arg5, arg4, zll_main_compute435_out);
  ZLL_Main_compute401  instR2 (zll_main_compute435_out, arg2, zll_main_compute401_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute401_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute352  instR3 (arg5, arg3, zll_main_compute352_out);
  ZLL_Main_compute435  instR4 (zll_main_compute352_out, arg4, zll_main_compute435_outR1);
  ZLL_Main_compute401  instR5 (zll_main_compute435_outR1, arg2, zll_main_compute401_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute401_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute307 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_compute282 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [0:0] arg3,
  output logic [2:0] res);
  logic [2:0] zll_main_compute417_out;
  logic [2:0] zll_main_compute401_out;
  ZLL_Main_compute417  inst (arg0, zll_main_compute417_out);
  ZLL_Main_compute401  instR1 (arg0, arg2, zll_main_compute401_out);
  assign res = (arg3 == 1'h1) ? zll_main_compute417_out : zll_main_compute401_out;
endmodule

module ZLL_Main_compute190 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute307_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute435_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute352_out;
  logic [2:0] zll_main_compute435_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute307  inst (arg4, arg0, zll_main_compute307_out);
  assign zi0 = zll_main_compute307_out;
  ZLL_Main_compute435  instR1 (arg4, arg1, zll_main_compute435_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute435_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute352  instR2 (arg4, arg0, zll_main_compute352_out);
  ZLL_Main_compute435  instR3 (zll_main_compute352_out, arg1, zll_main_compute435_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute435_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute175 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute403_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute396_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute352_out;
  logic [2:0] zll_main_compute396_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute403  inst (arg4, zll_main_compute403_out);
  assign zi0 = zll_main_compute403_out;
  ZLL_Main_compute396  instR1 (arg4, arg1, zll_main_compute396_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute396_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute352  instR2 (arg4, arg3, zll_main_compute352_out);
  ZLL_Main_compute396  instR3 (zll_main_compute352_out, arg1, zll_main_compute396_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute396_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute85 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute307_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute435_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute352_out;
  logic [2:0] zll_main_compute435_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute307  inst (arg4, arg0, zll_main_compute307_out);
  assign zi0 = zll_main_compute307_out;
  ZLL_Main_compute435  instR1 (arg4, arg2, zll_main_compute435_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute435_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute352  instR2 (arg4, arg0, zll_main_compute352_out);
  ZLL_Main_compute435  instR3 (zll_main_compute352_out, arg2, zll_main_compute435_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute435_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute53 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute307_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute435_out;
  logic [2:0] zll_main_compute401_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute352_out;
  logic [2:0] zll_main_compute435_outR1;
  logic [2:0] zll_main_compute401_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute307  inst (arg5, arg0, zll_main_compute307_out);
  assign zi0 = zll_main_compute307_out;
  ZLL_Main_compute435  instR1 (arg5, arg3, zll_main_compute435_out);
  ZLL_Main_compute401  instR2 (zll_main_compute435_out, arg2, zll_main_compute401_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute401_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute352  instR3 (arg5, arg0, zll_main_compute352_out);
  ZLL_Main_compute435  instR4 (zll_main_compute352_out, arg3, zll_main_compute435_outR1);
  ZLL_Main_compute401  instR5 (zll_main_compute435_outR1, arg2, zll_main_compute401_outR1);
  assign slice_inR1 = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute401_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h1) ? slice_in[7:0] : slice_inR1[7:0];
endmodule