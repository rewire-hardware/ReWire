module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zi0;
  logic [0:0] zll_main_dev175_out;
  logic [0:0] zll_main_dev175_outR1;
  logic [0:0] zll_main_dev175_outR2;
  logic [0:0] zll_main_dev175_outR3;
  logic [0:0] zll_main_dev175_outR4;
  logic [0:0] zll_main_dev175_outR5;
  logic [0:0] zll_main_dev175_outR6;
  logic [0:0] zll_main_dev175_outR7;
  logic [2:0] zll_main_dev282_out;
  logic [2:0] zll_main_dev235_out;
  logic [2:0] zi7;
  logic [2:0] zll_main_dev282_outR1;
  logic [0:0] zll_main_dev260_out;
  logic [0:0] zi8;
  logic [2:0] zll_main_dev233_out;
  logic [2:0] zll_main_dev247_out;
  logic [2:0] zi10;
  logic [0:0] zll_main_dev73_out;
  logic [0:0] zll_main_dev73_outR1;
  logic [0:0] zll_main_dev73_outR2;
  logic [0:0] zll_main_dev73_outR3;
  logic [0:0] zll_main_dev73_outR4;
  logic [0:0] zll_main_dev73_outR5;
  logic [0:0] zll_main_dev73_outR6;
  logic [0:0] zll_main_dev73_outR7;
  logic [2:0] zll_main_dev282_outR2;
  logic [2:0] zll_main_dev235_outR1;
  logic [2:0] zi14;
  logic [2:0] zll_main_dev282_outR3;
  logic [0:0] zll_main_dev260_outR1;
  logic [2:0] zll_main_dev195_out;
  logic [2:0] zi15;
  logic [0:0] zll_main_dev240_out;
  logic [0:0] zll_main_dev240_outR1;
  logic [0:0] zll_main_dev240_outR2;
  logic [0:0] zll_main_dev240_outR3;
  logic [0:0] zll_main_dev240_outR4;
  logic [0:0] zll_main_dev240_outR5;
  logic [0:0] zll_main_dev240_outR6;
  logic [0:0] zll_main_dev240_outR7;
  logic [2:0] zll_main_dev282_outR4;
  logic [2:0] zll_main_dev235_outR2;
  logic [2:0] zi19;
  logic [2:0] zll_main_dev282_outR5;
  logic [0:0] zll_main_dev260_outR2;
  logic [2:0] zll_main_dev195_outR1;
  logic [2:0] zi20;
  logic [0:0] zll_main_dev278_out;
  logic [0:0] zll_main_dev278_outR1;
  logic [0:0] zll_main_dev278_outR2;
  logic [0:0] zll_main_dev278_outR3;
  logic [0:0] zll_main_dev278_outR4;
  logic [0:0] zll_main_dev278_outR5;
  logic [0:0] zll_main_dev278_outR6;
  logic [0:0] zll_main_dev278_outR7;
  logic [31:0] zres;
  assign zi0 = __in0 * 8'h2;
  ZLL_Main_dev175  inst (__in0, 3'h1, 3'h2, zi0, 3'h0, zll_main_dev175_out);
  ZLL_Main_dev175  instR1 (__in0, 3'h1, 3'h2, zi0, 3'h1, zll_main_dev175_outR1);
  ZLL_Main_dev175  instR2 (__in0, 3'h1, 3'h2, zi0, 3'h2, zll_main_dev175_outR2);
  ZLL_Main_dev175  instR3 (__in0, 3'h1, 3'h2, zi0, 3'h3, zll_main_dev175_outR3);
  ZLL_Main_dev175  instR4 (__in0, 3'h1, 3'h2, zi0, 3'h4, zll_main_dev175_outR4);
  ZLL_Main_dev175  instR5 (__in0, 3'h1, 3'h2, zi0, 3'h5, zll_main_dev175_outR5);
  ZLL_Main_dev175  instR6 (__in0, 3'h1, 3'h2, zi0, 3'h6, zll_main_dev175_outR6);
  ZLL_Main_dev175  instR7 (__in0, 3'h1, 3'h2, zi0, 3'h7, zll_main_dev175_outR7);
  ZLL_Main_dev282  instR8 (__in0, zll_main_dev282_out);
  ZLL_Main_dev235  instR9 (zll_main_dev282_out, 3'h2, zll_main_dev235_out);
  assign zi7 = zll_main_dev235_out;
  ZLL_Main_dev282  instR10 (__in0, zll_main_dev282_outR1);
  ZLL_Main_dev260  instR11 (zll_main_dev282_outR1, zll_main_dev260_out);
  assign zi8 = zll_main_dev260_out;
  ZLL_Main_dev233  instR12 (zi7, zll_main_dev233_out);
  ZLL_Main_dev247  instR13 (zi7, 3'h1, zll_main_dev247_out);
  assign zi10 = (zi8 == 1'h1) ? zll_main_dev233_out : zll_main_dev247_out;
  ZLL_Main_dev73  instR14 (3'h1, 3'h2, zi10, __in0, zi0, 3'h0, zll_main_dev73_out);
  ZLL_Main_dev73  instR15 (3'h1, 3'h2, zi10, __in0, zi0, 3'h1, zll_main_dev73_outR1);
  ZLL_Main_dev73  instR16 (3'h1, 3'h2, zi10, __in0, zi0, 3'h2, zll_main_dev73_outR2);
  ZLL_Main_dev73  instR17 (3'h1, 3'h2, zi10, __in0, zi0, 3'h3, zll_main_dev73_outR3);
  ZLL_Main_dev73  instR18 (3'h1, 3'h2, zi10, __in0, zi0, 3'h4, zll_main_dev73_outR4);
  ZLL_Main_dev73  instR19 (3'h1, 3'h2, zi10, __in0, zi0, 3'h5, zll_main_dev73_outR5);
  ZLL_Main_dev73  instR20 (3'h1, 3'h2, zi10, __in0, zi0, 3'h6, zll_main_dev73_outR6);
  ZLL_Main_dev73  instR21 (3'h1, 3'h2, zi10, __in0, zi0, 3'h7, zll_main_dev73_outR7);
  ZLL_Main_dev282  instR22 (__in0, zll_main_dev282_outR2);
  ZLL_Main_dev235  instR23 (zll_main_dev282_outR2, 3'h2, zll_main_dev235_outR1);
  assign zi14 = zll_main_dev235_outR1;
  ZLL_Main_dev282  instR24 (__in0, zll_main_dev282_outR3);
  ZLL_Main_dev260  instR25 (zll_main_dev282_outR3, zll_main_dev260_outR1);
  ZLL_Main_dev195  instR26 (zi14, 3'h1, __in0, zll_main_dev260_outR1, zll_main_dev195_out);
  assign zi15 = zll_main_dev195_out;
  ZLL_Main_dev240  instR27 (zi0, 3'h2, zi15, __in0, 3'h0, zll_main_dev240_out);
  ZLL_Main_dev240  instR28 (zi0, 3'h2, zi15, __in0, 3'h1, zll_main_dev240_outR1);
  ZLL_Main_dev240  instR29 (zi0, 3'h2, zi15, __in0, 3'h2, zll_main_dev240_outR2);
  ZLL_Main_dev240  instR30 (zi0, 3'h2, zi15, __in0, 3'h3, zll_main_dev240_outR3);
  ZLL_Main_dev240  instR31 (zi0, 3'h2, zi15, __in0, 3'h4, zll_main_dev240_outR4);
  ZLL_Main_dev240  instR32 (zi0, 3'h2, zi15, __in0, 3'h5, zll_main_dev240_outR5);
  ZLL_Main_dev240  instR33 (zi0, 3'h2, zi15, __in0, 3'h6, zll_main_dev240_outR6);
  ZLL_Main_dev240  instR34 (zi0, 3'h2, zi15, __in0, 3'h7, zll_main_dev240_outR7);
  ZLL_Main_dev282  instR35 (__in0, zll_main_dev282_outR4);
  ZLL_Main_dev235  instR36 (zll_main_dev282_outR4, 3'h2, zll_main_dev235_outR2);
  assign zi19 = zll_main_dev235_outR2;
  ZLL_Main_dev282  instR37 (__in0, zll_main_dev282_outR5);
  ZLL_Main_dev260  instR38 (zll_main_dev282_outR5, zll_main_dev260_outR2);
  ZLL_Main_dev195  instR39 (zi19, 3'h1, __in0, zll_main_dev260_outR2, zll_main_dev195_outR1);
  assign zi20 = zll_main_dev195_outR1;
  ZLL_Main_dev278  instR40 (3'h1, __in0, zi0, 3'h2, zi20, 3'h0, zll_main_dev278_out);
  ZLL_Main_dev278  instR41 (3'h1, __in0, zi0, 3'h2, zi20, 3'h1, zll_main_dev278_outR1);
  ZLL_Main_dev278  instR42 (3'h1, __in0, zi0, 3'h2, zi20, 3'h2, zll_main_dev278_outR2);
  ZLL_Main_dev278  instR43 (3'h1, __in0, zi0, 3'h2, zi20, 3'h3, zll_main_dev278_outR3);
  ZLL_Main_dev278  instR44 (3'h1, __in0, zi0, 3'h2, zi20, 3'h4, zll_main_dev278_outR4);
  ZLL_Main_dev278  instR45 (3'h1, __in0, zi0, 3'h2, zi20, 3'h5, zll_main_dev278_outR5);
  ZLL_Main_dev278  instR46 (3'h1, __in0, zi0, 3'h2, zi20, 3'h6, zll_main_dev278_outR6);
  ZLL_Main_dev278  instR47 (3'h1, __in0, zi0, 3'h2, zi20, 3'h7, zll_main_dev278_outR7);
  assign zres = {zll_main_dev175_out, zll_main_dev175_outR1, zll_main_dev175_outR2, zll_main_dev175_outR3, zll_main_dev175_outR4, zll_main_dev175_outR5, zll_main_dev175_outR6, zll_main_dev175_outR7, {zll_main_dev73_out, zll_main_dev73_outR1, zll_main_dev73_outR2, zll_main_dev73_outR3, zll_main_dev73_outR4, zll_main_dev73_outR5, zll_main_dev73_outR6, zll_main_dev73_outR7}, {zll_main_dev240_out, zll_main_dev240_outR1, zll_main_dev240_outR2, zll_main_dev240_outR3, zll_main_dev240_outR4, zll_main_dev240_outR5, zll_main_dev240_outR6, zll_main_dev240_outR7}, {zll_main_dev278_out, zll_main_dev278_outR1, zll_main_dev278_outR2, zll_main_dev278_outR3, zll_main_dev278_outR4, zll_main_dev278_outR5, zll_main_dev278_outR6, zll_main_dev278_outR7}};
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_Main_dev284 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev282 (input logic [7:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_dev278 (input logic [2:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev224_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev284_out;
  logic [2:0] zll_main_dev247_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev74_out;
  logic [2:0] zll_main_dev284_outR1;
  logic [2:0] zll_main_dev247_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev224  inst (arg5, arg4, zll_main_dev224_out);
  assign zi0 = zll_main_dev224_out;
  ZLL_Main_dev284  instR1 (arg5, arg3, zll_main_dev284_out);
  ZLL_Main_dev247  instR2 (zll_main_dev284_out, arg0, zll_main_dev247_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev247_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev74  instR3 (arg5, arg4, zll_main_dev74_out);
  ZLL_Main_dev284  instR4 (zll_main_dev74_out, arg3, zll_main_dev284_outR1);
  ZLL_Main_dev247  instR5 (zll_main_dev284_outR1, arg0, zll_main_dev247_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev247_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev260 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_dev247 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev240 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev224_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev284_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev74_out;
  logic [2:0] zll_main_dev284_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev224  inst (arg4, arg2, zll_main_dev224_out);
  assign zi0 = zll_main_dev224_out;
  ZLL_Main_dev284  instR1 (arg4, arg1, zll_main_dev284_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev284_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev74  instR2 (arg4, arg2, zll_main_dev74_out);
  ZLL_Main_dev284  instR3 (zll_main_dev74_out, arg1, zll_main_dev284_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev284_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev235 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev233 (input logic [2:0] arg0,
  output logic [2:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_dev224 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_dev195 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [0:0] arg3,
  output logic [2:0] res);
  logic [2:0] zll_main_dev233_out;
  logic [2:0] zll_main_dev247_out;
  ZLL_Main_dev233  inst (arg0, zll_main_dev233_out);
  ZLL_Main_dev247  instR1 (arg0, arg1, zll_main_dev247_out);
  assign res = (arg3 == 1'h1) ? zll_main_dev233_out : zll_main_dev247_out;
endmodule

module ZLL_Main_dev175 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev260_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev235_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev74_out;
  logic [2:0] zll_main_dev235_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev260  inst (arg4, zll_main_dev260_out);
  assign zi0 = zll_main_dev260_out;
  ZLL_Main_dev235  instR1 (arg4, arg2, zll_main_dev235_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev235_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev74  instR2 (arg4, arg1, zll_main_dev74_out);
  ZLL_Main_dev235  instR3 (zll_main_dev74_out, arg2, zll_main_dev235_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev235_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev74 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev73 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [7:0] arg3,
  input logic [7:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev260_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev235_out;
  logic [2:0] zll_main_dev247_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev74_out;
  logic [2:0] zll_main_dev235_outR1;
  logic [2:0] zll_main_dev247_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev260  inst (arg5, zll_main_dev260_out);
  assign zi0 = zll_main_dev260_out;
  ZLL_Main_dev235  instR1 (arg5, arg1, zll_main_dev235_out);
  ZLL_Main_dev247  instR2 (arg2, zll_main_dev235_out, zll_main_dev247_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev247_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev74  instR3 (arg5, arg0, zll_main_dev74_out);
  ZLL_Main_dev235  instR4 (zll_main_dev74_out, arg1, zll_main_dev235_outR1);
  ZLL_Main_dev247  instR5 (arg2, zll_main_dev235_outR1, zll_main_dev247_outR1);
  assign slice_inR1 = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev247_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule