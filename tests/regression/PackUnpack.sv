module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zin;
  logic [7:0] zi0;
  logic [15:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [0:0] zll_main_dev72_out;
  logic [0:0] zll_main_dev72_outR1;
  logic [0:0] zll_main_dev72_outR2;
  logic [0:0] zll_main_dev72_outR3;
  logic [0:0] zll_main_dev72_outR4;
  logic [0:0] zll_main_dev72_outR5;
  logic [0:0] zll_main_dev72_outR6;
  logic [0:0] zll_main_dev72_outR7;
  logic [15:0] zi4;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [2:0] zll_main_dev285_out;
  logic [2:0] zll_main_dev264_out;
  logic [2:0] zi7;
  logic [2:0] zll_main_dev285_outR1;
  logic [0:0] zll_main_dev284_out;
  logic [0:0] zi8;
  logic [3:0] zi9;
  logic [2:0] zll_main_dev143_out;
  logic [2:0] zll_main_dev285_outR2;
  logic [0:0] zll_main_dev284_outR1;
  logic [2:0] zll_main_dev158_out;
  logic [2:0] zi10;
  logic [0:0] zll_main_dev204_out;
  logic [0:0] zll_main_dev204_outR1;
  logic [0:0] zll_main_dev204_outR2;
  logic [0:0] zll_main_dev204_outR3;
  logic [0:0] zll_main_dev204_outR4;
  logic [0:0] zll_main_dev204_outR5;
  logic [0:0] zll_main_dev204_outR6;
  logic [0:0] zll_main_dev204_outR7;
  logic [15:0] zi11;
  logic [7:0] zi12;
  logic [7:0] zi13;
  logic [2:0] zll_main_dev285_outR3;
  logic [2:0] zll_main_dev264_outR1;
  logic [2:0] zi14;
  logic [2:0] zll_main_dev285_outR4;
  logic [0:0] zll_main_dev284_outR2;
  logic [0:0] zi15;
  logic [3:0] zi16;
  logic [2:0] zll_main_dev143_outR1;
  logic [2:0] zll_main_dev285_outR5;
  logic [0:0] zll_main_dev284_outR3;
  logic [2:0] zll_main_dev158_outR1;
  logic [2:0] zi17;
  logic [0:0] zll_main_dev171_out;
  logic [0:0] zll_main_dev171_outR1;
  logic [0:0] zll_main_dev171_outR2;
  logic [0:0] zll_main_dev171_outR3;
  logic [0:0] zll_main_dev171_outR4;
  logic [0:0] zll_main_dev171_outR5;
  logic [0:0] zll_main_dev171_outR6;
  logic [0:0] zll_main_dev171_outR7;
  logic [15:0] zi18;
  logic [7:0] zi19;
  logic [7:0] zi20;
  logic [2:0] zll_main_dev285_outR6;
  logic [2:0] zll_main_dev264_outR2;
  logic [2:0] zi21;
  logic [2:0] zll_main_dev285_outR7;
  logic [0:0] zll_main_dev284_outR4;
  logic [0:0] zi22;
  logic [3:0] zi23;
  logic [2:0] zll_main_dev143_outR2;
  logic [2:0] zll_main_dev285_outR8;
  logic [0:0] zll_main_dev284_outR5;
  logic [0:0] zi24;
  logic [6:0] zi25;
  logic [2:0] zi26;
  logic [2:0] zi27;
  logic [2:0] zll_main_dev263_out;
  logic [2:0] zi28;
  logic [0:0] zll_main_dev276_out;
  logic [0:0] zll_main_dev276_outR1;
  logic [0:0] zll_main_dev276_outR2;
  logic [0:0] zll_main_dev276_outR3;
  logic [0:0] zll_main_dev276_outR4;
  logic [0:0] zll_main_dev276_outR5;
  logic [0:0] zll_main_dev276_outR6;
  logic [0:0] zll_main_dev276_outR7;
  logic [31:0] zres;
  assign zin = __in0;
  assign zi0 = zin * 8'h2;
  assign zi1 = {zin, zi0};
  assign zi2 = zi1[15:8];
  assign zi3 = zi1[7:0];
  ZLL_Main_dev72  inst (zi3, zi2, 3'h1, 3'h2, 3'h0, zll_main_dev72_out);
  ZLL_Main_dev72  instR1 (zi3, zi2, 3'h1, 3'h2, 3'h1, zll_main_dev72_outR1);
  ZLL_Main_dev72  instR2 (zi3, zi2, 3'h1, 3'h2, 3'h2, zll_main_dev72_outR2);
  ZLL_Main_dev72  instR3 (zi3, zi2, 3'h1, 3'h2, 3'h3, zll_main_dev72_outR3);
  ZLL_Main_dev72  instR4 (zi3, zi2, 3'h1, 3'h2, 3'h4, zll_main_dev72_outR4);
  ZLL_Main_dev72  instR5 (zi3, zi2, 3'h1, 3'h2, 3'h5, zll_main_dev72_outR5);
  ZLL_Main_dev72  instR6 (zi3, zi2, 3'h1, 3'h2, 3'h6, zll_main_dev72_outR6);
  ZLL_Main_dev72  instR7 (zi3, zi2, 3'h1, 3'h2, 3'h7, zll_main_dev72_outR7);
  assign zi4 = {zin, zi0};
  assign zi5 = zi4[15:8];
  assign zi6 = zi4[7:0];
  ZLL_Main_dev285  instR8 (zi5, zll_main_dev285_out);
  ZLL_Main_dev264  instR9 (zll_main_dev285_out, 3'h2, zll_main_dev264_out);
  assign zi7 = zll_main_dev264_out;
  ZLL_Main_dev285  instR10 (zi5, zll_main_dev285_outR1);
  ZLL_Main_dev284  instR11 (zll_main_dev285_outR1, zll_main_dev284_out);
  assign zi8 = zll_main_dev284_out;
  assign zi9 = {zi7, zi8};
  ZLL_Main_dev143  instR12 (zi9[3:1], zll_main_dev143_out);
  ZLL_Main_dev285  instR13 (zi5, zll_main_dev285_outR2);
  ZLL_Main_dev284  instR14 (zll_main_dev285_outR2, zll_main_dev284_outR1);
  ZLL_Main_dev158  instR15 (3'h1, zi7, zll_main_dev284_outR1, zll_main_dev158_out);
  assign zi10 = (zi9[0] == 1'h1) ? zll_main_dev143_out : zll_main_dev158_out;
  ZLL_Main_dev204  instR16 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h0, zll_main_dev204_out);
  ZLL_Main_dev204  instR17 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h1, zll_main_dev204_outR1);
  ZLL_Main_dev204  instR18 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h2, zll_main_dev204_outR2);
  ZLL_Main_dev204  instR19 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h3, zll_main_dev204_outR3);
  ZLL_Main_dev204  instR20 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h4, zll_main_dev204_outR4);
  ZLL_Main_dev204  instR21 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h5, zll_main_dev204_outR5);
  ZLL_Main_dev204  instR22 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h6, zll_main_dev204_outR6);
  ZLL_Main_dev204  instR23 (zi5, 3'h2, zi6, 3'h1, zi10, 3'h7, zll_main_dev204_outR7);
  assign zi11 = {zin, zi0};
  assign zi12 = zi11[15:8];
  assign zi13 = zi11[7:0];
  ZLL_Main_dev285  instR24 (zi12, zll_main_dev285_outR3);
  ZLL_Main_dev264  instR25 (zll_main_dev285_outR3, 3'h2, zll_main_dev264_outR1);
  assign zi14 = zll_main_dev264_outR1;
  ZLL_Main_dev285  instR26 (zi12, zll_main_dev285_outR4);
  ZLL_Main_dev284  instR27 (zll_main_dev285_outR4, zll_main_dev284_outR2);
  assign zi15 = zll_main_dev284_outR2;
  assign zi16 = {zi14, zi15};
  ZLL_Main_dev143  instR28 (zi16[3:1], zll_main_dev143_outR1);
  ZLL_Main_dev285  instR29 (zi12, zll_main_dev285_outR5);
  ZLL_Main_dev284  instR30 (zll_main_dev285_outR5, zll_main_dev284_outR3);
  ZLL_Main_dev158  instR31 (3'h1, zi14, zll_main_dev284_outR3, zll_main_dev158_outR1);
  assign zi17 = (zi16[0] == 1'h1) ? zll_main_dev143_outR1 : zll_main_dev158_outR1;
  ZLL_Main_dev171  instR32 (zi13, 3'h2, zi17, zi12, 3'h0, zll_main_dev171_out);
  ZLL_Main_dev171  instR33 (zi13, 3'h2, zi17, zi12, 3'h1, zll_main_dev171_outR1);
  ZLL_Main_dev171  instR34 (zi13, 3'h2, zi17, zi12, 3'h2, zll_main_dev171_outR2);
  ZLL_Main_dev171  instR35 (zi13, 3'h2, zi17, zi12, 3'h3, zll_main_dev171_outR3);
  ZLL_Main_dev171  instR36 (zi13, 3'h2, zi17, zi12, 3'h4, zll_main_dev171_outR4);
  ZLL_Main_dev171  instR37 (zi13, 3'h2, zi17, zi12, 3'h5, zll_main_dev171_outR5);
  ZLL_Main_dev171  instR38 (zi13, 3'h2, zi17, zi12, 3'h6, zll_main_dev171_outR6);
  ZLL_Main_dev171  instR39 (zi13, 3'h2, zi17, zi12, 3'h7, zll_main_dev171_outR7);
  assign zi18 = {zin, zi0};
  assign zi19 = zi18[15:8];
  assign zi20 = zi18[7:0];
  ZLL_Main_dev285  instR40 (zi19, zll_main_dev285_outR6);
  ZLL_Main_dev264  instR41 (zll_main_dev285_outR6, 3'h2, zll_main_dev264_outR2);
  assign zi21 = zll_main_dev264_outR2;
  ZLL_Main_dev285  instR42 (zi19, zll_main_dev285_outR7);
  ZLL_Main_dev284  instR43 (zll_main_dev285_outR7, zll_main_dev284_outR4);
  assign zi22 = zll_main_dev284_outR4;
  assign zi23 = {zi21, zi22};
  ZLL_Main_dev143  instR44 (zi23[3:1], zll_main_dev143_outR2);
  ZLL_Main_dev285  instR45 (zi19, zll_main_dev285_outR8);
  ZLL_Main_dev284  instR46 (zll_main_dev285_outR8, zll_main_dev284_outR5);
  assign zi24 = zll_main_dev284_outR5;
  assign zi25 = {zi21, 3'h1, zi24};
  assign zi26 = zi25[6:4];
  assign zi27 = zi25[3:1];
  ZLL_Main_dev263  instR47 (zi26, zi27, zll_main_dev263_out);
  assign zi28 = (zi23[0] == 1'h1) ? zll_main_dev143_outR2 : zll_main_dev263_out;
  ZLL_Main_dev276  instR48 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h0, zll_main_dev276_out);
  ZLL_Main_dev276  instR49 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h1, zll_main_dev276_outR1);
  ZLL_Main_dev276  instR50 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h2, zll_main_dev276_outR2);
  ZLL_Main_dev276  instR51 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h3, zll_main_dev276_outR3);
  ZLL_Main_dev276  instR52 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h4, zll_main_dev276_outR4);
  ZLL_Main_dev276  instR53 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h5, zll_main_dev276_outR5);
  ZLL_Main_dev276  instR54 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h6, zll_main_dev276_outR6);
  ZLL_Main_dev276  instR55 (zi19, zi28, 3'h2, 3'h1, zi20, 3'h7, zll_main_dev276_outR7);
  assign zres = {{zll_main_dev72_out, zll_main_dev72_outR1, zll_main_dev72_outR2, zll_main_dev72_outR3, zll_main_dev72_outR4, zll_main_dev72_outR5, zll_main_dev72_outR6, zll_main_dev72_outR7}, {zll_main_dev204_out, zll_main_dev204_outR1, zll_main_dev204_outR2, zll_main_dev204_outR3, zll_main_dev204_outR4, zll_main_dev204_outR5, zll_main_dev204_outR6, zll_main_dev204_outR7}, {zll_main_dev171_out, zll_main_dev171_outR1, zll_main_dev171_outR2, zll_main_dev171_outR3, zll_main_dev171_outR4, zll_main_dev171_outR5, zll_main_dev171_outR6, zll_main_dev171_outR7}, {zll_main_dev276_out, zll_main_dev276_outR1, zll_main_dev276_outR2, zll_main_dev276_outR3, zll_main_dev276_outR4, zll_main_dev276_outR5, zll_main_dev276_outR6, zll_main_dev276_outR7}};
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_Main_dev285 (input logic [7:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_dev284 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_dev276 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [7:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev274_out;
  logic [0:0] zi0;
  logic [17:0] zi1;
  logic [7:0] zi2;
  logic [2:0] zi3;
  logic [2:0] zi4;
  logic [2:0] zi5;
  logic [2:0] zll_main_dev233_out;
  logic [2:0] zll_main_dev263_out;
  logic [7:0] slice_in;
  logic [0:0] zll_main_dev274_outR1;
  logic [0:0] zi6;
  logic [20:0] zi7;
  logic [2:0] zi8;
  logic [2:0] zi9;
  logic [2:0] zi10;
  logic [2:0] zi11;
  logic [7:0] zi12;
  logic [2:0] zll_main_dev251_out;
  logic [2:0] zll_main_dev233_outR1;
  logic [2:0] zll_main_dev263_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev274  inst (arg5, arg1, zll_main_dev274_out);
  assign zi0 = zll_main_dev274_out;
  assign zi1 = {arg0, arg5, arg2, arg3, zi0};
  assign zi2 = zi1[17:10];
  assign zi3 = zi1[9:7];
  assign zi4 = zi1[6:4];
  assign zi5 = zi1[3:1];
  ZLL_Main_dev233  instR1 (zi3, zi4, zll_main_dev233_out);
  ZLL_Main_dev263  instR2 (zll_main_dev233_out, zi5, zll_main_dev263_out);
  assign slice_in = zi2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev263_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev274  instR3 (arg5, arg1, zll_main_dev274_outR1);
  assign zi6 = zll_main_dev274_outR1;
  assign zi7 = {arg5, arg1, arg2, arg3, arg4, zi6};
  assign zi8 = zi7[20:18];
  assign zi9 = zi7[17:15];
  assign zi10 = zi7[14:12];
  assign zi11 = zi7[11:9];
  assign zi12 = zi7[8:1];
  ZLL_Main_dev251  instR4 (zi8, zi9, zll_main_dev251_out);
  ZLL_Main_dev233  instR5 (zll_main_dev251_out, zi10, zll_main_dev233_outR1);
  ZLL_Main_dev263  instR6 (zll_main_dev233_outR1, zi11, zll_main_dev263_outR1);
  assign slice_inR1 = zi12 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev263_outR1}) - 128'h1) * 128'h1);
  assign res = (zi1[0] == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev274 (input logic [2:0] arg0,
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

module ZLL_Main_dev264 (input logic [2:0] arg0,
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

module ZLL_Main_dev263 (input logic [2:0] arg0,
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

module ZLL_Main_dev251 (input logic [2:0] arg0,
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

module ZLL_Main_dev233 (input logic [2:0] arg0,
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

module ZLL_Main_dev204 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev284_out;
  logic [0:0] zi0;
  logic [17:0] zi1;
  logic [7:0] zi2;
  logic [2:0] zi3;
  logic [2:0] zi4;
  logic [2:0] zi5;
  logic [2:0] zll_main_dev264_out;
  logic [2:0] zll_main_dev263_out;
  logic [7:0] slice_in;
  logic [0:0] zll_main_dev284_outR1;
  logic [0:0] zi6;
  logic [20:0] zi7;
  logic [2:0] zi8;
  logic [2:0] zi9;
  logic [7:0] zi10;
  logic [2:0] zi11;
  logic [2:0] zi12;
  logic [2:0] zll_main_dev251_out;
  logic [2:0] zll_main_dev264_outR1;
  logic [2:0] zll_main_dev263_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev284  inst (arg5, zll_main_dev284_out);
  assign zi0 = zll_main_dev284_out;
  assign zi1 = {arg0, arg5, arg1, arg4, zi0};
  assign zi2 = zi1[17:10];
  assign zi3 = zi1[9:7];
  assign zi4 = zi1[6:4];
  assign zi5 = zi1[3:1];
  ZLL_Main_dev264  instR1 (zi3, zi4, zll_main_dev264_out);
  ZLL_Main_dev263  instR2 (zi5, zll_main_dev264_out, zll_main_dev263_out);
  assign slice_in = zi2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev263_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev284  instR3 (arg5, zll_main_dev284_outR1);
  assign zi6 = zll_main_dev284_outR1;
  assign zi7 = {arg5, arg1, arg2, arg3, arg4, zi6};
  assign zi8 = zi7[20:18];
  assign zi9 = zi7[17:15];
  assign zi10 = zi7[14:7];
  assign zi11 = zi7[6:4];
  assign zi12 = zi7[3:1];
  ZLL_Main_dev251  instR4 (zi8, zi11, zll_main_dev251_out);
  ZLL_Main_dev264  instR5 (zll_main_dev251_out, zi9, zll_main_dev264_outR1);
  ZLL_Main_dev263  instR6 (zi12, zll_main_dev264_outR1, zll_main_dev263_outR1);
  assign slice_inR1 = zi10 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev263_outR1}) - 128'h1) * 128'h1);
  assign res = (zi1[0] == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev171 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev274_out;
  logic [0:0] zi0;
  logic [14:0] zi1;
  logic [2:0] zi2;
  logic [7:0] zi3;
  logic [2:0] zi4;
  logic [2:0] zll_main_dev233_out;
  logic [7:0] slice_in;
  logic [0:0] zll_main_dev274_outR1;
  logic [0:0] zi5;
  logic [17:0] zi6;
  logic [7:0] zi7;
  logic [2:0] zi8;
  logic [2:0] zi9;
  logic [2:0] zi10;
  logic [2:0] zll_main_dev251_out;
  logic [2:0] zll_main_dev233_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev274  inst (arg4, arg2, zll_main_dev274_out);
  assign zi0 = zll_main_dev274_out;
  assign zi1 = {arg1, arg3, arg4, zi0};
  assign zi2 = zi1[14:12];
  assign zi3 = zi1[11:4];
  assign zi4 = zi1[3:1];
  ZLL_Main_dev233  instR1 (zi4, zi2, zll_main_dev233_out);
  assign slice_in = zi3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev233_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev274  instR2 (arg4, arg2, zll_main_dev274_outR1);
  assign zi5 = zll_main_dev274_outR1;
  assign zi6 = {arg0, arg1, arg2, arg4, zi5};
  assign zi7 = zi6[17:10];
  assign zi8 = zi6[9:7];
  assign zi9 = zi6[6:4];
  assign zi10 = zi6[3:1];
  ZLL_Main_dev251  instR3 (zi10, zi9, zll_main_dev251_out);
  ZLL_Main_dev233  instR4 (zll_main_dev251_out, zi8, zll_main_dev233_outR1);
  assign slice_inR1 = zi7 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev233_outR1}) - 128'h1) * 128'h1);
  assign res = (zi1[0] == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev158 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [6:0] zt0;
  logic [2:0] zi0;
  logic [2:0] zi1;
  logic [2:0] zll_main_dev263_out;
  assign zt0 = {arg0, arg1, arg2};
  assign zi0 = zt0[6:4];
  assign zi1 = zt0[3:1];
  ZLL_Main_dev263  inst (zi1, zi0, zll_main_dev263_out);
  assign res = zll_main_dev263_out;
endmodule

module ZLL_Main_dev143 (input logic [2:0] arg0,
  output logic [2:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_dev72 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev284_out;
  logic [0:0] zi0;
  logic [14:0] zi1;
  logic [7:0] zi2;
  logic [2:0] zi3;
  logic [2:0] zi4;
  logic [2:0] zll_main_dev264_out;
  logic [7:0] slice_in;
  logic [0:0] zll_main_dev284_outR1;
  logic [0:0] zi5;
  logic [17:0] zi6;
  logic [7:0] zi7;
  logic [2:0] zi8;
  logic [2:0] zi9;
  logic [2:0] zi10;
  logic [2:0] zll_main_dev251_out;
  logic [2:0] zll_main_dev264_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev284  inst (arg4, zll_main_dev284_out);
  assign zi0 = zll_main_dev284_out;
  assign zi1 = {arg1, arg4, arg3, zi0};
  assign zi2 = zi1[14:7];
  assign zi3 = zi1[6:4];
  assign zi4 = zi1[3:1];
  ZLL_Main_dev264  instR1 (zi3, zi4, zll_main_dev264_out);
  assign slice_in = zi2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev264_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev284  instR2 (arg4, zll_main_dev284_outR1);
  assign zi5 = zll_main_dev284_outR1;
  assign zi6 = {arg0, arg2, arg4, arg3, zi5};
  assign zi7 = zi6[17:10];
  assign zi8 = zi6[9:7];
  assign zi9 = zi6[6:4];
  assign zi10 = zi6[3:1];
  ZLL_Main_dev251  instR3 (zi9, zi8, zll_main_dev251_out);
  ZLL_Main_dev264  instR4 (zll_main_dev251_out, zi10, zll_main_dev264_outR1);
  assign slice_inR1 = zi7 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev264_outR1}) - 128'h1) * 128'h1);
  assign res = (zi1[0] == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule