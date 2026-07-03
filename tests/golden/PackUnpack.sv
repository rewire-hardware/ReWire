module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zi0;
  logic [0:0] zll_main_dev36_out;
  logic [0:0] zll_main_dev36_outR1;
  logic [0:0] zll_main_dev36_outR2;
  logic [0:0] zll_main_dev36_outR3;
  logic [0:0] zll_main_dev36_outR4;
  logic [0:0] zll_main_dev36_outR5;
  logic [0:0] zll_main_dev36_outR6;
  logic [0:0] zll_main_dev36_outR7;
  logic [2:0] zll_main_dev33_out;
  logic [2:0] zll_main_dev45_out;
  logic [2:0] zi1;
  logic [2:0] zll_main_dev33_outR1;
  logic [0:0] zll_main_dev3_out;
  logic [0:0] zi2;
  logic [2:0] zll_main_dev_out;
  logic [2:0] zi3;
  logic [0:0] zll_main_dev19_out;
  logic [0:0] zll_main_dev19_outR1;
  logic [0:0] zll_main_dev19_outR2;
  logic [0:0] zll_main_dev19_outR3;
  logic [0:0] zll_main_dev19_outR4;
  logic [0:0] zll_main_dev19_outR5;
  logic [0:0] zll_main_dev19_outR6;
  logic [0:0] zll_main_dev19_outR7;
  logic [2:0] zll_main_dev33_outR2;
  logic [2:0] zll_main_dev45_outR1;
  logic [2:0] zi4;
  logic [2:0] zll_main_dev33_outR3;
  logic [0:0] zll_main_dev3_outR1;
  logic [0:0] zi5;
  logic [2:0] zll_main_dev31_out;
  logic [2:0] zi6;
  logic [0:0] zll_main_dev21_out;
  logic [0:0] zll_main_dev21_outR1;
  logic [0:0] zll_main_dev21_outR2;
  logic [0:0] zll_main_dev21_outR3;
  logic [0:0] zll_main_dev21_outR4;
  logic [0:0] zll_main_dev21_outR5;
  logic [0:0] zll_main_dev21_outR6;
  logic [0:0] zll_main_dev21_outR7;
  logic [2:0] zll_main_dev33_outR4;
  logic [2:0] zll_main_dev45_outR2;
  logic [2:0] zi7;
  logic [2:0] zll_main_dev33_outR5;
  logic [0:0] zll_main_dev3_outR2;
  logic [0:0] zi8;
  logic [2:0] zll_main_dev_outR1;
  logic [2:0] zi9;
  logic [0:0] zll_main_dev35_out;
  logic [0:0] zll_main_dev35_outR1;
  logic [0:0] zll_main_dev35_outR2;
  logic [0:0] zll_main_dev35_outR3;
  logic [0:0] zll_main_dev35_outR4;
  logic [0:0] zll_main_dev35_outR5;
  logic [0:0] zll_main_dev35_outR6;
  logic [0:0] zll_main_dev35_outR7;
  logic [31:0] zres;
  assign zi0 = __in0 * 8'h2;
  ZLL_Main_dev36  inst (3'h2, 3'h1, zi0, __in0, 3'h0, zll_main_dev36_out);
  ZLL_Main_dev36  instR1 (3'h2, 3'h1, zi0, __in0, 3'h1, zll_main_dev36_outR1);
  ZLL_Main_dev36  instR2 (3'h2, 3'h1, zi0, __in0, 3'h2, zll_main_dev36_outR2);
  ZLL_Main_dev36  instR3 (3'h2, 3'h1, zi0, __in0, 3'h3, zll_main_dev36_outR3);
  ZLL_Main_dev36  instR4 (3'h2, 3'h1, zi0, __in0, 3'h4, zll_main_dev36_outR4);
  ZLL_Main_dev36  instR5 (3'h2, 3'h1, zi0, __in0, 3'h5, zll_main_dev36_outR5);
  ZLL_Main_dev36  instR6 (3'h2, 3'h1, zi0, __in0, 3'h6, zll_main_dev36_outR6);
  ZLL_Main_dev36  instR7 (3'h2, 3'h1, zi0, __in0, 3'h7, zll_main_dev36_outR7);
  ZLL_Main_dev33  instR8 (__in0, zll_main_dev33_out);
  ZLL_Main_dev45  instR9 (zll_main_dev33_out, 3'h2, zll_main_dev45_out);
  assign zi1 = zll_main_dev45_out;
  ZLL_Main_dev33  instR10 (__in0, zll_main_dev33_outR1);
  ZLL_Main_dev3  instR11 (zll_main_dev33_outR1, zll_main_dev3_out);
  assign zi2 = zll_main_dev3_out;
  ZLL_Main_dev  instR12 (zi1, 3'h1, zll_main_dev_out);
  assign zi3 = (zi2 == 1'h0) ? zll_main_dev_out : zi1;
  ZLL_Main_dev19  instR13 (3'h1, 3'h2, __in0, zi0, zi3, 3'h0, zll_main_dev19_out);
  ZLL_Main_dev19  instR14 (3'h1, 3'h2, __in0, zi0, zi3, 3'h1, zll_main_dev19_outR1);
  ZLL_Main_dev19  instR15 (3'h1, 3'h2, __in0, zi0, zi3, 3'h2, zll_main_dev19_outR2);
  ZLL_Main_dev19  instR16 (3'h1, 3'h2, __in0, zi0, zi3, 3'h3, zll_main_dev19_outR3);
  ZLL_Main_dev19  instR17 (3'h1, 3'h2, __in0, zi0, zi3, 3'h4, zll_main_dev19_outR4);
  ZLL_Main_dev19  instR18 (3'h1, 3'h2, __in0, zi0, zi3, 3'h5, zll_main_dev19_outR5);
  ZLL_Main_dev19  instR19 (3'h1, 3'h2, __in0, zi0, zi3, 3'h6, zll_main_dev19_outR6);
  ZLL_Main_dev19  instR20 (3'h1, 3'h2, __in0, zi0, zi3, 3'h7, zll_main_dev19_outR7);
  ZLL_Main_dev33  instR21 (__in0, zll_main_dev33_outR2);
  ZLL_Main_dev45  instR22 (zll_main_dev33_outR2, 3'h2, zll_main_dev45_outR1);
  assign zi4 = zll_main_dev45_outR1;
  ZLL_Main_dev33  instR23 (__in0, zll_main_dev33_outR3);
  ZLL_Main_dev3  instR24 (zll_main_dev33_outR3, zll_main_dev3_outR1);
  assign zi5 = zll_main_dev3_outR1;
  ZLL_Main_dev31  instR25 (zi4, 3'h1, zll_main_dev31_out);
  assign zi6 = (zi5 == 1'h0) ? zll_main_dev31_out : zi4;
  ZLL_Main_dev21  instR26 (zi0, __in0, zi6, 3'h2, 3'h0, zll_main_dev21_out);
  ZLL_Main_dev21  instR27 (zi0, __in0, zi6, 3'h2, 3'h1, zll_main_dev21_outR1);
  ZLL_Main_dev21  instR28 (zi0, __in0, zi6, 3'h2, 3'h2, zll_main_dev21_outR2);
  ZLL_Main_dev21  instR29 (zi0, __in0, zi6, 3'h2, 3'h3, zll_main_dev21_outR3);
  ZLL_Main_dev21  instR30 (zi0, __in0, zi6, 3'h2, 3'h4, zll_main_dev21_outR4);
  ZLL_Main_dev21  instR31 (zi0, __in0, zi6, 3'h2, 3'h5, zll_main_dev21_outR5);
  ZLL_Main_dev21  instR32 (zi0, __in0, zi6, 3'h2, 3'h6, zll_main_dev21_outR6);
  ZLL_Main_dev21  instR33 (zi0, __in0, zi6, 3'h2, 3'h7, zll_main_dev21_outR7);
  ZLL_Main_dev33  instR34 (__in0, zll_main_dev33_outR4);
  ZLL_Main_dev45  instR35 (zll_main_dev33_outR4, 3'h2, zll_main_dev45_outR2);
  assign zi7 = zll_main_dev45_outR2;
  ZLL_Main_dev33  instR36 (__in0, zll_main_dev33_outR5);
  ZLL_Main_dev3  instR37 (zll_main_dev33_outR5, zll_main_dev3_outR2);
  assign zi8 = zll_main_dev3_outR2;
  ZLL_Main_dev  instR38 (zi7, 3'h1, zll_main_dev_outR1);
  assign zi9 = (zi8 == 1'h0) ? zll_main_dev_outR1 : zi7;
  ZLL_Main_dev35  instR39 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h0, zll_main_dev35_out);
  ZLL_Main_dev35  instR40 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h1, zll_main_dev35_outR1);
  ZLL_Main_dev35  instR41 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h2, zll_main_dev35_outR2);
  ZLL_Main_dev35  instR42 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h3, zll_main_dev35_outR3);
  ZLL_Main_dev35  instR43 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h4, zll_main_dev35_outR4);
  ZLL_Main_dev35  instR44 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h5, zll_main_dev35_outR5);
  ZLL_Main_dev35  instR45 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h6, zll_main_dev35_outR6);
  ZLL_Main_dev35  instR46 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h7, zll_main_dev35_outR7);
  assign zres = {zll_main_dev36_out, zll_main_dev36_outR1, zll_main_dev36_outR2, zll_main_dev36_outR3, zll_main_dev36_outR4, zll_main_dev36_outR5, zll_main_dev36_outR6, zll_main_dev36_outR7, {zll_main_dev19_out, zll_main_dev19_outR1, zll_main_dev19_outR2, zll_main_dev19_outR3, zll_main_dev19_outR4, zll_main_dev19_outR5, zll_main_dev19_outR6, zll_main_dev19_outR7}, {zll_main_dev21_out, zll_main_dev21_outR1, zll_main_dev21_outR2, zll_main_dev21_outR3, zll_main_dev21_outR4, zll_main_dev21_outR5, zll_main_dev21_outR6, zll_main_dev21_outR7}, {zll_main_dev35_out, zll_main_dev35_outR1, zll_main_dev35_outR2, zll_main_dev35_outR3, zll_main_dev35_outR4, zll_main_dev35_outR5, zll_main_dev35_outR6, zll_main_dev35_outR7}};
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_Main_dev45 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev41 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev36 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev3_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev41_out;
  logic [2:0] zll_main_dev45_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev45_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev3  inst (arg4, zll_main_dev3_out);
  assign zi0 = zll_main_dev3_out;
  ZLL_Main_dev41  instR1 (arg4, arg1, zll_main_dev41_out);
  ZLL_Main_dev45  instR2 (zll_main_dev41_out, arg0, zll_main_dev45_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev45_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev45  instR3 (arg4, arg0, zll_main_dev45_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev45_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev35 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev6_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev41_out;
  logic [2:0] zll_main_dev10_out;
  logic [2:0] zll_main_dev_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev10_outR1;
  logic [2:0] zll_main_dev_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev6  inst (arg5, arg3, zll_main_dev6_out);
  assign zi0 = zll_main_dev6_out;
  ZLL_Main_dev41  instR1 (arg5, arg3, zll_main_dev41_out);
  ZLL_Main_dev10  instR2 (zll_main_dev41_out, arg1, zll_main_dev10_out);
  ZLL_Main_dev  instR3 (zll_main_dev10_out, arg4, zll_main_dev_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev10  instR4 (arg5, arg1, zll_main_dev10_outR1);
  ZLL_Main_dev  instR5 (zll_main_dev10_outR1, arg4, zll_main_dev_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev33 (input logic [7:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_dev31 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev21 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev6_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev41_out;
  logic [2:0] zll_main_dev10_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev10_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev6  inst (arg4, arg2, zll_main_dev6_out);
  assign zi0 = zll_main_dev6_out;
  ZLL_Main_dev41  instR1 (arg4, arg2, zll_main_dev41_out);
  ZLL_Main_dev10  instR2 (zll_main_dev41_out, arg3, zll_main_dev10_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev10_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev10  instR3 (arg4, arg3, zll_main_dev10_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev10_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev19 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev3_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev41_out;
  logic [2:0] zll_main_dev45_out;
  logic [2:0] zll_main_dev_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev45_outR1;
  logic [2:0] zll_main_dev_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev3  inst (arg5, zll_main_dev3_out);
  assign zi0 = zll_main_dev3_out;
  ZLL_Main_dev41  instR1 (arg5, arg0, zll_main_dev41_out);
  ZLL_Main_dev45  instR2 (zll_main_dev41_out, arg1, zll_main_dev45_out);
  ZLL_Main_dev  instR3 (arg4, zll_main_dev45_out, zll_main_dev_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev45  instR4 (arg5, arg1, zll_main_dev45_outR1);
  ZLL_Main_dev  instR5 (arg4, zll_main_dev45_outR1, zll_main_dev_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev10 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev6 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_dev3 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_dev (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_dev31_out;
  ZLL_Main_dev31  inst (arg0, arg1, zll_main_dev31_out);
  assign res = zll_main_dev31_out;
endmodule