module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zi0;
  logic [0:0] zll_main_dev3_out;
  logic [0:0] zll_main_dev3_outR1;
  logic [0:0] zll_main_dev3_outR2;
  logic [0:0] zll_main_dev3_outR3;
  logic [0:0] zll_main_dev3_outR4;
  logic [0:0] zll_main_dev3_outR5;
  logic [0:0] zll_main_dev3_outR6;
  logic [0:0] zll_main_dev3_outR7;
  logic [2:0] zll_main_dev150_out;
  logic [2:0] zll_main_dev149_out;
  logic [2:0] zi7;
  logic [2:0] zll_main_dev150_outR1;
  logic [0:0] zll_main_dev141_out;
  logic [0:0] zi8;
  logic [2:0] zll_main_dev138_out;
  logic [2:0] zi10;
  logic [0:0] zll_main_dev71_out;
  logic [0:0] zll_main_dev71_outR1;
  logic [0:0] zll_main_dev71_outR2;
  logic [0:0] zll_main_dev71_outR3;
  logic [0:0] zll_main_dev71_outR4;
  logic [0:0] zll_main_dev71_outR5;
  logic [0:0] zll_main_dev71_outR6;
  logic [0:0] zll_main_dev71_outR7;
  logic [2:0] zll_main_dev150_outR2;
  logic [2:0] zll_main_dev149_outR1;
  logic [2:0] zi14;
  logic [2:0] zll_main_dev150_outR3;
  logic [0:0] zll_main_dev141_outR1;
  logic [0:0] zi15;
  logic [2:0] zll_main_dev138_outR1;
  logic [2:0] zi17;
  logic [0:0] zll_main_dev63_out;
  logic [0:0] zll_main_dev63_outR1;
  logic [0:0] zll_main_dev63_outR2;
  logic [0:0] zll_main_dev63_outR3;
  logic [0:0] zll_main_dev63_outR4;
  logic [0:0] zll_main_dev63_outR5;
  logic [0:0] zll_main_dev63_outR6;
  logic [0:0] zll_main_dev63_outR7;
  logic [2:0] zll_main_dev150_outR4;
  logic [2:0] zll_main_dev149_outR2;
  logic [2:0] zi21;
  logic [2:0] zll_main_dev150_outR5;
  logic [0:0] zll_main_dev141_outR2;
  logic [0:0] zi22;
  logic [2:0] zll_main_dev138_outR2;
  logic [2:0] zi24;
  logic [0:0] zll_main_dev152_out;
  logic [0:0] zll_main_dev152_outR1;
  logic [0:0] zll_main_dev152_outR2;
  logic [0:0] zll_main_dev152_outR3;
  logic [0:0] zll_main_dev152_outR4;
  logic [0:0] zll_main_dev152_outR5;
  logic [0:0] zll_main_dev152_outR6;
  logic [0:0] zll_main_dev152_outR7;
  logic [31:0] zres;
  assign zi0 = __in0 * 8'h2;
  ZLL_Main_dev3  inst (__in0, 3'h2, 3'h1, zi0, 3'h0, zll_main_dev3_out);
  ZLL_Main_dev3  instR1 (__in0, 3'h2, 3'h1, zi0, 3'h1, zll_main_dev3_outR1);
  ZLL_Main_dev3  instR2 (__in0, 3'h2, 3'h1, zi0, 3'h2, zll_main_dev3_outR2);
  ZLL_Main_dev3  instR3 (__in0, 3'h2, 3'h1, zi0, 3'h3, zll_main_dev3_outR3);
  ZLL_Main_dev3  instR4 (__in0, 3'h2, 3'h1, zi0, 3'h4, zll_main_dev3_outR4);
  ZLL_Main_dev3  instR5 (__in0, 3'h2, 3'h1, zi0, 3'h5, zll_main_dev3_outR5);
  ZLL_Main_dev3  instR6 (__in0, 3'h2, 3'h1, zi0, 3'h6, zll_main_dev3_outR6);
  ZLL_Main_dev3  instR7 (__in0, 3'h2, 3'h1, zi0, 3'h7, zll_main_dev3_outR7);
  ZLL_Main_dev150  instR8 (__in0, zll_main_dev150_out);
  ZLL_Main_dev149  instR9 (zll_main_dev150_out, 3'h2, zll_main_dev149_out);
  assign zi7 = zll_main_dev149_out;
  ZLL_Main_dev150  instR10 (__in0, zll_main_dev150_outR1);
  ZLL_Main_dev141  instR11 (zll_main_dev150_outR1, zll_main_dev141_out);
  assign zi8 = zll_main_dev141_out;
  ZLL_Main_dev138  instR12 (zi7, 3'h1, zll_main_dev138_out);
  assign zi10 = (zi8 == 1'h1) ? zi7 : zll_main_dev138_out;
  ZLL_Main_dev71  instR13 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h0, zll_main_dev71_out);
  ZLL_Main_dev71  instR14 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h1, zll_main_dev71_outR1);
  ZLL_Main_dev71  instR15 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h2, zll_main_dev71_outR2);
  ZLL_Main_dev71  instR16 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h3, zll_main_dev71_outR3);
  ZLL_Main_dev71  instR17 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h4, zll_main_dev71_outR4);
  ZLL_Main_dev71  instR18 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h5, zll_main_dev71_outR5);
  ZLL_Main_dev71  instR19 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h6, zll_main_dev71_outR6);
  ZLL_Main_dev71  instR20 (zi0, 3'h2, zi10, 3'h1, __in0, 3'h7, zll_main_dev71_outR7);
  ZLL_Main_dev150  instR21 (__in0, zll_main_dev150_outR2);
  ZLL_Main_dev149  instR22 (zll_main_dev150_outR2, 3'h2, zll_main_dev149_outR1);
  assign zi14 = zll_main_dev149_outR1;
  ZLL_Main_dev150  instR23 (__in0, zll_main_dev150_outR3);
  ZLL_Main_dev141  instR24 (zll_main_dev150_outR3, zll_main_dev141_outR1);
  assign zi15 = zll_main_dev141_outR1;
  ZLL_Main_dev138  instR25 (zi14, 3'h1, zll_main_dev138_outR1);
  assign zi17 = (zi15 == 1'h1) ? zi14 : zll_main_dev138_outR1;
  ZLL_Main_dev63  instR26 (__in0, zi0, zi17, 3'h2, 3'h0, zll_main_dev63_out);
  ZLL_Main_dev63  instR27 (__in0, zi0, zi17, 3'h2, 3'h1, zll_main_dev63_outR1);
  ZLL_Main_dev63  instR28 (__in0, zi0, zi17, 3'h2, 3'h2, zll_main_dev63_outR2);
  ZLL_Main_dev63  instR29 (__in0, zi0, zi17, 3'h2, 3'h3, zll_main_dev63_outR3);
  ZLL_Main_dev63  instR30 (__in0, zi0, zi17, 3'h2, 3'h4, zll_main_dev63_outR4);
  ZLL_Main_dev63  instR31 (__in0, zi0, zi17, 3'h2, 3'h5, zll_main_dev63_outR5);
  ZLL_Main_dev63  instR32 (__in0, zi0, zi17, 3'h2, 3'h6, zll_main_dev63_outR6);
  ZLL_Main_dev63  instR33 (__in0, zi0, zi17, 3'h2, 3'h7, zll_main_dev63_outR7);
  ZLL_Main_dev150  instR34 (__in0, zll_main_dev150_outR4);
  ZLL_Main_dev149  instR35 (zll_main_dev150_outR4, 3'h2, zll_main_dev149_outR2);
  assign zi21 = zll_main_dev149_outR2;
  ZLL_Main_dev150  instR36 (__in0, zll_main_dev150_outR5);
  ZLL_Main_dev141  instR37 (zll_main_dev150_outR5, zll_main_dev141_outR2);
  assign zi22 = zll_main_dev141_outR2;
  ZLL_Main_dev138  instR38 (zi21, 3'h1, zll_main_dev138_outR2);
  assign zi24 = (zi22 == 1'h1) ? zi21 : zll_main_dev138_outR2;
  ZLL_Main_dev152  instR39 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h0, zll_main_dev152_out);
  ZLL_Main_dev152  instR40 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h1, zll_main_dev152_outR1);
  ZLL_Main_dev152  instR41 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h2, zll_main_dev152_outR2);
  ZLL_Main_dev152  instR42 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h3, zll_main_dev152_outR3);
  ZLL_Main_dev152  instR43 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h4, zll_main_dev152_outR4);
  ZLL_Main_dev152  instR44 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h5, zll_main_dev152_outR5);
  ZLL_Main_dev152  instR45 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h6, zll_main_dev152_outR6);
  ZLL_Main_dev152  instR46 (zi24, zi0, 3'h2, __in0, 3'h1, 3'h7, zll_main_dev152_outR7);
  assign zres = {zll_main_dev3_out, zll_main_dev3_outR1, zll_main_dev3_outR2, zll_main_dev3_outR3, zll_main_dev3_outR4, zll_main_dev3_outR5, zll_main_dev3_outR6, zll_main_dev3_outR7, {zll_main_dev71_out, zll_main_dev71_outR1, zll_main_dev71_outR2, zll_main_dev71_outR3, zll_main_dev71_outR4, zll_main_dev71_outR5, zll_main_dev71_outR6, zll_main_dev71_outR7}, {zll_main_dev63_out, zll_main_dev63_outR1, zll_main_dev63_outR2, zll_main_dev63_outR3, zll_main_dev63_outR4, zll_main_dev63_outR5, zll_main_dev63_outR6, zll_main_dev63_outR7}, {zll_main_dev152_out, zll_main_dev152_outR1, zll_main_dev152_outR2, zll_main_dev152_outR3, zll_main_dev152_outR4, zll_main_dev152_outR5, zll_main_dev152_outR6, zll_main_dev152_outR7}};
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_Main_dev152 (input logic [2:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev133_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev122_out;
  logic [2:0] zll_main_dev138_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev136_out;
  logic [2:0] zll_main_dev122_outR1;
  logic [2:0] zll_main_dev138_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev133  inst (arg5, arg0, zll_main_dev133_out);
  assign zi0 = zll_main_dev133_out;
  ZLL_Main_dev122  instR1 (arg5, arg2, zll_main_dev122_out);
  ZLL_Main_dev138  instR2 (zll_main_dev122_out, arg4, zll_main_dev138_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev138_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev136  instR3 (arg5, arg0, zll_main_dev136_out);
  ZLL_Main_dev122  instR4 (zll_main_dev136_out, arg2, zll_main_dev122_outR1);
  ZLL_Main_dev138  instR5 (zll_main_dev122_outR1, arg4, zll_main_dev138_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev138_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev150 (input logic [7:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_dev149 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev141 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_dev138 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev136 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev133 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_dev122 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev71 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [7:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev141_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev149_out;
  logic [2:0] zll_main_dev138_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev136_out;
  logic [2:0] zll_main_dev149_outR1;
  logic [2:0] zll_main_dev138_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev141  inst (arg5, zll_main_dev141_out);
  assign zi0 = zll_main_dev141_out;
  ZLL_Main_dev149  instR1 (arg5, arg1, zll_main_dev149_out);
  ZLL_Main_dev138  instR2 (arg2, zll_main_dev149_out, zll_main_dev138_out);
  assign slice_in = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev138_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev136  instR3 (arg5, arg3, zll_main_dev136_out);
  ZLL_Main_dev149  instR4 (zll_main_dev136_out, arg1, zll_main_dev149_outR1);
  ZLL_Main_dev138  instR5 (arg2, zll_main_dev149_outR1, zll_main_dev138_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev138_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev63 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev133_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev122_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev136_out;
  logic [2:0] zll_main_dev122_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev133  inst (arg4, arg2, zll_main_dev133_out);
  assign zi0 = zll_main_dev133_out;
  ZLL_Main_dev122  instR1 (arg4, arg3, zll_main_dev122_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev122_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev136  instR2 (arg4, arg2, zll_main_dev136_out);
  ZLL_Main_dev122  instR3 (zll_main_dev136_out, arg3, zll_main_dev122_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev122_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev3 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev141_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev149_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev136_out;
  logic [2:0] zll_main_dev149_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev141  inst (arg4, zll_main_dev141_out);
  assign zi0 = zll_main_dev141_out;
  ZLL_Main_dev149  instR1 (arg4, arg1, zll_main_dev149_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev149_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev136  instR2 (arg4, arg2, zll_main_dev136_out);
  ZLL_Main_dev149  instR3 (zll_main_dev136_out, arg1, zll_main_dev149_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev149_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule