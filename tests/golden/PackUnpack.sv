module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zi0;
  logic [0:0] zll_main_dev81_out;
  logic [0:0] zll_main_dev81_outR1;
  logic [0:0] zll_main_dev81_outR2;
  logic [0:0] zll_main_dev81_outR3;
  logic [0:0] zll_main_dev81_outR4;
  logic [0:0] zll_main_dev81_outR5;
  logic [0:0] zll_main_dev81_outR6;
  logic [0:0] zll_main_dev81_outR7;
  logic [2:0] zll_main_dev82_out;
  logic [2:0] zll_main_dev101_out;
  logic [2:0] zi1;
  logic [2:0] zll_main_dev82_outR1;
  logic [0:0] zll_main_dev97_out;
  logic [0:0] zi2;
  logic [2:0] zll_main_dev100_out;
  logic [2:0] zi3;
  logic [0:0] zll_main_dev41_out;
  logic [0:0] zll_main_dev41_outR1;
  logic [0:0] zll_main_dev41_outR2;
  logic [0:0] zll_main_dev41_outR3;
  logic [0:0] zll_main_dev41_outR4;
  logic [0:0] zll_main_dev41_outR5;
  logic [0:0] zll_main_dev41_outR6;
  logic [0:0] zll_main_dev41_outR7;
  logic [2:0] zll_main_dev82_outR2;
  logic [2:0] zll_main_dev101_outR1;
  logic [2:0] zi4;
  logic [2:0] zll_main_dev82_outR3;
  logic [0:0] zll_main_dev97_outR1;
  logic [0:0] zi5;
  logic [2:0] zll_main_dev70_out;
  logic [2:0] zi6;
  logic [0:0] zll_main_dev47_out;
  logic [0:0] zll_main_dev47_outR1;
  logic [0:0] zll_main_dev47_outR2;
  logic [0:0] zll_main_dev47_outR3;
  logic [0:0] zll_main_dev47_outR4;
  logic [0:0] zll_main_dev47_outR5;
  logic [0:0] zll_main_dev47_outR6;
  logic [0:0] zll_main_dev47_outR7;
  logic [2:0] zll_main_dev82_outR4;
  logic [2:0] zll_main_dev101_outR2;
  logic [2:0] zi7;
  logic [2:0] zll_main_dev82_outR5;
  logic [0:0] zll_main_dev97_outR2;
  logic [0:0] zi8;
  logic [2:0] zll_main_dev100_outR1;
  logic [2:0] zi9;
  logic [0:0] zll_main_dev80_out;
  logic [0:0] zll_main_dev80_outR1;
  logic [0:0] zll_main_dev80_outR2;
  logic [0:0] zll_main_dev80_outR3;
  logic [0:0] zll_main_dev80_outR4;
  logic [0:0] zll_main_dev80_outR5;
  logic [0:0] zll_main_dev80_outR6;
  logic [0:0] zll_main_dev80_outR7;
  logic [31:0] zres;
  assign zi0 = __in0 * 8'h2;
  ZLL_Main_dev81  inst (3'h2, 3'h1, zi0, __in0, 3'h0, zll_main_dev81_out);
  ZLL_Main_dev81  instR1 (3'h2, 3'h1, zi0, __in0, 3'h1, zll_main_dev81_outR1);
  ZLL_Main_dev81  instR2 (3'h2, 3'h1, zi0, __in0, 3'h2, zll_main_dev81_outR2);
  ZLL_Main_dev81  instR3 (3'h2, 3'h1, zi0, __in0, 3'h3, zll_main_dev81_outR3);
  ZLL_Main_dev81  instR4 (3'h2, 3'h1, zi0, __in0, 3'h4, zll_main_dev81_outR4);
  ZLL_Main_dev81  instR5 (3'h2, 3'h1, zi0, __in0, 3'h5, zll_main_dev81_outR5);
  ZLL_Main_dev81  instR6 (3'h2, 3'h1, zi0, __in0, 3'h6, zll_main_dev81_outR6);
  ZLL_Main_dev81  instR7 (3'h2, 3'h1, zi0, __in0, 3'h7, zll_main_dev81_outR7);
  ZLL_Main_dev82  instR8 (__in0, zll_main_dev82_out);
  ZLL_Main_dev101  instR9 (zll_main_dev82_out, 3'h2, zll_main_dev101_out);
  assign zi1 = zll_main_dev101_out;
  ZLL_Main_dev82  instR10 (__in0, zll_main_dev82_outR1);
  ZLL_Main_dev97  instR11 (zll_main_dev82_outR1, zll_main_dev97_out);
  assign zi2 = zll_main_dev97_out;
  ZLL_Main_dev100  instR12 (zi1, 3'h1, zll_main_dev100_out);
  assign zi3 = (zi2 == 1'h0) ? zll_main_dev100_out : zi1;
  ZLL_Main_dev41  instR13 (3'h1, 3'h2, __in0, zi0, zi3, 3'h0, zll_main_dev41_out);
  ZLL_Main_dev41  instR14 (3'h1, 3'h2, __in0, zi0, zi3, 3'h1, zll_main_dev41_outR1);
  ZLL_Main_dev41  instR15 (3'h1, 3'h2, __in0, zi0, zi3, 3'h2, zll_main_dev41_outR2);
  ZLL_Main_dev41  instR16 (3'h1, 3'h2, __in0, zi0, zi3, 3'h3, zll_main_dev41_outR3);
  ZLL_Main_dev41  instR17 (3'h1, 3'h2, __in0, zi0, zi3, 3'h4, zll_main_dev41_outR4);
  ZLL_Main_dev41  instR18 (3'h1, 3'h2, __in0, zi0, zi3, 3'h5, zll_main_dev41_outR5);
  ZLL_Main_dev41  instR19 (3'h1, 3'h2, __in0, zi0, zi3, 3'h6, zll_main_dev41_outR6);
  ZLL_Main_dev41  instR20 (3'h1, 3'h2, __in0, zi0, zi3, 3'h7, zll_main_dev41_outR7);
  ZLL_Main_dev82  instR21 (__in0, zll_main_dev82_outR2);
  ZLL_Main_dev101  instR22 (zll_main_dev82_outR2, 3'h2, zll_main_dev101_outR1);
  assign zi4 = zll_main_dev101_outR1;
  ZLL_Main_dev82  instR23 (__in0, zll_main_dev82_outR3);
  ZLL_Main_dev97  instR24 (zll_main_dev82_outR3, zll_main_dev97_outR1);
  assign zi5 = zll_main_dev97_outR1;
  ZLL_Main_dev70  instR25 (zi4, 3'h1, zll_main_dev70_out);
  assign zi6 = (zi5 == 1'h0) ? zll_main_dev70_out : zi4;
  ZLL_Main_dev47  instR26 (zi0, __in0, zi6, 3'h2, 3'h0, zll_main_dev47_out);
  ZLL_Main_dev47  instR27 (zi0, __in0, zi6, 3'h2, 3'h1, zll_main_dev47_outR1);
  ZLL_Main_dev47  instR28 (zi0, __in0, zi6, 3'h2, 3'h2, zll_main_dev47_outR2);
  ZLL_Main_dev47  instR29 (zi0, __in0, zi6, 3'h2, 3'h3, zll_main_dev47_outR3);
  ZLL_Main_dev47  instR30 (zi0, __in0, zi6, 3'h2, 3'h4, zll_main_dev47_outR4);
  ZLL_Main_dev47  instR31 (zi0, __in0, zi6, 3'h2, 3'h5, zll_main_dev47_outR5);
  ZLL_Main_dev47  instR32 (zi0, __in0, zi6, 3'h2, 3'h6, zll_main_dev47_outR6);
  ZLL_Main_dev47  instR33 (zi0, __in0, zi6, 3'h2, 3'h7, zll_main_dev47_outR7);
  ZLL_Main_dev82  instR34 (__in0, zll_main_dev82_outR4);
  ZLL_Main_dev101  instR35 (zll_main_dev82_outR4, 3'h2, zll_main_dev101_outR2);
  assign zi7 = zll_main_dev101_outR2;
  ZLL_Main_dev82  instR36 (__in0, zll_main_dev82_outR5);
  ZLL_Main_dev97  instR37 (zll_main_dev82_outR5, zll_main_dev97_outR2);
  assign zi8 = zll_main_dev97_outR2;
  ZLL_Main_dev100  instR38 (zi7, 3'h1, zll_main_dev100_outR1);
  assign zi9 = (zi8 == 1'h0) ? zll_main_dev100_outR1 : zi7;
  ZLL_Main_dev80  instR39 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h0, zll_main_dev80_out);
  ZLL_Main_dev80  instR40 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h1, zll_main_dev80_outR1);
  ZLL_Main_dev80  instR41 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h2, zll_main_dev80_outR2);
  ZLL_Main_dev80  instR42 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h3, zll_main_dev80_outR3);
  ZLL_Main_dev80  instR43 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h4, zll_main_dev80_outR4);
  ZLL_Main_dev80  instR44 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h5, zll_main_dev80_outR5);
  ZLL_Main_dev80  instR45 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h6, zll_main_dev80_outR6);
  ZLL_Main_dev80  instR46 (__in0, 3'h2, zi0, zi9, 3'h1, 3'h7, zll_main_dev80_outR7);
  assign zres = {zll_main_dev81_out, zll_main_dev81_outR1, zll_main_dev81_outR2, zll_main_dev81_outR3, zll_main_dev81_outR4, zll_main_dev81_outR5, zll_main_dev81_outR6, zll_main_dev81_outR7, {zll_main_dev41_out, zll_main_dev41_outR1, zll_main_dev41_outR2, zll_main_dev41_outR3, zll_main_dev41_outR4, zll_main_dev41_outR5, zll_main_dev41_outR6, zll_main_dev41_outR7}, {zll_main_dev47_out, zll_main_dev47_outR1, zll_main_dev47_outR2, zll_main_dev47_outR3, zll_main_dev47_outR4, zll_main_dev47_outR5, zll_main_dev47_outR6, zll_main_dev47_outR7}, {zll_main_dev80_out, zll_main_dev80_outR1, zll_main_dev80_outR2, zll_main_dev80_outR3, zll_main_dev80_outR4, zll_main_dev80_outR5, zll_main_dev80_outR6, zll_main_dev80_outR7}};
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_Main_dev101 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev100 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_dev70_out;
  ZLL_Main_dev70  inst (arg0, arg1, zll_main_dev70_out);
  assign res = zll_main_dev70_out;
endmodule

module ZLL_Main_dev97 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_dev88 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev82 (input logic [7:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_dev81 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev97_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev88_out;
  logic [2:0] zll_main_dev101_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev101_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev97  inst (arg4, zll_main_dev97_out);
  assign zi0 = zll_main_dev97_out;
  ZLL_Main_dev88  instR1 (arg4, arg1, zll_main_dev88_out);
  ZLL_Main_dev101  instR2 (zll_main_dev88_out, arg0, zll_main_dev101_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev101_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev101  instR3 (arg4, arg0, zll_main_dev101_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev101_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev80 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev67_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev88_out;
  logic [2:0] zll_main_dev71_out;
  logic [2:0] zll_main_dev100_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev71_outR1;
  logic [2:0] zll_main_dev100_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev67  inst (arg5, arg3, zll_main_dev67_out);
  assign zi0 = zll_main_dev67_out;
  ZLL_Main_dev88  instR1 (arg5, arg3, zll_main_dev88_out);
  ZLL_Main_dev71  instR2 (zll_main_dev88_out, arg1, zll_main_dev71_out);
  ZLL_Main_dev100  instR3 (zll_main_dev71_out, arg4, zll_main_dev100_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev100_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev71  instR4 (arg5, arg1, zll_main_dev71_outR1);
  ZLL_Main_dev100  instR5 (zll_main_dev71_outR1, arg4, zll_main_dev100_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev100_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev71 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev70 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev67 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_dev47 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev67_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev88_out;
  logic [2:0] zll_main_dev71_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev71_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev67  inst (arg4, arg2, zll_main_dev67_out);
  assign zi0 = zll_main_dev67_out;
  ZLL_Main_dev88  instR1 (arg4, arg2, zll_main_dev88_out);
  ZLL_Main_dev71  instR2 (zll_main_dev88_out, arg3, zll_main_dev71_out);
  assign slice_in = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev71_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev71  instR3 (arg4, arg3, zll_main_dev71_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev71_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev41 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev97_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev88_out;
  logic [2:0] zll_main_dev101_out;
  logic [2:0] zll_main_dev100_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev101_outR1;
  logic [2:0] zll_main_dev100_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev97  inst (arg5, zll_main_dev97_out);
  assign zi0 = zll_main_dev97_out;
  ZLL_Main_dev88  instR1 (arg5, arg0, zll_main_dev88_out);
  ZLL_Main_dev101  instR2 (zll_main_dev88_out, arg1, zll_main_dev101_out);
  ZLL_Main_dev100  instR3 (arg4, zll_main_dev101_out, zll_main_dev100_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev100_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev101  instR4 (arg5, arg1, zll_main_dev101_outR1);
  ZLL_Main_dev100  instR5 (arg4, zll_main_dev101_outR1, zll_main_dev100_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev100_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_in[0] : slice_inR1[0];
endmodule