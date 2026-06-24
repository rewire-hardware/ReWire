module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zi0;
  logic [0:0] zll_main_dev96_out;
  logic [0:0] zll_main_dev96_outR1;
  logic [0:0] zll_main_dev96_outR2;
  logic [0:0] zll_main_dev96_outR3;
  logic [0:0] zll_main_dev96_outR4;
  logic [0:0] zll_main_dev96_outR5;
  logic [0:0] zll_main_dev96_outR6;
  logic [0:0] zll_main_dev96_outR7;
  logic [2:0] zll_main_dev263_out;
  logic [2:0] zll_main_dev234_out;
  logic [2:0] zi7;
  logic [2:0] zll_main_dev263_outR1;
  logic [0:0] zll_main_dev271_out;
  logic [0:0] zi8;
  logic [2:0] zll_main_dev221_out;
  logic [2:0] zll_main_dev263_outR2;
  logic [0:0] zll_main_dev271_outR1;
  logic [2:0] zll_main_dev252_out;
  logic [2:0] zi9;
  logic [0:0] zll_main_dev104_out;
  logic [0:0] zll_main_dev104_outR1;
  logic [0:0] zll_main_dev104_outR2;
  logic [0:0] zll_main_dev104_outR3;
  logic [0:0] zll_main_dev104_outR4;
  logic [0:0] zll_main_dev104_outR5;
  logic [0:0] zll_main_dev104_outR6;
  logic [0:0] zll_main_dev104_outR7;
  logic [2:0] zll_main_dev263_outR3;
  logic [2:0] zll_main_dev234_outR1;
  logic [2:0] zi13;
  logic [2:0] zll_main_dev263_outR4;
  logic [0:0] zll_main_dev271_outR2;
  logic [0:0] zi14;
  logic [2:0] zll_main_dev221_outR1;
  logic [2:0] zll_main_dev269_out;
  logic [2:0] zi16;
  logic [0:0] zll_main_dev203_out;
  logic [0:0] zll_main_dev203_outR1;
  logic [0:0] zll_main_dev203_outR2;
  logic [0:0] zll_main_dev203_outR3;
  logic [0:0] zll_main_dev203_outR4;
  logic [0:0] zll_main_dev203_outR5;
  logic [0:0] zll_main_dev203_outR6;
  logic [0:0] zll_main_dev203_outR7;
  logic [2:0] zll_main_dev263_outR5;
  logic [2:0] zll_main_dev234_outR2;
  logic [2:0] zi20;
  logic [2:0] zll_main_dev263_outR6;
  logic [0:0] zll_main_dev271_outR3;
  logic [0:0] zi21;
  logic [2:0] zll_main_dev221_outR2;
  logic [2:0] zll_main_dev263_outR7;
  logic [0:0] zll_main_dev271_outR4;
  logic [2:0] zll_main_dev252_outR1;
  logic [2:0] zi22;
  logic [0:0] zll_main_dev220_out;
  logic [0:0] zll_main_dev220_outR1;
  logic [0:0] zll_main_dev220_outR2;
  logic [0:0] zll_main_dev220_outR3;
  logic [0:0] zll_main_dev220_outR4;
  logic [0:0] zll_main_dev220_outR5;
  logic [0:0] zll_main_dev220_outR6;
  logic [0:0] zll_main_dev220_outR7;
  logic [31:0] zres;
  assign zi0 = __in0 * 8'h2;
  ZLL_Main_dev96  inst (3'h2, zi0, __in0, 3'h1, 3'h0, zll_main_dev96_out);
  ZLL_Main_dev96  instR1 (3'h2, zi0, __in0, 3'h1, 3'h1, zll_main_dev96_outR1);
  ZLL_Main_dev96  instR2 (3'h2, zi0, __in0, 3'h1, 3'h2, zll_main_dev96_outR2);
  ZLL_Main_dev96  instR3 (3'h2, zi0, __in0, 3'h1, 3'h3, zll_main_dev96_outR3);
  ZLL_Main_dev96  instR4 (3'h2, zi0, __in0, 3'h1, 3'h4, zll_main_dev96_outR4);
  ZLL_Main_dev96  instR5 (3'h2, zi0, __in0, 3'h1, 3'h5, zll_main_dev96_outR5);
  ZLL_Main_dev96  instR6 (3'h2, zi0, __in0, 3'h1, 3'h6, zll_main_dev96_outR6);
  ZLL_Main_dev96  instR7 (3'h2, zi0, __in0, 3'h1, 3'h7, zll_main_dev96_outR7);
  ZLL_Main_dev263  instR8 (__in0, zll_main_dev263_out);
  ZLL_Main_dev234  instR9 (zll_main_dev263_out, 3'h2, zll_main_dev234_out);
  assign zi7 = zll_main_dev234_out;
  ZLL_Main_dev263  instR10 (__in0, zll_main_dev263_outR1);
  ZLL_Main_dev271  instR11 (zll_main_dev263_outR1, zll_main_dev271_out);
  assign zi8 = zll_main_dev271_out;
  ZLL_Main_dev221  instR12 (zi7, zll_main_dev221_out);
  ZLL_Main_dev263  instR13 (__in0, zll_main_dev263_outR2);
  ZLL_Main_dev271  instR14 (zll_main_dev263_outR2, zll_main_dev271_outR1);
  ZLL_Main_dev252  instR15 (3'h1, zi7, zll_main_dev271_outR1, zll_main_dev252_out);
  assign zi9 = (zi8 == 1'h1) ? zll_main_dev221_out : zll_main_dev252_out;
  ZLL_Main_dev104  instR16 (3'h2, 3'h1, zi0, __in0, zi9, 3'h0, zll_main_dev104_out);
  ZLL_Main_dev104  instR17 (3'h2, 3'h1, zi0, __in0, zi9, 3'h1, zll_main_dev104_outR1);
  ZLL_Main_dev104  instR18 (3'h2, 3'h1, zi0, __in0, zi9, 3'h2, zll_main_dev104_outR2);
  ZLL_Main_dev104  instR19 (3'h2, 3'h1, zi0, __in0, zi9, 3'h3, zll_main_dev104_outR3);
  ZLL_Main_dev104  instR20 (3'h2, 3'h1, zi0, __in0, zi9, 3'h4, zll_main_dev104_outR4);
  ZLL_Main_dev104  instR21 (3'h2, 3'h1, zi0, __in0, zi9, 3'h5, zll_main_dev104_outR5);
  ZLL_Main_dev104  instR22 (3'h2, 3'h1, zi0, __in0, zi9, 3'h6, zll_main_dev104_outR6);
  ZLL_Main_dev104  instR23 (3'h2, 3'h1, zi0, __in0, zi9, 3'h7, zll_main_dev104_outR7);
  ZLL_Main_dev263  instR24 (__in0, zll_main_dev263_outR3);
  ZLL_Main_dev234  instR25 (zll_main_dev263_outR3, 3'h2, zll_main_dev234_outR1);
  assign zi13 = zll_main_dev234_outR1;
  ZLL_Main_dev263  instR26 (__in0, zll_main_dev263_outR4);
  ZLL_Main_dev271  instR27 (zll_main_dev263_outR4, zll_main_dev271_outR2);
  assign zi14 = zll_main_dev271_outR2;
  ZLL_Main_dev221  instR28 (zi13, zll_main_dev221_outR1);
  ZLL_Main_dev269  instR29 (zi13, 3'h1, zll_main_dev269_out);
  assign zi16 = (zi14 == 1'h1) ? zll_main_dev221_outR1 : zll_main_dev269_out;
  ZLL_Main_dev203  instR30 (3'h2, __in0, zi0, zi16, 3'h0, zll_main_dev203_out);
  ZLL_Main_dev203  instR31 (3'h2, __in0, zi0, zi16, 3'h1, zll_main_dev203_outR1);
  ZLL_Main_dev203  instR32 (3'h2, __in0, zi0, zi16, 3'h2, zll_main_dev203_outR2);
  ZLL_Main_dev203  instR33 (3'h2, __in0, zi0, zi16, 3'h3, zll_main_dev203_outR3);
  ZLL_Main_dev203  instR34 (3'h2, __in0, zi0, zi16, 3'h4, zll_main_dev203_outR4);
  ZLL_Main_dev203  instR35 (3'h2, __in0, zi0, zi16, 3'h5, zll_main_dev203_outR5);
  ZLL_Main_dev203  instR36 (3'h2, __in0, zi0, zi16, 3'h6, zll_main_dev203_outR6);
  ZLL_Main_dev203  instR37 (3'h2, __in0, zi0, zi16, 3'h7, zll_main_dev203_outR7);
  ZLL_Main_dev263  instR38 (__in0, zll_main_dev263_outR5);
  ZLL_Main_dev234  instR39 (zll_main_dev263_outR5, 3'h2, zll_main_dev234_outR2);
  assign zi20 = zll_main_dev234_outR2;
  ZLL_Main_dev263  instR40 (__in0, zll_main_dev263_outR6);
  ZLL_Main_dev271  instR41 (zll_main_dev263_outR6, zll_main_dev271_outR3);
  assign zi21 = zll_main_dev271_outR3;
  ZLL_Main_dev221  instR42 (zi20, zll_main_dev221_outR2);
  ZLL_Main_dev263  instR43 (__in0, zll_main_dev263_outR7);
  ZLL_Main_dev271  instR44 (zll_main_dev263_outR7, zll_main_dev271_outR4);
  ZLL_Main_dev252  instR45 (3'h1, zi20, zll_main_dev271_outR4, zll_main_dev252_outR1);
  assign zi22 = (zi21 == 1'h1) ? zll_main_dev221_outR2 : zll_main_dev252_outR1;
  ZLL_Main_dev220  instR46 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h0, zll_main_dev220_out);
  ZLL_Main_dev220  instR47 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h1, zll_main_dev220_outR1);
  ZLL_Main_dev220  instR48 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h2, zll_main_dev220_outR2);
  ZLL_Main_dev220  instR49 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h3, zll_main_dev220_outR3);
  ZLL_Main_dev220  instR50 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h4, zll_main_dev220_outR4);
  ZLL_Main_dev220  instR51 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h5, zll_main_dev220_outR5);
  ZLL_Main_dev220  instR52 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h6, zll_main_dev220_outR6);
  ZLL_Main_dev220  instR53 (zi22, zi0, 3'h2, __in0, 3'h1, 3'h7, zll_main_dev220_outR7);
  assign zres = {zll_main_dev96_out, zll_main_dev96_outR1, zll_main_dev96_outR2, zll_main_dev96_outR3, zll_main_dev96_outR4, zll_main_dev96_outR5, zll_main_dev96_outR6, zll_main_dev96_outR7, {zll_main_dev104_out, zll_main_dev104_outR1, zll_main_dev104_outR2, zll_main_dev104_outR3, zll_main_dev104_outR4, zll_main_dev104_outR5, zll_main_dev104_outR6, zll_main_dev104_outR7}, {zll_main_dev203_out, zll_main_dev203_outR1, zll_main_dev203_outR2, zll_main_dev203_outR3, zll_main_dev203_outR4, zll_main_dev203_outR5, zll_main_dev203_outR6, zll_main_dev203_outR7}, {zll_main_dev220_out, zll_main_dev220_outR1, zll_main_dev220_outR2, zll_main_dev220_outR3, zll_main_dev220_outR4, zll_main_dev220_outR5, zll_main_dev220_outR6, zll_main_dev220_outR7}};
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_Main_dev281 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev271 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_dev269 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev263 (input logic [7:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_dev252 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [2:0] zll_main_dev269_out;
  ZLL_Main_dev269  inst (arg1, arg0, zll_main_dev269_out);
  assign res = zll_main_dev269_out;
endmodule

module ZLL_Main_dev248 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev234 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_dev221 (input logic [2:0] arg0,
  output logic [2:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_dev220 (input logic [2:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev177_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev248_out;
  logic [2:0] zll_main_dev269_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev281_out;
  logic [2:0] zll_main_dev248_outR1;
  logic [2:0] zll_main_dev269_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev177  inst (arg5, arg0, zll_main_dev177_out);
  assign zi0 = zll_main_dev177_out;
  ZLL_Main_dev248  instR1 (arg5, arg2, zll_main_dev248_out);
  ZLL_Main_dev269  instR2 (zll_main_dev248_out, arg4, zll_main_dev269_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev269_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev281  instR3 (arg5, arg0, zll_main_dev281_out);
  ZLL_Main_dev248  instR4 (zll_main_dev281_out, arg2, zll_main_dev248_outR1);
  ZLL_Main_dev269  instR5 (zll_main_dev248_outR1, arg4, zll_main_dev269_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev269_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev203 (input logic [2:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev177_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev248_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev281_out;
  logic [2:0] zll_main_dev248_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev177  inst (arg4, arg3, zll_main_dev177_out);
  assign zi0 = zll_main_dev177_out;
  ZLL_Main_dev248  instR1 (arg4, arg0, zll_main_dev248_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev248_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev281  instR2 (arg4, arg3, zll_main_dev281_out);
  ZLL_Main_dev248  instR3 (zll_main_dev281_out, arg0, zll_main_dev248_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev248_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev177 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_dev104 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [7:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zll_main_dev271_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev234_out;
  logic [2:0] zll_main_dev269_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev281_out;
  logic [2:0] zll_main_dev234_outR1;
  logic [2:0] zll_main_dev269_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev271  inst (arg5, zll_main_dev271_out);
  assign zi0 = zll_main_dev271_out;
  ZLL_Main_dev234  instR1 (arg5, arg0, zll_main_dev234_out);
  ZLL_Main_dev269  instR2 (arg4, zll_main_dev234_out, zll_main_dev269_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev269_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev281  instR3 (arg5, arg1, zll_main_dev281_out);
  ZLL_Main_dev234  instR4 (zll_main_dev281_out, arg0, zll_main_dev234_outR1);
  ZLL_Main_dev269  instR5 (arg4, zll_main_dev234_outR1, zll_main_dev269_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev269_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule

module ZLL_Main_dev96 (input logic [2:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zll_main_dev271_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_dev234_out;
  logic [7:0] slice_in;
  logic [2:0] zll_main_dev281_out;
  logic [2:0] zll_main_dev234_outR1;
  logic [7:0] slice_inR1;
  ZLL_Main_dev271  inst (arg4, zll_main_dev271_out);
  assign zi0 = zll_main_dev271_out;
  ZLL_Main_dev234  instR1 (arg4, arg0, zll_main_dev234_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev234_out}) - 128'h1) * 128'h1);
  ZLL_Main_dev281  instR2 (arg4, arg3, zll_main_dev281_out);
  ZLL_Main_dev234  instR3 (zll_main_dev281_out, arg0, zll_main_dev234_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_dev234_outR1}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h1) ? slice_in[0] : slice_inR1[0];
endmodule