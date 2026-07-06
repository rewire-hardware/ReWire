module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [7:0] zll_main_compute16_out;
  logic [7:0] zll_main_compute16_outR1;
  logic [7:0] zll_main_compute16_outR2;
  logic [7:0] zll_main_compute16_outR3;
  logic [7:0] zll_main_compute16_outR4;
  logic [7:0] zll_main_compute16_outR5;
  logic [7:0] zll_main_compute16_outR6;
  logic [7:0] zll_main_compute16_outR7;
  logic [7:0] zll_main_compute36_out;
  logic [7:0] zll_main_compute36_outR1;
  logic [7:0] zll_main_compute36_outR2;
  logic [7:0] zll_main_compute36_outR3;
  logic [7:0] zll_main_compute36_outR4;
  logic [7:0] zll_main_compute36_outR5;
  logic [7:0] zll_main_compute36_outR6;
  logic [7:0] zll_main_compute36_outR7;
  logic [127:0] zi6;
  logic [63:0] zi7;
  logic [63:0] zi8;
  logic [63:0] zi9;
  logic [63:0] zi10;
  logic [7:0] zll_main_compute14_out;
  logic [7:0] zll_main_compute14_outR1;
  logic [7:0] zll_main_compute14_outR2;
  logic [7:0] zll_main_compute14_outR3;
  logic [7:0] zll_main_compute14_outR4;
  logic [7:0] zll_main_compute14_outR5;
  logic [7:0] zll_main_compute14_outR6;
  logic [7:0] zll_main_compute14_outR7;
  logic [7:0] zll_main_compute27_out;
  logic [7:0] zll_main_compute27_outR1;
  logic [7:0] zll_main_compute27_outR2;
  logic [7:0] zll_main_compute27_outR3;
  logic [7:0] zll_main_compute27_outR4;
  logic [7:0] zll_main_compute27_outR5;
  logic [7:0] zll_main_compute27_outR6;
  logic [7:0] zll_main_compute27_outR7;
  logic [127:0] zi15;
  logic [63:0] zi16;
  logic [63:0] zi17;
  logic [63:0] zi18;
  logic [63:0] zi19;
  logic [7:0] zll_main_compute4_out;
  logic [7:0] zll_main_compute4_outR1;
  logic [7:0] zll_main_compute4_outR2;
  logic [7:0] zll_main_compute4_outR3;
  logic [7:0] zll_main_compute4_outR4;
  logic [7:0] zll_main_compute4_outR5;
  logic [7:0] zll_main_compute4_outR6;
  logic [7:0] zll_main_compute4_outR7;
  logic [7:0] zll_main_compute47_out;
  logic [7:0] zll_main_compute47_outR1;
  logic [7:0] zll_main_compute47_outR2;
  logic [7:0] zll_main_compute47_outR3;
  logic [7:0] zll_main_compute47_outR4;
  logic [7:0] zll_main_compute47_outR5;
  logic [7:0] zll_main_compute47_outR6;
  logic [7:0] zll_main_compute47_outR7;
  logic [127:0] zi22;
  logic [63:0] zi23;
  logic [63:0] zi24;
  logic [127:0] zi25;
  logic [127:0] zres;
  ZLL_Main_compute16  inst (__in0, 3'h2, __in1, 3'h4, 3'h0, zll_main_compute16_out);
  ZLL_Main_compute16  instR1 (__in0, 3'h2, __in1, 3'h4, 3'h1, zll_main_compute16_outR1);
  ZLL_Main_compute16  instR2 (__in0, 3'h2, __in1, 3'h4, 3'h2, zll_main_compute16_outR2);
  ZLL_Main_compute16  instR3 (__in0, 3'h2, __in1, 3'h4, 3'h3, zll_main_compute16_outR3);
  ZLL_Main_compute16  instR4 (__in0, 3'h2, __in1, 3'h4, 3'h4, zll_main_compute16_outR4);
  ZLL_Main_compute16  instR5 (__in0, 3'h2, __in1, 3'h4, 3'h5, zll_main_compute16_outR5);
  ZLL_Main_compute16  instR6 (__in0, 3'h2, __in1, 3'h4, 3'h6, zll_main_compute16_outR6);
  ZLL_Main_compute16  instR7 (__in0, 3'h2, __in1, 3'h4, 3'h7, zll_main_compute16_outR7);
  ZLL_Main_compute36  instR8 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h0, zll_main_compute36_out);
  ZLL_Main_compute36  instR9 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h1, zll_main_compute36_outR1);
  ZLL_Main_compute36  instR10 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h2, zll_main_compute36_outR2);
  ZLL_Main_compute36  instR11 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h3, zll_main_compute36_outR3);
  ZLL_Main_compute36  instR12 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h4, zll_main_compute36_outR4);
  ZLL_Main_compute36  instR13 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h5, zll_main_compute36_outR5);
  ZLL_Main_compute36  instR14 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h6, zll_main_compute36_outR6);
  ZLL_Main_compute36  instR15 (__in0, 3'h2, 3'h4, __in1, 3'h1, 3'h7, zll_main_compute36_outR7);
  assign zi6 = {zll_main_compute16_out, zll_main_compute16_outR1, zll_main_compute16_outR2, zll_main_compute16_outR3, zll_main_compute16_outR4, zll_main_compute16_outR5, zll_main_compute16_outR6, zll_main_compute16_outR7, zll_main_compute36_out, zll_main_compute36_outR1, zll_main_compute36_outR2, zll_main_compute36_outR3, zll_main_compute36_outR4, zll_main_compute36_outR5, zll_main_compute36_outR6, zll_main_compute36_outR7};
  assign zi7 = zi6[63:0];
  assign zi8 = zi7;
  assign zi9 = zi6[127:64];
  assign zi10 = zi9;
  ZLL_Main_compute14  instR16 (3'h2, zi10, zi8, 3'h4, 3'h0, zll_main_compute14_out);
  ZLL_Main_compute14  instR17 (3'h2, zi10, zi8, 3'h4, 3'h1, zll_main_compute14_outR1);
  ZLL_Main_compute14  instR18 (3'h2, zi10, zi8, 3'h4, 3'h2, zll_main_compute14_outR2);
  ZLL_Main_compute14  instR19 (3'h2, zi10, zi8, 3'h4, 3'h3, zll_main_compute14_outR3);
  ZLL_Main_compute14  instR20 (3'h2, zi10, zi8, 3'h4, 3'h4, zll_main_compute14_outR4);
  ZLL_Main_compute14  instR21 (3'h2, zi10, zi8, 3'h4, 3'h5, zll_main_compute14_outR5);
  ZLL_Main_compute14  instR22 (3'h2, zi10, zi8, 3'h4, 3'h6, zll_main_compute14_outR6);
  ZLL_Main_compute14  instR23 (3'h2, zi10, zi8, 3'h4, 3'h7, zll_main_compute14_outR7);
  ZLL_Main_compute27  instR24 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h0, zll_main_compute27_out);
  ZLL_Main_compute27  instR25 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h1, zll_main_compute27_outR1);
  ZLL_Main_compute27  instR26 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h2, zll_main_compute27_outR2);
  ZLL_Main_compute27  instR27 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h3, zll_main_compute27_outR3);
  ZLL_Main_compute27  instR28 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h4, zll_main_compute27_outR4);
  ZLL_Main_compute27  instR29 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h5, zll_main_compute27_outR5);
  ZLL_Main_compute27  instR30 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h6, zll_main_compute27_outR6);
  ZLL_Main_compute27  instR31 (3'h4, 3'h2, zi10, zi8, 3'h1, 3'h7, zll_main_compute27_outR7);
  assign zi15 = {zll_main_compute14_out, zll_main_compute14_outR1, zll_main_compute14_outR2, zll_main_compute14_outR3, zll_main_compute14_outR4, zll_main_compute14_outR5, zll_main_compute14_outR6, zll_main_compute14_outR7, zll_main_compute27_out, zll_main_compute27_outR1, zll_main_compute27_outR2, zll_main_compute27_outR3, zll_main_compute27_outR4, zll_main_compute27_outR5, zll_main_compute27_outR6, zll_main_compute27_outR7};
  assign zi16 = zi15[63:0];
  assign zi17 = zi16;
  assign zi18 = zi15[127:64];
  assign zi19 = zi18;
  ZLL_Main_compute4  instR32 (3'h2, zi19, zi17, 3'h0, zll_main_compute4_out);
  ZLL_Main_compute4  instR33 (3'h2, zi19, zi17, 3'h1, zll_main_compute4_outR1);
  ZLL_Main_compute4  instR34 (3'h2, zi19, zi17, 3'h2, zll_main_compute4_outR2);
  ZLL_Main_compute4  instR35 (3'h2, zi19, zi17, 3'h3, zll_main_compute4_outR3);
  ZLL_Main_compute4  instR36 (3'h2, zi19, zi17, 3'h4, zll_main_compute4_outR4);
  ZLL_Main_compute4  instR37 (3'h2, zi19, zi17, 3'h5, zll_main_compute4_outR5);
  ZLL_Main_compute4  instR38 (3'h2, zi19, zi17, 3'h6, zll_main_compute4_outR6);
  ZLL_Main_compute4  instR39 (3'h2, zi19, zi17, 3'h7, zll_main_compute4_outR7);
  ZLL_Main_compute47  instR40 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h0, zll_main_compute47_out);
  ZLL_Main_compute47  instR41 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h1, zll_main_compute47_outR1);
  ZLL_Main_compute47  instR42 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h2, zll_main_compute47_outR2);
  ZLL_Main_compute47  instR43 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h3, zll_main_compute47_outR3);
  ZLL_Main_compute47  instR44 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h4, zll_main_compute47_outR4);
  ZLL_Main_compute47  instR45 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h5, zll_main_compute47_outR5);
  ZLL_Main_compute47  instR46 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h6, zll_main_compute47_outR6);
  ZLL_Main_compute47  instR47 (3'h4, zi19, 3'h2, zi17, 3'h1, 3'h7, zll_main_compute47_outR7);
  assign zi22 = {zll_main_compute4_out, zll_main_compute4_outR1, zll_main_compute4_outR2, zll_main_compute4_outR3, zll_main_compute4_outR4, zll_main_compute4_outR5, zll_main_compute4_outR6, zll_main_compute4_outR7, zll_main_compute47_out, zll_main_compute47_outR1, zll_main_compute47_outR2, zll_main_compute47_outR3, zll_main_compute47_outR4, zll_main_compute47_outR5, zll_main_compute47_outR6, zll_main_compute47_outR7};
  assign zi23 = zi22[127:64];
  assign zi24 = zi22[63:0];
  assign zi25 = {zi23, zi24};
  assign zres = zi25;
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute47 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zll_main_compute31_out;
  logic [0:0] zi0;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [127:0] slice_inR3;
  logic [63:0] slice_inR4;
  logic [127:0] slice_inR5;
  logic [127:0] slice_inR6;
  logic [63:0] slice_inR7;
  assign slice_in = {{7'h7d{1'h0}}, arg5};
  assign zt0 = slice_in[0];
  ZLL_Main_compute31  inst (zt0, zll_main_compute31_out);
  assign zi0 = zll_main_compute31_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) % 128'h8);
  assign slice_inR4 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR6 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) % 128'h8);
  assign slice_inR7 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR6[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR4[7:0] : slice_inR7[7:0];
endmodule

module ZLL_Main_compute36 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [63:0] slice_inR3;
  logic [127:0] slice_inR4;
  logic [127:0] slice_inR5;
  logic [63:0] slice_inR6;
  assign zi0 = {{7'h7d{1'h0}}, arg5} < {{7'h7d{1'h0}}, arg2};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg2}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg2}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR3 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR6 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR5[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR3[7:0] : slice_inR6[7:0];
endmodule

module ZLL_Main_compute31 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute27 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [63:0] slice_inR3;
  logic [127:0] slice_inR4;
  logic [127:0] slice_inR5;
  logic [63:0] slice_inR6;
  assign zi0 = {{7'h7d{1'h0}}, arg5} < {{7'h7d{1'h0}}, arg0};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR3 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR6 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR5[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR3[7:0] : slice_inR6[7:0];
endmodule

module ZLL_Main_compute16 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [63:0] slice_inR2;
  logic [127:0] slice_inR3;
  logic [63:0] slice_inR4;
  assign zi0 = {{7'h7d{1'h0}}, arg4} < {{7'h7d{1'h0}}, arg3};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR2 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR1[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR4 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR2[7:0] : slice_inR4[7:0];
endmodule

module ZLL_Main_compute14 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [63:0] slice_inR2;
  logic [127:0] slice_inR3;
  logic [63:0] slice_inR4;
  assign zi0 = {{7'h7d{1'h0}}, arg4} < {{7'h7d{1'h0}}, arg3};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR2 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR1[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR4 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR2[7:0] : slice_inR4[7:0];
endmodule

module ZLL_Main_compute4 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  output logic [7:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zll_main_compute31_out;
  logic [0:0] zi0;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [63:0] slice_inR3;
  logic [127:0] slice_inR4;
  logic [63:0] slice_inR5;
  assign slice_in = {{7'h7d{1'h0}}, arg3};
  assign zt0 = slice_in[0];
  ZLL_Main_compute31  inst (zt0, zll_main_compute31_out);
  assign zi0 = zll_main_compute31_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg3} - 128'h1) : (({{7'h7d{1'h0}}, arg3} - 128'h1) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg0} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg0})) : ((({{7'h7d{1'h0}}, arg0} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg0})) % 128'h8);
  assign slice_inR3 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg0} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg0})) : ((({{7'h7d{1'h0}}, arg0} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg0})) % 128'h8);
  assign slice_inR5 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR4[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR3[7:0] : slice_inR5[7:0];
endmodule