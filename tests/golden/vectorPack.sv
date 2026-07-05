module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [7:0] zll_main_compute24_out;
  logic [7:0] zll_main_compute24_outR1;
  logic [7:0] zll_main_compute24_outR2;
  logic [7:0] zll_main_compute24_outR3;
  logic [7:0] zll_main_compute24_outR4;
  logic [7:0] zll_main_compute24_outR5;
  logic [7:0] zll_main_compute24_outR6;
  logic [7:0] zll_main_compute24_outR7;
  logic [7:0] zll_main_compute19_out;
  logic [7:0] zll_main_compute19_outR1;
  logic [7:0] zll_main_compute19_outR2;
  logic [7:0] zll_main_compute19_outR3;
  logic [7:0] zll_main_compute19_outR4;
  logic [7:0] zll_main_compute19_outR5;
  logic [7:0] zll_main_compute19_outR6;
  logic [7:0] zll_main_compute19_outR7;
  logic [127:0] zi6;
  logic [63:0] zi7;
  logic [63:0] zi8;
  logic [63:0] zi9;
  logic [63:0] zi10;
  logic [7:0] zll_main_compute33_out;
  logic [7:0] zll_main_compute33_outR1;
  logic [7:0] zll_main_compute33_outR2;
  logic [7:0] zll_main_compute33_outR3;
  logic [7:0] zll_main_compute33_outR4;
  logic [7:0] zll_main_compute33_outR5;
  logic [7:0] zll_main_compute33_outR6;
  logic [7:0] zll_main_compute33_outR7;
  logic [7:0] zll_main_compute15_out;
  logic [7:0] zll_main_compute15_outR1;
  logic [7:0] zll_main_compute15_outR2;
  logic [7:0] zll_main_compute15_outR3;
  logic [7:0] zll_main_compute15_outR4;
  logic [7:0] zll_main_compute15_outR5;
  logic [7:0] zll_main_compute15_outR6;
  logic [7:0] zll_main_compute15_outR7;
  logic [127:0] zi15;
  logic [63:0] zi16;
  logic [63:0] zi17;
  logic [63:0] zi18;
  logic [63:0] zi19;
  logic [7:0] zll_main_compute39_out;
  logic [7:0] zll_main_compute39_outR1;
  logic [7:0] zll_main_compute39_outR2;
  logic [7:0] zll_main_compute39_outR3;
  logic [7:0] zll_main_compute39_outR4;
  logic [7:0] zll_main_compute39_outR5;
  logic [7:0] zll_main_compute39_outR6;
  logic [7:0] zll_main_compute39_outR7;
  logic [7:0] zll_main_compute34_out;
  logic [7:0] zll_main_compute34_outR1;
  logic [7:0] zll_main_compute34_outR2;
  logic [7:0] zll_main_compute34_outR3;
  logic [7:0] zll_main_compute34_outR4;
  logic [7:0] zll_main_compute34_outR5;
  logic [7:0] zll_main_compute34_outR6;
  logic [7:0] zll_main_compute34_outR7;
  logic [127:0] zi22;
  logic [63:0] zi23;
  logic [63:0] zi24;
  logic [127:0] zi25;
  logic [127:0] zres;
  ZLL_Main_compute24  inst (__in0, 3'h2, __in1, 3'h4, 3'h0, zll_main_compute24_out);
  ZLL_Main_compute24  instR1 (__in0, 3'h2, __in1, 3'h4, 3'h1, zll_main_compute24_outR1);
  ZLL_Main_compute24  instR2 (__in0, 3'h2, __in1, 3'h4, 3'h2, zll_main_compute24_outR2);
  ZLL_Main_compute24  instR3 (__in0, 3'h2, __in1, 3'h4, 3'h3, zll_main_compute24_outR3);
  ZLL_Main_compute24  instR4 (__in0, 3'h2, __in1, 3'h4, 3'h4, zll_main_compute24_outR4);
  ZLL_Main_compute24  instR5 (__in0, 3'h2, __in1, 3'h4, 3'h5, zll_main_compute24_outR5);
  ZLL_Main_compute24  instR6 (__in0, 3'h2, __in1, 3'h4, 3'h6, zll_main_compute24_outR6);
  ZLL_Main_compute24  instR7 (__in0, 3'h2, __in1, 3'h4, 3'h7, zll_main_compute24_outR7);
  ZLL_Main_compute19  instR8 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h0, zll_main_compute19_out);
  ZLL_Main_compute19  instR9 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h1, zll_main_compute19_outR1);
  ZLL_Main_compute19  instR10 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h2, zll_main_compute19_outR2);
  ZLL_Main_compute19  instR11 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h3, zll_main_compute19_outR3);
  ZLL_Main_compute19  instR12 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h4, zll_main_compute19_outR4);
  ZLL_Main_compute19  instR13 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h5, zll_main_compute19_outR5);
  ZLL_Main_compute19  instR14 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h6, zll_main_compute19_outR6);
  ZLL_Main_compute19  instR15 (__in0, 3'h4, 3'h1, 3'h2, __in1, 3'h7, zll_main_compute19_outR7);
  assign zi6 = {zll_main_compute24_out, zll_main_compute24_outR1, zll_main_compute24_outR2, zll_main_compute24_outR3, zll_main_compute24_outR4, zll_main_compute24_outR5, zll_main_compute24_outR6, zll_main_compute24_outR7, zll_main_compute19_out, zll_main_compute19_outR1, zll_main_compute19_outR2, zll_main_compute19_outR3, zll_main_compute19_outR4, zll_main_compute19_outR5, zll_main_compute19_outR6, zll_main_compute19_outR7};
  assign zi7 = zi6[63:0];
  assign zi8 = zi7;
  assign zi9 = zi6[127:64];
  assign zi10 = zi9;
  ZLL_Main_compute33  instR16 (3'h2, zi8, zi10, 3'h4, 3'h0, zll_main_compute33_out);
  ZLL_Main_compute33  instR17 (3'h2, zi8, zi10, 3'h4, 3'h1, zll_main_compute33_outR1);
  ZLL_Main_compute33  instR18 (3'h2, zi8, zi10, 3'h4, 3'h2, zll_main_compute33_outR2);
  ZLL_Main_compute33  instR19 (3'h2, zi8, zi10, 3'h4, 3'h3, zll_main_compute33_outR3);
  ZLL_Main_compute33  instR20 (3'h2, zi8, zi10, 3'h4, 3'h4, zll_main_compute33_outR4);
  ZLL_Main_compute33  instR21 (3'h2, zi8, zi10, 3'h4, 3'h5, zll_main_compute33_outR5);
  ZLL_Main_compute33  instR22 (3'h2, zi8, zi10, 3'h4, 3'h6, zll_main_compute33_outR6);
  ZLL_Main_compute33  instR23 (3'h2, zi8, zi10, 3'h4, 3'h7, zll_main_compute33_outR7);
  ZLL_Main_compute15  instR24 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h0, zll_main_compute15_out);
  ZLL_Main_compute15  instR25 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h1, zll_main_compute15_outR1);
  ZLL_Main_compute15  instR26 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h2, zll_main_compute15_outR2);
  ZLL_Main_compute15  instR27 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h3, zll_main_compute15_outR3);
  ZLL_Main_compute15  instR28 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h4, zll_main_compute15_outR4);
  ZLL_Main_compute15  instR29 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h5, zll_main_compute15_outR5);
  ZLL_Main_compute15  instR30 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h6, zll_main_compute15_outR6);
  ZLL_Main_compute15  instR31 (zi8, 3'h2, zi10, 3'h1, 3'h4, 3'h7, zll_main_compute15_outR7);
  assign zi15 = {zll_main_compute33_out, zll_main_compute33_outR1, zll_main_compute33_outR2, zll_main_compute33_outR3, zll_main_compute33_outR4, zll_main_compute33_outR5, zll_main_compute33_outR6, zll_main_compute33_outR7, zll_main_compute15_out, zll_main_compute15_outR1, zll_main_compute15_outR2, zll_main_compute15_outR3, zll_main_compute15_outR4, zll_main_compute15_outR5, zll_main_compute15_outR6, zll_main_compute15_outR7};
  assign zi16 = zi15[63:0];
  assign zi17 = zi16;
  assign zi18 = zi15[127:64];
  assign zi19 = zi18;
  ZLL_Main_compute39  instR32 (zi17, zi19, 3'h2, 3'h0, zll_main_compute39_out);
  ZLL_Main_compute39  instR33 (zi17, zi19, 3'h2, 3'h1, zll_main_compute39_outR1);
  ZLL_Main_compute39  instR34 (zi17, zi19, 3'h2, 3'h2, zll_main_compute39_outR2);
  ZLL_Main_compute39  instR35 (zi17, zi19, 3'h2, 3'h3, zll_main_compute39_outR3);
  ZLL_Main_compute39  instR36 (zi17, zi19, 3'h2, 3'h4, zll_main_compute39_outR4);
  ZLL_Main_compute39  instR37 (zi17, zi19, 3'h2, 3'h5, zll_main_compute39_outR5);
  ZLL_Main_compute39  instR38 (zi17, zi19, 3'h2, 3'h6, zll_main_compute39_outR6);
  ZLL_Main_compute39  instR39 (zi17, zi19, 3'h2, 3'h7, zll_main_compute39_outR7);
  ZLL_Main_compute34  instR40 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h0, zll_main_compute34_out);
  ZLL_Main_compute34  instR41 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h1, zll_main_compute34_outR1);
  ZLL_Main_compute34  instR42 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h2, zll_main_compute34_outR2);
  ZLL_Main_compute34  instR43 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h3, zll_main_compute34_outR3);
  ZLL_Main_compute34  instR44 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h4, zll_main_compute34_outR4);
  ZLL_Main_compute34  instR45 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h5, zll_main_compute34_outR5);
  ZLL_Main_compute34  instR46 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h6, zll_main_compute34_outR6);
  ZLL_Main_compute34  instR47 (zi17, 3'h4, 3'h1, zi19, 3'h2, 3'h7, zll_main_compute34_outR7);
  assign zi22 = {zll_main_compute39_out, zll_main_compute39_outR1, zll_main_compute39_outR2, zll_main_compute39_outR3, zll_main_compute39_outR4, zll_main_compute39_outR5, zll_main_compute39_outR6, zll_main_compute39_outR7, zll_main_compute34_out, zll_main_compute34_outR1, zll_main_compute34_outR2, zll_main_compute34_outR3, zll_main_compute34_outR4, zll_main_compute34_outR5, zll_main_compute34_outR6, zll_main_compute34_outR7};
  assign zi23 = zi22[127:64];
  assign zi24 = zi22[63:0];
  assign zi25 = {zi23, zi24};
  assign zres = zi25;
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute39 (input logic [63:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  output logic [7:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zll_main_compute20_out;
  logic [0:0] zi0;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [63:0] slice_inR3;
  logic [127:0] slice_inR4;
  logic [63:0] slice_inR5;
  assign slice_in = {{7'h7d{1'h0}}, arg3};
  assign zt0 = slice_in[0];
  ZLL_Main_compute20  inst (zt0, zll_main_compute20_out);
  assign zi0 = zll_main_compute20_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg3} - 128'h1) : (({{7'h7d{1'h0}}, arg3} - 128'h1) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR3 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR5 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR4[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR3[7:0] : slice_inR5[7:0];
endmodule

module ZLL_Main_compute34 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zll_main_compute20_out;
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
  ZLL_Main_compute20  inst (zt0, zll_main_compute20_out);
  assign zi0 = zll_main_compute20_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg2}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg2}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg4} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg4})) : ((({{7'h7d{1'h0}}, arg4} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg4})) % 128'h8);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg1} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) : (({{7'h7d{1'h0}}, arg1} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) % 128'h8);
  assign slice_inR4 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg4} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg4})) : ((({{7'h7d{1'h0}}, arg4} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg4})) % 128'h8);
  assign slice_inR6 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg1} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) : (({{7'h7d{1'h0}}, arg1} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) % 128'h8);
  assign slice_inR7 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR6[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR4[7:0] : slice_inR7[7:0];
endmodule

module ZLL_Main_compute33 (input logic [2:0] arg0,
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
  assign slice_inR2 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR1[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg0}) : (({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg0}) % 128'h8);
  assign slice_inR4 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR2[7:0] : slice_inR4[7:0];
endmodule

module ZLL_Main_compute24 (input logic [63:0] arg0,
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

module ZLL_Main_compute20 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute19 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
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
  assign zi0 = {{7'h7d{1'h0}}, arg5} < {{7'h7d{1'h0}}, arg1};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg2}) : (({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg2}) % 128'h8);
  assign slice_inR3 = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg2}) : (({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg2}) % 128'h8);
  assign slice_inR6 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR5[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR3[7:0] : slice_inR6[7:0];
endmodule

module ZLL_Main_compute15 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
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
  assign zi0 = {{7'h7d{1'h0}}, arg5} < {{7'h7d{1'h0}}, arg4};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR3 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h8);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR6 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR5[2:0]}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_inR3[7:0] : slice_inR6[7:0];
endmodule