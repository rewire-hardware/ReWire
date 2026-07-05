module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [7:0] __out2,
  output logic [7:0] __out3);
  logic [7:0] zi0;
  logic [0:0] zll_main_dev12_out;
  logic [0:0] zll_main_dev12_outR1;
  logic [0:0] zll_main_dev12_outR2;
  logic [0:0] zll_main_dev12_outR3;
  logic [0:0] zll_main_dev12_outR4;
  logic [0:0] zll_main_dev12_outR5;
  logic [0:0] zll_main_dev12_outR6;
  logic [0:0] zll_main_dev12_outR7;
  logic [0:0] zll_main_dev14_out;
  logic [0:0] zll_main_dev14_outR1;
  logic [0:0] zll_main_dev14_outR2;
  logic [0:0] zll_main_dev14_outR3;
  logic [0:0] zll_main_dev14_outR4;
  logic [0:0] zll_main_dev14_outR5;
  logic [0:0] zll_main_dev14_outR6;
  logic [0:0] zll_main_dev14_outR7;
  logic [0:0] zll_main_dev22_out;
  logic [0:0] zll_main_dev22_outR1;
  logic [0:0] zll_main_dev22_outR2;
  logic [0:0] zll_main_dev22_outR3;
  logic [0:0] zll_main_dev22_outR4;
  logic [0:0] zll_main_dev22_outR5;
  logic [0:0] zll_main_dev22_outR6;
  logic [0:0] zll_main_dev22_outR7;
  logic [0:0] zll_main_dev11_out;
  logic [0:0] zll_main_dev11_outR1;
  logic [0:0] zll_main_dev11_outR2;
  logic [0:0] zll_main_dev11_outR3;
  logic [0:0] zll_main_dev11_outR4;
  logic [0:0] zll_main_dev11_outR5;
  logic [0:0] zll_main_dev11_outR6;
  logic [0:0] zll_main_dev11_outR7;
  logic [31:0] zres;
  assign zi0 = __in0 * 8'h2;
  ZLL_Main_dev12  inst (__in0, zi0, 3'h2, 3'h0, zll_main_dev12_out);
  ZLL_Main_dev12  instR1 (__in0, zi0, 3'h2, 3'h1, zll_main_dev12_outR1);
  ZLL_Main_dev12  instR2 (__in0, zi0, 3'h2, 3'h2, zll_main_dev12_outR2);
  ZLL_Main_dev12  instR3 (__in0, zi0, 3'h2, 3'h3, zll_main_dev12_outR3);
  ZLL_Main_dev12  instR4 (__in0, zi0, 3'h2, 3'h4, zll_main_dev12_outR4);
  ZLL_Main_dev12  instR5 (__in0, zi0, 3'h2, 3'h5, zll_main_dev12_outR5);
  ZLL_Main_dev12  instR6 (__in0, zi0, 3'h2, 3'h6, zll_main_dev12_outR6);
  ZLL_Main_dev12  instR7 (__in0, zi0, 3'h2, 3'h7, zll_main_dev12_outR7);
  ZLL_Main_dev14  instR8 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h0, zll_main_dev14_out);
  ZLL_Main_dev14  instR9 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h1, zll_main_dev14_outR1);
  ZLL_Main_dev14  instR10 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h2, zll_main_dev14_outR2);
  ZLL_Main_dev14  instR11 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h3, zll_main_dev14_outR3);
  ZLL_Main_dev14  instR12 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h4, zll_main_dev14_outR4);
  ZLL_Main_dev14  instR13 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h5, zll_main_dev14_outR5);
  ZLL_Main_dev14  instR14 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h6, zll_main_dev14_outR6);
  ZLL_Main_dev14  instR15 (__in0, zi0, 3'h4, 3'h2, 3'h1, 3'h7, zll_main_dev14_outR7);
  ZLL_Main_dev22  instR16 (__in0, zi0, 3'h4, 3'h2, 3'h0, zll_main_dev22_out);
  ZLL_Main_dev22  instR17 (__in0, zi0, 3'h4, 3'h2, 3'h1, zll_main_dev22_outR1);
  ZLL_Main_dev22  instR18 (__in0, zi0, 3'h4, 3'h2, 3'h2, zll_main_dev22_outR2);
  ZLL_Main_dev22  instR19 (__in0, zi0, 3'h4, 3'h2, 3'h3, zll_main_dev22_outR3);
  ZLL_Main_dev22  instR20 (__in0, zi0, 3'h4, 3'h2, 3'h4, zll_main_dev22_outR4);
  ZLL_Main_dev22  instR21 (__in0, zi0, 3'h4, 3'h2, 3'h5, zll_main_dev22_outR5);
  ZLL_Main_dev22  instR22 (__in0, zi0, 3'h4, 3'h2, 3'h6, zll_main_dev22_outR6);
  ZLL_Main_dev22  instR23 (__in0, zi0, 3'h4, 3'h2, 3'h7, zll_main_dev22_outR7);
  ZLL_Main_dev11  instR24 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h0, zll_main_dev11_out);
  ZLL_Main_dev11  instR25 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h1, zll_main_dev11_outR1);
  ZLL_Main_dev11  instR26 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h2, zll_main_dev11_outR2);
  ZLL_Main_dev11  instR27 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h3, zll_main_dev11_outR3);
  ZLL_Main_dev11  instR28 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h4, zll_main_dev11_outR4);
  ZLL_Main_dev11  instR29 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h5, zll_main_dev11_outR5);
  ZLL_Main_dev11  instR30 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h6, zll_main_dev11_outR6);
  ZLL_Main_dev11  instR31 (__in0, 3'h1, zi0, 3'h2, 3'h4, 3'h7, zll_main_dev11_outR7);
  assign zres = {zll_main_dev12_out, zll_main_dev12_outR1, zll_main_dev12_outR2, zll_main_dev12_outR3, zll_main_dev12_outR4, zll_main_dev12_outR5, zll_main_dev12_outR6, zll_main_dev12_outR7, zll_main_dev14_out, zll_main_dev14_outR1, zll_main_dev14_outR2, zll_main_dev14_outR3, zll_main_dev14_outR4, zll_main_dev14_outR5, zll_main_dev14_outR6, zll_main_dev14_outR7, zll_main_dev22_out, zll_main_dev22_outR1, zll_main_dev22_outR2, zll_main_dev22_outR3, zll_main_dev22_outR4, zll_main_dev22_outR5, zll_main_dev22_outR6, zll_main_dev22_outR7, zll_main_dev11_out, zll_main_dev11_outR1, zll_main_dev11_outR2, zll_main_dev11_outR3, zll_main_dev11_outR4, zll_main_dev11_outR5, zll_main_dev11_outR6, zll_main_dev11_outR7};
  assign __out0 = zres[31:24];
  assign __out1 = zres[23:16];
  assign __out2 = zres[15:8];
  assign __out3 = zres[7:0];
endmodule

module ZLL_Main_dev22 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [0:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [7:0] slice_inR2;
  logic [127:0] slice_inR3;
  logic [7:0] slice_inR4;
  assign zi0 = {{7'h7d{1'h0}}, arg4} < {{7'h7d{1'h0}}, arg2};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg2}) : (({{7'h7d{1'h0}}, arg4} - {{7'h7d{1'h0}}, arg2}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR2 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR1[2:0]}) - 128'h1) * 128'h1);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, arg4} * {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR4 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_inR2[0] : slice_inR4[0];
endmodule

module ZLL_Main_dev16 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_dev14 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zll_main_dev16_out;
  logic [0:0] zi0;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [127:0] slice_inR3;
  logic [7:0] slice_inR4;
  logic [127:0] slice_inR5;
  logic [127:0] slice_inR6;
  logic [7:0] slice_inR7;
  assign slice_in = {{7'h7d{1'h0}}, arg5};
  assign zt0 = slice_in[0];
  ZLL_Main_dev16  inst (zt0, zll_main_dev16_out);
  assign zi0 = zll_main_dev16_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg3} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg3})) : ((({{7'h7d{1'h0}}, arg3} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg3})) % 128'h8);
  assign slice_inR3 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg2} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) : (({{7'h7d{1'h0}}, arg2} + {{7'h7d{1'h0}}, slice_inR2[2:0]}) % 128'h8);
  assign slice_inR4 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR3[2:0]}) - 128'h1) * 128'h1);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg3} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg3})) : ((({{7'h7d{1'h0}}, arg3} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg5} / {{7'h7d{1'h0}}, arg3})) % 128'h8);
  assign slice_inR6 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg2} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) : (({{7'h7d{1'h0}}, arg2} + {{7'h7d{1'h0}}, slice_inR5[2:0]}) % 128'h8);
  assign slice_inR7 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR6[2:0]}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_inR4[0] : slice_inR7[0];
endmodule

module ZLL_Main_dev12 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [2:0] arg2,
  input logic [2:0] arg3,
  output logic [0:0] res);
  logic [127:0] slice_in;
  logic [0:0] zt0;
  logic [0:0] zll_main_dev16_out;
  logic [0:0] zi0;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [7:0] slice_inR3;
  logic [127:0] slice_inR4;
  logic [7:0] slice_inR5;
  assign slice_in = {{7'h7d{1'h0}}, arg3};
  assign zt0 = slice_in[0];
  ZLL_Main_dev16  inst (zt0, zll_main_dev16_out);
  assign zi0 = zll_main_dev16_out;
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg3} - 128'h1) : (({{7'h7d{1'h0}}, arg3} - 128'h1) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, slice_inR1[2:0]} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR3 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h1);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg2})) : ((({{7'h7d{1'h0}}, arg2} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg3} / {{7'h7d{1'h0}}, arg2})) % 128'h8);
  assign slice_inR5 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR4[2:0]}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_inR3[0] : slice_inR5[0];
endmodule

module ZLL_Main_dev11 (input logic [7:0] arg0,
  input logic [2:0] arg1,
  input logic [7:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [0:0] res);
  logic [0:0] zi0;
  logic [127:0] slice_in;
  logic [127:0] slice_inR1;
  logic [127:0] slice_inR2;
  logic [7:0] slice_inR3;
  logic [127:0] slice_inR4;
  logic [127:0] slice_inR5;
  logic [7:0] slice_inR6;
  assign zi0 = {{7'h7d{1'h0}}, arg5} < {{7'h7d{1'h0}}, arg4};
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) : (({{7'h7d{1'h0}}, arg5} - {{7'h7d{1'h0}}, arg4}) % 128'h8);
  assign slice_inR1 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, slice_in[2:0]} * {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR2 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, slice_inR1[2:0]} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR3 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR2[2:0]}) - 128'h1) * 128'h1);
  assign slice_inR4 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg3}) : (({{7'h7d{1'h0}}, arg5} * {{7'h7d{1'h0}}, arg3}) % 128'h8);
  assign slice_inR5 = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, slice_inR4[2:0]} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign slice_inR6 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, slice_inR5[2:0]}) - 128'h1) * 128'h1);
  assign res = (zi0 == 1'h0) ? slice_inR3[0] : slice_inR6[0];
endmodule