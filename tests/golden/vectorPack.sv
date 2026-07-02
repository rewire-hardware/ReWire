module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [2:0] zll_main_compute161_out;
  logic [2:0] zll_main_compute144_out;
  logic [2:0] zi2;
  logic [2:0] zll_main_compute161_outR1;
  logic [0:0] zll_main_compute120_out;
  logic [2:0] zll_main_compute151_out;
  logic [2:0] zi3;
  logic [7:0] zll_main_compute121_out;
  logic [7:0] zll_main_compute121_outR1;
  logic [7:0] zll_main_compute121_outR2;
  logic [7:0] zll_main_compute121_outR3;
  logic [7:0] zll_main_compute121_outR4;
  logic [7:0] zll_main_compute121_outR5;
  logic [7:0] zll_main_compute121_outR6;
  logic [7:0] zll_main_compute121_outR7;
  logic [2:0] zll_main_compute161_outR2;
  logic [2:0] zll_main_compute144_outR1;
  logic [2:0] zi4;
  logic [2:0] zll_main_compute161_outR3;
  logic [0:0] zll_main_compute120_outR1;
  logic [0:0] zi5;
  logic [2:0] zll_main_compute150_out;
  logic [2:0] zi6;
  logic [7:0] zll_main_compute94_out;
  logic [7:0] zll_main_compute94_outR1;
  logic [7:0] zll_main_compute94_outR2;
  logic [7:0] zll_main_compute94_outR3;
  logic [7:0] zll_main_compute94_outR4;
  logic [7:0] zll_main_compute94_outR5;
  logic [7:0] zll_main_compute94_outR6;
  logic [7:0] zll_main_compute94_outR7;
  logic [127:0] zi7;
  logic [63:0] zi8;
  logic [63:0] zi9;
  logic [63:0] zi10;
  logic [63:0] zi11;
  logic [2:0] zll_main_compute161_outR4;
  logic [2:0] zll_main_compute144_outR2;
  logic [2:0] zi12;
  logic [2:0] zll_main_compute161_outR5;
  logic [0:0] zll_main_compute120_outR2;
  logic [2:0] zll_main_compute151_outR1;
  logic [2:0] zi13;
  logic [7:0] zll_main_compute16_out;
  logic [7:0] zll_main_compute16_outR1;
  logic [7:0] zll_main_compute16_outR2;
  logic [7:0] zll_main_compute16_outR3;
  logic [7:0] zll_main_compute16_outR4;
  logic [7:0] zll_main_compute16_outR5;
  logic [7:0] zll_main_compute16_outR6;
  logic [7:0] zll_main_compute16_outR7;
  logic [2:0] zll_main_compute161_outR6;
  logic [2:0] zll_main_compute144_outR3;
  logic [2:0] zi14;
  logic [2:0] zll_main_compute161_outR7;
  logic [0:0] zll_main_compute120_outR3;
  logic [2:0] zll_main_compute71_out;
  logic [2:0] zi15;
  logic [7:0] zll_main_compute18_out;
  logic [7:0] zll_main_compute18_outR1;
  logic [7:0] zll_main_compute18_outR2;
  logic [7:0] zll_main_compute18_outR3;
  logic [7:0] zll_main_compute18_outR4;
  logic [7:0] zll_main_compute18_outR5;
  logic [7:0] zll_main_compute18_outR6;
  logic [7:0] zll_main_compute18_outR7;
  logic [127:0] zi16;
  logic [63:0] zi17;
  logic [63:0] zi18;
  logic [63:0] zi19;
  logic [63:0] zi20;
  logic [7:0] zll_main_compute97_out;
  logic [7:0] zll_main_compute97_outR1;
  logic [7:0] zll_main_compute97_outR2;
  logic [7:0] zll_main_compute97_outR3;
  logic [7:0] zll_main_compute97_outR4;
  logic [7:0] zll_main_compute97_outR5;
  logic [7:0] zll_main_compute97_outR6;
  logic [7:0] zll_main_compute97_outR7;
  logic [2:0] zll_main_compute161_outR8;
  logic [2:0] zll_main_compute144_outR4;
  logic [2:0] zi21;
  logic [2:0] zll_main_compute161_outR9;
  logic [0:0] zll_main_compute120_outR4;
  logic [2:0] zll_main_compute71_outR1;
  logic [2:0] zi22;
  logic [7:0] zll_main_compute112_out;
  logic [7:0] zll_main_compute112_outR1;
  logic [7:0] zll_main_compute112_outR2;
  logic [7:0] zll_main_compute112_outR3;
  logic [7:0] zll_main_compute112_outR4;
  logic [7:0] zll_main_compute112_outR5;
  logic [7:0] zll_main_compute112_outR6;
  logic [7:0] zll_main_compute112_outR7;
  logic [127:0] zi23;
  logic [63:0] zi24;
  logic [63:0] zi25;
  logic [127:0] zi26;
  logic [127:0] zres;
  ZLL_Main_compute161  inst (__in0, zll_main_compute161_out);
  ZLL_Main_compute144  instR1 (zll_main_compute161_out, 3'h2, zll_main_compute144_out);
  assign zi2 = zll_main_compute144_out;
  ZLL_Main_compute161  instR2 (__in0, zll_main_compute161_outR1);
  ZLL_Main_compute120  instR3 (zll_main_compute161_outR1, zll_main_compute120_out);
  ZLL_Main_compute151  instR4 (zi2, zll_main_compute120_out, zll_main_compute151_out);
  assign zi3 = zll_main_compute151_out;
  ZLL_Main_compute121  instR5 (zi3, __in1, __in0, 3'h2, 3'h0, zll_main_compute121_out);
  ZLL_Main_compute121  instR6 (zi3, __in1, __in0, 3'h2, 3'h1, zll_main_compute121_outR1);
  ZLL_Main_compute121  instR7 (zi3, __in1, __in0, 3'h2, 3'h2, zll_main_compute121_outR2);
  ZLL_Main_compute121  instR8 (zi3, __in1, __in0, 3'h2, 3'h3, zll_main_compute121_outR3);
  ZLL_Main_compute121  instR9 (zi3, __in1, __in0, 3'h2, 3'h4, zll_main_compute121_outR4);
  ZLL_Main_compute121  instR10 (zi3, __in1, __in0, 3'h2, 3'h5, zll_main_compute121_outR5);
  ZLL_Main_compute121  instR11 (zi3, __in1, __in0, 3'h2, 3'h6, zll_main_compute121_outR6);
  ZLL_Main_compute121  instR12 (zi3, __in1, __in0, 3'h2, 3'h7, zll_main_compute121_outR7);
  ZLL_Main_compute161  instR13 (__in0, zll_main_compute161_outR2);
  ZLL_Main_compute144  instR14 (zll_main_compute161_outR2, 3'h2, zll_main_compute144_outR1);
  assign zi4 = zll_main_compute144_outR1;
  ZLL_Main_compute161  instR15 (__in0, zll_main_compute161_outR3);
  ZLL_Main_compute120  instR16 (zll_main_compute161_outR3, zll_main_compute120_outR1);
  assign zi5 = zll_main_compute120_outR1;
  ZLL_Main_compute150  instR17 (zi4, 3'h1, zll_main_compute150_out);
  assign zi6 = (zi5 == 1'h0) ? zll_main_compute150_out : zi4;
  ZLL_Main_compute94  instR18 (3'h2, zi6, __in1, 3'h1, __in0, 3'h0, zll_main_compute94_out);
  ZLL_Main_compute94  instR19 (3'h2, zi6, __in1, 3'h1, __in0, 3'h1, zll_main_compute94_outR1);
  ZLL_Main_compute94  instR20 (3'h2, zi6, __in1, 3'h1, __in0, 3'h2, zll_main_compute94_outR2);
  ZLL_Main_compute94  instR21 (3'h2, zi6, __in1, 3'h1, __in0, 3'h3, zll_main_compute94_outR3);
  ZLL_Main_compute94  instR22 (3'h2, zi6, __in1, 3'h1, __in0, 3'h4, zll_main_compute94_outR4);
  ZLL_Main_compute94  instR23 (3'h2, zi6, __in1, 3'h1, __in0, 3'h5, zll_main_compute94_outR5);
  ZLL_Main_compute94  instR24 (3'h2, zi6, __in1, 3'h1, __in0, 3'h6, zll_main_compute94_outR6);
  ZLL_Main_compute94  instR25 (3'h2, zi6, __in1, 3'h1, __in0, 3'h7, zll_main_compute94_outR7);
  assign zi7 = {{zll_main_compute121_out, zll_main_compute121_outR1, zll_main_compute121_outR2, zll_main_compute121_outR3, zll_main_compute121_outR4, zll_main_compute121_outR5, zll_main_compute121_outR6, zll_main_compute121_outR7}, {zll_main_compute94_out, zll_main_compute94_outR1, zll_main_compute94_outR2, zll_main_compute94_outR3, zll_main_compute94_outR4, zll_main_compute94_outR5, zll_main_compute94_outR6, zll_main_compute94_outR7}};
  assign zi8 = zi7[63:0];
  assign zi9 = zi8;
  assign zi10 = zi7[127:64];
  assign zi11 = zi10;
  ZLL_Main_compute161  instR26 (zi9, zll_main_compute161_outR4);
  ZLL_Main_compute144  instR27 (zll_main_compute161_outR4, 3'h2, zll_main_compute144_outR2);
  assign zi12 = zll_main_compute144_outR2;
  ZLL_Main_compute161  instR28 (zi9, zll_main_compute161_outR5);
  ZLL_Main_compute120  instR29 (zll_main_compute161_outR5, zll_main_compute120_outR2);
  ZLL_Main_compute151  instR30 (zi12, zll_main_compute120_outR2, zll_main_compute151_outR1);
  assign zi13 = zll_main_compute151_outR1;
  ZLL_Main_compute16  instR31 (zi9, 3'h2, zi11, zi13, 3'h0, zll_main_compute16_out);
  ZLL_Main_compute16  instR32 (zi9, 3'h2, zi11, zi13, 3'h1, zll_main_compute16_outR1);
  ZLL_Main_compute16  instR33 (zi9, 3'h2, zi11, zi13, 3'h2, zll_main_compute16_outR2);
  ZLL_Main_compute16  instR34 (zi9, 3'h2, zi11, zi13, 3'h3, zll_main_compute16_outR3);
  ZLL_Main_compute16  instR35 (zi9, 3'h2, zi11, zi13, 3'h4, zll_main_compute16_outR4);
  ZLL_Main_compute16  instR36 (zi9, 3'h2, zi11, zi13, 3'h5, zll_main_compute16_outR5);
  ZLL_Main_compute16  instR37 (zi9, 3'h2, zi11, zi13, 3'h6, zll_main_compute16_outR6);
  ZLL_Main_compute16  instR38 (zi9, 3'h2, zi11, zi13, 3'h7, zll_main_compute16_outR7);
  ZLL_Main_compute161  instR39 (zi9, zll_main_compute161_outR6);
  ZLL_Main_compute144  instR40 (zll_main_compute161_outR6, 3'h2, zll_main_compute144_outR3);
  assign zi14 = zll_main_compute144_outR3;
  ZLL_Main_compute161  instR41 (zi9, zll_main_compute161_outR7);
  ZLL_Main_compute120  instR42 (zll_main_compute161_outR7, zll_main_compute120_outR3);
  ZLL_Main_compute71  instR43 (zi14, 3'h1, zll_main_compute120_outR3, zll_main_compute71_out);
  assign zi15 = zll_main_compute71_out;
  ZLL_Main_compute18  instR44 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h0, zll_main_compute18_out);
  ZLL_Main_compute18  instR45 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h1, zll_main_compute18_outR1);
  ZLL_Main_compute18  instR46 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h2, zll_main_compute18_outR2);
  ZLL_Main_compute18  instR47 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h3, zll_main_compute18_outR3);
  ZLL_Main_compute18  instR48 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h4, zll_main_compute18_outR4);
  ZLL_Main_compute18  instR49 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h5, zll_main_compute18_outR5);
  ZLL_Main_compute18  instR50 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h6, zll_main_compute18_outR6);
  ZLL_Main_compute18  instR51 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h7, zll_main_compute18_outR7);
  assign zi16 = {{zll_main_compute16_out, zll_main_compute16_outR1, zll_main_compute16_outR2, zll_main_compute16_outR3, zll_main_compute16_outR4, zll_main_compute16_outR5, zll_main_compute16_outR6, zll_main_compute16_outR7}, {zll_main_compute18_out, zll_main_compute18_outR1, zll_main_compute18_outR2, zll_main_compute18_outR3, zll_main_compute18_outR4, zll_main_compute18_outR5, zll_main_compute18_outR6, zll_main_compute18_outR7}};
  assign zi17 = zi16[63:0];
  assign zi18 = zi17;
  assign zi19 = zi16[127:64];
  assign zi20 = zi19;
  ZLL_Main_compute97  instR52 (3'h1, zi20, 3'h2, zi18, 3'h0, zll_main_compute97_out);
  ZLL_Main_compute97  instR53 (3'h1, zi20, 3'h2, zi18, 3'h1, zll_main_compute97_outR1);
  ZLL_Main_compute97  instR54 (3'h1, zi20, 3'h2, zi18, 3'h2, zll_main_compute97_outR2);
  ZLL_Main_compute97  instR55 (3'h1, zi20, 3'h2, zi18, 3'h3, zll_main_compute97_outR3);
  ZLL_Main_compute97  instR56 (3'h1, zi20, 3'h2, zi18, 3'h4, zll_main_compute97_outR4);
  ZLL_Main_compute97  instR57 (3'h1, zi20, 3'h2, zi18, 3'h5, zll_main_compute97_outR5);
  ZLL_Main_compute97  instR58 (3'h1, zi20, 3'h2, zi18, 3'h6, zll_main_compute97_outR6);
  ZLL_Main_compute97  instR59 (3'h1, zi20, 3'h2, zi18, 3'h7, zll_main_compute97_outR7);
  ZLL_Main_compute161  instR60 (zi20, zll_main_compute161_outR8);
  ZLL_Main_compute144  instR61 (zll_main_compute161_outR8, 3'h2, zll_main_compute144_outR4);
  assign zi21 = zll_main_compute144_outR4;
  ZLL_Main_compute161  instR62 (zi20, zll_main_compute161_outR9);
  ZLL_Main_compute120  instR63 (zll_main_compute161_outR9, zll_main_compute120_outR4);
  ZLL_Main_compute71  instR64 (zi21, 3'h1, zll_main_compute120_outR4, zll_main_compute71_outR1);
  assign zi22 = zll_main_compute71_outR1;
  ZLL_Main_compute112  instR65 (3'h2, zi18, zi22, zi20, 3'h1, 3'h0, zll_main_compute112_out);
  ZLL_Main_compute112  instR66 (3'h2, zi18, zi22, zi20, 3'h1, 3'h1, zll_main_compute112_outR1);
  ZLL_Main_compute112  instR67 (3'h2, zi18, zi22, zi20, 3'h1, 3'h2, zll_main_compute112_outR2);
  ZLL_Main_compute112  instR68 (3'h2, zi18, zi22, zi20, 3'h1, 3'h3, zll_main_compute112_outR3);
  ZLL_Main_compute112  instR69 (3'h2, zi18, zi22, zi20, 3'h1, 3'h4, zll_main_compute112_outR4);
  ZLL_Main_compute112  instR70 (3'h2, zi18, zi22, zi20, 3'h1, 3'h5, zll_main_compute112_outR5);
  ZLL_Main_compute112  instR71 (3'h2, zi18, zi22, zi20, 3'h1, 3'h6, zll_main_compute112_outR6);
  ZLL_Main_compute112  instR72 (3'h2, zi18, zi22, zi20, 3'h1, 3'h7, zll_main_compute112_outR7);
  assign zi23 = {zll_main_compute97_out, zll_main_compute97_outR1, zll_main_compute97_outR2, zll_main_compute97_outR3, zll_main_compute97_outR4, zll_main_compute97_outR5, zll_main_compute97_outR6, zll_main_compute97_outR7, {zll_main_compute112_out, zll_main_compute112_outR1, zll_main_compute112_outR2, zll_main_compute112_outR3, zll_main_compute112_outR4, zll_main_compute112_outR5, zll_main_compute112_outR6, zll_main_compute112_outR7}};
  assign zi24 = zi23[127:64];
  assign zi25 = zi23[63:0];
  assign zi26 = {zi24, zi25};
  assign zres = zi26;
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute161 (input logic [63:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_compute159 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute158 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute151 (input logic [2:0] arg0,
  input logic [0:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_compute148_out;
  ZLL_Main_compute148  inst (arg0, 3'h1, zll_main_compute148_out);
  assign res = (arg1 == 1'h0) ? zll_main_compute148_out : arg0;
endmodule

module ZLL_Main_compute150 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_compute148_out;
  ZLL_Main_compute148  inst (arg0, arg1, zll_main_compute148_out);
  assign res = zll_main_compute148_out;
endmodule

module ZLL_Main_compute148 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute144 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute132 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_compute121 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute132_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute158_out;
  logic [2:0] zll_main_compute159_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute159_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute132  inst (arg4, arg0, zll_main_compute132_out);
  assign zi0 = zll_main_compute132_out;
  ZLL_Main_compute158  instR1 (arg4, arg0, zll_main_compute158_out);
  ZLL_Main_compute159  instR2 (zll_main_compute158_out, arg3, zll_main_compute159_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute159_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute159  instR3 (arg4, arg3, zll_main_compute159_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute159_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute120 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute112 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute120_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute158_out;
  logic [2:0] zll_main_compute144_out;
  logic [2:0] zll_main_compute150_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute144_outR1;
  logic [2:0] zll_main_compute150_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute120  inst (arg5, zll_main_compute120_out);
  assign zi0 = zll_main_compute120_out;
  ZLL_Main_compute158  instR1 (arg5, arg4, zll_main_compute158_out);
  ZLL_Main_compute144  instR2 (zll_main_compute158_out, arg0, zll_main_compute144_out);
  ZLL_Main_compute150  instR3 (arg2, zll_main_compute144_out, zll_main_compute150_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute150_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute144  instR4 (arg5, arg0, zll_main_compute144_outR1);
  ZLL_Main_compute150  instR5 (arg2, zll_main_compute144_outR1, zll_main_compute150_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute150_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute97 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute120_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute158_out;
  logic [2:0] zll_main_compute144_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute144_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute120  inst (arg4, zll_main_compute120_out);
  assign zi0 = zll_main_compute120_out;
  ZLL_Main_compute158  instR1 (arg4, arg0, zll_main_compute158_out);
  ZLL_Main_compute144  instR2 (zll_main_compute158_out, arg2, zll_main_compute144_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute144_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute144  instR3 (arg4, arg2, zll_main_compute144_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute144_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute94 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute132_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute158_out;
  logic [2:0] zll_main_compute159_out;
  logic [2:0] zll_main_compute150_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute159_outR1;
  logic [2:0] zll_main_compute150_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute132  inst (arg5, arg1, zll_main_compute132_out);
  assign zi0 = zll_main_compute132_out;
  ZLL_Main_compute158  instR1 (arg5, arg1, zll_main_compute158_out);
  ZLL_Main_compute159  instR2 (zll_main_compute158_out, arg0, zll_main_compute159_out);
  ZLL_Main_compute150  instR3 (zll_main_compute159_out, arg3, zll_main_compute150_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute150_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute159  instR4 (arg5, arg0, zll_main_compute159_outR1);
  ZLL_Main_compute150  instR5 (zll_main_compute159_outR1, arg3, zll_main_compute150_outR1);
  assign slice_inR1 = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute150_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute71 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [2:0] zll_main_compute150_out;
  ZLL_Main_compute150  inst (arg0, arg1, zll_main_compute150_out);
  assign res = (arg2 == 1'h0) ? zll_main_compute150_out : arg0;
endmodule

module ZLL_Main_compute18 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute132_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute158_out;
  logic [2:0] zll_main_compute159_out;
  logic [2:0] zll_main_compute150_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute159_outR1;
  logic [2:0] zll_main_compute150_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute132  inst (arg5, arg2, zll_main_compute132_out);
  assign zi0 = zll_main_compute132_out;
  ZLL_Main_compute158  instR1 (arg5, arg2, zll_main_compute158_out);
  ZLL_Main_compute159  instR2 (zll_main_compute158_out, arg4, zll_main_compute159_out);
  ZLL_Main_compute150  instR3 (zll_main_compute159_out, arg1, zll_main_compute150_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute150_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute159  instR4 (arg5, arg4, zll_main_compute159_outR1);
  ZLL_Main_compute150  instR5 (zll_main_compute159_outR1, arg1, zll_main_compute150_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute150_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute16 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute132_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute158_out;
  logic [2:0] zll_main_compute159_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute159_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute132  inst (arg4, arg3, zll_main_compute132_out);
  assign zi0 = zll_main_compute132_out;
  ZLL_Main_compute158  instR1 (arg4, arg3, zll_main_compute158_out);
  ZLL_Main_compute159  instR2 (zll_main_compute158_out, arg1, zll_main_compute159_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute159_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute159  instR3 (arg4, arg1, zll_main_compute159_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute159_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule