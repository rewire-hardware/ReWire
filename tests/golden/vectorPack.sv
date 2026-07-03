module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  logic [2:0] zll_main_compute15_out;
  logic [2:0] zll_main_compute1_out;
  logic [2:0] zi2;
  logic [2:0] zll_main_compute15_outR1;
  logic [0:0] zll_main_compute51_out;
  logic [2:0] zll_main_compute62_out;
  logic [2:0] zi3;
  logic [7:0] zll_main_compute52_out;
  logic [7:0] zll_main_compute52_outR1;
  logic [7:0] zll_main_compute52_outR2;
  logic [7:0] zll_main_compute52_outR3;
  logic [7:0] zll_main_compute52_outR4;
  logic [7:0] zll_main_compute52_outR5;
  logic [7:0] zll_main_compute52_outR6;
  logic [7:0] zll_main_compute52_outR7;
  logic [2:0] zll_main_compute15_outR2;
  logic [2:0] zll_main_compute1_outR1;
  logic [2:0] zi4;
  logic [2:0] zll_main_compute15_outR3;
  logic [0:0] zll_main_compute51_outR1;
  logic [0:0] zi5;
  logic [2:0] zll_main_compute50_out;
  logic [2:0] zi6;
  logic [7:0] zll_main_compute37_out;
  logic [7:0] zll_main_compute37_outR1;
  logic [7:0] zll_main_compute37_outR2;
  logic [7:0] zll_main_compute37_outR3;
  logic [7:0] zll_main_compute37_outR4;
  logic [7:0] zll_main_compute37_outR5;
  logic [7:0] zll_main_compute37_outR6;
  logic [7:0] zll_main_compute37_outR7;
  logic [127:0] zi7;
  logic [63:0] zi8;
  logic [63:0] zi9;
  logic [63:0] zi10;
  logic [63:0] zi11;
  logic [2:0] zll_main_compute15_outR4;
  logic [2:0] zll_main_compute1_outR2;
  logic [2:0] zi12;
  logic [2:0] zll_main_compute15_outR5;
  logic [0:0] zll_main_compute51_outR2;
  logic [2:0] zll_main_compute62_outR1;
  logic [2:0] zi13;
  logic [7:0] zll_main_compute5_out;
  logic [7:0] zll_main_compute5_outR1;
  logic [7:0] zll_main_compute5_outR2;
  logic [7:0] zll_main_compute5_outR3;
  logic [7:0] zll_main_compute5_outR4;
  logic [7:0] zll_main_compute5_outR5;
  logic [7:0] zll_main_compute5_outR6;
  logic [7:0] zll_main_compute5_outR7;
  logic [2:0] zll_main_compute15_outR6;
  logic [2:0] zll_main_compute1_outR3;
  logic [2:0] zi14;
  logic [2:0] zll_main_compute15_outR7;
  logic [0:0] zll_main_compute51_outR3;
  logic [2:0] zll_main_compute4_out;
  logic [2:0] zi15;
  logic [7:0] zll_main_compute7_out;
  logic [7:0] zll_main_compute7_outR1;
  logic [7:0] zll_main_compute7_outR2;
  logic [7:0] zll_main_compute7_outR3;
  logic [7:0] zll_main_compute7_outR4;
  logic [7:0] zll_main_compute7_outR5;
  logic [7:0] zll_main_compute7_outR6;
  logic [7:0] zll_main_compute7_outR7;
  logic [127:0] zi16;
  logic [63:0] zi17;
  logic [63:0] zi18;
  logic [63:0] zi19;
  logic [63:0] zi20;
  logic [7:0] zll_main_compute39_out;
  logic [7:0] zll_main_compute39_outR1;
  logic [7:0] zll_main_compute39_outR2;
  logic [7:0] zll_main_compute39_outR3;
  logic [7:0] zll_main_compute39_outR4;
  logic [7:0] zll_main_compute39_outR5;
  logic [7:0] zll_main_compute39_outR6;
  logic [7:0] zll_main_compute39_outR7;
  logic [2:0] zll_main_compute15_outR8;
  logic [2:0] zll_main_compute1_outR4;
  logic [2:0] zi21;
  logic [2:0] zll_main_compute15_outR9;
  logic [0:0] zll_main_compute51_outR4;
  logic [2:0] zll_main_compute4_outR1;
  logic [2:0] zi22;
  logic [7:0] zll_main_compute45_out;
  logic [7:0] zll_main_compute45_outR1;
  logic [7:0] zll_main_compute45_outR2;
  logic [7:0] zll_main_compute45_outR3;
  logic [7:0] zll_main_compute45_outR4;
  logic [7:0] zll_main_compute45_outR5;
  logic [7:0] zll_main_compute45_outR6;
  logic [7:0] zll_main_compute45_outR7;
  logic [127:0] zi23;
  logic [63:0] zi24;
  logic [63:0] zi25;
  logic [127:0] zi26;
  logic [127:0] zres;
  ZLL_Main_compute15  inst (__in0, zll_main_compute15_out);
  ZLL_Main_compute1  instR1 (zll_main_compute15_out, 3'h2, zll_main_compute1_out);
  assign zi2 = zll_main_compute1_out;
  ZLL_Main_compute15  instR2 (__in0, zll_main_compute15_outR1);
  ZLL_Main_compute51  instR3 (zll_main_compute15_outR1, zll_main_compute51_out);
  ZLL_Main_compute62  instR4 (zi2, zll_main_compute51_out, zll_main_compute62_out);
  assign zi3 = zll_main_compute62_out;
  ZLL_Main_compute52  instR5 (zi3, __in1, __in0, 3'h2, 3'h0, zll_main_compute52_out);
  ZLL_Main_compute52  instR6 (zi3, __in1, __in0, 3'h2, 3'h1, zll_main_compute52_outR1);
  ZLL_Main_compute52  instR7 (zi3, __in1, __in0, 3'h2, 3'h2, zll_main_compute52_outR2);
  ZLL_Main_compute52  instR8 (zi3, __in1, __in0, 3'h2, 3'h3, zll_main_compute52_outR3);
  ZLL_Main_compute52  instR9 (zi3, __in1, __in0, 3'h2, 3'h4, zll_main_compute52_outR4);
  ZLL_Main_compute52  instR10 (zi3, __in1, __in0, 3'h2, 3'h5, zll_main_compute52_outR5);
  ZLL_Main_compute52  instR11 (zi3, __in1, __in0, 3'h2, 3'h6, zll_main_compute52_outR6);
  ZLL_Main_compute52  instR12 (zi3, __in1, __in0, 3'h2, 3'h7, zll_main_compute52_outR7);
  ZLL_Main_compute15  instR13 (__in0, zll_main_compute15_outR2);
  ZLL_Main_compute1  instR14 (zll_main_compute15_outR2, 3'h2, zll_main_compute1_outR1);
  assign zi4 = zll_main_compute1_outR1;
  ZLL_Main_compute15  instR15 (__in0, zll_main_compute15_outR3);
  ZLL_Main_compute51  instR16 (zll_main_compute15_outR3, zll_main_compute51_outR1);
  assign zi5 = zll_main_compute51_outR1;
  ZLL_Main_compute50  instR17 (zi4, 3'h1, zll_main_compute50_out);
  assign zi6 = (zi5 == 1'h0) ? zll_main_compute50_out : zi4;
  ZLL_Main_compute37  instR18 (3'h2, zi6, __in1, 3'h1, __in0, 3'h0, zll_main_compute37_out);
  ZLL_Main_compute37  instR19 (3'h2, zi6, __in1, 3'h1, __in0, 3'h1, zll_main_compute37_outR1);
  ZLL_Main_compute37  instR20 (3'h2, zi6, __in1, 3'h1, __in0, 3'h2, zll_main_compute37_outR2);
  ZLL_Main_compute37  instR21 (3'h2, zi6, __in1, 3'h1, __in0, 3'h3, zll_main_compute37_outR3);
  ZLL_Main_compute37  instR22 (3'h2, zi6, __in1, 3'h1, __in0, 3'h4, zll_main_compute37_outR4);
  ZLL_Main_compute37  instR23 (3'h2, zi6, __in1, 3'h1, __in0, 3'h5, zll_main_compute37_outR5);
  ZLL_Main_compute37  instR24 (3'h2, zi6, __in1, 3'h1, __in0, 3'h6, zll_main_compute37_outR6);
  ZLL_Main_compute37  instR25 (3'h2, zi6, __in1, 3'h1, __in0, 3'h7, zll_main_compute37_outR7);
  assign zi7 = {{zll_main_compute52_out, zll_main_compute52_outR1, zll_main_compute52_outR2, zll_main_compute52_outR3, zll_main_compute52_outR4, zll_main_compute52_outR5, zll_main_compute52_outR6, zll_main_compute52_outR7}, {zll_main_compute37_out, zll_main_compute37_outR1, zll_main_compute37_outR2, zll_main_compute37_outR3, zll_main_compute37_outR4, zll_main_compute37_outR5, zll_main_compute37_outR6, zll_main_compute37_outR7}};
  assign zi8 = zi7[63:0];
  assign zi9 = zi8;
  assign zi10 = zi7[127:64];
  assign zi11 = zi10;
  ZLL_Main_compute15  instR26 (zi9, zll_main_compute15_outR4);
  ZLL_Main_compute1  instR27 (zll_main_compute15_outR4, 3'h2, zll_main_compute1_outR2);
  assign zi12 = zll_main_compute1_outR2;
  ZLL_Main_compute15  instR28 (zi9, zll_main_compute15_outR5);
  ZLL_Main_compute51  instR29 (zll_main_compute15_outR5, zll_main_compute51_outR2);
  ZLL_Main_compute62  instR30 (zi12, zll_main_compute51_outR2, zll_main_compute62_outR1);
  assign zi13 = zll_main_compute62_outR1;
  ZLL_Main_compute5  instR31 (zi9, 3'h2, zi11, zi13, 3'h0, zll_main_compute5_out);
  ZLL_Main_compute5  instR32 (zi9, 3'h2, zi11, zi13, 3'h1, zll_main_compute5_outR1);
  ZLL_Main_compute5  instR33 (zi9, 3'h2, zi11, zi13, 3'h2, zll_main_compute5_outR2);
  ZLL_Main_compute5  instR34 (zi9, 3'h2, zi11, zi13, 3'h3, zll_main_compute5_outR3);
  ZLL_Main_compute5  instR35 (zi9, 3'h2, zi11, zi13, 3'h4, zll_main_compute5_outR4);
  ZLL_Main_compute5  instR36 (zi9, 3'h2, zi11, zi13, 3'h5, zll_main_compute5_outR5);
  ZLL_Main_compute5  instR37 (zi9, 3'h2, zi11, zi13, 3'h6, zll_main_compute5_outR6);
  ZLL_Main_compute5  instR38 (zi9, 3'h2, zi11, zi13, 3'h7, zll_main_compute5_outR7);
  ZLL_Main_compute15  instR39 (zi9, zll_main_compute15_outR6);
  ZLL_Main_compute1  instR40 (zll_main_compute15_outR6, 3'h2, zll_main_compute1_outR3);
  assign zi14 = zll_main_compute1_outR3;
  ZLL_Main_compute15  instR41 (zi9, zll_main_compute15_outR7);
  ZLL_Main_compute51  instR42 (zll_main_compute15_outR7, zll_main_compute51_outR3);
  ZLL_Main_compute4  instR43 (zi14, 3'h1, zll_main_compute51_outR3, zll_main_compute4_out);
  assign zi15 = zll_main_compute4_out;
  ZLL_Main_compute7  instR44 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h0, zll_main_compute7_out);
  ZLL_Main_compute7  instR45 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h1, zll_main_compute7_outR1);
  ZLL_Main_compute7  instR46 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h2, zll_main_compute7_outR2);
  ZLL_Main_compute7  instR47 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h3, zll_main_compute7_outR3);
  ZLL_Main_compute7  instR48 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h4, zll_main_compute7_outR4);
  ZLL_Main_compute7  instR49 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h5, zll_main_compute7_outR5);
  ZLL_Main_compute7  instR50 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h6, zll_main_compute7_outR6);
  ZLL_Main_compute7  instR51 (zi9, 3'h1, zi15, zi11, 3'h2, 3'h7, zll_main_compute7_outR7);
  assign zi16 = {{zll_main_compute5_out, zll_main_compute5_outR1, zll_main_compute5_outR2, zll_main_compute5_outR3, zll_main_compute5_outR4, zll_main_compute5_outR5, zll_main_compute5_outR6, zll_main_compute5_outR7}, {zll_main_compute7_out, zll_main_compute7_outR1, zll_main_compute7_outR2, zll_main_compute7_outR3, zll_main_compute7_outR4, zll_main_compute7_outR5, zll_main_compute7_outR6, zll_main_compute7_outR7}};
  assign zi17 = zi16[63:0];
  assign zi18 = zi17;
  assign zi19 = zi16[127:64];
  assign zi20 = zi19;
  ZLL_Main_compute39  instR52 (3'h1, zi20, 3'h2, zi18, 3'h0, zll_main_compute39_out);
  ZLL_Main_compute39  instR53 (3'h1, zi20, 3'h2, zi18, 3'h1, zll_main_compute39_outR1);
  ZLL_Main_compute39  instR54 (3'h1, zi20, 3'h2, zi18, 3'h2, zll_main_compute39_outR2);
  ZLL_Main_compute39  instR55 (3'h1, zi20, 3'h2, zi18, 3'h3, zll_main_compute39_outR3);
  ZLL_Main_compute39  instR56 (3'h1, zi20, 3'h2, zi18, 3'h4, zll_main_compute39_outR4);
  ZLL_Main_compute39  instR57 (3'h1, zi20, 3'h2, zi18, 3'h5, zll_main_compute39_outR5);
  ZLL_Main_compute39  instR58 (3'h1, zi20, 3'h2, zi18, 3'h6, zll_main_compute39_outR6);
  ZLL_Main_compute39  instR59 (3'h1, zi20, 3'h2, zi18, 3'h7, zll_main_compute39_outR7);
  ZLL_Main_compute15  instR60 (zi20, zll_main_compute15_outR8);
  ZLL_Main_compute1  instR61 (zll_main_compute15_outR8, 3'h2, zll_main_compute1_outR4);
  assign zi21 = zll_main_compute1_outR4;
  ZLL_Main_compute15  instR62 (zi20, zll_main_compute15_outR9);
  ZLL_Main_compute51  instR63 (zll_main_compute15_outR9, zll_main_compute51_outR4);
  ZLL_Main_compute4  instR64 (zi21, 3'h1, zll_main_compute51_outR4, zll_main_compute4_outR1);
  assign zi22 = zll_main_compute4_outR1;
  ZLL_Main_compute45  instR65 (3'h2, zi18, zi22, zi20, 3'h1, 3'h0, zll_main_compute45_out);
  ZLL_Main_compute45  instR66 (3'h2, zi18, zi22, zi20, 3'h1, 3'h1, zll_main_compute45_outR1);
  ZLL_Main_compute45  instR67 (3'h2, zi18, zi22, zi20, 3'h1, 3'h2, zll_main_compute45_outR2);
  ZLL_Main_compute45  instR68 (3'h2, zi18, zi22, zi20, 3'h1, 3'h3, zll_main_compute45_outR3);
  ZLL_Main_compute45  instR69 (3'h2, zi18, zi22, zi20, 3'h1, 3'h4, zll_main_compute45_outR4);
  ZLL_Main_compute45  instR70 (3'h2, zi18, zi22, zi20, 3'h1, 3'h5, zll_main_compute45_outR5);
  ZLL_Main_compute45  instR71 (3'h2, zi18, zi22, zi20, 3'h1, 3'h6, zll_main_compute45_outR6);
  ZLL_Main_compute45  instR72 (3'h2, zi18, zi22, zi20, 3'h1, 3'h7, zll_main_compute45_outR7);
  assign zi23 = {zll_main_compute39_out, zll_main_compute39_outR1, zll_main_compute39_outR2, zll_main_compute39_outR3, zll_main_compute39_outR4, zll_main_compute39_outR5, zll_main_compute39_outR6, zll_main_compute39_outR7, {zll_main_compute45_out, zll_main_compute45_outR1, zll_main_compute45_outR2, zll_main_compute45_outR3, zll_main_compute45_outR4, zll_main_compute45_outR5, zll_main_compute45_outR6, zll_main_compute45_outR7}};
  assign zi24 = zi23[127:64];
  assign zi25 = zi23[63:0];
  assign zi26 = {zi24, zi25};
  assign zres = zi26;
  assign __out0 = zres[127:64];
  assign __out1 = zres[63:0];
endmodule

module ZLL_Main_compute62 (input logic [2:0] arg0,
  input logic [0:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_compute9_out;
  ZLL_Main_compute9  inst (arg0, 3'h1, zll_main_compute9_out);
  assign res = (arg1 == 1'h0) ? zll_main_compute9_out : arg0;
endmodule

module ZLL_Main_compute58 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} * {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute52 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute8_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute6_out;
  logic [2:0] zll_main_compute58_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute58_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute8  inst (arg4, arg0, zll_main_compute8_out);
  assign zi0 = zll_main_compute8_out;
  ZLL_Main_compute6  instR1 (arg4, arg0, zll_main_compute6_out);
  ZLL_Main_compute58  instR2 (zll_main_compute6_out, arg3, zll_main_compute58_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute58_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute58  instR3 (arg4, arg3, zll_main_compute58_outR1);
  assign slice_inR1 = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute58_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute51 (input logic [2:0] arg0,
  output logic [0:0] res);
  logic [127:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {{7'h7d{1'h0}}, arg0};
  assign zi1 = zi0[0];
  assign zi2 = zi1;
  assign res = (zi2 == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_compute50 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [2:0] zll_main_compute9_out;
  ZLL_Main_compute9  inst (arg0, arg1, zll_main_compute9_out);
  assign res = zll_main_compute9_out;
endmodule

module ZLL_Main_compute45 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute51_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute6_out;
  logic [2:0] zll_main_compute1_out;
  logic [2:0] zll_main_compute50_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute1_outR1;
  logic [2:0] zll_main_compute50_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute51  inst (arg5, zll_main_compute51_out);
  assign zi0 = zll_main_compute51_out;
  ZLL_Main_compute6  instR1 (arg5, arg4, zll_main_compute6_out);
  ZLL_Main_compute1  instR2 (zll_main_compute6_out, arg0, zll_main_compute1_out);
  ZLL_Main_compute50  instR3 (arg2, zll_main_compute1_out, zll_main_compute50_out);
  assign slice_in = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute50_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute1  instR4 (arg5, arg0, zll_main_compute1_outR1);
  ZLL_Main_compute50  instR5 (arg2, zll_main_compute1_outR1, zll_main_compute50_outR1);
  assign slice_inR1 = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute50_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute39 (input logic [2:0] arg0,
  input logic [63:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute51_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute6_out;
  logic [2:0] zll_main_compute1_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute1_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute51  inst (arg4, zll_main_compute51_out);
  assign zi0 = zll_main_compute51_out;
  ZLL_Main_compute6  instR1 (arg4, arg0, zll_main_compute6_out);
  ZLL_Main_compute1  instR2 (zll_main_compute6_out, arg2, zll_main_compute1_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute1_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute1  instR3 (arg4, arg2, zll_main_compute1_outR1);
  assign slice_inR1 = arg1 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute1_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute37 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [63:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute8_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute6_out;
  logic [2:0] zll_main_compute58_out;
  logic [2:0] zll_main_compute50_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute58_outR1;
  logic [2:0] zll_main_compute50_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute8  inst (arg5, arg1, zll_main_compute8_out);
  assign zi0 = zll_main_compute8_out;
  ZLL_Main_compute6  instR1 (arg5, arg1, zll_main_compute6_out);
  ZLL_Main_compute58  instR2 (zll_main_compute6_out, arg0, zll_main_compute58_out);
  ZLL_Main_compute50  instR3 (zll_main_compute58_out, arg3, zll_main_compute50_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute50_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute58  instR4 (arg5, arg0, zll_main_compute58_outR1);
  ZLL_Main_compute50  instR5 (zll_main_compute58_outR1, arg3, zll_main_compute50_outR1);
  assign slice_inR1 = arg4 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute50_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute15 (input logic [63:0] arg0,
  output logic [2:0] res);
  assign res = 3'h7;
endmodule

module ZLL_Main_compute9 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} + {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute8 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [0:0] res);
  assign res = {{7'h7d{1'h0}}, arg0} < {{7'h7d{1'h0}}, arg1};
endmodule

module ZLL_Main_compute7 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [2:0] arg2,
  input logic [63:0] arg3,
  input logic [2:0] arg4,
  input logic [2:0] arg5,
  output logic [7:0] res);
  logic [0:0] zll_main_compute8_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute6_out;
  logic [2:0] zll_main_compute58_out;
  logic [2:0] zll_main_compute50_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute58_outR1;
  logic [2:0] zll_main_compute50_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute8  inst (arg5, arg2, zll_main_compute8_out);
  assign zi0 = zll_main_compute8_out;
  ZLL_Main_compute6  instR1 (arg5, arg2, zll_main_compute6_out);
  ZLL_Main_compute58  instR2 (zll_main_compute6_out, arg4, zll_main_compute58_out);
  ZLL_Main_compute50  instR3 (zll_main_compute58_out, arg1, zll_main_compute50_out);
  assign slice_in = arg3 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute50_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute58  instR4 (arg5, arg4, zll_main_compute58_outR1);
  ZLL_Main_compute50  instR5 (zll_main_compute58_outR1, arg1, zll_main_compute50_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute50_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute6 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? ({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) : (({{7'h7d{1'h0}}, arg0} - {{7'h7d{1'h0}}, arg1}) % 128'h8);
  assign res = slice_in[2:0];
endmodule

module ZLL_Main_compute5 (input logic [63:0] arg0,
  input logic [2:0] arg1,
  input logic [63:0] arg2,
  input logic [2:0] arg3,
  input logic [2:0] arg4,
  output logic [7:0] res);
  logic [0:0] zll_main_compute8_out;
  logic [0:0] zi0;
  logic [2:0] zll_main_compute6_out;
  logic [2:0] zll_main_compute58_out;
  logic [63:0] slice_in;
  logic [2:0] zll_main_compute58_outR1;
  logic [63:0] slice_inR1;
  ZLL_Main_compute8  inst (arg4, arg3, zll_main_compute8_out);
  assign zi0 = zll_main_compute8_out;
  ZLL_Main_compute6  instR1 (arg4, arg3, zll_main_compute6_out);
  ZLL_Main_compute58  instR2 (zll_main_compute6_out, arg1, zll_main_compute58_out);
  assign slice_in = arg2 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute58_out}) - 128'h1) * 128'h8);
  ZLL_Main_compute58  instR3 (arg4, arg1, zll_main_compute58_outR1);
  assign slice_inR1 = arg0 >> (((128'h8 - {{7'h7d{1'h0}}, zll_main_compute58_outR1}) - 128'h1) * 128'h8);
  assign res = (zi0 == 1'h0) ? slice_in[7:0] : slice_inR1[7:0];
endmodule

module ZLL_Main_compute4 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  input logic [0:0] arg2,
  output logic [2:0] res);
  logic [2:0] zll_main_compute50_out;
  ZLL_Main_compute50  inst (arg0, arg1, zll_main_compute50_out);
  assign res = (arg2 == 1'h0) ? zll_main_compute50_out : arg0;
endmodule

module ZLL_Main_compute1 (input logic [2:0] arg0,
  input logic [2:0] arg1,
  output logic [2:0] res);
  logic [127:0] slice_in;
  assign slice_in = (128'h8 == {8'h80{1'h0}}) ? (({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) : ((({{7'h7d{1'h0}}, arg1} == {8'h80{1'h0}}) ? {8'h80{1'h1}} : ({{7'h7d{1'h0}}, arg0} / {{7'h7d{1'h0}}, arg1})) % 128'h8);
  assign res = slice_in[2:0];
endmodule