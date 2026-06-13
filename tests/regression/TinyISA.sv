module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [16:0] __in0,
  output logic [14:0] __out0);
  logic [3:0] __resumption_tag;
  logic [3:0] __resumption_tag_next;
  logic [69:0] __st0;
  logic [69:0] __st0_next;
  logic [69:0] main_putins_out;
  logic [142:0] zll_main_loop218_out;
  logic [142:0] zi2;
  logic [69:0] zi3;
  logic [75:0] main_getpc_out;
  logic [69:0] zll_main_reset32_out;
  logic [142:0] zll_main_loop218_outR1;
  logic [142:0] zi4;
  logic [69:0] zi5;
  logic [84:0] main_getout_out;
  logic [142:0] zll_main_loop217_out;
  logic [142:0] zi6;
  logic [14:0] zi7;
  logic [69:0] zi8;
  logic [69:0] main_putins_outR1;
  logic [142:0] zll_main_loop218_outR2;
  logic [142:0] zi10;
  logic [69:0] zi11;
  logic [69:0] main_incrpc_out;
  logic [142:0] zll_main_loop218_outR3;
  logic [142:0] zi12;
  logic [69:0] zi13;
  logic [75:0] main_getpc_outR1;
  logic [69:0] zll_main_reset32_outR1;
  logic [142:0] zll_main_loop218_outR4;
  logic [142:0] zi14;
  logic [69:0] zi15;
  logic [84:0] main_getout_outR1;
  logic [142:0] zll_main_loop217_outR1;
  logic [142:0] zi16;
  logic [14:0] zi17;
  logic [69:0] zi18;
  logic [142:0] zll_pure_dispatch7_out;
  logic [142:0] zll_pure_dispatch7_outR1;
  logic [142:0] zll_pure_dispatch7_outR2;
  logic [69:0] main_putins_outR2;
  logic [142:0] zll_main_loop218_outR5;
  logic [142:0] zi20;
  logic [69:0] zi21;
  logic [86:0] main_getins_out;
  logic [86:0] zi22;
  logic [69:0] zi24;
  logic [7:0] zi26;
  logic [77:0] zi27;
  logic [7:0] zi28;
  logic [69:0] zi29;
  logic [69:0] zll_main_putreg67_out;
  logic [142:0] zll_main_loop218_outR6;
  logic [142:0] zll_main_loop163_out;
  logic [142:0] zll_pure_dispatch7_outR3;
  logic [69:0] main_putins_outR3;
  logic [142:0] zll_main_loop218_outR7;
  logic [142:0] zi31;
  logic [69:0] zi32;
  logic [69:0] main_incrpc_outR1;
  logic [142:0] zll_main_loop218_outR8;
  logic [142:0] zi33;
  logic [69:0] zi34;
  logic [75:0] main_getpc_outR2;
  logic [69:0] zll_main_reset32_outR2;
  logic [142:0] zll_main_loop218_outR9;
  logic [142:0] zi35;
  logic [69:0] zi36;
  logic [84:0] main_getout_outR2;
  logic [142:0] zll_main_loop217_outR2;
  logic [142:0] zi37;
  logic [14:0] zi38;
  logic [69:0] zi39;
  logic [142:0] zll_pure_dispatch7_outR4;
  logic [142:0] zres;
  Main_putIns  inst (__in0, __st0, main_putins_out);
  ZLL_Main_loop218  instR1 (main_putins_out, zll_main_loop218_out);
  assign zi2 = zll_main_loop218_out;
  assign zi3 = zi2[69:0];
  Main_getPC  instR2 (zi3, main_getpc_out);
  ZLL_Main_reset32  instR3 (main_getpc_out, zll_main_reset32_out);
  ZLL_Main_loop218  instR4 (zll_main_reset32_out, zll_main_loop218_outR1);
  assign zi4 = zll_main_loop218_outR1;
  assign zi5 = zi4[69:0];
  Main_getOut  instR5 (zi5, main_getout_out);
  ZLL_Main_loop217  instR6 (main_getout_out, zll_main_loop217_out);
  assign zi6 = zll_main_loop217_out;
  assign zi7 = zi6[84:70];
  assign zi8 = zi6[69:0];
  Main_putIns  instR7 (__in0, __st0, main_putins_outR1);
  ZLL_Main_loop218  instR8 (main_putins_outR1, zll_main_loop218_outR2);
  assign zi10 = zll_main_loop218_outR2;
  assign zi11 = zi10[69:0];
  Main_incrPC  instR9 (zi11, main_incrpc_out);
  ZLL_Main_loop218  instR10 (main_incrpc_out, zll_main_loop218_outR3);
  assign zi12 = zll_main_loop218_outR3;
  assign zi13 = zi12[69:0];
  Main_getPC  instR11 (zi13, main_getpc_outR1);
  ZLL_Main_reset32  instR12 (main_getpc_outR1, zll_main_reset32_outR1);
  ZLL_Main_loop218  instR13 (zll_main_reset32_outR1, zll_main_loop218_outR4);
  assign zi14 = zll_main_loop218_outR4;
  assign zi15 = zi14[69:0];
  Main_getOut  instR14 (zi15, main_getout_outR1);
  ZLL_Main_loop217  instR15 (main_getout_outR1, zll_main_loop217_outR1);
  assign zi16 = zll_main_loop217_outR1;
  assign zi17 = zi16[84:70];
  assign zi18 = zi16[69:0];
  ZLL_Pure_dispatch7  instR16 (__in0, __st0, zll_pure_dispatch7_out);
  ZLL_Pure_dispatch7  instR17 (__in0, __st0, zll_pure_dispatch7_outR1);
  ZLL_Pure_dispatch7  instR18 (__in0, __st0, zll_pure_dispatch7_outR2);
  Main_putIns  instR19 (__in0, __st0, main_putins_outR2);
  ZLL_Main_loop218  instR20 (main_putins_outR2, zll_main_loop218_outR5);
  assign zi20 = zll_main_loop218_outR5;
  assign zi21 = zi20[69:0];
  Main_getIns  instR21 (zi21, main_getins_out);
  assign zi22 = main_getins_out;
  assign zi24 = zi22[69:0];
  assign zi26 = zi22[77:70];
  assign zi27 = {zi26, zi24};
  assign zi28 = zi27[77:70];
  assign zi29 = zi27[69:0];
  ZLL_Main_putReg67  instR22 (zi28, 2'h0, {2'h0, zi28}, zi29, zll_main_putreg67_out);
  ZLL_Main_loop218  instR23 (zll_main_putreg67_out, zll_main_loop218_outR6);
  ZLL_Main_loop163  instR24 (zll_main_loop218_outR6, zll_main_loop163_out);
  ZLL_Pure_dispatch7  instR25 (__in0, __st0, zll_pure_dispatch7_outR3);
  Main_putIns  instR26 (__in0, __st0, main_putins_outR3);
  ZLL_Main_loop218  instR27 (main_putins_outR3, zll_main_loop218_outR7);
  assign zi31 = zll_main_loop218_outR7;
  assign zi32 = zi31[69:0];
  Main_incrPC  instR28 (zi32, main_incrpc_outR1);
  ZLL_Main_loop218  instR29 (main_incrpc_outR1, zll_main_loop218_outR8);
  assign zi33 = zll_main_loop218_outR8;
  assign zi34 = zi33[69:0];
  Main_getPC  instR30 (zi34, main_getpc_outR2);
  ZLL_Main_reset32  instR31 (main_getpc_outR2, zll_main_reset32_outR2);
  ZLL_Main_loop218  instR32 (zll_main_reset32_outR2, zll_main_loop218_outR9);
  assign zi35 = zll_main_loop218_outR9;
  assign zi36 = zi35[69:0];
  Main_getOut  instR33 (zi36, main_getout_outR2);
  ZLL_Main_loop217  instR34 (main_getout_outR2, zll_main_loop217_outR2);
  assign zi37 = zll_main_loop217_outR2;
  assign zi38 = zi37[84:70];
  assign zi39 = zi37[69:0];
  ZLL_Pure_dispatch7  instR35 (__in0, __st0, zll_pure_dispatch7_outR4);
  assign zres = (__resumption_tag == 4'h1) ? {{1'h1, {6'h35{1'h0}}}, zi7, 4'h3, zi8} : ((__resumption_tag == 4'h2) ? {{1'h1, {6'h35{1'h0}}}, zi17, 4'h6, zi18} : ((__resumption_tag == 4'h3) ? zll_pure_dispatch7_out : ((__resumption_tag == 4'h4) ? zll_pure_dispatch7_outR1 : ((__resumption_tag == 4'h5) ? zll_pure_dispatch7_outR2 : ((__resumption_tag == 4'h6) ? zll_main_loop163_out : ((__resumption_tag == 4'h7) ? zll_pure_dispatch7_outR3 : ((__resumption_tag == 4'h8) ? {{1'h1, {6'h35{1'h0}}}, zi38, 4'h5, zi39} : zll_pure_dispatch7_outR4)))))));
  assign __resumption_tag_next = zres[73:70];
  assign __st0_next = zres[69:0];
  assign __out0 = zres[88:74];
  initial {__resumption_tag, __st0} = {4'h1, {7'h46{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {4'h1, {7'h46{1'h0}}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_Main_putReg67 (input logic [7:0] arg0,
  input logic [1:0] arg1,
  input logic [9:0] arg2,
  input logic [69:0] arg3,
  output logic [69:0] res);
  logic [7:0] zi0;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [7:0] zi7;
  logic [5:0] zi8;
  logic [16:0] zi9;
  logic [14:0] zi10;
  logic [69:0] zll_main_putreg48_out;
  logic [7:0] zi16;
  logic [7:0] zi18;
  logic [7:0] zi19;
  logic [5:0] zi20;
  logic [16:0] zi21;
  logic [14:0] zi22;
  logic [7:0] zi28;
  logic [7:0] zi29;
  logic [7:0] zi31;
  logic [5:0] zi32;
  logic [16:0] zi33;
  logic [14:0] zi34;
  logic [7:0] zi40;
  logic [7:0] zi41;
  logic [7:0] zi42;
  logic [5:0] zi44;
  logic [16:0] zi45;
  logic [14:0] zi46;
  assign zi0 = arg2[7:0];
  assign zi5 = arg3[61:54];
  assign zi6 = arg3[53:46];
  assign zi7 = arg3[45:38];
  assign zi8 = arg3[37:32];
  assign zi9 = arg3[31:15];
  assign zi10 = arg3[14:0];
  ZLL_Main_putReg48  inst (zi8, zi0, zi7, zi5, zi6, zi9, zi10, zll_main_putreg48_out);
  assign zi16 = arg3[69:62];
  assign zi18 = arg3[53:46];
  assign zi19 = arg3[45:38];
  assign zi20 = arg3[37:32];
  assign zi21 = arg3[31:15];
  assign zi22 = arg3[14:0];
  assign zi28 = arg3[69:62];
  assign zi29 = arg3[61:54];
  assign zi31 = arg3[45:38];
  assign zi32 = arg3[37:32];
  assign zi33 = arg3[31:15];
  assign zi34 = arg3[14:0];
  assign zi40 = arg3[69:62];
  assign zi41 = arg3[61:54];
  assign zi42 = arg3[53:46];
  assign zi44 = arg3[37:32];
  assign zi45 = arg3[31:15];
  assign zi46 = arg3[14:0];
  assign res = (arg2[9:8] == 2'h0) ? zll_main_putreg48_out : ((arg1 == 2'h1) ? {zi16, arg0, zi18, zi19, zi20, zi21, zi22} : ((arg1 == 2'h2) ? {zi28, zi29, arg0, zi31, zi32, zi33, zi34} : {zi40, zi41, zi42, arg0, zi44, zi45, zi46}));
endmodule

module ZLL_Main_loop218 (input logic [69:0] arg0,
  output logic [142:0] res);
  assign res = {{2'h1, {7'h47{1'h0}}}, arg0};
endmodule

module ZLL_Main_getReg23 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [69:0] arg2,
  output logic [77:0] res);
  logic [7:0] zi3;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [5:0] zi7;
  logic [16:0] zi8;
  logic [14:0] zi9;
  logic [7:0] zll_main_r05_out;
  logic [7:0] zi14;
  logic [7:0] zi15;
  logic [7:0] zi16;
  logic [5:0] zi17;
  logic [16:0] zi18;
  logic [14:0] zi19;
  logic [7:0] zll_main_r05_outR1;
  logic [7:0] zi25;
  logic [7:0] zi26;
  logic [5:0] zi27;
  logic [16:0] zi28;
  logic [14:0] zi29;
  logic [7:0] zll_main_r15_out;
  logic [7:0] zi36;
  logic [5:0] zi37;
  logic [16:0] zi38;
  logic [14:0] zi39;
  logic [7:0] zll_main_r37_out;
  assign zi3 = arg2[69:62];
  assign zi5 = arg2[53:46];
  assign zi6 = arg2[45:38];
  assign zi7 = arg2[37:32];
  assign zi8 = arg2[31:15];
  assign zi9 = arg2[14:0];
  ZLL_Main_r05  inst (zi3, zi5, zi6, zi7, zi8, zi9, zll_main_r05_out);
  assign zi14 = arg2[61:54];
  assign zi15 = arg2[53:46];
  assign zi16 = arg2[45:38];
  assign zi17 = arg2[37:32];
  assign zi18 = arg2[31:15];
  assign zi19 = arg2[14:0];
  ZLL_Main_r05  instR1 (zi14, zi15, zi16, zi17, zi18, zi19, zll_main_r05_outR1);
  assign zi25 = arg2[53:46];
  assign zi26 = arg2[45:38];
  assign zi27 = arg2[37:32];
  assign zi28 = arg2[31:15];
  assign zi29 = arg2[14:0];
  ZLL_Main_r15  instR2 (zi25, zi26, zi27, zi28, zi29, zll_main_r15_out);
  assign zi36 = arg2[45:38];
  assign zi37 = arg2[37:32];
  assign zi38 = arg2[31:15];
  assign zi39 = arg2[14:0];
  ZLL_Main_r37  instR3 (zi36, zi37, zi38, zi39, zll_main_r37_out);
  assign res = (arg1 == 2'h0) ? {zll_main_r05_out, arg2} : ((arg0 == 2'h1) ? {zll_main_r05_outR1, arg2} : ((arg0 == 2'h2) ? {zll_main_r15_out, arg2} : {zll_main_r37_out, arg2}));
endmodule

module ZLL_Main_loop217 (input logic [84:0] arg0,
  output logic [142:0] res);
  logic [14:0] zi0;
  logic [69:0] zi1;
  assign zi0 = arg0[84:70];
  assign zi1 = arg0[69:0];
  assign res = {{3'h1, {6'h37{1'h0}}}, zi0, zi1};
endmodule

module Main_putPC1 (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [16:0] zi8;
  logic [14:0] zi9;
  logic [69:0] zll_main_putreg48_out;
  assign zi3 = arg1[69:62];
  assign zi4 = arg1[61:54];
  assign zi5 = arg1[53:46];
  assign zi6 = arg1[45:38];
  assign zi8 = arg1[31:15];
  assign zi9 = arg1[14:0];
  ZLL_Main_putReg48  inst (arg0, zi3, zi6, zi4, zi5, zi8, zi9, zll_main_putreg48_out);
  assign res = zll_main_putreg48_out;
endmodule

module Main_putIns (input logic [16:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [5:0] zi7;
  logic [14:0] zi9;
  assign zi3 = arg1[69:62];
  assign zi4 = arg1[61:54];
  assign zi5 = arg1[53:46];
  assign zi6 = arg1[45:38];
  assign zi7 = arg1[37:32];
  assign zi9 = arg1[14:0];
  assign res = {zi3, zi4, zi5, zi6, zi7, arg0, zi9};
endmodule

module ZLL_Main_r37 (input logic [7:0] arg0,
  input logic [5:0] arg1,
  input logic [16:0] arg2,
  input logic [14:0] arg3,
  output logic [7:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_r15 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [5:0] arg2,
  input logic [16:0] arg3,
  input logic [14:0] arg4,
  output logic [7:0] res);
  logic [7:0] zll_main_r37_out;
  ZLL_Main_r37  inst (arg0, arg2, arg3, arg4, zll_main_r37_out);
  assign res = zll_main_r37_out;
endmodule

module ZLL_Pure_dispatch7 (input logic [16:0] arg0,
  input logic [69:0] arg1,
  output logic [142:0] res);
  logic [69:0] main_putins_out;
  logic [142:0] zll_main_loop218_out;
  logic [142:0] zll_main_loop163_out;
  Main_putIns  inst (arg0, arg1, main_putins_out);
  ZLL_Main_loop218  instR1 (main_putins_out, zll_main_loop218_out);
  ZLL_Main_loop163  instR2 (zll_main_loop218_out, zll_main_loop163_out);
  assign res = zll_main_loop163_out;
endmodule

module ZLL_Main_putReg48 (input logic [5:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [7:0] arg3,
  input logic [7:0] arg4,
  input logic [16:0] arg5,
  input logic [14:0] arg6,
  output logic [69:0] res);
  assign res = {arg1, arg3, arg4, arg2, arg0, arg5, arg6};
endmodule

module Main_getReg1 (input logic [1:0] arg0,
  input logic [69:0] arg1,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg23_out;
  ZLL_Main_getReg23  inst (arg0, arg0, arg1, zll_main_getreg23_out);
  assign res = zll_main_getreg23_out;
endmodule

module ZLL_Main_reset32 (input logic [75:0] arg0,
  output logic [69:0] res);
  logic [5:0] zi0;
  logic [69:0] zi1;
  logic [69:0] main_putaddrout_out;
  logic [69:0] zll_main_finishinstr3_out;
  assign zi0 = arg0[75:70];
  assign zi1 = arg0[69:0];
  Main_putAddrOut  inst (zi0, zi1, main_putaddrout_out);
  ZLL_Main_finishInstr3  instR1 (main_putaddrout_out, zll_main_finishinstr3_out);
  assign res = zll_main_finishinstr3_out;
endmodule

module ZLL_Main_finishInstr3 (input logic [69:0] arg0,
  output logic [69:0] res);
  logic [69:0] zll_main_putweout4_out;
  ZLL_Main_putWeOut4  inst (1'h0, arg0, zll_main_putweout4_out);
  assign res = zll_main_putweout4_out;
endmodule

module ZLL_Main_loop163 (input logic [142:0] arg0,
  output logic [142:0] res);
  logic [69:0] zi0;
  logic [86:0] main_getins_out;
  logic [86:0] zi1;
  logic [69:0] zi3;
  logic [8:0] zi4;
  logic [78:0] zi6;
  logic [8:0] zi7;
  logic [69:0] zi8;
  logic [142:0] zi9;
  logic [69:0] zi11;
  logic [69:0] main_incrpc_out;
  logic [142:0] zll_main_loop218_out;
  logic [142:0] zi18;
  logic [69:0] zi19;
  logic [75:0] main_getpc_out;
  logic [69:0] zll_main_reset32_out;
  logic [142:0] zll_main_loop218_outR1;
  logic [142:0] zi20;
  logic [69:0] zi21;
  logic [84:0] main_getout_out;
  logic [142:0] zll_main_loop217_out;
  logic [142:0] zi22;
  logic [14:0] zi23;
  logic [69:0] zi24;
  logic [5:0] zi25;
  logic [69:0] main_putaddrout_out;
  logic [69:0] zll_main_finishinstr3_out;
  logic [142:0] zll_main_loop218_outR2;
  logic [142:0] zi26;
  logic [69:0] zi27;
  logic [84:0] main_getout_outR1;
  logic [142:0] zll_main_loop217_outR1;
  logic [142:0] zi28;
  logic [14:0] zi29;
  logic [69:0] zi30;
  logic [5:0] zi31;
  logic [77:0] main_getreg_out;
  logic [77:0] zi32;
  logic [7:0] zi33;
  logic [69:0] zi34;
  logic [69:0] main_putaddrout_outR1;
  logic [69:0] zi35;
  logic [84:0] main_getout_outR2;
  logic [84:0] zi36;
  logic [69:0] zi38;
  logic [0:0] zi39;
  logic [5:0] zi40;
  logic [69:0] main_putout_out;
  logic [69:0] zi42;
  logic [69:0] zll_main_putweout4_out;
  logic [142:0] zll_main_loop218_outR3;
  logic [142:0] zi43;
  logic [69:0] zi44;
  logic [84:0] main_getout_outR3;
  logic [142:0] zll_main_loop217_outR2;
  logic [142:0] zi45;
  logic [14:0] zi46;
  logic [69:0] zi47;
  logic [1:0] zi48;
  logic [1:0] zi49;
  logic [1:0] zi50;
  logic [77:0] main_getreg1_out;
  logic [77:0] zi51;
  logic [7:0] zi52;
  logic [69:0] zi53;
  logic [77:0] main_getreg1_outR1;
  logic [77:0] zi54;
  logic [7:0] zi55;
  logic [69:0] zi56;
  logic [7:0] zi57;
  logic [69:0] zll_main_putreg67_out;
  logic [69:0] zi58;
  logic [69:0] main_incrpc_outR1;
  logic [142:0] zll_main_loop218_outR4;
  logic [142:0] zi59;
  logic [69:0] zi60;
  logic [75:0] main_getpc_outR1;
  logic [69:0] zll_main_reset32_outR1;
  logic [142:0] zll_main_loop218_outR5;
  logic [142:0] zi61;
  logic [69:0] zi62;
  logic [84:0] main_getout_outR4;
  logic [142:0] zll_main_loop217_outR3;
  logic [142:0] zi63;
  logic [14:0] zi64;
  logic [69:0] zi65;
  logic [5:0] zi66;
  logic [77:0] main_getreg_outR1;
  logic [77:0] zi67;
  logic [7:0] zi68;
  logic [69:0] zi69;
  logic [0:0] zi70;
  logic [69:0] main_incrpc_outR2;
  logic [69:0] main_putpc1_out;
  logic [142:0] zll_main_loop218_outR6;
  logic [142:0] zi72;
  logic [69:0] zi73;
  logic [75:0] main_getpc_outR2;
  logic [69:0] zll_main_reset32_outR2;
  logic [142:0] zll_main_loop218_outR7;
  logic [142:0] zi74;
  logic [69:0] zi75;
  logic [84:0] main_getout_outR5;
  logic [142:0] zll_main_loop217_outR4;
  logic [142:0] zi76;
  logic [14:0] zi77;
  logic [69:0] zi78;
  assign zi0 = arg0[69:0];
  Main_getIns  inst (zi0, main_getins_out);
  assign zi1 = main_getins_out;
  assign zi3 = zi1[69:0];
  assign zi4 = zi1[86:78];
  assign zi6 = {zi4, zi3};
  assign zi7 = zi6[78:70];
  assign zi8 = zi6[69:0];
  assign zi9 = {{7'h40{1'h0}}, zi7, zi8};
  assign zi11 = zi9[69:0];
  Main_incrPC  instR1 (zi11, main_incrpc_out);
  ZLL_Main_loop218  instR2 (main_incrpc_out, zll_main_loop218_out);
  assign zi18 = zll_main_loop218_out;
  assign zi19 = zi18[69:0];
  Main_getPC  instR3 (zi19, main_getpc_out);
  ZLL_Main_reset32  instR4 (main_getpc_out, zll_main_reset32_out);
  ZLL_Main_loop218  instR5 (zll_main_reset32_out, zll_main_loop218_outR1);
  assign zi20 = zll_main_loop218_outR1;
  assign zi21 = zi20[69:0];
  Main_getOut  instR6 (zi21, main_getout_out);
  ZLL_Main_loop217  instR7 (main_getout_out, zll_main_loop217_out);
  assign zi22 = zll_main_loop217_out;
  assign zi23 = zi22[84:70];
  assign zi24 = zi22[69:0];
  assign zi25 = zi9[75:70];
  Main_putAddrOut  instR8 (zi25, zi11, main_putaddrout_out);
  ZLL_Main_finishInstr3  instR9 (main_putaddrout_out, zll_main_finishinstr3_out);
  ZLL_Main_loop218  instR10 (zll_main_finishinstr3_out, zll_main_loop218_outR2);
  assign zi26 = zll_main_loop218_outR2;
  assign zi27 = zi26[69:0];
  Main_getOut  instR11 (zi27, main_getout_outR1);
  ZLL_Main_loop217  instR12 (main_getout_outR1, zll_main_loop217_outR1);
  assign zi28 = zll_main_loop217_outR1;
  assign zi29 = zi28[84:70];
  assign zi30 = zi28[69:0];
  assign zi31 = zi9[75:70];
  Main_getReg  instR13 (zi11, main_getreg_out);
  assign zi32 = main_getreg_out;
  assign zi33 = zi32[77:70];
  assign zi34 = zi32[69:0];
  Main_putAddrOut  instR14 (zi31, zi34, main_putaddrout_outR1);
  assign zi35 = main_putaddrout_outR1;
  Main_getOut  instR15 (zi35, main_getout_outR2);
  assign zi36 = main_getout_outR2;
  assign zi38 = zi36[69:0];
  assign zi39 = zi36[84];
  assign zi40 = zi36[83:78];
  Main_putOut  instR16 ({zi39, zi40, zi33}, zi38, main_putout_out);
  assign zi42 = main_putout_out;
  ZLL_Main_putWeOut4  instR17 (1'h1, zi42, zll_main_putweout4_out);
  ZLL_Main_loop218  instR18 (zll_main_putweout4_out, zll_main_loop218_outR3);
  assign zi43 = zll_main_loop218_outR3;
  assign zi44 = zi43[69:0];
  Main_getOut  instR19 (zi44, main_getout_outR3);
  ZLL_Main_loop217  instR20 (main_getout_outR3, zll_main_loop217_outR2);
  assign zi45 = zll_main_loop217_outR2;
  assign zi46 = zi45[84:70];
  assign zi47 = zi45[69:0];
  assign zi48 = zi9[75:74];
  assign zi49 = zi9[73:72];
  assign zi50 = zi9[71:70];
  Main_getReg1  instR21 (zi49, zi11, main_getreg1_out);
  assign zi51 = main_getreg1_out;
  assign zi52 = zi51[77:70];
  assign zi53 = zi51[69:0];
  Main_getReg1  instR22 (zi50, zi53, main_getreg1_outR1);
  assign zi54 = main_getreg1_outR1;
  assign zi55 = zi54[77:70];
  assign zi56 = zi54[69:0];
  assign zi57 = ~(zi52 & zi55);
  ZLL_Main_putReg67  instR23 (zi57, zi48, {zi48, zi57}, zi56, zll_main_putreg67_out);
  assign zi58 = zll_main_putreg67_out;
  Main_incrPC  instR24 (zi58, main_incrpc_outR1);
  ZLL_Main_loop218  instR25 (main_incrpc_outR1, zll_main_loop218_outR4);
  assign zi59 = zll_main_loop218_outR4;
  assign zi60 = zi59[69:0];
  Main_getPC  instR26 (zi60, main_getpc_outR1);
  ZLL_Main_reset32  instR27 (main_getpc_outR1, zll_main_reset32_outR1);
  ZLL_Main_loop218  instR28 (zll_main_reset32_outR1, zll_main_loop218_outR5);
  assign zi61 = zll_main_loop218_outR5;
  assign zi62 = zi61[69:0];
  Main_getOut  instR29 (zi62, main_getout_outR4);
  ZLL_Main_loop217  instR30 (main_getout_outR4, zll_main_loop217_outR3);
  assign zi63 = zll_main_loop217_outR3;
  assign zi64 = zi63[84:70];
  assign zi65 = zi63[69:0];
  assign zi66 = zi9[75:70];
  Main_getReg  instR31 (zi11, main_getreg_outR1);
  assign zi67 = main_getreg_outR1;
  assign zi68 = zi67[77:70];
  assign zi69 = zi67[69:0];
  assign zi70 = zi68 == 8'h0;
  Main_incrPC  instR32 (zi69, main_incrpc_outR2);
  Main_putPC1  instR33 (zi66, zi69, main_putpc1_out);
  ZLL_Main_loop218  instR34 ((zi70 == 1'h1) ? main_incrpc_outR2 : main_putpc1_out, zll_main_loop218_outR6);
  assign zi72 = zll_main_loop218_outR6;
  assign zi73 = zi72[69:0];
  Main_getPC  instR35 (zi73, main_getpc_outR2);
  ZLL_Main_reset32  instR36 (main_getpc_outR2, zll_main_reset32_outR2);
  ZLL_Main_loop218  instR37 (zll_main_reset32_outR2, zll_main_loop218_outR7);
  assign zi74 = zll_main_loop218_outR7;
  assign zi75 = zi74[69:0];
  Main_getOut  instR38 (zi75, main_getout_outR5);
  ZLL_Main_loop217  instR39 (main_getout_outR5, zll_main_loop217_outR4);
  assign zi76 = zll_main_loop217_outR4;
  assign zi77 = zi76[84:70];
  assign zi78 = zi76[69:0];
  assign res = (zi9[78:76] == 3'h0) ? {{1'h1, {6'h35{1'h0}}}, zi23, 4'h4, zi24} : ((zi9[78:76] == 3'h1) ? {{1'h1, {6'h35{1'h0}}}, zi29, 4'h2, zi30} : ((zi9[78:76] == 3'h2) ? {{1'h1, {6'h35{1'h0}}}, zi46, 4'h8, zi47} : ((zi9[78:76] == 3'h3) ? {{1'h1, {6'h35{1'h0}}}, zi64, 4'h0, zi65} : {{1'h1, {6'h35{1'h0}}}, zi77, 4'h7, zi78})));
endmodule

module ZLL_Main_r05 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [5:0] arg3,
  input logic [16:0] arg4,
  input logic [14:0] arg5,
  output logic [7:0] res);
  logic [7:0] zll_main_r15_out;
  ZLL_Main_r15  inst (arg0, arg2, arg3, arg4, arg5, zll_main_r15_out);
  assign res = zll_main_r15_out;
endmodule

module ZLL_Main_putWeOut4 (input logic [0:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [84:0] main_getout_out;
  logic [84:0] zi0;
  logic [69:0] zi2;
  logic [5:0] zi4;
  logic [7:0] zi5;
  logic [69:0] main_putout_out;
  Main_getOut  inst (arg1, main_getout_out);
  assign zi0 = main_getout_out;
  assign zi2 = zi0[69:0];
  assign zi4 = zi0[83:78];
  assign zi5 = zi0[77:70];
  Main_putOut  instR1 ({arg0, zi4, zi5}, zi2, main_putout_out);
  assign res = main_putout_out;
endmodule

module Main_getIns (input logic [69:0] arg0,
  output logic [86:0] res);
  logic [16:0] zi8;
  assign zi8 = arg0[31:15];
  assign res = {zi8, arg0};
endmodule

module Main_getReg (input logic [69:0] arg0,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg23_out;
  ZLL_Main_getReg23  inst (2'h0, 2'h0, arg0, zll_main_getreg23_out);
  assign res = zll_main_getreg23_out;
endmodule

module Main_getOut (input logic [69:0] arg0,
  output logic [84:0] res);
  logic [14:0] zi9;
  assign zi9 = arg0[14:0];
  assign res = {zi9, arg0};
endmodule

module Main_putAddrOut (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [84:0] main_getout_out;
  logic [84:0] zi0;
  logic [69:0] zi2;
  logic [0:0] zi3;
  logic [7:0] zi5;
  logic [69:0] main_putout_out;
  Main_getOut  inst (arg1, main_getout_out);
  assign zi0 = main_getout_out;
  assign zi2 = zi0[69:0];
  assign zi3 = zi0[84];
  assign zi5 = zi0[77:70];
  Main_putOut  instR1 ({zi3, arg0, zi5}, zi2, main_putout_out);
  assign res = main_putout_out;
endmodule

module Main_incrPC (input logic [69:0] arg0,
  output logic [69:0] res);
  logic [75:0] main_getpc_out;
  logic [75:0] zi0;
  logic [5:0] zi1;
  logic [69:0] zi2;
  logic [69:0] main_putpc1_out;
  Main_getPC  inst (arg0, main_getpc_out);
  assign zi0 = main_getpc_out;
  assign zi1 = zi0[75:70];
  assign zi2 = zi0[69:0];
  Main_putPC1  instR1 (zi1 + 6'h1, zi2, main_putpc1_out);
  assign res = main_putpc1_out;
endmodule

module Main_getPC (input logic [69:0] arg0,
  output logic [75:0] res);
  logic [5:0] zi7;
  assign zi7 = arg0[37:32];
  assign res = {zi7, arg0};
endmodule

module Main_putOut (input logic [14:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [5:0] zi7;
  logic [16:0] zi8;
  assign zi3 = arg1[69:62];
  assign zi4 = arg1[61:54];
  assign zi5 = arg1[53:46];
  assign zi6 = arg1[45:38];
  assign zi7 = arg1[37:32];
  assign zi8 = arg1[31:15];
  assign res = {zi3, zi4, zi5, zi6, zi7, zi8, arg0};
endmodule