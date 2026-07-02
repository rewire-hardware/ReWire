module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [16:0] __in0,
  output logic [14:0] __out0);
  logic [3:0] __resumption_tag;
  logic [3:0] __resumption_tag_next;
  logic [69:0] __st0;
  logic [69:0] __st0_next;
  logic [69:0] main_putins_out;
  logic [69:0] zi2;
  logic [89:0] zi3;
  logic [69:0] zi4;
  logic [69:0] main_incrpc_out;
  logic [69:0] zi5;
  logic [89:0] zi6;
  logic [69:0] zi7;
  logic [75:0] main_getpc_out;
  logic [75:0] zi8;
  logic [5:0] zi9;
  logic [69:0] zi10;
  logic [69:0] zll_main_finishinstr1_out;
  logic [69:0] zi11;
  logic [89:0] zi12;
  logic [69:0] zi13;
  logic [84:0] main_getout_out;
  logic [84:0] zi14;
  logic [14:0] zi15;
  logic [69:0] zi16;
  logic [89:0] zi17;
  logic [14:0] zi18;
  logic [69:0] zi19;
  logic [89:0] zll_main_loop31_out;
  logic [69:0] main_putins_outR1;
  logic [69:0] zi22;
  logic [89:0] zi23;
  logic [69:0] zi24;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi25;
  logic [89:0] zi26;
  logic [69:0] zi27;
  logic [75:0] main_getpc_outR1;
  logic [75:0] zi28;
  logic [5:0] zi29;
  logic [69:0] zi30;
  logic [69:0] zll_main_finishinstr1_outR1;
  logic [69:0] zi31;
  logic [89:0] zi32;
  logic [69:0] zi33;
  logic [84:0] main_getout_outR1;
  logic [84:0] zi34;
  logic [14:0] zi35;
  logic [69:0] zi36;
  logic [89:0] zi37;
  logic [14:0] zi38;
  logic [69:0] zi39;
  logic [69:0] main_putins_outR2;
  logic [69:0] zi41;
  logic [89:0] zi42;
  logic [69:0] zi43;
  logic [86:0] main_getins_out;
  logic [86:0] zi44;
  logic [69:0] zi46;
  logic [7:0] zi47;
  logic [77:0] zi48;
  logic [7:0] zi49;
  logic [69:0] zi50;
  logic [69:0] zll_main_putreg3_out;
  logic [69:0] zi51;
  logic [89:0] zi52;
  logic [69:0] zi53;
  logic [89:0] zll_main_loop23_out;
  logic [89:0] zll_main_loop31_outR1;
  logic [89:0] zll_main_loop31_outR2;
  logic [89:0] zll_main_loop31_outR3;
  logic [89:0] zll_main_loop31_outR4;
  logic [69:0] main_putins_outR3;
  logic [69:0] zi59;
  logic [89:0] zi60;
  logic [69:0] zi61;
  logic [75:0] main_getpc_outR2;
  logic [75:0] zi62;
  logic [5:0] zi63;
  logic [69:0] zi64;
  logic [69:0] zll_main_finishinstr1_outR2;
  logic [69:0] zi65;
  logic [89:0] zi66;
  logic [69:0] zi67;
  logic [84:0] main_getout_outR2;
  logic [84:0] zi68;
  logic [14:0] zi69;
  logic [69:0] zi70;
  logic [89:0] zi71;
  logic [14:0] zi72;
  logic [69:0] zi73;
  logic [89:0] zres;
  Main_putIns  inst (__in0, __st0, main_putins_out);
  assign zi2 = main_putins_out;
  assign zi3 = {20'h10000, zi2};
  assign zi4 = zi3[69:0];
  Main_incrPC  instR1 (zi4, main_incrpc_out);
  assign zi5 = main_incrpc_out;
  assign zi6 = {20'h10000, zi5};
  assign zi7 = zi6[69:0];
  Main_getPC  instR2 (zi7, main_getpc_out);
  assign zi8 = main_getpc_out;
  assign zi9 = zi8[75:70];
  assign zi10 = zi8[69:0];
  ZLL_Main_finishInstr1  instR3 (zi9, zi10, zll_main_finishinstr1_out);
  assign zi11 = zll_main_finishinstr1_out;
  assign zi12 = {20'h10000, zi11};
  assign zi13 = zi12[69:0];
  Main_getOut  instR4 (zi13, main_getout_out);
  assign zi14 = main_getout_out;
  assign zi15 = zi14[84:70];
  assign zi16 = zi14[69:0];
  assign zi17 = {5'h1, zi15, zi16};
  assign zi18 = zi17[84:70];
  assign zi19 = zi17[69:0];
  ZLL_Main_loop31  instR5 (__in0, __st0, zll_main_loop31_out);
  Main_putIns  instR6 (__in0, __st0, main_putins_outR1);
  assign zi22 = main_putins_outR1;
  assign zi23 = {20'h10000, zi22};
  assign zi24 = zi23[69:0];
  Main_incrPC  instR7 (zi24, main_incrpc_outR1);
  assign zi25 = main_incrpc_outR1;
  assign zi26 = {20'h10000, zi25};
  assign zi27 = zi26[69:0];
  Main_getPC  instR8 (zi27, main_getpc_outR1);
  assign zi28 = main_getpc_outR1;
  assign zi29 = zi28[75:70];
  assign zi30 = zi28[69:0];
  ZLL_Main_finishInstr1  instR9 (zi29, zi30, zll_main_finishinstr1_outR1);
  assign zi31 = zll_main_finishinstr1_outR1;
  assign zi32 = {20'h10000, zi31};
  assign zi33 = zi32[69:0];
  Main_getOut  instR10 (zi33, main_getout_outR1);
  assign zi34 = main_getout_outR1;
  assign zi35 = zi34[84:70];
  assign zi36 = zi34[69:0];
  assign zi37 = {5'h1, zi35, zi36};
  assign zi38 = zi37[84:70];
  assign zi39 = zi37[69:0];
  Main_putIns  instR11 (__in0, __st0, main_putins_outR2);
  assign zi41 = main_putins_outR2;
  assign zi42 = {20'h10000, zi41};
  assign zi43 = zi42[69:0];
  Main_getIns  instR12 (zi43, main_getins_out);
  assign zi44 = main_getins_out;
  assign zi46 = zi44[69:0];
  assign zi47 = zi44[77:70];
  assign zi48 = {zi47, zi46};
  assign zi49 = zi48[77:70];
  assign zi50 = zi48[69:0];
  ZLL_Main_putReg3  instR13 (zi49, 2'h0, zi50, zll_main_putreg3_out);
  assign zi51 = zll_main_putreg3_out;
  assign zi52 = {20'h10000, zi51};
  assign zi53 = zi52[69:0];
  ZLL_Main_loop23  instR14 (zi53, zll_main_loop23_out);
  ZLL_Main_loop31  instR15 (__in0, __st0, zll_main_loop31_outR1);
  ZLL_Main_loop31  instR16 (__in0, __st0, zll_main_loop31_outR2);
  ZLL_Main_loop31  instR17 (__in0, __st0, zll_main_loop31_outR3);
  ZLL_Main_loop31  instR18 (__in0, __st0, zll_main_loop31_outR4);
  Main_putIns  instR19 (__in0, __st0, main_putins_outR3);
  assign zi59 = main_putins_outR3;
  assign zi60 = {20'h10000, zi59};
  assign zi61 = zi60[69:0];
  Main_getPC  instR20 (zi61, main_getpc_outR2);
  assign zi62 = main_getpc_outR2;
  assign zi63 = zi62[75:70];
  assign zi64 = zi62[69:0];
  ZLL_Main_finishInstr1  instR21 (zi63, zi64, zll_main_finishinstr1_outR2);
  assign zi65 = zll_main_finishinstr1_outR2;
  assign zi66 = {20'h10000, zi65};
  assign zi67 = zi66[69:0];
  Main_getOut  instR22 (zi67, main_getout_outR2);
  assign zi68 = main_getout_outR2;
  assign zi69 = zi68[84:70];
  assign zi70 = zi68[69:0];
  assign zi71 = {5'h1, zi69, zi70};
  assign zi72 = zi71[84:70];
  assign zi73 = zi71[69:0];
  assign zres = (__resumption_tag == 4'h1) ? {1'h1, zi18, 4'h5, zi19} : ((__resumption_tag == 4'h2) ? zll_main_loop31_out : ((__resumption_tag == 4'h3) ? {1'h1, zi38, 4'h4, zi39} : ((__resumption_tag == 4'h4) ? zll_main_loop23_out : ((__resumption_tag == 4'h5) ? zll_main_loop31_outR1 : ((__resumption_tag == 4'h6) ? zll_main_loop31_outR2 : ((__resumption_tag == 4'h7) ? zll_main_loop31_outR3 : ((__resumption_tag == 4'h8) ? zll_main_loop31_outR4 : {1'h1, zi72, 4'h7, zi73})))))));
  assign __resumption_tag_next = zres[73:70];
  assign __st0_next = zres[69:0];
  assign __out0 = zres[88:74];
  initial {__resumption_tag, __st0} = {7'h4a{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {7'h4a{1'h0}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module Main_putPC1 (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [16:0] zi4;
  logic [14:0] zi5;
  assign zi0 = arg1[69:62];
  assign zi1 = arg1[61:54];
  assign zi2 = arg1[53:46];
  assign zi3 = arg1[45:38];
  assign zi4 = arg1[31:15];
  assign zi5 = arg1[14:0];
  assign res = {zi0, zi1, zi2, zi3, arg0, zi4, zi5};
endmodule

module Main_putIns (input logic [16:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [5:0] zi4;
  logic [14:0] zi5;
  assign zi0 = arg1[69:62];
  assign zi1 = arg1[61:54];
  assign zi2 = arg1[53:46];
  assign zi3 = arg1[45:38];
  assign zi4 = arg1[37:32];
  assign zi5 = arg1[14:0];
  assign res = {zi0, zi1, zi2, zi3, zi4, arg0, zi5};
endmodule

module ZLL_Main_loop31 (input logic [16:0] arg0,
  input logic [69:0] arg1,
  output logic [89:0] res);
  logic [69:0] main_putins_out;
  logic [69:0] zt0;
  logic [89:0] zt1;
  logic [69:0] s0;
  logic [89:0] zll_main_loop23_out;
  Main_putIns  inst (arg0, arg1, main_putins_out);
  assign zt0 = main_putins_out;
  assign zt1 = {20'h10000, zt0};
  assign s0 = zt1[69:0];
  ZLL_Main_loop23  instR1 (s0, zll_main_loop23_out);
  assign res = zll_main_loop23_out;
endmodule

module Main_getReg1 (input logic [1:0] arg0,
  input logic [69:0] arg1,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg3_out;
  ZLL_Main_getReg3  inst (arg0, arg1, zll_main_getreg3_out);
  assign res = zll_main_getreg3_out;
endmodule

module ZLL_Main_loop23 (input logic [69:0] arg0,
  output logic [89:0] res);
  logic [86:0] main_getins_out;
  logic [86:0] zi0;
  logic [69:0] zi2;
  logic [8:0] zi3;
  logic [78:0] zi4;
  logic [8:0] zi5;
  logic [69:0] zi6;
  logic [89:0] zi7;
  logic [69:0] zi9;
  logic [69:0] main_incrpc_out;
  logic [69:0] zi10;
  logic [89:0] zi11;
  logic [69:0] zi12;
  logic [75:0] main_getpc_out;
  logic [75:0] zi13;
  logic [5:0] zi14;
  logic [69:0] zi15;
  logic [69:0] zll_main_finishinstr1_out;
  logic [69:0] zi16;
  logic [89:0] zi17;
  logic [69:0] zi18;
  logic [84:0] main_getout_out;
  logic [84:0] zi19;
  logic [14:0] zi20;
  logic [69:0] zi21;
  logic [89:0] zi22;
  logic [14:0] zi23;
  logic [69:0] zi24;
  logic [5:0] zi25;
  logic [69:0] main_putaddrout_out;
  logic [69:0] zi26;
  logic [69:0] zll_main_ld_out;
  logic [69:0] zi27;
  logic [89:0] zi28;
  logic [69:0] zi29;
  logic [84:0] main_getout_outR1;
  logic [84:0] zi30;
  logic [14:0] zi31;
  logic [69:0] zi32;
  logic [89:0] zi33;
  logic [14:0] zi34;
  logic [69:0] zi35;
  logic [5:0] zi36;
  logic [77:0] main_getreg_out;
  logic [77:0] zi37;
  logic [7:0] zi38;
  logic [69:0] zi39;
  logic [69:0] main_putaddrout_outR1;
  logic [69:0] zi40;
  logic [84:0] main_getout_outR2;
  logic [84:0] zi41;
  logic [69:0] zi43;
  logic [0:0] zi44;
  logic [5:0] zi45;
  logic [69:0] main_putout_out;
  logic [69:0] zi46;
  logic [84:0] main_getout_outR3;
  logic [84:0] zi47;
  logic [14:0] zi48;
  logic [69:0] zi49;
  logic [69:0] zll_main_putweout_out;
  logic [69:0] zi50;
  logic [89:0] zi51;
  logic [69:0] zi52;
  logic [84:0] main_getout_outR4;
  logic [84:0] zi53;
  logic [14:0] zi54;
  logic [69:0] zi55;
  logic [89:0] zi56;
  logic [14:0] zi57;
  logic [69:0] zi58;
  logic [1:0] zi59;
  logic [1:0] zi60;
  logic [1:0] zi61;
  logic [77:0] main_getreg1_out;
  logic [77:0] zi62;
  logic [7:0] zi63;
  logic [69:0] zi64;
  logic [77:0] main_getreg1_outR1;
  logic [77:0] zi65;
  logic [7:0] zi66;
  logic [69:0] zi67;
  logic [7:0] zi68;
  logic [69:0] zll_main_putreg3_out;
  logic [69:0] zi69;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi70;
  logic [89:0] zi71;
  logic [69:0] zi72;
  logic [75:0] main_getpc_outR1;
  logic [75:0] zi73;
  logic [5:0] zi74;
  logic [69:0] zi75;
  logic [69:0] zll_main_finishinstr1_outR1;
  logic [69:0] zi76;
  logic [89:0] zi77;
  logic [69:0] zi78;
  logic [84:0] main_getout_outR5;
  logic [84:0] zi79;
  logic [14:0] zi80;
  logic [69:0] zi81;
  logic [89:0] zi82;
  logic [14:0] zi83;
  logic [69:0] zi84;
  logic [5:0] zi85;
  logic [77:0] main_getreg_outR1;
  logic [77:0] zi86;
  logic [7:0] zi87;
  logic [69:0] zi88;
  logic [0:0] zi89;
  logic [69:0] main_putpc1_out;
  logic [69:0] main_incrpc_outR2;
  logic [69:0] zi90;
  logic [89:0] zi91;
  logic [69:0] zi92;
  logic [75:0] main_getpc_outR2;
  logic [75:0] zi93;
  logic [5:0] zi94;
  logic [69:0] zi95;
  logic [69:0] zll_main_finishinstr1_outR2;
  logic [69:0] zi96;
  logic [89:0] zi97;
  logic [69:0] zi98;
  logic [84:0] main_getout_outR6;
  logic [84:0] zi99;
  logic [14:0] zi100;
  logic [69:0] zi101;
  logic [89:0] zi102;
  logic [14:0] zi103;
  logic [69:0] zi104;
  Main_getIns  inst (arg0, main_getins_out);
  assign zi0 = main_getins_out;
  assign zi2 = zi0[69:0];
  assign zi3 = zi0[86:78];
  assign zi4 = {zi3, zi2};
  assign zi5 = zi4[78:70];
  assign zi6 = zi4[69:0];
  assign zi7 = {11'h0, zi5, zi6};
  assign zi9 = zi7[69:0];
  Main_incrPC  instR1 (zi9, main_incrpc_out);
  assign zi10 = main_incrpc_out;
  assign zi11 = {20'h10000, zi10};
  assign zi12 = zi11[69:0];
  Main_getPC  instR2 (zi12, main_getpc_out);
  assign zi13 = main_getpc_out;
  assign zi14 = zi13[75:70];
  assign zi15 = zi13[69:0];
  ZLL_Main_finishInstr1  instR3 (zi14, zi15, zll_main_finishinstr1_out);
  assign zi16 = zll_main_finishinstr1_out;
  assign zi17 = {20'h10000, zi16};
  assign zi18 = zi17[69:0];
  Main_getOut  instR4 (zi18, main_getout_out);
  assign zi19 = main_getout_out;
  assign zi20 = zi19[84:70];
  assign zi21 = zi19[69:0];
  assign zi22 = {5'h1, zi20, zi21};
  assign zi23 = zi22[84:70];
  assign zi24 = zi22[69:0];
  assign zi25 = zi7[75:70];
  Main_putAddrOut  instR5 (zi25, zi9, main_putaddrout_out);
  assign zi26 = main_putaddrout_out;
  ZLL_Main_ld  instR6 (zi26, zll_main_ld_out);
  assign zi27 = zll_main_ld_out;
  assign zi28 = {20'h10000, zi27};
  assign zi29 = zi28[69:0];
  Main_getOut  instR7 (zi29, main_getout_outR1);
  assign zi30 = main_getout_outR1;
  assign zi31 = zi30[84:70];
  assign zi32 = zi30[69:0];
  assign zi33 = {5'h1, zi31, zi32};
  assign zi34 = zi33[84:70];
  assign zi35 = zi33[69:0];
  assign zi36 = zi7[75:70];
  Main_getReg  instR8 (zi9, main_getreg_out);
  assign zi37 = main_getreg_out;
  assign zi38 = zi37[77:70];
  assign zi39 = zi37[69:0];
  Main_putAddrOut  instR9 (zi36, zi39, main_putaddrout_outR1);
  assign zi40 = main_putaddrout_outR1;
  Main_getOut  instR10 (zi40, main_getout_outR2);
  assign zi41 = main_getout_outR2;
  assign zi43 = zi41[69:0];
  assign zi44 = zi41[84];
  assign zi45 = zi41[83:78];
  Main_putOut  instR11 ({zi44, zi45, zi38}, zi43, main_putout_out);
  assign zi46 = main_putout_out;
  Main_getOut  instR12 (zi46, main_getout_outR3);
  assign zi47 = main_getout_outR3;
  assign zi48 = zi47[84:70];
  assign zi49 = zi47[69:0];
  ZLL_Main_putWeOut  instR13 (1'h1, zi48, zi49, zll_main_putweout_out);
  assign zi50 = zll_main_putweout_out;
  assign zi51 = {20'h10000, zi50};
  assign zi52 = zi51[69:0];
  Main_getOut  instR14 (zi52, main_getout_outR4);
  assign zi53 = main_getout_outR4;
  assign zi54 = zi53[84:70];
  assign zi55 = zi53[69:0];
  assign zi56 = {5'h1, zi54, zi55};
  assign zi57 = zi56[84:70];
  assign zi58 = zi56[69:0];
  assign zi59 = zi7[75:74];
  assign zi60 = zi7[73:72];
  assign zi61 = zi7[71:70];
  Main_getReg1  instR15 (zi60, zi9, main_getreg1_out);
  assign zi62 = main_getreg1_out;
  assign zi63 = zi62[77:70];
  assign zi64 = zi62[69:0];
  Main_getReg1  instR16 (zi61, zi64, main_getreg1_outR1);
  assign zi65 = main_getreg1_outR1;
  assign zi66 = zi65[77:70];
  assign zi67 = zi65[69:0];
  assign zi68 = ~(zi63 & zi66);
  ZLL_Main_putReg3  instR17 (zi68, zi59, zi67, zll_main_putreg3_out);
  assign zi69 = zll_main_putreg3_out;
  Main_incrPC  instR18 (zi69, main_incrpc_outR1);
  assign zi70 = main_incrpc_outR1;
  assign zi71 = {20'h10000, zi70};
  assign zi72 = zi71[69:0];
  Main_getPC  instR19 (zi72, main_getpc_outR1);
  assign zi73 = main_getpc_outR1;
  assign zi74 = zi73[75:70];
  assign zi75 = zi73[69:0];
  ZLL_Main_finishInstr1  instR20 (zi74, zi75, zll_main_finishinstr1_outR1);
  assign zi76 = zll_main_finishinstr1_outR1;
  assign zi77 = {20'h10000, zi76};
  assign zi78 = zi77[69:0];
  Main_getOut  instR21 (zi78, main_getout_outR5);
  assign zi79 = main_getout_outR5;
  assign zi80 = zi79[84:70];
  assign zi81 = zi79[69:0];
  assign zi82 = {5'h1, zi80, zi81};
  assign zi83 = zi82[84:70];
  assign zi84 = zi82[69:0];
  assign zi85 = zi7[75:70];
  Main_getReg  instR22 (zi9, main_getreg_outR1);
  assign zi86 = main_getreg_outR1;
  assign zi87 = zi86[77:70];
  assign zi88 = zi86[69:0];
  assign zi89 = zi87 == 8'h0;
  Main_putPC1  instR23 (zi85, zi88, main_putpc1_out);
  Main_incrPC  instR24 (zi88, main_incrpc_outR2);
  assign zi90 = (zi89 == 1'h0) ? main_putpc1_out : main_incrpc_outR2;
  assign zi91 = {20'h10000, zi90};
  assign zi92 = zi91[69:0];
  Main_getPC  instR25 (zi92, main_getpc_outR2);
  assign zi93 = main_getpc_outR2;
  assign zi94 = zi93[75:70];
  assign zi95 = zi93[69:0];
  ZLL_Main_finishInstr1  instR26 (zi94, zi95, zll_main_finishinstr1_outR2);
  assign zi96 = zll_main_finishinstr1_outR2;
  assign zi97 = {20'h10000, zi96};
  assign zi98 = zi97[69:0];
  Main_getOut  instR27 (zi98, main_getout_outR6);
  assign zi99 = main_getout_outR6;
  assign zi100 = zi99[84:70];
  assign zi101 = zi99[69:0];
  assign zi102 = {5'h1, zi100, zi101};
  assign zi103 = zi102[84:70];
  assign zi104 = zi102[69:0];
  assign res = (zi7[78:76] == 3'h0) ? {1'h1, zi23, 4'h2, zi24} : ((zi7[78:76] == 3'h1) ? {1'h1, zi34, 4'h3, zi35} : ((zi7[78:76] == 3'h2) ? {1'h1, zi57, 4'h1, zi58} : ((zi7[78:76] == 3'h3) ? {1'h1, zi83, 4'h8, zi84} : {1'h1, zi103, 4'h6, zi104})));
endmodule

module ZLL_Main_getReg3 (input logic [1:0] arg0,
  input logic [69:0] arg1,
  output logic [77:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  assign zi0 = arg1[69:62];
  assign zi1 = arg1[61:54];
  assign zi2 = arg1[53:46];
  assign zi3 = arg1[45:38];
  assign res = (arg0 == 2'h0) ? {zi0, arg1} : ((arg0 == 2'h1) ? {zi1, arg1} : ((arg0 == 2'h2) ? {zi2, arg1} : {zi3, arg1}));
endmodule

module ZLL_Main_putWeOut (input logic [0:0] arg0,
  input logic [14:0] arg1,
  input logic [69:0] arg2,
  output logic [69:0] res);
  logic [5:0] addrout;
  logic [7:0] dataout;
  logic [69:0] main_putout_out;
  assign addrout = arg1[13:8];
  assign dataout = arg1[7:0];
  Main_putOut  inst ({arg0, addrout, dataout}, arg2, main_putout_out);
  assign res = main_putout_out;
endmodule

module Main_getIns (input logic [69:0] arg0,
  output logic [86:0] res);
  logic [16:0] zi0;
  assign zi0 = arg0[31:15];
  assign res = {zi0, arg0};
endmodule

module Main_getOut (input logic [69:0] arg0,
  output logic [84:0] res);
  logic [14:0] zi0;
  assign zi0 = arg0[14:0];
  assign res = {zi0, arg0};
endmodule

module ZLL_Main_ld (input logic [69:0] arg0,
  output logic [69:0] res);
  logic [84:0] main_getout_out;
  logic [84:0] zi0;
  logic [14:0] zi1;
  logic [69:0] zi2;
  logic [69:0] zll_main_putweout_out;
  Main_getOut  inst (arg0, main_getout_out);
  assign zi0 = main_getout_out;
  assign zi1 = zi0[84:70];
  assign zi2 = zi0[69:0];
  ZLL_Main_putWeOut  instR1 (1'h0, zi1, zi2, zll_main_putweout_out);
  assign res = zll_main_putweout_out;
endmodule

module Main_putAddrOut (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [84:0] main_getout_out;
  logic [84:0] zt0;
  logic [69:0] st1;
  logic [0:0] zi0;
  logic [7:0] zi1;
  logic [69:0] main_putout_out;
  Main_getOut  inst (arg1, main_getout_out);
  assign zt0 = main_getout_out;
  assign st1 = zt0[69:0];
  assign zi0 = zt0[84];
  assign zi1 = zt0[77:70];
  Main_putOut  instR1 ({zi0, arg0, zi1}, st1, main_putout_out);
  assign res = main_putout_out;
endmodule

module ZLL_Main_putReg3 (input logic [7:0] arg0,
  input logic [1:0] arg1,
  input logic [69:0] arg2,
  output logic [69:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [5:0] zi3;
  logic [16:0] zi4;
  logic [14:0] zi5;
  logic [7:0] zi6;
  logic [7:0] zi7;
  logic [7:0] zi8;
  logic [5:0] zi9;
  logic [16:0] zi10;
  logic [14:0] zi11;
  logic [7:0] zi12;
  logic [7:0] zi13;
  logic [7:0] zi14;
  logic [5:0] zi15;
  logic [16:0] zi16;
  logic [14:0] zi17;
  logic [7:0] zi18;
  logic [7:0] zi19;
  logic [7:0] zi20;
  logic [5:0] zi21;
  logic [16:0] zi22;
  logic [14:0] zi23;
  assign zi0 = arg2[61:54];
  assign zi1 = arg2[53:46];
  assign zi2 = arg2[45:38];
  assign zi3 = arg2[37:32];
  assign zi4 = arg2[31:15];
  assign zi5 = arg2[14:0];
  assign zi6 = arg2[69:62];
  assign zi7 = arg2[53:46];
  assign zi8 = arg2[45:38];
  assign zi9 = arg2[37:32];
  assign zi10 = arg2[31:15];
  assign zi11 = arg2[14:0];
  assign zi12 = arg2[69:62];
  assign zi13 = arg2[61:54];
  assign zi14 = arg2[45:38];
  assign zi15 = arg2[37:32];
  assign zi16 = arg2[31:15];
  assign zi17 = arg2[14:0];
  assign zi18 = arg2[69:62];
  assign zi19 = arg2[61:54];
  assign zi20 = arg2[53:46];
  assign zi21 = arg2[37:32];
  assign zi22 = arg2[31:15];
  assign zi23 = arg2[14:0];
  assign res = (arg1 == 2'h0) ? {arg0, zi0, zi1, zi2, zi3, zi4, zi5} : ((arg1 == 2'h1) ? {zi6, arg0, zi7, zi8, zi9, zi10, zi11} : ((arg1 == 2'h2) ? {zi12, zi13, arg0, zi14, zi15, zi16, zi17} : {zi18, zi19, zi20, arg0, zi21, zi22, zi23}));
endmodule

module ZLL_Main_finishInstr1 (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [69:0] main_putaddrout_out;
  logic [69:0] zt0;
  logic [69:0] zll_main_ld_out;
  Main_putAddrOut  inst (arg0, arg1, main_putaddrout_out);
  assign zt0 = main_putaddrout_out;
  ZLL_Main_ld  instR1 (zt0, zll_main_ld_out);
  assign res = zll_main_ld_out;
endmodule

module Main_incrPC (input logic [69:0] arg0,
  output logic [69:0] res);
  logic [75:0] main_getpc_out;
  logic [75:0] zt0;
  logic [5:0] st0;
  logic [69:0] st1;
  logic [69:0] main_putpc1_out;
  Main_getPC  inst (arg0, main_getpc_out);
  assign zt0 = main_getpc_out;
  assign st0 = zt0[75:70];
  assign st1 = zt0[69:0];
  Main_putPC1  instR1 (st0 + 6'h1, st1, main_putpc1_out);
  assign res = main_putpc1_out;
endmodule

module Main_getReg (input logic [69:0] arg0,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg3_out;
  ZLL_Main_getReg3  inst (2'h0, arg0, zll_main_getreg3_out);
  assign res = zll_main_getreg3_out;
endmodule

module Main_getPC (input logic [69:0] arg0,
  output logic [75:0] res);
  logic [5:0] zi0;
  assign zi0 = arg0[37:32];
  assign res = {zi0, arg0};
endmodule

module Main_putOut (input logic [14:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [5:0] zi4;
  logic [16:0] zi5;
  assign zi0 = arg1[69:62];
  assign zi1 = arg1[61:54];
  assign zi2 = arg1[53:46];
  assign zi3 = arg1[45:38];
  assign zi4 = arg1[37:32];
  assign zi5 = arg1[31:15];
  assign res = {zi0, zi1, zi2, zi3, zi4, zi5, arg0};
endmodule