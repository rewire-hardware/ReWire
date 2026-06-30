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
  logic [89:0] zll_main_loop31_outR1;
  logic [89:0] zll_main_loop31_outR2;
  logic [69:0] main_putins_outR1;
  logic [69:0] zi24;
  logic [89:0] zi25;
  logic [69:0] zi26;
  logic [75:0] main_getpc_outR1;
  logic [75:0] zi27;
  logic [5:0] zi28;
  logic [69:0] zi29;
  logic [69:0] zll_main_finishinstr1_outR1;
  logic [69:0] zi30;
  logic [89:0] zi31;
  logic [69:0] zi32;
  logic [84:0] main_getout_outR1;
  logic [84:0] zi33;
  logic [14:0] zi34;
  logic [69:0] zi35;
  logic [89:0] zi36;
  logic [14:0] zi37;
  logic [69:0] zi38;
  logic [69:0] main_putins_outR2;
  logic [69:0] zi40;
  logic [89:0] zi41;
  logic [69:0] zi42;
  logic [86:0] main_getins_out;
  logic [86:0] zi43;
  logic [69:0] zi45;
  logic [7:0] zi46;
  logic [77:0] zi47;
  logic [7:0] zi48;
  logic [69:0] zi49;
  logic [69:0] zll_main_putreg3_out;
  logic [69:0] zi50;
  logic [89:0] zi51;
  logic [69:0] zi52;
  logic [89:0] zll_main_reset6_out;
  logic [69:0] main_putins_outR3;
  logic [69:0] zi54;
  logic [89:0] zi55;
  logic [69:0] zi56;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi57;
  logic [89:0] zi58;
  logic [69:0] zi59;
  logic [75:0] main_getpc_outR2;
  logic [75:0] zi60;
  logic [5:0] zi61;
  logic [69:0] zi62;
  logic [69:0] zll_main_finishinstr1_outR2;
  logic [69:0] zi63;
  logic [89:0] zi64;
  logic [69:0] zi65;
  logic [84:0] main_getout_outR2;
  logic [84:0] zi66;
  logic [14:0] zi67;
  logic [69:0] zi68;
  logic [89:0] zi69;
  logic [14:0] zi70;
  logic [69:0] zi71;
  logic [89:0] zll_main_loop31_outR3;
  logic [89:0] zll_main_loop31_outR4;
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
  ZLL_Main_loop31  instR6 (__in0, __st0, zll_main_loop31_outR1);
  ZLL_Main_loop31  instR7 (__in0, __st0, zll_main_loop31_outR2);
  Main_putIns  instR8 (__in0, __st0, main_putins_outR1);
  assign zi24 = main_putins_outR1;
  assign zi25 = {20'h10000, zi24};
  assign zi26 = zi25[69:0];
  Main_getPC  instR9 (zi26, main_getpc_outR1);
  assign zi27 = main_getpc_outR1;
  assign zi28 = zi27[75:70];
  assign zi29 = zi27[69:0];
  ZLL_Main_finishInstr1  instR10 (zi28, zi29, zll_main_finishinstr1_outR1);
  assign zi30 = zll_main_finishinstr1_outR1;
  assign zi31 = {20'h10000, zi30};
  assign zi32 = zi31[69:0];
  Main_getOut  instR11 (zi32, main_getout_outR1);
  assign zi33 = main_getout_outR1;
  assign zi34 = zi33[84:70];
  assign zi35 = zi33[69:0];
  assign zi36 = {5'h1, zi34, zi35};
  assign zi37 = zi36[84:70];
  assign zi38 = zi36[69:0];
  Main_putIns  instR12 (__in0, __st0, main_putins_outR2);
  assign zi40 = main_putins_outR2;
  assign zi41 = {20'h10000, zi40};
  assign zi42 = zi41[69:0];
  Main_getIns  instR13 (zi42, main_getins_out);
  assign zi43 = main_getins_out;
  assign zi45 = zi43[69:0];
  assign zi46 = zi43[77:70];
  assign zi47 = {zi46, zi45};
  assign zi48 = zi47[77:70];
  assign zi49 = zi47[69:0];
  ZLL_Main_putReg3  instR14 (zi48, 2'h0, {2'h0, zi48}, zi49, zll_main_putreg3_out);
  assign zi50 = zll_main_putreg3_out;
  assign zi51 = {20'h10000, zi50};
  assign zi52 = zi51[69:0];
  ZLL_Main_reset6  instR15 (zi52, zll_main_reset6_out);
  Main_putIns  instR16 (__in0, __st0, main_putins_outR3);
  assign zi54 = main_putins_outR3;
  assign zi55 = {20'h10000, zi54};
  assign zi56 = zi55[69:0];
  Main_incrPC  instR17 (zi56, main_incrpc_outR1);
  assign zi57 = main_incrpc_outR1;
  assign zi58 = {20'h10000, zi57};
  assign zi59 = zi58[69:0];
  Main_getPC  instR18 (zi59, main_getpc_outR2);
  assign zi60 = main_getpc_outR2;
  assign zi61 = zi60[75:70];
  assign zi62 = zi60[69:0];
  ZLL_Main_finishInstr1  instR19 (zi61, zi62, zll_main_finishinstr1_outR2);
  assign zi63 = zll_main_finishinstr1_outR2;
  assign zi64 = {20'h10000, zi63};
  assign zi65 = zi64[69:0];
  Main_getOut  instR20 (zi65, main_getout_outR2);
  assign zi66 = main_getout_outR2;
  assign zi67 = zi66[84:70];
  assign zi68 = zi66[69:0];
  assign zi69 = {5'h1, zi67, zi68};
  assign zi70 = zi69[84:70];
  assign zi71 = zi69[69:0];
  ZLL_Main_loop31  instR21 (__in0, __st0, zll_main_loop31_outR3);
  ZLL_Main_loop31  instR22 (__in0, __st0, zll_main_loop31_outR4);
  assign zres = (__resumption_tag == 4'h1) ? {1'h1, zi18, 4'h6, zi19} : ((__resumption_tag == 4'h2) ? zll_main_loop31_out : ((__resumption_tag == 4'h3) ? zll_main_loop31_outR1 : ((__resumption_tag == 4'h4) ? zll_main_loop31_outR2 : ((__resumption_tag == 4'h5) ? {1'h1, zi37, 4'h3, zi38} : ((__resumption_tag == 4'h6) ? zll_main_reset6_out : ((__resumption_tag == 4'h7) ? {1'h1, zi70, 4'h4, zi71} : ((__resumption_tag == 4'h8) ? zll_main_loop31_outR3 : zll_main_loop31_outR4)))))));
  assign __resumption_tag_next = zres[73:70];
  assign __st0_next = zres[69:0];
  assign __out0 = zres[88:74];
  initial {__resumption_tag, __st0} = {4'h5, {7'h46{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {4'h5, {7'h46{1'h0}}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_Main_loop31 (input logic [16:0] arg0,
  input logic [69:0] arg1,
  output logic [89:0] res);
  logic [69:0] main_putins_out;
  logic [69:0] zt0;
  logic [89:0] zt1;
  logic [69:0] s0;
  logic [89:0] zll_main_reset6_out;
  Main_putIns  inst (arg0, arg1, main_putins_out);
  assign zt0 = main_putins_out;
  assign zt1 = {20'h10000, zt0};
  assign s0 = zt1[69:0];
  ZLL_Main_reset6  instR1 (s0, zll_main_reset6_out);
  assign res = zll_main_reset6_out;
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

module ZLL_Main_reset6 (input logic [69:0] arg0,
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
  logic [69:0] zll_main_finishinstr_out;
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
  logic [69:0] zll_main_putweout3_out;
  logic [69:0] zi47;
  logic [89:0] zi48;
  logic [69:0] zi49;
  logic [84:0] main_getout_outR3;
  logic [84:0] zi50;
  logic [14:0] zi51;
  logic [69:0] zi52;
  logic [89:0] zi53;
  logic [14:0] zi54;
  logic [69:0] zi55;
  logic [1:0] zi56;
  logic [1:0] zi57;
  logic [1:0] zi58;
  logic [77:0] main_getreg1_out;
  logic [77:0] zi59;
  logic [7:0] zi60;
  logic [69:0] zi61;
  logic [77:0] main_getreg1_outR1;
  logic [77:0] zi62;
  logic [7:0] zi63;
  logic [69:0] zi64;
  logic [7:0] zi65;
  logic [69:0] zll_main_putreg3_out;
  logic [69:0] zi66;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi67;
  logic [89:0] zi68;
  logic [69:0] zi69;
  logic [75:0] main_getpc_outR1;
  logic [75:0] zi70;
  logic [5:0] zi71;
  logic [69:0] zi72;
  logic [69:0] zll_main_finishinstr1_outR1;
  logic [69:0] zi73;
  logic [89:0] zi74;
  logic [69:0] zi75;
  logic [84:0] main_getout_outR4;
  logic [84:0] zi76;
  logic [14:0] zi77;
  logic [69:0] zi78;
  logic [89:0] zi79;
  logic [14:0] zi80;
  logic [69:0] zi81;
  logic [5:0] zi82;
  logic [77:0] main_getreg_outR1;
  logic [77:0] zi83;
  logic [7:0] zi84;
  logic [69:0] zi85;
  logic [0:0] zi86;
  logic [69:0] main_incrpc_outR2;
  logic [69:0] main_putpc1_out;
  logic [69:0] zi88;
  logic [89:0] zi89;
  logic [69:0] zi90;
  logic [75:0] main_getpc_outR2;
  logic [75:0] zi91;
  logic [5:0] zi92;
  logic [69:0] zi93;
  logic [69:0] zll_main_finishinstr1_outR2;
  logic [69:0] zi94;
  logic [89:0] zi95;
  logic [69:0] zi96;
  logic [84:0] main_getout_outR5;
  logic [84:0] zi97;
  logic [14:0] zi98;
  logic [69:0] zi99;
  logic [89:0] zi100;
  logic [14:0] zi101;
  logic [69:0] zi102;
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
  ZLL_Main_finishInstr  instR6 (zi26, zll_main_finishinstr_out);
  assign zi27 = zll_main_finishinstr_out;
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
  ZLL_Main_putWeOut3  instR12 (1'h1, zi46, zll_main_putweout3_out);
  assign zi47 = zll_main_putweout3_out;
  assign zi48 = {20'h10000, zi47};
  assign zi49 = zi48[69:0];
  Main_getOut  instR13 (zi49, main_getout_outR3);
  assign zi50 = main_getout_outR3;
  assign zi51 = zi50[84:70];
  assign zi52 = zi50[69:0];
  assign zi53 = {5'h1, zi51, zi52};
  assign zi54 = zi53[84:70];
  assign zi55 = zi53[69:0];
  assign zi56 = zi7[75:74];
  assign zi57 = zi7[73:72];
  assign zi58 = zi7[71:70];
  Main_getReg1  instR14 (zi57, zi9, main_getreg1_out);
  assign zi59 = main_getreg1_out;
  assign zi60 = zi59[77:70];
  assign zi61 = zi59[69:0];
  Main_getReg1  instR15 (zi58, zi61, main_getreg1_outR1);
  assign zi62 = main_getreg1_outR1;
  assign zi63 = zi62[77:70];
  assign zi64 = zi62[69:0];
  assign zi65 = ~(zi60 & zi63);
  ZLL_Main_putReg3  instR16 (zi65, zi56, {zi56, zi65}, zi64, zll_main_putreg3_out);
  assign zi66 = zll_main_putreg3_out;
  Main_incrPC  instR17 (zi66, main_incrpc_outR1);
  assign zi67 = main_incrpc_outR1;
  assign zi68 = {20'h10000, zi67};
  assign zi69 = zi68[69:0];
  Main_getPC  instR18 (zi69, main_getpc_outR1);
  assign zi70 = main_getpc_outR1;
  assign zi71 = zi70[75:70];
  assign zi72 = zi70[69:0];
  ZLL_Main_finishInstr1  instR19 (zi71, zi72, zll_main_finishinstr1_outR1);
  assign zi73 = zll_main_finishinstr1_outR1;
  assign zi74 = {20'h10000, zi73};
  assign zi75 = zi74[69:0];
  Main_getOut  instR20 (zi75, main_getout_outR4);
  assign zi76 = main_getout_outR4;
  assign zi77 = zi76[84:70];
  assign zi78 = zi76[69:0];
  assign zi79 = {5'h1, zi77, zi78};
  assign zi80 = zi79[84:70];
  assign zi81 = zi79[69:0];
  assign zi82 = zi7[75:70];
  Main_getReg  instR21 (zi9, main_getreg_outR1);
  assign zi83 = main_getreg_outR1;
  assign zi84 = zi83[77:70];
  assign zi85 = zi83[69:0];
  assign zi86 = zi84 == 8'h0;
  Main_incrPC  instR22 (zi85, main_incrpc_outR2);
  Main_putPC1  instR23 (zi82, zi85, main_putpc1_out);
  assign zi88 = (zi86 == 1'h1) ? main_incrpc_outR2 : main_putpc1_out;
  assign zi89 = {20'h10000, zi88};
  assign zi90 = zi89[69:0];
  Main_getPC  instR24 (zi90, main_getpc_outR2);
  assign zi91 = main_getpc_outR2;
  assign zi92 = zi91[75:70];
  assign zi93 = zi91[69:0];
  ZLL_Main_finishInstr1  instR25 (zi92, zi93, zll_main_finishinstr1_outR2);
  assign zi94 = zll_main_finishinstr1_outR2;
  assign zi95 = {20'h10000, zi94};
  assign zi96 = zi95[69:0];
  Main_getOut  instR26 (zi96, main_getout_outR5);
  assign zi97 = main_getout_outR5;
  assign zi98 = zi97[84:70];
  assign zi99 = zi97[69:0];
  assign zi100 = {5'h1, zi98, zi99};
  assign zi101 = zi100[84:70];
  assign zi102 = zi100[69:0];
  assign res = (zi7[78:76] == 3'h0) ? {1'h1, zi23, 4'h2, zi24} : ((zi7[78:76] == 3'h1) ? {1'h1, zi34, 4'h1, zi35} : ((zi7[78:76] == 3'h2) ? {1'h1, zi54, 4'h7, zi55} : ((zi7[78:76] == 3'h3) ? {1'h1, zi80, 4'h8, zi81} : {1'h1, zi101, 4'h0, zi102})));
endmodule

module ZLL_Main_finishInstr1 (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [69:0] main_putaddrout_out;
  logic [69:0] zt0;
  logic [69:0] zll_main_finishinstr_out;
  Main_putAddrOut  inst (arg0, arg1, main_putaddrout_out);
  assign zt0 = main_putaddrout_out;
  ZLL_Main_finishInstr  instR1 (zt0, zll_main_finishinstr_out);
  assign res = zll_main_finishinstr_out;
endmodule

module ZLL_Main_getReg5 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [69:0] arg2,
  output logic [77:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  assign zi0 = arg2[69:62];
  assign zi1 = arg2[61:54];
  assign zi2 = arg2[53:46];
  assign zi3 = arg2[45:38];
  assign res = (arg1 == 2'h0) ? {zi0, arg2} : ((arg0 == 2'h1) ? {zi1, arg2} : ((arg0 == 2'h2) ? {zi2, arg2} : {zi3, arg2}));
endmodule

module Main_getReg1 (input logic [1:0] arg0,
  input logic [69:0] arg1,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg5_out;
  ZLL_Main_getReg5  inst (arg0, arg0, arg1, zll_main_getreg5_out);
  assign res = zll_main_getreg5_out;
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

module ZLL_Main_putWeOut3 (input logic [0:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [84:0] main_getout_out;
  logic [84:0] zt0;
  logic [69:0] st1;
  logic [5:0] zi0;
  logic [7:0] zi1;
  logic [69:0] main_putout_out;
  Main_getOut  inst (arg1, main_getout_out);
  assign zt0 = main_getout_out;
  assign st1 = zt0[69:0];
  assign zi0 = zt0[83:78];
  assign zi1 = zt0[77:70];
  Main_putOut  instR1 ({arg0, zi0, zi1}, st1, main_putout_out);
  assign res = main_putout_out;
endmodule

module ZLL_Main_finishInstr (input logic [69:0] arg0,
  output logic [69:0] res);
  logic [69:0] zll_main_putweout3_out;
  ZLL_Main_putWeOut3  inst (1'h0, arg0, zll_main_putweout3_out);
  assign res = zll_main_putweout3_out;
endmodule

module Main_putAddrOut (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [69:0] res);
  logic [84:0] main_getout_out;
  logic [84:0] zi0;
  logic [69:0] zi2;
  logic [0:0] zi3;
  logic [7:0] zi4;
  logic [69:0] main_putout_out;
  Main_getOut  inst (arg1, main_getout_out);
  assign zi0 = main_getout_out;
  assign zi2 = zi0[69:0];
  assign zi3 = zi0[84];
  assign zi4 = zi0[77:70];
  Main_putOut  instR1 ({zi3, arg0, zi4}, zi2, main_putout_out);
  assign res = main_putout_out;
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
  logic [77:0] zll_main_getreg5_out;
  ZLL_Main_getReg5  inst (2'h0, 2'h0, arg0, zll_main_getreg5_out);
  assign res = zll_main_getreg5_out;
endmodule

module ZLL_Main_putReg3 (input logic [7:0] arg0,
  input logic [1:0] arg1,
  input logic [9:0] arg2,
  input logic [69:0] arg3,
  output logic [69:0] res);
  logic [7:0] b;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [5:0] zi3;
  logic [16:0] zi4;
  logic [14:0] zi5;
  logic [7:0] zi8;
  logic [7:0] zi9;
  logic [7:0] zi10;
  logic [5:0] zi11;
  logic [16:0] zi12;
  logic [14:0] zi13;
  logic [7:0] zi16;
  logic [7:0] zi17;
  logic [7:0] zi18;
  logic [5:0] zi19;
  logic [16:0] zi20;
  logic [14:0] zi21;
  logic [7:0] zi24;
  logic [7:0] zi25;
  logic [7:0] zi26;
  logic [5:0] zi27;
  logic [16:0] zi28;
  logic [14:0] zi29;
  assign b = arg2[7:0];
  assign zi0 = arg3[61:54];
  assign zi1 = arg3[53:46];
  assign zi2 = arg3[45:38];
  assign zi3 = arg3[37:32];
  assign zi4 = arg3[31:15];
  assign zi5 = arg3[14:0];
  assign zi8 = arg3[69:62];
  assign zi9 = arg3[53:46];
  assign zi10 = arg3[45:38];
  assign zi11 = arg3[37:32];
  assign zi12 = arg3[31:15];
  assign zi13 = arg3[14:0];
  assign zi16 = arg3[69:62];
  assign zi17 = arg3[61:54];
  assign zi18 = arg3[45:38];
  assign zi19 = arg3[37:32];
  assign zi20 = arg3[31:15];
  assign zi21 = arg3[14:0];
  assign zi24 = arg3[69:62];
  assign zi25 = arg3[61:54];
  assign zi26 = arg3[53:46];
  assign zi27 = arg3[37:32];
  assign zi28 = arg3[31:15];
  assign zi29 = arg3[14:0];
  assign res = (arg2[9:8] == 2'h0) ? {b, zi0, zi1, zi2, zi3, zi4, zi5} : ((arg1 == 2'h1) ? {zi8, arg0, zi9, zi10, zi11, zi12, zi13} : ((arg1 == 2'h2) ? {zi16, zi17, arg0, zi18, zi19, zi20, zi21} : {zi24, zi25, zi26, arg0, zi27, zi28, zi29}));
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