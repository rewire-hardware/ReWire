module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [16:0] __in0,
  output logic [14:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [69:0] __st0;
  logic [69:0] __st0_next;
  logic [69:0] main_putins_out;
  logic [69:0] zi2;
  logic [88:0] zi3;
  logic [69:0] zi4;
  logic [69:0] main_incrpc_out;
  logic [69:0] zi5;
  logic [88:0] zi6;
  logic [69:0] zi7;
  logic [88:0] zll_main_loop11_out;
  logic [69:0] main_putins_outR1;
  logic [69:0] zi9;
  logic [88:0] zi10;
  logic [69:0] zi11;
  logic [88:0] zll_main_loop13_out;
  logic [69:0] main_putins_outR2;
  logic [69:0] zi13;
  logic [88:0] zi14;
  logic [69:0] zi15;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi16;
  logic [88:0] zi17;
  logic [69:0] zi18;
  logic [75:0] main_getpc_out;
  logic [75:0] zi19;
  logic [5:0] zi20;
  logic [69:0] zi21;
  logic [69:0] zll_main_finishinstr1_out;
  logic [69:0] zi22;
  logic [88:0] zi23;
  logic [69:0] zi24;
  logic [84:0] main_getout_out;
  logic [84:0] zi25;
  logic [14:0] zi26;
  logic [69:0] zi27;
  logic [88:0] zi28;
  logic [14:0] zi29;
  logic [69:0] zi30;
  logic [69:0] main_putins_outR3;
  logic [69:0] zi32;
  logic [88:0] zi33;
  logic [69:0] zi34;
  logic [86:0] main_getins_out;
  logic [86:0] zi35;
  logic [69:0] zi37;
  logic [7:0] zi38;
  logic [77:0] zi39;
  logic [7:0] zi40;
  logic [69:0] zi41;
  logic [69:0] zll_main_putreg3_out;
  logic [69:0] zi42;
  logic [88:0] zi43;
  logic [69:0] zi44;
  logic [88:0] zll_main_loop13_outR1;
  logic [69:0] main_putins_outR4;
  logic [69:0] zi46;
  logic [88:0] zi47;
  logic [69:0] zi48;
  logic [88:0] zll_main_loop11_outR1;
  logic [88:0] zres;
  Main_putIns  inst (__in0, __st0, main_putins_out);
  assign zi2 = main_putins_out;
  assign zi3 = {19'h10000, zi2};
  assign zi4 = zi3[69:0];
  Main_incrPC  instR1 (zi4, main_incrpc_out);
  assign zi5 = main_incrpc_out;
  assign zi6 = {19'h10000, zi5};
  assign zi7 = zi6[69:0];
  ZLL_Main_loop11  instR2 (zi7, zll_main_loop11_out);
  Main_putIns  instR3 (__in0, __st0, main_putins_outR1);
  assign zi9 = main_putins_outR1;
  assign zi10 = {19'h10000, zi9};
  assign zi11 = zi10[69:0];
  ZLL_Main_loop13  instR4 (zi11, zll_main_loop13_out);
  Main_putIns  instR5 (__in0, __st0, main_putins_outR2);
  assign zi13 = main_putins_outR2;
  assign zi14 = {19'h10000, zi13};
  assign zi15 = zi14[69:0];
  Main_incrPC  instR6 (zi15, main_incrpc_outR1);
  assign zi16 = main_incrpc_outR1;
  assign zi17 = {19'h10000, zi16};
  assign zi18 = zi17[69:0];
  Main_getPC  instR7 (zi18, main_getpc_out);
  assign zi19 = main_getpc_out;
  assign zi20 = zi19[75:70];
  assign zi21 = zi19[69:0];
  ZLL_Main_finishInstr1  instR8 (zi20, zi21, zll_main_finishinstr1_out);
  assign zi22 = zll_main_finishinstr1_out;
  assign zi23 = {19'h10000, zi22};
  assign zi24 = zi23[69:0];
  Main_getOut  instR9 (zi24, main_getout_out);
  assign zi25 = main_getout_out;
  assign zi26 = zi25[84:70];
  assign zi27 = zi25[69:0];
  assign zi28 = {4'h1, zi26, zi27};
  assign zi29 = zi28[84:70];
  assign zi30 = zi28[69:0];
  Main_putIns  instR10 (__in0, __st0, main_putins_outR3);
  assign zi32 = main_putins_outR3;
  assign zi33 = {19'h10000, zi32};
  assign zi34 = zi33[69:0];
  Main_getIns  instR11 (zi34, main_getins_out);
  assign zi35 = main_getins_out;
  assign zi37 = zi35[69:0];
  assign zi38 = zi35[77:70];
  assign zi39 = {zi38, zi37};
  assign zi40 = zi39[77:70];
  assign zi41 = zi39[69:0];
  ZLL_Main_putReg3  instR12 (zi40, 2'h0, zi41, zll_main_putreg3_out);
  assign zi42 = zll_main_putreg3_out;
  assign zi43 = {19'h10000, zi42};
  assign zi44 = zi43[69:0];
  ZLL_Main_loop13  instR13 (zi44, zll_main_loop13_outR1);
  Main_putIns  instR14 (__in0, __st0, main_putins_outR4);
  assign zi46 = main_putins_outR4;
  assign zi47 = {19'h10000, zi46};
  assign zi48 = zi47[69:0];
  ZLL_Main_loop11  instR15 (zi48, zll_main_loop11_outR1);
  assign zres = (__resumption_tag == 3'h1) ? zll_main_loop11_out : ((__resumption_tag == 3'h2) ? zll_main_loop13_out : ((__resumption_tag == 3'h3) ? {1'h1, zi29, 3'h4, zi30} : ((__resumption_tag == 3'h4) ? zll_main_loop13_outR1 : zll_main_loop11_outR1)));
  assign __resumption_tag_next = zres[72:70];
  assign __st0_next = zres[69:0];
  assign __out0 = zres[87:73];
  initial {__resumption_tag, __st0} = {7'h49{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {7'h49{1'h0}};
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

module Main_getReg1 (input logic [1:0] arg0,
  input logic [69:0] arg1,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg3_out;
  ZLL_Main_getReg3  inst (arg0, arg1, zll_main_getreg3_out);
  assign res = zll_main_getreg3_out;
endmodule

module ZLL_Main_loop13 (input logic [69:0] arg0,
  output logic [88:0] res);
  logic [86:0] main_getins_out;
  logic [86:0] zi0;
  logic [69:0] zi2;
  logic [8:0] zi3;
  logic [78:0] zi4;
  logic [8:0] zi5;
  logic [69:0] zi6;
  logic [88:0] zi7;
  logic [69:0] zi9;
  logic [69:0] main_incrpc_out;
  logic [69:0] zi10;
  logic [88:0] zi11;
  logic [69:0] zi12;
  logic [88:0] zll_main_loop11_out;
  logic [5:0] zi13;
  logic [69:0] main_putaddrout_out;
  logic [69:0] zi14;
  logic [69:0] zll_main_finishinstr_out;
  logic [69:0] zi15;
  logic [88:0] zi16;
  logic [69:0] zi17;
  logic [84:0] main_getout_out;
  logic [84:0] zi18;
  logic [14:0] zi19;
  logic [69:0] zi20;
  logic [88:0] zi21;
  logic [14:0] zi22;
  logic [69:0] zi23;
  logic [5:0] zi24;
  logic [77:0] main_getreg_out;
  logic [77:0] zi25;
  logic [7:0] zi26;
  logic [69:0] zi27;
  logic [69:0] main_putaddrout_outR1;
  logic [69:0] zi28;
  logic [84:0] main_getout_outR1;
  logic [84:0] zi29;
  logic [69:0] zi31;
  logic [0:0] zi32;
  logic [5:0] zi33;
  logic [69:0] main_putout_out;
  logic [69:0] zi34;
  logic [84:0] main_getout_outR2;
  logic [84:0] zi35;
  logic [14:0] zi36;
  logic [69:0] zi37;
  logic [69:0] zll_main_putweout_out;
  logic [69:0] zi38;
  logic [88:0] zi39;
  logic [69:0] zi40;
  logic [84:0] main_getout_outR3;
  logic [84:0] zi41;
  logic [14:0] zi42;
  logic [69:0] zi43;
  logic [88:0] zi44;
  logic [14:0] zi45;
  logic [69:0] zi46;
  logic [1:0] zi47;
  logic [1:0] zi48;
  logic [1:0] zi49;
  logic [77:0] main_getreg1_out;
  logic [77:0] zi50;
  logic [7:0] zi51;
  logic [69:0] zi52;
  logic [77:0] main_getreg1_outR1;
  logic [77:0] zi53;
  logic [7:0] zi54;
  logic [69:0] zi55;
  logic [7:0] zi56;
  logic [69:0] zll_main_putreg3_out;
  logic [69:0] zi57;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi58;
  logic [88:0] zi59;
  logic [69:0] zi60;
  logic [88:0] zll_main_loop11_outR1;
  logic [5:0] zi61;
  logic [77:0] main_getreg_outR1;
  logic [77:0] zi62;
  logic [7:0] zi63;
  logic [69:0] zi64;
  logic [0:0] zi65;
  logic [69:0] main_putpc1_out;
  logic [69:0] main_incrpc_outR2;
  logic [69:0] zi66;
  logic [88:0] zi67;
  logic [69:0] zi68;
  logic [88:0] zll_main_loop11_outR2;
  Main_getIns  inst (arg0, main_getins_out);
  assign zi0 = main_getins_out;
  assign zi2 = zi0[69:0];
  assign zi3 = zi0[86:78];
  assign zi4 = {zi3, zi2};
  assign zi5 = zi4[78:70];
  assign zi6 = zi4[69:0];
  assign zi7 = {10'h0, zi5, zi6};
  assign zi9 = zi7[69:0];
  Main_incrPC  instR1 (zi9, main_incrpc_out);
  assign zi10 = main_incrpc_out;
  assign zi11 = {19'h10000, zi10};
  assign zi12 = zi11[69:0];
  ZLL_Main_loop11  instR2 (zi12, zll_main_loop11_out);
  assign zi13 = zi7[75:70];
  Main_putAddrOut  instR3 (zi13, zi9, main_putaddrout_out);
  assign zi14 = main_putaddrout_out;
  ZLL_Main_finishInstr  instR4 (zi14, zll_main_finishinstr_out);
  assign zi15 = zll_main_finishinstr_out;
  assign zi16 = {19'h10000, zi15};
  assign zi17 = zi16[69:0];
  Main_getOut  instR5 (zi17, main_getout_out);
  assign zi18 = main_getout_out;
  assign zi19 = zi18[84:70];
  assign zi20 = zi18[69:0];
  assign zi21 = {4'h1, zi19, zi20};
  assign zi22 = zi21[84:70];
  assign zi23 = zi21[69:0];
  assign zi24 = zi7[75:70];
  Main_getReg  instR6 (zi9, main_getreg_out);
  assign zi25 = main_getreg_out;
  assign zi26 = zi25[77:70];
  assign zi27 = zi25[69:0];
  Main_putAddrOut  instR7 (zi24, zi27, main_putaddrout_outR1);
  assign zi28 = main_putaddrout_outR1;
  Main_getOut  instR8 (zi28, main_getout_outR1);
  assign zi29 = main_getout_outR1;
  assign zi31 = zi29[69:0];
  assign zi32 = zi29[84];
  assign zi33 = zi29[83:78];
  Main_putOut  instR9 ({zi32, zi33, zi26}, zi31, main_putout_out);
  assign zi34 = main_putout_out;
  Main_getOut  instR10 (zi34, main_getout_outR2);
  assign zi35 = main_getout_outR2;
  assign zi36 = zi35[84:70];
  assign zi37 = zi35[69:0];
  ZLL_Main_putWeOut  instR11 (1'h1, zi36, zi37, zll_main_putweout_out);
  assign zi38 = zll_main_putweout_out;
  assign zi39 = {19'h10000, zi38};
  assign zi40 = zi39[69:0];
  Main_getOut  instR12 (zi40, main_getout_outR3);
  assign zi41 = main_getout_outR3;
  assign zi42 = zi41[84:70];
  assign zi43 = zi41[69:0];
  assign zi44 = {4'h1, zi42, zi43};
  assign zi45 = zi44[84:70];
  assign zi46 = zi44[69:0];
  assign zi47 = zi7[75:74];
  assign zi48 = zi7[73:72];
  assign zi49 = zi7[71:70];
  Main_getReg1  instR13 (zi48, zi9, main_getreg1_out);
  assign zi50 = main_getreg1_out;
  assign zi51 = zi50[77:70];
  assign zi52 = zi50[69:0];
  Main_getReg1  instR14 (zi49, zi52, main_getreg1_outR1);
  assign zi53 = main_getreg1_outR1;
  assign zi54 = zi53[77:70];
  assign zi55 = zi53[69:0];
  assign zi56 = ~(zi51 & zi54);
  ZLL_Main_putReg3  instR15 (zi56, zi47, zi55, zll_main_putreg3_out);
  assign zi57 = zll_main_putreg3_out;
  Main_incrPC  instR16 (zi57, main_incrpc_outR1);
  assign zi58 = main_incrpc_outR1;
  assign zi59 = {19'h10000, zi58};
  assign zi60 = zi59[69:0];
  ZLL_Main_loop11  instR17 (zi60, zll_main_loop11_outR1);
  assign zi61 = zi7[75:70];
  Main_getReg  instR18 (zi9, main_getreg_outR1);
  assign zi62 = main_getreg_outR1;
  assign zi63 = zi62[77:70];
  assign zi64 = zi62[69:0];
  assign zi65 = zi63 == 8'h0;
  Main_putPC1  instR19 (zi61, zi64, main_putpc1_out);
  Main_incrPC  instR20 (zi64, main_incrpc_outR2);
  assign zi66 = (zi65 == 1'h0) ? main_putpc1_out : main_incrpc_outR2;
  assign zi67 = {19'h10000, zi66};
  assign zi68 = zi67[69:0];
  ZLL_Main_loop11  instR21 (zi68, zll_main_loop11_outR2);
  assign res = (zi7[78:76] == 3'h0) ? zll_main_loop11_out : ((zi7[78:76] == 3'h1) ? {1'h1, zi22, 3'h3, zi23} : ((zi7[78:76] == 3'h2) ? {1'h1, zi45, 3'h1, zi46} : ((zi7[78:76] == 3'h3) ? zll_main_loop11_outR1 : zll_main_loop11_outR2)));
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

module ZLL_Main_loop11 (input logic [69:0] arg0,
  output logic [88:0] res);
  logic [75:0] main_getpc_out;
  logic [75:0] zt0;
  logic [5:0] st0;
  logic [69:0] st1;
  logic [69:0] zll_main_finishinstr1_out;
  logic [69:0] zt1;
  logic [88:0] zt2;
  logic [69:0] s0;
  logic [84:0] main_getout_out;
  logic [84:0] zi0;
  logic [14:0] zi1;
  logic [69:0] zi2;
  logic [88:0] zi3;
  logic [14:0] zi4;
  logic [69:0] zi5;
  Main_getPC  inst (arg0, main_getpc_out);
  assign zt0 = main_getpc_out;
  assign st0 = zt0[75:70];
  assign st1 = zt0[69:0];
  ZLL_Main_finishInstr1  instR1 (st0, st1, zll_main_finishinstr1_out);
  assign zt1 = zll_main_finishinstr1_out;
  assign zt2 = {19'h10000, zt1};
  assign s0 = zt2[69:0];
  Main_getOut  instR2 (s0, main_getout_out);
  assign zi0 = main_getout_out;
  assign zi1 = zi0[84:70];
  assign zi2 = zi0[69:0];
  assign zi3 = {4'h1, zi1, zi2};
  assign zi4 = zi3[84:70];
  assign zi5 = zi3[69:0];
  assign res = {1'h1, zi4, 3'h2, zi5};
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
  logic [69:0] zll_main_finishinstr_out;
  Main_putAddrOut  inst (arg0, arg1, main_putaddrout_out);
  assign zt0 = main_putaddrout_out;
  ZLL_Main_finishInstr  instR1 (zt0, zll_main_finishinstr_out);
  assign res = zll_main_finishinstr_out;
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

module ZLL_Main_finishInstr (input logic [69:0] arg0,
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