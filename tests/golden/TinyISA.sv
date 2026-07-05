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
  logic [88:0] zll_main_loop12_out;
  logic [69:0] main_putins_outR1;
  logic [69:0] zi6;
  logic [88:0] zi7;
  logic [69:0] zi8;
  logic [69:0] main_incrpc_out;
  logic [69:0] zi9;
  logic [88:0] zi10;
  logic [69:0] zi11;
  logic [75:0] main_getpc_out;
  logic [75:0] zi12;
  logic [5:0] zi13;
  logic [69:0] zi14;
  logic [69:0] zll_main_finishinstr1_out;
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
  logic [69:0] main_putins_outR2;
  logic [69:0] zi25;
  logic [88:0] zi26;
  logic [69:0] zi27;
  logic [88:0] zll_main_loop15_out;
  logic [69:0] main_putins_outR3;
  logic [69:0] zi29;
  logic [88:0] zi30;
  logic [69:0] zi31;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi32;
  logic [88:0] zi33;
  logic [69:0] zi34;
  logic [88:0] zll_main_loop12_outR1;
  logic [69:0] main_putins_outR4;
  logic [69:0] zi36;
  logic [88:0] zi37;
  logic [69:0] zi38;
  logic [86:0] main_getins_out;
  logic [86:0] zi39;
  logic [69:0] zi41;
  logic [7:0] zi42;
  logic [77:0] zi43;
  logic [7:0] zi44;
  logic [69:0] zi45;
  logic [69:0] zll_main_putreg_out;
  logic [69:0] zi46;
  logic [88:0] zi47;
  logic [69:0] zi48;
  logic [88:0] zll_main_loop15_outR1;
  logic [88:0] zres;
  Main_putIns  inst (__in0, __st0, main_putins_out);
  assign zi2 = main_putins_out;
  assign zi3 = {19'h10000, zi2};
  assign zi4 = zi3[69:0];
  ZLL_Main_loop12  instR1 (zi4, zll_main_loop12_out);
  Main_putIns  instR2 (__in0, __st0, main_putins_outR1);
  assign zi6 = main_putins_outR1;
  assign zi7 = {19'h10000, zi6};
  assign zi8 = zi7[69:0];
  Main_incrPC  instR3 (zi8, main_incrpc_out);
  assign zi9 = main_incrpc_out;
  assign zi10 = {19'h10000, zi9};
  assign zi11 = zi10[69:0];
  Main_getPC  instR4 (zi11, main_getpc_out);
  assign zi12 = main_getpc_out;
  assign zi13 = zi12[75:70];
  assign zi14 = zi12[69:0];
  ZLL_Main_finishInstr1  instR5 (zi13, zi14, zll_main_finishinstr1_out);
  assign zi15 = zll_main_finishinstr1_out;
  assign zi16 = {19'h10000, zi15};
  assign zi17 = zi16[69:0];
  Main_getOut  instR6 (zi17, main_getout_out);
  assign zi18 = main_getout_out;
  assign zi19 = zi18[84:70];
  assign zi20 = zi18[69:0];
  assign zi21 = {4'h1, zi19, zi20};
  assign zi22 = zi21[84:70];
  assign zi23 = zi21[69:0];
  Main_putIns  instR7 (__in0, __st0, main_putins_outR2);
  assign zi25 = main_putins_outR2;
  assign zi26 = {19'h10000, zi25};
  assign zi27 = zi26[69:0];
  ZLL_Main_loop15  instR8 (zi27, zll_main_loop15_out);
  Main_putIns  instR9 (__in0, __st0, main_putins_outR3);
  assign zi29 = main_putins_outR3;
  assign zi30 = {19'h10000, zi29};
  assign zi31 = zi30[69:0];
  Main_incrPC  instR10 (zi31, main_incrpc_outR1);
  assign zi32 = main_incrpc_outR1;
  assign zi33 = {19'h10000, zi32};
  assign zi34 = zi33[69:0];
  ZLL_Main_loop12  instR11 (zi34, zll_main_loop12_outR1);
  Main_putIns  instR12 (__in0, __st0, main_putins_outR4);
  assign zi36 = main_putins_outR4;
  assign zi37 = {19'h10000, zi36};
  assign zi38 = zi37[69:0];
  Main_getIns  instR13 (zi38, main_getins_out);
  assign zi39 = main_getins_out;
  assign zi41 = zi39[69:0];
  assign zi42 = zi39[77:70];
  assign zi43 = {zi42, zi41};
  assign zi44 = zi43[77:70];
  assign zi45 = zi43[69:0];
  ZLL_Main_putReg  instR14 (zi44, zi45, zi45, zll_main_putreg_out);
  assign zi46 = zll_main_putreg_out;
  assign zi47 = {19'h10000, zi46};
  assign zi48 = zi47[69:0];
  ZLL_Main_loop15  instR15 (zi48, zll_main_loop15_outR1);
  assign zres = (__resumption_tag == 3'h1) ? zll_main_loop12_out : ((__resumption_tag == 3'h2) ? {1'h1, zi22, 3'h0, zi23} : ((__resumption_tag == 3'h3) ? zll_main_loop15_out : ((__resumption_tag == 3'h4) ? zll_main_loop12_outR1 : zll_main_loop15_outR1)));
  assign __resumption_tag_next = zres[72:70];
  assign __st0_next = zres[69:0];
  assign __out0 = zres[87:73];
  initial {__resumption_tag, __st0} = {3'h1, {7'h46{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {3'h1, {7'h46{1'h0}}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module Main_putPC (input logic [5:0] arg0,
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

module Main_getReg$s1 (input logic [69:0] arg0,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg3_out;
  ZLL_Main_getReg3  inst (arg0, arg0, zll_main_getreg3_out);
  assign res = zll_main_getreg3_out;
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

module Main_getReg (input logic [1:0] arg0,
  input logic [69:0] arg1,
  output logic [77:0] res);
  logic [77:0] zll_main_getreg3_out;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  ZLL_Main_getReg3  inst (arg1, arg1, zll_main_getreg3_out);
  assign zi0 = arg1[61:54];
  assign zi1 = arg1[53:46];
  assign zi2 = arg1[45:38];
  assign res = (arg0 == 2'h0) ? zll_main_getreg3_out : ((arg0 == 2'h1) ? {zi0, arg1} : ((arg0 == 2'h2) ? {zi1, arg1} : {zi2, arg1}));
endmodule

module ZLL_Main_getReg3 (input logic [69:0] arg0,
  input logic [69:0] arg1,
  output logic [77:0] res);
  logic [7:0] zi0;
  assign zi0 = arg0[69:62];
  assign res = {zi0, arg1};
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

module ZLL_Main_loop15 (input logic [69:0] arg0,
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
  logic [88:0] zll_main_loop12_out;
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
  logic [77:0] main_getreg$s1_out;
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
  logic [69:0] zi37;
  logic [5:0] zi38;
  logic [7:0] zi39;
  logic [69:0] main_putout_outR1;
  logic [69:0] zi40;
  logic [88:0] zi41;
  logic [69:0] zi42;
  logic [84:0] main_getout_outR3;
  logic [84:0] zi43;
  logic [14:0] zi44;
  logic [69:0] zi45;
  logic [88:0] zi46;
  logic [14:0] zi47;
  logic [69:0] zi48;
  logic [1:0] zi49;
  logic [1:0] zi50;
  logic [1:0] zi51;
  logic [77:0] main_getreg_out;
  logic [77:0] zi52;
  logic [7:0] zi53;
  logic [69:0] zi54;
  logic [77:0] main_getreg_outR1;
  logic [77:0] zi55;
  logic [7:0] zi56;
  logic [69:0] zi57;
  logic [7:0] zi58;
  logic [69:0] zll_main_putreg_out;
  logic [7:0] zi59;
  logic [7:0] zi60;
  logic [7:0] zi61;
  logic [5:0] zi62;
  logic [16:0] zi63;
  logic [14:0] zi64;
  logic [7:0] zi65;
  logic [7:0] zi66;
  logic [7:0] zi67;
  logic [5:0] zi68;
  logic [16:0] zi69;
  logic [14:0] zi70;
  logic [7:0] zi71;
  logic [7:0] zi72;
  logic [7:0] zi73;
  logic [5:0] zi74;
  logic [16:0] zi75;
  logic [14:0] zi76;
  logic [69:0] zi77;
  logic [69:0] main_incrpc_outR1;
  logic [69:0] zi78;
  logic [88:0] zi79;
  logic [69:0] zi80;
  logic [88:0] zll_main_loop12_outR1;
  logic [5:0] zi81;
  logic [77:0] main_getreg$s1_outR1;
  logic [77:0] zi82;
  logic [7:0] zi83;
  logic [69:0] zi84;
  logic [0:0] zi85;
  logic [69:0] main_putpc_out;
  logic [69:0] main_incrpc_outR2;
  logic [69:0] zi86;
  logic [88:0] zi87;
  logic [69:0] zi88;
  logic [88:0] zll_main_loop12_outR2;
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
  ZLL_Main_loop12  instR2 (zi12, zll_main_loop12_out);
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
  Main_getReg$s1  instR6 (zi9, main_getreg$s1_out);
  assign zi25 = main_getreg$s1_out;
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
  assign zi37 = zi35[69:0];
  assign zi38 = zi35[83:78];
  assign zi39 = zi35[77:70];
  Main_putOut  instR11 ({1'h1, zi38, zi39}, zi37, main_putout_outR1);
  assign zi40 = main_putout_outR1;
  assign zi41 = {19'h10000, zi40};
  assign zi42 = zi41[69:0];
  Main_getOut  instR12 (zi42, main_getout_outR3);
  assign zi43 = main_getout_outR3;
  assign zi44 = zi43[84:70];
  assign zi45 = zi43[69:0];
  assign zi46 = {4'h1, zi44, zi45};
  assign zi47 = zi46[84:70];
  assign zi48 = zi46[69:0];
  assign zi49 = zi7[75:74];
  assign zi50 = zi7[73:72];
  assign zi51 = zi7[71:70];
  Main_getReg  instR13 (zi50, zi9, main_getreg_out);
  assign zi52 = main_getreg_out;
  assign zi53 = zi52[77:70];
  assign zi54 = zi52[69:0];
  Main_getReg  instR14 (zi51, zi54, main_getreg_outR1);
  assign zi55 = main_getreg_outR1;
  assign zi56 = zi55[77:70];
  assign zi57 = zi55[69:0];
  assign zi58 = ~(zi53 & zi56);
  ZLL_Main_putReg  instR15 (zi58, zi57, zi57, zll_main_putreg_out);
  assign zi59 = zi55[69:62];
  assign zi60 = zi55[53:46];
  assign zi61 = zi55[45:38];
  assign zi62 = zi55[37:32];
  assign zi63 = zi55[31:15];
  assign zi64 = zi55[14:0];
  assign zi65 = zi55[69:62];
  assign zi66 = zi55[61:54];
  assign zi67 = zi55[45:38];
  assign zi68 = zi55[37:32];
  assign zi69 = zi55[31:15];
  assign zi70 = zi55[14:0];
  assign zi71 = zi55[69:62];
  assign zi72 = zi55[61:54];
  assign zi73 = zi55[53:46];
  assign zi74 = zi55[37:32];
  assign zi75 = zi55[31:15];
  assign zi76 = zi55[14:0];
  assign zi77 = (zi49 == 2'h0) ? zll_main_putreg_out : ((zi49 == 2'h1) ? {zi59, zi58, zi60, zi61, zi62, zi63, zi64} : ((zi49 == 2'h2) ? {zi65, zi66, zi58, zi67, zi68, zi69, zi70} : {zi71, zi72, zi73, zi58, zi74, zi75, zi76}));
  Main_incrPC  instR16 (zi77, main_incrpc_outR1);
  assign zi78 = main_incrpc_outR1;
  assign zi79 = {19'h10000, zi78};
  assign zi80 = zi79[69:0];
  ZLL_Main_loop12  instR17 (zi80, zll_main_loop12_outR1);
  assign zi81 = zi7[75:70];
  Main_getReg$s1  instR18 (zi9, main_getreg$s1_outR1);
  assign zi82 = main_getreg$s1_outR1;
  assign zi83 = zi82[77:70];
  assign zi84 = zi82[69:0];
  assign zi85 = zi83 == 8'h0;
  Main_putPC  instR19 (zi81, zi84, main_putpc_out);
  Main_incrPC  instR20 (zi84, main_incrpc_outR2);
  assign zi86 = (zi85 == 1'h0) ? main_putpc_out : main_incrpc_outR2;
  assign zi87 = {19'h10000, zi86};
  assign zi88 = zi87[69:0];
  ZLL_Main_loop12  instR21 (zi88, zll_main_loop12_outR2);
  assign res = (zi7[78:76] == 3'h0) ? zll_main_loop12_out : ((zi7[78:76] == 3'h1) ? {1'h1, zi22, 3'h2, zi23} : ((zi7[78:76] == 3'h2) ? {1'h1, zi47, 3'h4, zi48} : ((zi7[78:76] == 3'h3) ? zll_main_loop12_outR1 : zll_main_loop12_outR2)));
endmodule

module Main_getIns (input logic [69:0] arg0,
  output logic [86:0] res);
  logic [16:0] zi0;
  assign zi0 = arg0[31:15];
  assign res = {zi0, arg0};
endmodule

module ZLL_Main_loop12 (input logic [69:0] arg0,
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
  assign res = {1'h1, zi4, 3'h3, zi5};
endmodule

module Main_getOut (input logic [69:0] arg0,
  output logic [84:0] res);
  logic [14:0] zi0;
  assign zi0 = arg0[14:0];
  assign res = {zi0, arg0};
endmodule

module ZLL_Main_putReg (input logic [7:0] arg0,
  input logic [69:0] arg1,
  input logic [69:0] arg2,
  output logic [69:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [5:0] zi3;
  logic [16:0] zi4;
  logic [14:0] zi5;
  assign zi0 = arg1[61:54];
  assign zi1 = arg1[53:46];
  assign zi2 = arg1[45:38];
  assign zi3 = arg1[37:32];
  assign zi4 = arg1[31:15];
  assign zi5 = arg1[14:0];
  assign res = {arg0, zi0, zi1, zi2, zi3, zi4, zi5};
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

module Main_incrPC (input logic [69:0] arg0,
  output logic [69:0] res);
  logic [75:0] main_getpc_out;
  logic [75:0] zt0;
  logic [5:0] st0;
  logic [69:0] st1;
  logic [69:0] main_putpc_out;
  Main_getPC  inst (arg0, main_getpc_out);
  assign zt0 = main_getpc_out;
  assign st0 = zt0[75:70];
  assign st1 = zt0[69:0];
  Main_putPC  instR1 (st0 + 6'h1, st1, main_putpc_out);
  assign res = main_putpc_out;
endmodule

module ZLL_Main_finishInstr (input logic [69:0] arg0,
  output logic [69:0] res);
  logic [84:0] main_getout_out;
  logic [84:0] zi0;
  logic [69:0] zi2;
  logic [5:0] zi3;
  logic [7:0] zi4;
  logic [69:0] main_putout_out;
  Main_getOut  inst (arg0, main_getout_out);
  assign zi0 = main_getout_out;
  assign zi2 = zi0[69:0];
  assign zi3 = zi0[83:78];
  assign zi4 = zi0[77:70];
  Main_putOut  instR1 ({1'h0, zi3, zi4}, zi2, main_putout_out);
  assign res = main_putout_out;
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