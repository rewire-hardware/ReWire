module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [45:0] __in0,
  output logic [32:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [76:0] __st0;
  logic [76:0] __st0_next;
  logic [127:0] __st1;
  logic [127:0] __st1_next;
  logic [204:0] zi1;
  logic [214:0] main_get_addr_reg_out;
  logic [214:0] zi2;
  logic [9:0] zi3;
  logic [76:0] zi5;
  logic [127:0] zi6;
  logic [127:0] main_memtweak_out;
  logic [204:0] zi7;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [0:0] zi10;
  logic [0:0] zi11;
  logic [0:0] zi12;
  logic [0:0] zi13;
  logic [0:0] zi14;
  logic [0:0] zi15;
  logic [0:0] zi16;
  logic [0:0] zi17;
  logic [204:0] main_set_addr_reg_out;
  logic [204:0] zi18;
  logic [409:0] zll_main_idle17_out;
  logic [409:0] zi19;
  logic [204:0] zi20;
  logic [204:0] zi21;
  logic [411:0] zi22;
  logic [204:0] zi23;
  logic [204:0] zi24;
  logic [411:0] main_while_addr_reg_0_out;
  logic [204:0] zi25;
  logic [204:0] main_set_data_out_reg$s1_out;
  logic [204:0] zi26;
  logic [214:0] main_get_addr_reg_outR1;
  logic [214:0] zi27;
  logic [9:0] zi28;
  logic [204:0] zi29;
  logic [31:0] zi31;
  logic [236:0] zi32;
  logic [31:0] zi33;
  logic [76:0] zi35;
  logic [127:0] zi36;
  logic [127:0] main_memtweak_outR1;
  logic [204:0] zi37;
  logic [204:0] zll_main_perform_read2_out;
  logic [204:0] zi38;
  logic [409:0] zll_main_idle17_outR1;
  logic [409:0] zi39;
  logic [204:0] zi40;
  logic [204:0] zi41;
  logic [411:0] zi42;
  logic [204:0] zi43;
  logic [204:0] zi44;
  logic [411:0] zll_main_idle9_out;
  logic [204:0] zi45;
  logic [214:0] main_get_addr_reg_outR2;
  logic [214:0] zi46;
  logic [204:0] zi48;
  logic [127:0] zi49;
  logic [0:0] zi50;
  logic [0:0] zi51;
  logic [0:0] zi52;
  logic [0:0] zi53;
  logic [0:0] zi54;
  logic [0:0] zi55;
  logic [0:0] zi56;
  logic [0:0] zi57;
  logic [0:0] zi58;
  logic [0:0] zi59;
  logic [31:0] zi60;
  logic [31:0] zi61;
  logic [31:0] zi62;
  logic [31:0] zi63;
  logic [31:0] zll_main_memlookup_fail1_out;
  logic [31:0] zll_main_memlookup_fail1_outR1;
  logic [31:0] zll_main_memlookup_fail1_outR2;
  logic [31:0] zll_main_memlookup_fail1_outR3;
  logic [31:0] zll_main_memlookup_fail1_outR4;
  logic [31:0] zll_main_memlookup_fail1_outR5;
  logic [31:0] zll_main_memlookup_fail1_outR6;
  logic [31:0] zll_main_memlookup_fail1_outR7;
  logic [236:0] zi64;
  logic [31:0] zi65;
  logic [127:0] zi67;
  logic [1:0] zi68;
  logic [9:0] zi69;
  logic [31:0] zi70;
  logic [0:0] zi71;
  logic [204:0] zi72;
  logic [204:0] zll_main_perform_read2_outR1;
  logic [204:0] zi73;
  logic [409:0] zll_main_idle17_outR2;
  logic [409:0] zi74;
  logic [204:0] zi75;
  logic [204:0] zi76;
  logic [411:0] zi77;
  logic [204:0] zi78;
  logic [204:0] zi79;
  logic [411:0] zll_main_idle9_outR1;
  logic [204:0] zi80;
  logic [1:0] zi81;
  logic [1:0] zi82;
  logic [9:0] zi83;
  logic [9:0] zi84;
  logic [0:0] zi85;
  logic [0:0] zi86;
  logic [0:0] zi87;
  logic [0:0] zi88;
  logic [9:0] mainzuzlzazgzuout;
  logic [204:0] main_set_addr_reg_outR1;
  logic [204:0] zi89;
  logic [31:0] zi90;
  logic [31:0] zi91;
  logic [127:0] zi92;
  logic [1:0] zi93;
  logic [9:0] zi94;
  logic [0:0] zi95;
  logic [31:0] zi96;
  logic [204:0] zi97;
  logic [204:0] zll_main_idle10_out;
  logic [204:0] zi98;
  logic [409:0] zll_main_idle17_outR3;
  logic [409:0] zi99;
  logic [204:0] zi100;
  logic [204:0] zi101;
  logic [411:0] zi102;
  logic [204:0] zi103;
  logic [76:0] zi105;
  logic [32:0] zll_main_connect_out;
  logic [9:0] mainzuzlzazgzuoutR1;
  logic [204:0] main_set_addr_reg_outR2;
  logic [204:0] zi106;
  logic [204:0] zll_main_idle10_outR1;
  logic [204:0] zi107;
  logic [409:0] zll_main_idle17_outR4;
  logic [409:0] zi108;
  logic [204:0] zi109;
  logic [204:0] zi110;
  logic [411:0] zi111;
  logic [204:0] zi112;
  logic [76:0] zi114;
  logic [32:0] zll_main_connect_outR1;
  logic [204:0] main_set_ack_reg$s2_out;
  logic [204:0] zi115;
  logic [204:0] zll_main_idle18_out;
  logic [204:0] zi116;
  logic [409:0] zll_main_idle17_outR5;
  logic [409:0] zi117;
  logic [204:0] zi118;
  logic [204:0] zi119;
  logic [411:0] zi120;
  logic [204:0] zi121;
  logic [204:0] zi122;
  logic [411:0] zll_main_idle9_outR2;
  logic [204:0] zi123;
  logic [411:0] main_while_addr_reg_0_outR1;
  logic [411:0] zres;
  assign zi1 = {__st0, __st1};
  Main_get__addr__reg  inst (zi1, main_get_addr_reg_out);
  assign zi2 = main_get_addr_reg_out;
  assign zi3 = zi2[214:205];
  assign zi5 = zi2[204:128];
  assign zi6 = zi2[127:0];
  Main_memTweak  instR1 (zi3, {6'h20{1'h0}}, zi6, main_memtweak_out);
  assign zi7 = {zi5, main_memtweak_out};
  assign zi8 = zi2[214];
  assign zi9 = zi2[213];
  assign zi10 = zi2[212];
  assign zi11 = zi2[211];
  assign zi12 = zi2[210];
  assign zi13 = zi2[209];
  assign zi14 = zi2[208];
  assign zi15 = zi2[207];
  assign zi16 = zi2[206];
  assign zi17 = zi2[205];
  Main_set__addr__reg  instR2 ((zi8 == 1'h0) ? ((zi9 == 1'h0) ? ((zi10 == 1'h0) ? ((zi11 == 1'h0) ? ((zi12 == 1'h0) ? ((zi13 == 1'h0) ? ((zi14 == 1'h0) ? ((zi15 == 1'h0) ? ((zi16 == 1'h0) ? ((zi17 == 1'h1) ? 10'h0 : 10'h3ff) : ((zi17 == 1'h0) ? 10'h1 : 10'h2)) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff, zi7, main_set_addr_reg_out);
  assign zi18 = main_set_addr_reg_out;
  ZLL_Main_idle17  instR3 (zi18, zll_main_idle17_out);
  assign zi19 = zll_main_idle17_out;
  assign zi20 = zi19[409:205];
  assign zi21 = zi19[204:0];
  assign zi22 = {2'h1, zi20, zi21};
  assign zi23 = zi22[409:205];
  assign zi24 = zi22[204:0];
  Main_while__addr__reg__0  instR4 (zi23, zi24, main_while_addr_reg_0_out);
  assign zi25 = {__st0, __st1};
  Main_set__data__out__reg$s1  instR5 (zi25, main_set_data_out_reg$s1_out);
  assign zi26 = main_set_data_out_reg$s1_out;
  Main_get__addr__reg  instR6 (zi26, main_get_addr_reg_outR1);
  assign zi27 = main_get_addr_reg_outR1;
  assign zi28 = zi27[214:205];
  assign zi29 = zi27[204:0];
  assign zi31 = zi27[192:161];
  assign zi32 = {zi31, zi29};
  assign zi33 = zi32[236:205];
  assign zi35 = zi32[204:128];
  assign zi36 = zi32[127:0];
  Main_memTweak  instR7 (zi28, zi33, zi36, main_memtweak_outR1);
  assign zi37 = {zi35, main_memtweak_outR1};
  ZLL_Main_perform__read2  instR8 (zi37, zll_main_perform_read2_out);
  assign zi38 = zll_main_perform_read2_out;
  ZLL_Main_idle17  instR9 (zi38, zll_main_idle17_outR1);
  assign zi39 = zll_main_idle17_outR1;
  assign zi40 = zi39[409:205];
  assign zi41 = zi39[204:0];
  assign zi42 = {2'h1, zi40, zi41};
  assign zi43 = zi42[409:205];
  assign zi44 = zi42[204:0];
  ZLL_Main_idle9  instR10 (zi43, zi44, zll_main_idle9_out);
  assign zi45 = {__st0, __st1};
  Main_get__addr__reg  instR11 (zi45, main_get_addr_reg_outR2);
  assign zi46 = main_get_addr_reg_outR2;
  assign zi48 = zi46[204:0];
  assign zi49 = zi46[127:0];
  assign zi50 = zi46[214];
  assign zi51 = zi46[213];
  assign zi52 = zi46[212];
  assign zi53 = zi46[211];
  assign zi54 = zi46[210];
  assign zi55 = zi46[209];
  assign zi56 = zi46[208];
  assign zi57 = zi46[207];
  assign zi58 = zi46[206];
  assign zi59 = zi46[205];
  assign zi60 = zi46[127:96];
  assign zi61 = zi46[95:64];
  assign zi62 = zi46[63:32];
  assign zi63 = zi46[31:0];
  ZLL_Main_memLookup_fail1  instR12 (zi49, zll_main_memlookup_fail1_out);
  ZLL_Main_memLookup_fail1  instR13 (zi49, zll_main_memlookup_fail1_outR1);
  ZLL_Main_memLookup_fail1  instR14 (zi49, zll_main_memlookup_fail1_outR2);
  ZLL_Main_memLookup_fail1  instR15 (zi49, zll_main_memlookup_fail1_outR3);
  ZLL_Main_memLookup_fail1  instR16 (zi49, zll_main_memlookup_fail1_outR4);
  ZLL_Main_memLookup_fail1  instR17 (zi49, zll_main_memlookup_fail1_outR5);
  ZLL_Main_memLookup_fail1  instR18 (zi49, zll_main_memlookup_fail1_outR6);
  ZLL_Main_memLookup_fail1  instR19 (zi49, zll_main_memlookup_fail1_outR7);
  assign zi64 = {(zi50 == 1'h0) ? ((zi51 == 1'h0) ? ((zi52 == 1'h0) ? ((zi53 == 1'h0) ? ((zi54 == 1'h0) ? ((zi55 == 1'h0) ? ((zi56 == 1'h0) ? ((zi57 == 1'h0) ? ((zi58 == 1'h0) ? ((zi59 == 1'h0) ? zi60 : zi61) : ((zi59 == 1'h0) ? zi62 : zi63)) : zll_main_memlookup_fail1_out) : zll_main_memlookup_fail1_outR1) : zll_main_memlookup_fail1_outR2) : zll_main_memlookup_fail1_outR3) : zll_main_memlookup_fail1_outR4) : zll_main_memlookup_fail1_outR5) : zll_main_memlookup_fail1_outR6) : zll_main_memlookup_fail1_outR7, zi48};
  assign zi65 = zi64[236:205];
  assign zi67 = zi64[127:0];
  assign zi68 = zi64[204:203];
  assign zi69 = zi64[202:193];
  assign zi70 = zi64[192:161];
  assign zi71 = zi64[160];
  assign zi72 = {{zi68, zi69, zi70, zi71, zi65}, zi67};
  ZLL_Main_perform__read2  instR20 (zi72, zll_main_perform_read2_outR1);
  assign zi73 = zll_main_perform_read2_outR1;
  ZLL_Main_idle17  instR21 (zi73, zll_main_idle17_outR2);
  assign zi74 = zll_main_idle17_outR2;
  assign zi75 = zi74[409:205];
  assign zi76 = zi74[204:0];
  assign zi77 = {2'h1, zi75, zi76};
  assign zi78 = zi77[409:205];
  assign zi79 = zi77[204:0];
  ZLL_Main_idle9  instR22 (zi78, zi79, zll_main_idle9_outR1);
  assign zi80 = {__st0, __st1};
  assign zi81 = __in0[1:0];
  assign zi82 = zi81;
  assign zi83 = __in0[13:4];
  assign zi84 = zi83;
  assign zi85 = __in0[3];
  assign zi86 = zi85;
  assign zi87 = __in0[2];
  assign zi88 = zi87;
  Mainzuzlzazg  instR23 (zi82, zi84, mainzuzlzazgzuout);
  Main_set__addr__reg  instR24 (mainzuzlzazgzuout, zi80, main_set_addr_reg_outR1);
  assign zi89 = main_set_addr_reg_outR1;
  assign zi90 = __in0[45:14];
  assign zi91 = zi90;
  assign zi92 = zi89[127:0];
  assign zi93 = zi89[204:203];
  assign zi94 = zi89[202:193];
  assign zi95 = zi89[160];
  assign zi96 = zi89[159:128];
  assign zi97 = {{zi93, zi94, zi91, zi95, zi96}, zi92};
  ZLL_Main_idle10  instR25 (zi82, zi97, zll_main_idle10_out);
  assign zi98 = zll_main_idle10_out;
  ZLL_Main_idle17  instR26 (zi98, zll_main_idle17_outR3);
  assign zi99 = zll_main_idle17_outR3;
  assign zi100 = zi99[409:205];
  assign zi101 = zi99[204:0];
  assign zi102 = {2'h1, zi100, zi101};
  assign zi103 = zi102[409:205];
  assign zi105 = zi102[409:333];
  ZLL_Main_connect  instR27 (zi105, zll_main_connect_out);
  Mainzuzlzazg  instR28 (zi82, zi84, mainzuzlzazgzuoutR1);
  Main_set__addr__reg  instR29 (mainzuzlzazgzuoutR1, zi80, main_set_addr_reg_outR2);
  assign zi106 = main_set_addr_reg_outR2;
  ZLL_Main_idle10  instR30 (zi82, zi106, zll_main_idle10_outR1);
  assign zi107 = zll_main_idle10_outR1;
  ZLL_Main_idle17  instR31 (zi107, zll_main_idle17_outR4);
  assign zi108 = zll_main_idle17_outR4;
  assign zi109 = zi108[409:205];
  assign zi110 = zi108[204:0];
  assign zi111 = {2'h1, zi109, zi110};
  assign zi112 = zi111[409:205];
  assign zi114 = zi111[409:333];
  ZLL_Main_connect  instR32 (zi114, zll_main_connect_outR1);
  Main_set__ack__reg$s2  instR33 (zi80, main_set_ack_reg$s2_out);
  assign zi115 = main_set_ack_reg$s2_out;
  ZLL_Main_idle18  instR34 (zi115, zll_main_idle18_out);
  assign zi116 = zll_main_idle18_out;
  ZLL_Main_idle17  instR35 (zi116, zll_main_idle17_outR5);
  assign zi117 = zll_main_idle17_outR5;
  assign zi118 = zi117[409:205];
  assign zi119 = zi117[204:0];
  assign zi120 = {2'h1, zi118, zi119};
  assign zi121 = zi120[409:205];
  assign zi122 = zi120[204:0];
  ZLL_Main_idle9  instR36 (zi121, zi122, zll_main_idle9_outR2);
  assign zi123 = {__st0, __st1};
  Main_while__addr__reg__0  instR37 (zi123, zi123, main_while_addr_reg_0_outR1);
  assign zres = (__resumption_tag == 3'h1) ? main_while_addr_reg_0_out : ((__resumption_tag == 3'h2) ? zll_main_idle9_out : ((__resumption_tag == 3'h3) ? zll_main_idle9_outR1 : ((__resumption_tag == 3'h4) ? ((zi86 == 1'h1) ? ((zi88 == 1'h0) ? {{1'h1, {8'haa{1'h0}}}, zll_main_connect_out, 3'h2, zi103} : {{1'h1, {8'haa{1'h0}}}, zll_main_connect_outR1, 3'h3, zi112}) : zll_main_idle9_outR2) : main_while_addr_reg_0_outR1)));
  assign __resumption_tag_next = zres[207:205];
  assign __st0_next = zres[204:128];
  assign __st1_next = zres[127:0];
  assign __out0 = zres[240:208];
  initial {__resumption_tag, __st0, __st1} = {15'h3, {8'hc1{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= {15'h3, {8'hc1{1'h0}}};
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_idle18 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] main_set_data_out_reg$s1_out;
  Main_set__data__out__reg$s1  inst (arg0, main_set_data_out_reg$s1_out);
  assign res = main_set_data_out_reg$s1_out;
endmodule

module Main_data__out__reg (input logic [76:0] arg0,
  output logic [31:0] res);
  logic [31:0] ds5;
  assign ds5 = arg0[31:0];
  assign res = ds5;
endmodule

module ZLL_Main_idle17 (input logic [204:0] arg0,
  output logic [409:0] res);
  assign res = {arg0, arg0};
endmodule

module Main_set__ack__reg$s2 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [127:0] zi0;
  logic [1:0] zi1;
  logic [9:0] zi2;
  logic [31:0] zi3;
  logic [31:0] zi4;
  assign zi0 = arg0[127:0];
  assign zi1 = arg0[204:203];
  assign zi2 = arg0[202:193];
  assign zi3 = arg0[192:161];
  assign zi4 = arg0[159:128];
  assign res = {{zi1, zi2, zi3, 1'h0, zi4}, zi0};
endmodule

module Main_set__addr__reg (input logic [9:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi0;
  logic [1:0] zi1;
  logic [31:0] zi2;
  logic [0:0] zi3;
  logic [31:0] zi4;
  assign zi0 = arg1[127:0];
  assign zi1 = arg1[204:203];
  assign zi2 = arg1[192:161];
  assign zi3 = arg1[160];
  assign zi4 = arg1[159:128];
  assign res = {{zi1, arg0, zi2, zi3, zi4}, zi0};
endmodule

module Main_get__addr__reg (input logic [204:0] arg0,
  output logic [214:0] res);
  logic [9:0] zi1;
  assign zi1 = arg0[202:193];
  assign res = {zi1, arg0};
endmodule

module Main_set__data__out__reg$s1 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [127:0] zi0;
  logic [1:0] zi1;
  logic [9:0] zi2;
  logic [31:0] zi3;
  logic [0:0] zi4;
  assign zi0 = arg0[127:0];
  assign zi1 = arg0[204:203];
  assign zi2 = arg0[202:193];
  assign zi3 = arg0[192:161];
  assign zi4 = arg0[160];
  assign res = {{zi1, zi2, zi3, zi4, {6'h20{1'h0}}}, zi0};
endmodule

module ZLL_Main_perform__read2 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [127:0] zi0;
  logic [1:0] zi1;
  logic [9:0] zi2;
  logic [31:0] zi3;
  logic [31:0] zi4;
  assign zi0 = arg0[127:0];
  assign zi1 = arg0[204:203];
  assign zi2 = arg0[202:193];
  assign zi3 = arg0[192:161];
  assign zi4 = arg0[159:128];
  assign res = {{zi1, zi2, zi3, 1'h1, zi4}, zi0};
endmodule

module ZLL_Main_idle10 (input logic [1:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi0;
  logic [9:0] zi1;
  logic [31:0] zi2;
  logic [0:0] zi3;
  logic [31:0] zi4;
  logic [204:0] zt0;
  logic [204:0] main_set_ack_reg$s2_out;
  logic [204:0] zi5;
  logic [204:0] zll_main_idle18_out;
  assign zi0 = arg1[127:0];
  assign zi1 = arg1[202:193];
  assign zi2 = arg1[192:161];
  assign zi3 = arg1[160];
  assign zi4 = arg1[159:128];
  assign zt0 = {{arg0, zi1, zi2, zi3, zi4}, zi0};
  Main_set__ack__reg$s2  inst (zt0, main_set_ack_reg$s2_out);
  assign zi5 = main_set_ack_reg$s2_out;
  ZLL_Main_idle18  instR1 (zi5, zll_main_idle18_out);
  assign res = zll_main_idle18_out;
endmodule

module Main_while__addr__reg__0 (input logic [204:0] arg0,
  input logic [204:0] arg1,
  output logic [411:0] res);
  logic [214:0] main_get_addr_reg_out;
  logic [214:0] zt0;
  logic [9:0] v;
  logic [204:0] s0;
  logic [411:0] zt1;
  logic [204:0] s0R1;
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  logic [0:0] zi6;
  logic [0:0] zi7;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [0:0] zi10;
  logic [76:0] zi11;
  logic [0:0] main_ack_reg_out;
  logic [31:0] main_data_out_reg_out;
  logic [76:0] zi12;
  logic [32:0] zll_main_connect_out;
  Main_get__addr__reg  inst (arg1, main_get_addr_reg_out);
  assign zt0 = main_get_addr_reg_out;
  assign v = zt0[214:205];
  assign s0 = zt0[204:0];
  assign zt1 = {{8'hc5{1'h0}}, v, s0};
  assign s0R1 = zt1[204:0];
  assign zi0 = zt1[214];
  assign zi1 = zt1[213];
  assign zi2 = zt1[212];
  assign zi3 = zt1[211];
  assign zi4 = zt1[210];
  assign zi5 = zt1[209];
  assign zi6 = zt1[208];
  assign zi7 = zt1[207];
  assign zi8 = zt1[206];
  assign zi9 = zt1[205];
  assign zi10 = (zi0 == 1'h1) ? ((zi1 == 1'h1) ? ((zi2 == 1'h1) ? ((zi3 == 1'h1) ? ((zi4 == 1'h1) ? ((zi5 == 1'h1) ? ((zi6 == 1'h1) ? ((zi7 == 1'h1) ? ((zi8 == 1'h1) ? ((zi9 == 1'h1) ? 1'h0 : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1;
  assign zi11 = zt1[204:128];
  Main_ack__reg  instR1 (zi11, main_ack_reg_out);
  Main_data__out__reg  instR2 (zi11, main_data_out_reg_out);
  assign zi12 = arg0[204:128];
  ZLL_Main_connect  instR3 (zi12, zll_main_connect_out);
  assign res = (zi10 == 1'h0) ? {{1'h1, {8'haa{1'h0}}}, main_ack_reg_out, main_data_out_reg_out, 3'h4, s0R1} : {{1'h1, {8'haa{1'h0}}}, zll_main_connect_out, 3'h1, arg0};
endmodule

module ZLL_Main_idle9 (input logic [204:0] arg0,
  input logic [204:0] arg1,
  output logic [411:0] res);
  logic [76:0] zi0;
  logic [32:0] zll_main_connect_out;
  assign zi0 = arg0[204:128];
  ZLL_Main_connect  inst (zi0, zll_main_connect_out);
  assign res = {{1'h1, {8'haa{1'h0}}}, zll_main_connect_out, 3'h4, arg0};
endmodule

module Mainzuzlzazg (input logic [1:0] arg0,
  input logic [9:0] arg1,
  output logic [9:0] res);
  logic [0:0] b0;
  logic [0:0] b1;
  logic [0:0] b2;
  logic [0:0] b3;
  logic [0:0] b4;
  logic [0:0] b5;
  logic [0:0] b6;
  logic [0:0] b7;
  logic [0:0] b8;
  logic [0:0] b9;
  assign b0 = arg0[1];
  assign b1 = arg0[0];
  assign b2 = arg1[7];
  assign b3 = arg1[6];
  assign b4 = arg1[5];
  assign b5 = arg1[4];
  assign b6 = arg1[3];
  assign b7 = arg1[2];
  assign b8 = arg1[1];
  assign b9 = arg1[0];
  assign res = {b0, b1, b2, b3, b4, b5, b6, b7, b8, b9};
endmodule

module ZLL_Main_connect (input logic [76:0] arg0,
  output logic [32:0] res);
  logic [0:0] main_ack_reg_out;
  logic [31:0] main_data_out_reg_out;
  Main_ack__reg  inst (arg0, main_ack_reg_out);
  Main_data__out__reg  instR1 (arg0, main_data_out_reg_out);
  assign res = {main_ack_reg_out, main_data_out_reg_out};
endmodule

module ZLL_Main_memLookup_fail1 (input logic [127:0] arg0,
  output logic [31:0] res);
  assign res = {6'h20{1'h0}};
endmodule

module Main_ack__reg (input logic [76:0] arg0,
  output logic [0:0] res);
  logic [0:0] ds4;
  assign ds4 = arg0[32];
  assign res = ds4;
endmodule

module Main_memTweak (input logic [9:0] arg0,
  input logic [31:0] arg1,
  input logic [127:0] arg2,
  output logic [127:0] res);
  logic [0:0] ds1;
  logic [0:0] ds2;
  logic [0:0] ds3;
  logic [0:0] ds4;
  logic [0:0] ds5;
  logic [0:0] ds6;
  logic [0:0] ds7;
  logic [0:0] ds8;
  logic [0:0] ds9;
  logic [0:0] ds10;
  logic [31:0] zi0;
  logic [31:0] zi1;
  logic [31:0] zi2;
  logic [31:0] zi3;
  logic [31:0] zi4;
  logic [31:0] zi5;
  logic [31:0] zi6;
  logic [31:0] zi7;
  logic [31:0] zi8;
  logic [31:0] zi9;
  logic [31:0] zi10;
  logic [31:0] zi11;
  assign ds1 = arg0[9];
  assign ds2 = arg0[8];
  assign ds3 = arg0[7];
  assign ds4 = arg0[6];
  assign ds5 = arg0[5];
  assign ds6 = arg0[4];
  assign ds7 = arg0[3];
  assign ds8 = arg0[2];
  assign ds9 = arg0[1];
  assign ds10 = arg0[0];
  assign zi0 = arg2[95:64];
  assign zi1 = arg2[63:32];
  assign zi2 = arg2[31:0];
  assign zi3 = arg2[127:96];
  assign zi4 = arg2[63:32];
  assign zi5 = arg2[31:0];
  assign zi6 = arg2[127:96];
  assign zi7 = arg2[95:64];
  assign zi8 = arg2[31:0];
  assign zi9 = arg2[127:96];
  assign zi10 = arg2[95:64];
  assign zi11 = arg2[63:32];
  assign res = (ds1 == 1'h0) ? ((ds2 == 1'h0) ? ((ds3 == 1'h0) ? ((ds4 == 1'h0) ? ((ds5 == 1'h0) ? ((ds6 == 1'h0) ? ((ds7 == 1'h0) ? ((ds8 == 1'h0) ? ((ds9 == 1'h0) ? ((ds10 == 1'h0) ? {arg1, zi0, zi1, zi2} : {zi3, arg1, zi4, zi5}) : ((ds10 == 1'h0) ? {zi6, zi7, arg1, zi8} : {zi9, zi10, zi11, arg1})) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}};
endmodule