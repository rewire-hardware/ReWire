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
  logic [204:0] main_set_data_out_reg_out;
  logic [204:0] zi2;
  logic [214:0] main_get_addr_reg_out;
  logic [214:0] zi3;
  logic [9:0] zi4;
  logic [204:0] zi5;
  logic [31:0] zi13;
  logic [31:0] zi15;
  logic [31:0] zll_main_mem14_out;
  logic [236:0] zi16;
  logic [31:0] zi17;
  logic [204:0] zi18;
  logic [204:0] zll_main_setloc2_out;
  logic [204:0] zll_main_perform_read23_out;
  logic [409:0] zll_main_perform_write33_out;
  logic [412:0] zll_main_idle87_out;
  logic [412:0] zll_main_idle84_out;
  logic [204:0] zi19;
  logic [214:0] main_get_addr_reg_outR1;
  logic [214:0] zi20;
  logic [204:0] zi22;
  logic [31:0] zi28;
  logic [31:0] zi30;
  logic [31:0] zi31;
  logic [31:0] zll_main_mem02_out;
  logic [31:0] zi33;
  logic [31:0] zi34;
  logic [31:0] zi35;
  logic [31:0] zll_main_mem02_outR1;
  logic [31:0] zi38;
  logic [31:0] zi39;
  logic [31:0] zll_main_mem14_outR1;
  logic [31:0] zi43;
  logic [31:0] zll_main_data_out_reg3_out;
  logic [236:0] zi44;
  logic [31:0] zi45;
  logic [204:0] zi46;
  logic [204:0] zll_main_set_data_out_reg7_out;
  logic [204:0] zll_main_perform_read23_outR1;
  logic [409:0] zll_main_perform_write33_outR1;
  logic [412:0] zll_main_idle87_outR1;
  logic [412:0] zll_main_idle84_outR1;
  logic [204:0] zi47;
  logic [214:0] main_get_addr_reg_outR2;
  logic [214:0] zi48;
  logic [9:0] zi49;
  logic [204:0] zi50;
  logic [204:0] zll_main_setloc2_outR1;
  logic [204:0] zi51;
  logic [204:0] main_set_addr_reg1_out;
  logic [409:0] zll_main_perform_write33_outR2;
  logic [412:0] zll_main_idle87_outR2;
  logic [412:0] zll_main_while_addr_reg_052_out;
  logic [204:0] zi52;
  logic [0:0] main_go_out;
  logic [0:0] main_rnw_out;
  logic [252:0] zi53;
  logic [204:0] zi54;
  logic [45:0] zi55;
  logic [1:0] main_partition_in_out;
  logic [1:0] zi56;
  logic [9:0] main_addr_in_out;
  logic [9:0] zi57;
  logic [9:0] mainzuzlzazgzuout;
  logic [204:0] main_set_addr_reg1_outR1;
  logic [204:0] zll_main_idle98_out;
  logic [409:0] zll_main_perform_write33_outR3;
  logic [412:0] zll_main_idle87_outR3;
  logic [412:0] zi61;
  logic [204:0] zi62;
  logic [412:0] zll_main_perform_write20_out;
  logic [412:0] zi64;
  logic [204:0] zi65;
  logic [76:0] zll_main_connect4_out;
  logic [32:0] zll_main_connect3_out;
  logic [0:0] main_go_outR1;
  logic [0:0] main_rnw_outR1;
  logic [252:0] zi66;
  logic [204:0] zi67;
  logic [45:0] zi68;
  logic [1:0] main_partition_in_outR1;
  logic [1:0] zi69;
  logic [9:0] main_addr_in_outR1;
  logic [9:0] zi70;
  logic [31:0] zi71;
  logic [31:0] zi76;
  logic [9:0] mainzuzlzazgzuoutR1;
  logic [204:0] main_set_addr_reg1_outR2;
  logic [204:0] zi81;
  logic [127:0] zi86;
  logic [1:0] zi87;
  logic [9:0] zi88;
  logic [0:0] zi90;
  logic [31:0] zi91;
  logic [204:0] zll_main_idle98_outR1;
  logic [409:0] zll_main_perform_write33_outR4;
  logic [412:0] zll_main_idle87_outR4;
  logic [412:0] zi92;
  logic [204:0] zi93;
  logic [412:0] zll_main_perform_write20_outR1;
  logic [412:0] zi95;
  logic [204:0] zi96;
  logic [76:0] zll_main_connect4_outR1;
  logic [32:0] zll_main_connect3_outR1;
  logic [204:0] main_set_ack_reg1_out;
  logic [204:0] zi97;
  logic [204:0] zll_main_idle83_out;
  logic [409:0] zll_main_perform_write33_outR5;
  logic [412:0] zll_main_idle87_outR5;
  logic [412:0] zll_main_idle84_outR2;
  logic [204:0] zi98;
  logic [412:0] zll_main_idle87_outR6;
  logic [412:0] zll_main_while_addr_reg_052_outR1;
  logic [412:0] zres;
  assign zi1 = {__st0, __st1};
  Main_set__data__out__reg  inst (zi1, main_set_data_out_reg_out);
  assign zi2 = main_set_data_out_reg_out;
  Main_get__addr__reg  instR1 (zi2, main_get_addr_reg_out);
  assign zi3 = main_get_addr_reg_out;
  assign zi4 = zi3[214:205];
  assign zi5 = zi3[204:0];
  assign zi13 = zi3[192:161];
  assign zi15 = zi3[159:128];
  ZLL_Main_mem14  instR2 (zi13, zi15, zll_main_mem14_out);
  assign zi16 = {zll_main_mem14_out, zi5};
  assign zi17 = zi16[236:205];
  assign zi18 = zi16[204:0];
  ZLL_Main_setloc2  instR3 ({zi4, zi17}, zi18, zll_main_setloc2_out);
  ZLL_Main_perform__read23  instR4 (zll_main_setloc2_out, zll_main_perform_read23_out);
  ZLL_Main_perform__write33  instR5 (zll_main_perform_read23_out, zll_main_perform_write33_out);
  ZLL_Main_idle87  instR6 (zll_main_perform_write33_out, zll_main_idle87_out);
  ZLL_Main_idle84  instR7 (zll_main_idle87_out, zll_main_idle84_out);
  assign zi19 = {__st0, __st1};
  Main_get__addr__reg  instR8 (zi19, main_get_addr_reg_outR1);
  assign zi20 = main_get_addr_reg_outR1;
  assign zi22 = zi20[204:0];
  assign zi28 = zi20[127:96];
  assign zi30 = zi20[63:32];
  assign zi31 = zi20[31:0];
  ZLL_Main_mem02  instR9 (zi28, zi30, zi31, zll_main_mem02_out);
  assign zi33 = zi20[95:64];
  assign zi34 = zi20[63:32];
  assign zi35 = zi20[31:0];
  ZLL_Main_mem02  instR10 (zi33, zi34, zi35, zll_main_mem02_outR1);
  assign zi38 = zi20[63:32];
  assign zi39 = zi20[31:0];
  ZLL_Main_mem14  instR11 (zi38, zi39, zll_main_mem14_outR1);
  assign zi43 = zi20[31:0];
  ZLL_Main_data__out__reg3  instR12 (zi43, zll_main_data_out_reg3_out);
  assign zi44 = {((zi20[214] == 1'h0) & ((zi20[213] == 1'h0) & ((zi20[212] == 1'h0) & ((zi20[211] == 1'h0) & ((zi20[210] == 1'h0) & ((zi20[209] == 1'h0) & ((zi20[208] == 1'h0) & ((zi20[207] == 1'h0) & ((zi20[206] == 1'h0) & (zi20[205] == 1'h0)))))))))) ? zll_main_mem02_out : (((zi20[214] == 1'h0) & ((zi20[213] == 1'h0) & ((zi20[212] == 1'h0) & ((zi20[211] == 1'h0) & ((zi20[210] == 1'h0) & ((zi20[209] == 1'h0) & ((zi20[208] == 1'h0) & ((zi20[207] == 1'h0) & ((zi20[206] == 1'h0) & (zi20[205] == 1'h1)))))))))) ? zll_main_mem02_outR1 : (((zi20[214] == 1'h0) & ((zi20[213] == 1'h0) & ((zi20[212] == 1'h0) & ((zi20[211] == 1'h0) & ((zi20[210] == 1'h0) & ((zi20[209] == 1'h0) & ((zi20[208] == 1'h0) & ((zi20[207] == 1'h0) & ((zi20[206] == 1'h1) & (zi20[205] == 1'h0)))))))))) ? zll_main_mem14_outR1 : zll_main_data_out_reg3_out)), zi22};
  assign zi45 = zi44[236:205];
  assign zi46 = zi44[204:0];
  ZLL_Main_set__data__out__reg7  instR13 (zi45, zi46, zll_main_set_data_out_reg7_out);
  ZLL_Main_perform__read23  instR14 (zll_main_set_data_out_reg7_out, zll_main_perform_read23_outR1);
  ZLL_Main_perform__write33  instR15 (zll_main_perform_read23_outR1, zll_main_perform_write33_outR1);
  ZLL_Main_idle87  instR16 (zll_main_perform_write33_outR1, zll_main_idle87_outR1);
  ZLL_Main_idle84  instR17 (zll_main_idle87_outR1, zll_main_idle84_outR1);
  assign zi47 = {__st0, __st1};
  Main_get__addr__reg  instR18 (zi47, main_get_addr_reg_outR2);
  assign zi48 = main_get_addr_reg_outR2;
  assign zi49 = zi48[214:205];
  assign zi50 = zi48[204:0];
  ZLL_Main_setloc2  instR19 ({zi49, {6'h20{1'h1}}}, zi50, zll_main_setloc2_outR1);
  assign zi51 = zll_main_setloc2_outR1;
  Main_set__addr__reg1  instR20 (((zi48[214] == 1'h0) & ((zi48[213] == 1'h0) & ((zi48[212] == 1'h0) & ((zi48[211] == 1'h0) & ((zi48[210] == 1'h0) & ((zi48[209] == 1'h0) & ((zi48[208] == 1'h0) & ((zi48[207] == 1'h0) & ((zi48[206] == 1'h1) & (zi48[205] == 1'h1)))))))))) ? 10'h2 : (((zi48[214] == 1'h0) & ((zi48[213] == 1'h0) & ((zi48[212] == 1'h0) & ((zi48[211] == 1'h0) & ((zi48[210] == 1'h0) & ((zi48[209] == 1'h0) & ((zi48[208] == 1'h0) & ((zi48[207] == 1'h0) & ((zi48[206] == 1'h1) & (zi48[205] == 1'h0)))))))))) ? 10'h1 : (((zi48[214] == 1'h0) & ((zi48[213] == 1'h0) & ((zi48[212] == 1'h0) & ((zi48[211] == 1'h0) & ((zi48[210] == 1'h0) & ((zi48[209] == 1'h0) & ((zi48[208] == 1'h0) & ((zi48[207] == 1'h0) & ((zi48[206] == 1'h0) & (zi48[205] == 1'h1)))))))))) ? 10'h0 : 10'h3ff)), zi51, main_set_addr_reg1_out);
  ZLL_Main_perform__write33  instR21 (main_set_addr_reg1_out, zll_main_perform_write33_outR2);
  ZLL_Main_idle87  instR22 (zll_main_perform_write33_outR2, zll_main_idle87_outR2);
  ZLL_Main_while__addr__reg__052  instR23 (zll_main_idle87_outR2, zll_main_while_addr_reg_052_out);
  assign zi52 = {__st0, __st1};
  Main_go  instR24 (__in0, main_go_out);
  Main_rnw  instR25 (__in0, main_rnw_out);
  assign zi53 = {zi52, __in0, main_go_out, main_rnw_out};
  assign zi54 = zi53[252:48];
  assign zi55 = zi53[47:2];
  Main_partition__in  instR26 (zi55, main_partition_in_out);
  assign zi56 = main_partition_in_out;
  Main_addr__in  instR27 (zi55, main_addr_in_out);
  assign zi57 = main_addr_in_out;
  Mainzuzlzazg  instR28 (zi56, zi57, mainzuzlzazgzuout);
  Main_set__addr__reg1  instR29 (mainzuzlzazgzuout, zi54, main_set_addr_reg1_outR1);
  ZLL_Main_idle98  instR30 (zi56, main_set_addr_reg1_outR1, zll_main_idle98_out);
  ZLL_Main_perform__write33  instR31 (zll_main_idle98_out, zll_main_perform_write33_outR3);
  ZLL_Main_idle87  instR32 (zll_main_perform_write33_outR3, zll_main_idle87_outR3);
  assign zi61 = zll_main_idle87_outR3;
  assign zi62 = zi61[409:205];
  ZLL_Main_perform__write20  instR33 (zi62, zll_main_perform_write20_out);
  assign zi64 = zll_main_perform_write20_out;
  assign zi65 = zi64[204:0];
  ZLL_Main_connect4  instR34 (zi62, zll_main_connect4_out);
  ZLL_Main_connect3  instR35 (zll_main_connect4_out, zll_main_connect3_out);
  Main_go  instR36 (__in0, main_go_outR1);
  Main_rnw  instR37 (__in0, main_rnw_outR1);
  assign zi66 = {zi52, __in0, main_go_outR1, main_rnw_outR1};
  assign zi67 = zi66[252:48];
  assign zi68 = zi66[47:2];
  Main_partition__in  instR38 (zi68, main_partition_in_outR1);
  assign zi69 = main_partition_in_outR1;
  Main_addr__in  instR39 (zi68, main_addr_in_outR1);
  assign zi70 = main_addr_in_outR1;
  assign zi71 = zi66[47:16];
  assign zi76 = zi71;
  Mainzuzlzazg  instR40 (zi69, zi70, mainzuzlzazgzuoutR1);
  Main_set__addr__reg1  instR41 (mainzuzlzazgzuoutR1, zi67, main_set_addr_reg1_outR2);
  assign zi81 = main_set_addr_reg1_outR2;
  assign zi86 = zi81[127:0];
  assign zi87 = zi81[204:203];
  assign zi88 = zi81[202:193];
  assign zi90 = zi81[160];
  assign zi91 = zi81[159:128];
  ZLL_Main_idle98  instR42 (zi69, {{zi87, zi88, zi76, zi90, zi91}, zi86}, zll_main_idle98_outR1);
  ZLL_Main_perform__write33  instR43 (zll_main_idle98_outR1, zll_main_perform_write33_outR4);
  ZLL_Main_idle87  instR44 (zll_main_perform_write33_outR4, zll_main_idle87_outR4);
  assign zi92 = zll_main_idle87_outR4;
  assign zi93 = zi92[409:205];
  ZLL_Main_perform__write20  instR45 (zi93, zll_main_perform_write20_outR1);
  assign zi95 = zll_main_perform_write20_outR1;
  assign zi96 = zi95[204:0];
  ZLL_Main_connect4  instR46 (zi93, zll_main_connect4_outR1);
  ZLL_Main_connect3  instR47 (zll_main_connect4_outR1, zll_main_connect3_outR1);
  Main_set__ack__reg1  instR48 (zi52, main_set_ack_reg1_out);
  assign zi97 = main_set_ack_reg1_out;
  ZLL_Main_idle83  instR49 (zi97, zll_main_idle83_out);
  ZLL_Main_perform__write33  instR50 (zll_main_idle83_out, zll_main_perform_write33_outR5);
  ZLL_Main_idle87  instR51 (zll_main_perform_write33_outR5, zll_main_idle87_outR5);
  ZLL_Main_idle84  instR52 (zll_main_idle87_outR5, zll_main_idle84_outR2);
  assign zi98 = {__st0, __st1};
  ZLL_Main_idle87  instR53 ({zi98, zi98}, zll_main_idle87_outR6);
  ZLL_Main_while__addr__reg__052  instR54 (zll_main_idle87_outR6, zll_main_while_addr_reg_052_outR1);
  assign zres = (__resumption_tag == 3'h1) ? zll_main_idle84_out : ((__resumption_tag == 3'h2) ? zll_main_idle84_outR1 : ((__resumption_tag == 3'h3) ? zll_main_while_addr_reg_052_out : ((__resumption_tag == 3'h4) ? (((zi53[1] == 1'h1) & (zi53[0] == 1'h1)) ? {{1'h1, {8'hab{1'h0}}}, zll_main_connect3_out, 3'h2, zi65} : (((zi66[1] == 1'h1) & (zi66[0] == 1'h0)) ? {{1'h1, {8'hab{1'h0}}}, zll_main_connect3_outR1, 3'h1, zi96} : zll_main_idle84_outR2)) : zll_main_while_addr_reg_052_outR1)));
  assign __resumption_tag_next = zres[207:205];
  assign __st0_next = zres[204:128];
  assign __st1_next = zres[127:0];
  assign __out0 = zres[240:208];
  initial {__resumption_tag, __st0, __st1} = 208'h7fffffffeffffffffffffffffffffffffffffffffffffffff;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 208'h7fffffffeffffffffffffffffffffffffffffffffffffffff;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_perform__write33 (input logic [204:0] arg0,
  output logic [409:0] res);
  assign res = {arg0, arg0};
endmodule

module Main_set__ack__reg1 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] zll_main_set_ack_reg13_out;
  ZLL_Main_set__ack__reg13  inst (1'h0, arg0, zll_main_set_ack_reg13_out);
  assign res = zll_main_set_ack_reg13_out;
endmodule

module ZLL_Main_data__out__reg3 (input logic [31:0] arg0,
  output logic [31:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_while__addr__reg__052 (input logic [412:0] arg0,
  output logic [412:0] res);
  logic [204:0] zi0;
  logic [204:0] zi1;
  logic [214:0] main_get_addr_reg_out;
  logic [214:0] zi2;
  logic [9:0] zi3;
  logic [204:0] zi4;
  logic [412:0] zi5;
  logic [9:0] zi6;
  logic [204:0] zi7;
  logic [0:0] main_scrubbing_out;
  logic [410:0] zi8;
  logic [204:0] zi10;
  logic [412:0] zll_main_perform_write20_out;
  logic [412:0] zi11;
  logic [204:0] zi12;
  logic [76:0] zll_main_connect4_out;
  logic [32:0] zll_main_connect3_out;
  logic [0:0] main_scrubbing_outR1;
  logic [205:0] zi13;
  logic [204:0] zi14;
  logic [412:0] zll_main_idle87_out;
  logic [412:0] zi15;
  logic [204:0] zi17;
  logic [76:0] zi18;
  logic [32:0] zll_main_connect3_outR1;
  assign zi0 = arg0[409:205];
  assign zi1 = arg0[204:0];
  Main_get__addr__reg  inst (zi1, main_get_addr_reg_out);
  assign zi2 = main_get_addr_reg_out;
  assign zi3 = zi2[214:205];
  assign zi4 = zi2[204:0];
  assign zi5 = {{8'hc6{1'h0}}, zi3, zi4};
  assign zi6 = zi5[214:205];
  assign zi7 = zi5[204:0];
  Main_scrubbing  instR1 (zi6, main_scrubbing_out);
  assign zi8 = {zi7, zi0, main_scrubbing_out};
  assign zi10 = zi8[205:1];
  ZLL_Main_perform__write20  instR2 (zi10, zll_main_perform_write20_out);
  assign zi11 = zll_main_perform_write20_out;
  assign zi12 = zi11[204:0];
  ZLL_Main_connect4  instR3 (zi10, zll_main_connect4_out);
  ZLL_Main_connect3  instR4 (zll_main_connect4_out, zll_main_connect3_out);
  Main_scrubbing  instR5 (zi6, main_scrubbing_outR1);
  assign zi13 = {zi7, main_scrubbing_outR1};
  assign zi14 = zi13[205:1];
  ZLL_Main_idle87  instR6 ({zi14, zi14}, zll_main_idle87_out);
  assign zi15 = zll_main_idle87_out;
  assign zi17 = zi15[204:0];
  assign zi18 = zi15[409:333];
  ZLL_Main_connect3  instR7 (zi18, zll_main_connect3_outR1);
  assign res = (zi8[0] == 1'h1) ? {{1'h1, {8'hab{1'h0}}}, zll_main_connect3_out, 3'h3, zi12} : {{1'h1, {8'hab{1'h0}}}, zll_main_connect3_outR1, 3'h4, zi17};
endmodule

module ZLL_Main_set__ack__reg13 (input logic [0:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi4;
  logic [1:0] zi5;
  logic [9:0] zi6;
  logic [31:0] zi7;
  logic [31:0] zi9;
  assign zi4 = arg1[127:0];
  assign zi5 = arg1[204:203];
  assign zi6 = arg1[202:193];
  assign zi7 = arg1[192:161];
  assign zi9 = arg1[159:128];
  assign res = {{zi5, zi6, zi7, arg0, zi9}, zi4};
endmodule

module ZLL_Main_idle98 (input logic [1:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi4;
  logic [9:0] zi6;
  logic [31:0] zi7;
  logic [0:0] zi8;
  logic [31:0] zi9;
  logic [76:0] zll_main_set_partition_reg3_out;
  logic [204:0] zi10;
  logic [204:0] main_set_ack_reg1_out;
  logic [204:0] zi11;
  logic [204:0] zll_main_idle83_out;
  assign zi4 = arg1[127:0];
  assign zi6 = arg1[202:193];
  assign zi7 = arg1[192:161];
  assign zi8 = arg1[160];
  assign zi9 = arg1[159:128];
  ZLL_Main_set__partition__reg3  inst (zi6, arg0, zi7, zi8, zi9, zll_main_set_partition_reg3_out);
  assign zi10 = {zll_main_set_partition_reg3_out, zi4};
  Main_set__ack__reg1  instR1 (zi10, main_set_ack_reg1_out);
  assign zi11 = main_set_ack_reg1_out;
  ZLL_Main_idle83  instR2 (zi11, zll_main_idle83_out);
  assign res = zll_main_idle83_out;
endmodule

module ZLL_Main_perform__read23 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] zll_main_set_ack_reg13_out;
  ZLL_Main_set__ack__reg13  inst (1'h1, arg0, zll_main_set_ack_reg13_out);
  assign res = zll_main_set_ack_reg13_out;
endmodule

module ZLL_Main_connect4 (input logic [204:0] arg0,
  output logic [76:0] res);
  logic [76:0] zi0;
  assign zi0 = arg0[204:128];
  assign res = zi0;
endmodule

module Main_set__addr__reg1 (input logic [9:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi4;
  logic [1:0] zi5;
  logic [31:0] zi7;
  logic [0:0] zi8;
  logic [31:0] zi9;
  logic [76:0] zll_main_set_partition_reg3_out;
  assign zi4 = arg1[127:0];
  assign zi5 = arg1[204:203];
  assign zi7 = arg1[192:161];
  assign zi8 = arg1[160];
  assign zi9 = arg1[159:128];
  ZLL_Main_set__partition__reg3  inst (arg0, zi5, zi7, zi8, zi9, zll_main_set_partition_reg3_out);
  assign res = {zll_main_set_partition_reg3_out, zi4};
endmodule

module ZLL_Main_idle87 (input logic [409:0] arg0,
  output logic [412:0] res);
  logic [204:0] zi0;
  logic [204:0] zi1;
  assign zi0 = arg0[409:205];
  assign zi1 = arg0[204:0];
  assign res = {3'h1, zi0, zi1};
endmodule

module ZLL_Main_idle84 (input logic [412:0] arg0,
  output logic [412:0] res);
  logic [204:0] zi0;
  logic [412:0] zll_main_perform_write20_out;
  logic [412:0] zi2;
  logic [204:0] zi3;
  logic [76:0] zll_main_connect4_out;
  logic [32:0] zll_main_connect3_out;
  assign zi0 = arg0[409:205];
  ZLL_Main_perform__write20  inst (zi0, zll_main_perform_write20_out);
  assign zi2 = zll_main_perform_write20_out;
  assign zi3 = zi2[204:0];
  ZLL_Main_connect4  instR1 (zi0, zll_main_connect4_out);
  ZLL_Main_connect3  instR2 (zll_main_connect4_out, zll_main_connect3_out);
  assign res = {{1'h1, {8'hab{1'h0}}}, zll_main_connect3_out, 3'h4, zi3};
endmodule

module ZLL_Main_idle83 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] main_set_data_out_reg_out;
  Main_set__data__out__reg  inst (arg0, main_set_data_out_reg_out);
  assign res = main_set_data_out_reg_out;
endmodule

module ZLL_Main_connect3 (input logic [76:0] arg0,
  output logic [32:0] res);
  logic [0:0] zi3;
  logic [31:0] zi9;
  logic [31:0] zll_main_data_out_reg3_out;
  assign zi3 = arg0[32];
  assign zi9 = arg0[31:0];
  ZLL_Main_data__out__reg3  inst (zi9, zll_main_data_out_reg3_out);
  assign res = {zi3, zll_main_data_out_reg3_out};
endmodule

module ZLL_Main_perform__write20 (input logic [204:0] arg0,
  output logic [412:0] res);
  assign res = {{2'h1, {8'hce{1'h0}}}, arg0};
endmodule

module ZLL_Main_memTweak18 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  input logic [31:0] arg2,
  input logic [31:0] arg3,
  output logic [127:0] res);
  assign res = {arg0, arg1, arg2, arg3};
endmodule

module Main_get__addr__reg (input logic [204:0] arg0,
  output logic [214:0] res);
  logic [9:0] zi6;
  assign zi6 = arg0[202:193];
  assign res = {zi6, arg0};
endmodule

module ZLL_Main_set__data__out__reg7 (input logic [31:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi4;
  logic [1:0] zi5;
  logic [9:0] zi6;
  logic [31:0] zi7;
  logic [0:0] zi8;
  assign zi4 = arg1[127:0];
  assign zi5 = arg1[204:203];
  assign zi6 = arg1[202:193];
  assign zi7 = arg1[192:161];
  assign zi8 = arg1[160];
  assign res = {{zi5, zi6, zi7, zi8, arg0}, zi4};
endmodule

module Main_partition__in (input logic [45:0] arg0,
  output logic [1:0] res);
  logic [1:0] zi4;
  assign zi4 = arg0[1:0];
  assign res = zi4;
endmodule

module Main_rnw (input logic [45:0] arg0,
  output logic [0:0] res);
  logic [0:0] zi3;
  logic [1:0] zi4;
  logic [0:0] zll_main_rnw1_out;
  assign zi3 = arg0[2];
  assign zi4 = arg0[1:0];
  ZLL_Main_rnw1  inst (zi3, zi4, zll_main_rnw1_out);
  assign res = zll_main_rnw1_out;
endmodule

module ZLL_Main_mem14 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  output logic [31:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_mem02 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  input logic [31:0] arg2,
  output logic [31:0] res);
  logic [31:0] zll_main_mem14_out;
  ZLL_Main_mem14  inst (arg0, arg2, zll_main_mem14_out);
  assign res = zll_main_mem14_out;
endmodule

module ZLL_Main_setloc2 (input logic [41:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [31:0] zi1;
  logic [76:0] zi5;
  logic [31:0] zi11;
  logic [31:0] zi12;
  logic [31:0] zi13;
  logic [127:0] zll_main_memtweak18_out;
  logic [31:0] zi17;
  logic [31:0] zi19;
  logic [31:0] zi20;
  logic [127:0] zll_main_memtweak18_outR1;
  logic [31:0] zi24;
  logic [31:0] zi25;
  logic [31:0] zi27;
  logic [31:0] zi31;
  logic [31:0] zi32;
  logic [31:0] zi33;
  assign zi1 = arg0[31:0];
  assign zi5 = arg1[204:128];
  assign zi11 = arg1[95:64];
  assign zi12 = arg1[63:32];
  assign zi13 = arg1[31:0];
  ZLL_Main_memTweak18  inst (zi1, zi11, zi12, zi13, zll_main_memtweak18_out);
  assign zi17 = arg1[127:96];
  assign zi19 = arg1[63:32];
  assign zi20 = arg1[31:0];
  ZLL_Main_memTweak18  instR1 (zi17, zi1, zi19, zi20, zll_main_memtweak18_outR1);
  assign zi24 = arg1[127:96];
  assign zi25 = arg1[95:64];
  assign zi27 = arg1[31:0];
  assign zi31 = arg1[127:96];
  assign zi32 = arg1[95:64];
  assign zi33 = arg1[63:32];
  assign res = {zi5, ((arg0[41] == 1'h0) & ((arg0[40] == 1'h0) & ((arg0[39] == 1'h0) & ((arg0[38] == 1'h0) & ((arg0[37] == 1'h0) & ((arg0[36] == 1'h0) & ((arg0[35] == 1'h0) & ((arg0[34] == 1'h0) & ((arg0[33] == 1'h0) & (arg0[32] == 1'h0)))))))))) ? zll_main_memtweak18_out : (((arg0[41] == 1'h0) & ((arg0[40] == 1'h0) & ((arg0[39] == 1'h0) & ((arg0[38] == 1'h0) & ((arg0[37] == 1'h0) & ((arg0[36] == 1'h0) & ((arg0[35] == 1'h0) & ((arg0[34] == 1'h0) & ((arg0[33] == 1'h0) & (arg0[32] == 1'h1)))))))))) ? zll_main_memtweak18_outR1 : (((arg0[41] == 1'h0) & ((arg0[40] == 1'h0) & ((arg0[39] == 1'h0) & ((arg0[38] == 1'h0) & ((arg0[37] == 1'h0) & ((arg0[36] == 1'h0) & ((arg0[35] == 1'h0) & ((arg0[34] == 1'h0) & ((arg0[33] == 1'h1) & (arg0[32] == 1'h0)))))))))) ? {zi24, zi25, zi1, zi27} : {zi31, zi32, zi33, zi1}))};
endmodule

module Mainzuzlzazg (input logic [1:0] arg0,
  input logic [9:0] arg1,
  output logic [9:0] res);
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [0:0] zi5;
  logic [0:0] zi6;
  logic [0:0] zi7;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [0:0] zi10;
  logic [0:0] zi11;
  logic [0:0] zi12;
  assign zi1 = arg0[1];
  assign zi2 = arg0[0];
  assign zi5 = arg1[7];
  assign zi6 = arg1[6];
  assign zi7 = arg1[5];
  assign zi8 = arg1[4];
  assign zi9 = arg1[3];
  assign zi10 = arg1[2];
  assign zi11 = arg1[1];
  assign zi12 = arg1[0];
  assign res = {zi1, zi2, zi5, zi6, zi7, zi8, zi9, zi10, zi11, zi12};
endmodule

module ZLL_Main_rnw1 (input logic [0:0] arg0,
  input logic [1:0] arg1,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module Main_scrubbing (input logic [9:0] arg0,
  output logic [0:0] res);
  assign res = ((arg0[9] == 1'h1) & ((arg0[8] == 1'h1) & ((arg0[7] == 1'h1) & ((arg0[6] == 1'h1) & ((arg0[5] == 1'h1) & ((arg0[4] == 1'h1) & ((arg0[3] == 1'h1) & ((arg0[2] == 1'h1) & ((arg0[1] == 1'h1) & (arg0[0] == 1'h1)))))))))) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_set__partition__reg3 (input logic [9:0] arg0,
  input logic [1:0] arg1,
  input logic [31:0] arg2,
  input logic [0:0] arg3,
  input logic [31:0] arg4,
  output logic [76:0] res);
  assign res = {arg1, arg0, arg2, arg3, arg4};
endmodule

module Main_set__data__out__reg (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] zll_main_set_data_out_reg7_out;
  ZLL_Main_set__data__out__reg7  inst ({6'h20{1'h1}}, arg0, zll_main_set_data_out_reg7_out);
  assign res = zll_main_set_data_out_reg7_out;
endmodule

module Main_go (input logic [45:0] arg0,
  output logic [0:0] res);
  logic [0:0] zi2;
  logic [1:0] zi4;
  logic [0:0] zll_main_rnw1_out;
  assign zi2 = arg0[3];
  assign zi4 = arg0[1:0];
  ZLL_Main_rnw1  inst (zi2, zi4, zll_main_rnw1_out);
  assign res = zll_main_rnw1_out;
endmodule

module Main_addr__in (input logic [45:0] arg0,
  output logic [9:0] res);
  logic [9:0] zi1;
  assign zi1 = arg0[13:4];
  assign res = zi1;
endmodule