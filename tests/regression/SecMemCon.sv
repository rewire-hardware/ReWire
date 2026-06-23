module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [45:0] __in0,
  output logic [32:0] __out0);
  logic [207:0] __resumption_tag;
  logic [207:0] __resumption_tag_next;
  logic [76:0] __st0;
  logic [76:0] __st0_next;
  logic [127:0] __st1;
  logic [127:0] __st1_next;
  logic [204:0] zi1;
  logic [204:0] zi2;
  logic [446:0] main_while_addr_reg_0_out;
  logic [204:0] zi3;
  logic [204:0] main_set_data_out_reg_out;
  logic [204:0] zi4;
  logic [214:0] main_get_addr_reg_out;
  logic [214:0] zi5;
  logic [9:0] zi6;
  logic [204:0] zi7;
  logic [31:0] zi15;
  logic [31:0] zi17;
  logic [31:0] zll_main_mem01_out;
  logic [236:0] zi18;
  logic [31:0] zi19;
  logic [204:0] zi20;
  logic [204:0] zll_main_setloc3_out;
  logic [204:0] zll_main_perform_write19_out;
  logic [409:0] zll_main_perform_write34_out;
  logic [446:0] zll_main_perform_read22_out;
  logic [446:0] zll_main_perform_write33_out;
  logic [204:0] zi21;
  logic [214:0] main_get_addr_reg_outR1;
  logic [214:0] zi22;
  logic [204:0] zi24;
  logic [31:0] zi30;
  logic [31:0] zi32;
  logic [31:0] zi33;
  logic [31:0] zll_main_mem02_out;
  logic [31:0] zi35;
  logic [31:0] zi36;
  logic [31:0] zi37;
  logic [31:0] zll_main_mem02_outR1;
  logic [31:0] zi40;
  logic [31:0] zi41;
  logic [31:0] zll_main_mem01_outR1;
  logic [31:0] zi45;
  logic [31:0] zll_main_data_out_reg3_out;
  logic [236:0] zi46;
  logic [31:0] zi47;
  logic [204:0] zi48;
  logic [204:0] zll_main_set_data_out_reg6_out;
  logic [204:0] zll_main_perform_write19_outR1;
  logic [409:0] zll_main_perform_write34_outR1;
  logic [446:0] zll_main_perform_read22_outR1;
  logic [446:0] zll_main_perform_write33_outR1;
  logic [204:0] zi49;
  logic [0:0] main_go_out;
  logic [0:0] main_rnw_out;
  logic [252:0] zi50;
  logic [204:0] zi51;
  logic [45:0] zi52;
  logic [1:0] main_partition_in_out;
  logic [1:0] zi53;
  logic [9:0] main_addr_in_out;
  logic [9:0] zi54;
  logic [9:0] mainzuzlzazgzuout;
  logic [204:0] main_set_addr_reg_out;
  logic [204:0] zll_main_idle75_out;
  logic [409:0] zll_main_perform_write34_outR2;
  logic [446:0] zll_main_perform_read22_outR2;
  logic [446:0] zi58;
  logic [204:0] zi59;
  logic [446:0] zll_main_idle115_out;
  logic [446:0] zi61;
  logic [204:0] zi62;
  logic [76:0] zll_main_connect4_out;
  logic [32:0] zll_main_while_addr_reg_023_out;
  logic [0:0] main_go_outR1;
  logic [0:0] main_rnw_outR1;
  logic [252:0] zi63;
  logic [204:0] zi64;
  logic [45:0] zi65;
  logic [1:0] main_partition_in_outR1;
  logic [1:0] zi66;
  logic [9:0] main_addr_in_outR1;
  logic [9:0] zi67;
  logic [31:0] zi68;
  logic [31:0] zi73;
  logic [9:0] mainzuzlzazgzuoutR1;
  logic [204:0] main_set_addr_reg_outR1;
  logic [204:0] zi78;
  logic [127:0] zi83;
  logic [1:0] zi84;
  logic [9:0] zi85;
  logic [0:0] zi87;
  logic [31:0] zi88;
  logic [204:0] zll_main_idle75_outR1;
  logic [409:0] zll_main_perform_write34_outR3;
  logic [446:0] zll_main_perform_read22_outR3;
  logic [446:0] zi89;
  logic [204:0] zi90;
  logic [446:0] zll_main_idle115_outR1;
  logic [446:0] zi92;
  logic [204:0] zi93;
  logic [76:0] zll_main_connect4_outR1;
  logic [32:0] zll_main_while_addr_reg_023_outR1;
  logic [204:0] main_set_ack_reg_out;
  logic [204:0] zi94;
  logic [204:0] zll_main_idle102_out;
  logic [409:0] zll_main_perform_write34_outR4;
  logic [446:0] zll_main_perform_read22_outR4;
  logic [446:0] zll_main_perform_write33_outR2;
  logic [204:0] zi95;
  logic [214:0] main_get_addr_reg_outR2;
  logic [214:0] zi96;
  logic [9:0] zi97;
  logic [204:0] zi98;
  logic [204:0] zll_main_setloc3_outR1;
  logic [204:0] zi99;
  logic [0:0] zi100;
  logic [0:0] zi101;
  logic [0:0] zi102;
  logic [0:0] zi103;
  logic [0:0] zi104;
  logic [0:0] zi105;
  logic [0:0] zi106;
  logic [0:0] zi107;
  logic [0:0] zi108;
  logic [0:0] zi120;
  logic [0:0] zi121;
  logic [0:0] zi122;
  logic [0:0] zi123;
  logic [0:0] zi124;
  logic [0:0] zi125;
  logic [0:0] zi126;
  logic [0:0] zi127;
  logic [0:0] zi139;
  logic [0:0] zi140;
  logic [0:0] zi141;
  logic [0:0] zi142;
  logic [0:0] zi143;
  logic [0:0] zi144;
  logic [0:0] zi145;
  logic [0:0] zi157;
  logic [0:0] zi158;
  logic [0:0] zi159;
  logic [0:0] zi160;
  logic [0:0] zi161;
  logic [0:0] zi162;
  logic [0:0] zi174;
  logic [0:0] zi175;
  logic [0:0] zi176;
  logic [0:0] zi177;
  logic [0:0] zi178;
  logic [0:0] zi190;
  logic [0:0] zi191;
  logic [0:0] zi192;
  logic [0:0] zi193;
  logic [0:0] zi205;
  logic [0:0] zi206;
  logic [0:0] zi207;
  logic [0:0] zi219;
  logic [0:0] zi220;
  logic [204:0] main_set_addr_reg_outR2;
  logic [409:0] zll_main_perform_write34_outR5;
  logic [446:0] zll_main_perform_read22_outR5;
  logic [446:0] zi232;
  logic [204:0] zi233;
  logic [204:0] zi234;
  logic [446:0] main_while_addr_reg_0_outR1;
  logic [446:0] zres;
  assign zi1 = __resumption_tag[204:0];
  assign zi2 = {__st0, __st1};
  Main_while__addr__reg__0  inst (zi1, zi2, main_while_addr_reg_0_out);
  assign zi3 = {__st0, __st1};
  Main_set__data__out__reg  instR1 (zi3, main_set_data_out_reg_out);
  assign zi4 = main_set_data_out_reg_out;
  Main_get__addr__reg  instR2 (zi4, main_get_addr_reg_out);
  assign zi5 = main_get_addr_reg_out;
  assign zi6 = zi5[214:205];
  assign zi7 = zi5[204:0];
  assign zi15 = zi5[192:161];
  assign zi17 = zi5[159:128];
  ZLL_Main_mem01  instR3 (zi15, zi17, zll_main_mem01_out);
  assign zi18 = {zll_main_mem01_out, zi7};
  assign zi19 = zi18[236:205];
  assign zi20 = zi18[204:0];
  ZLL_Main_setloc3  instR4 ({zi6, zi19}, zi20, zll_main_setloc3_out);
  ZLL_Main_perform__write19  instR5 (zll_main_setloc3_out, zll_main_perform_write19_out);
  ZLL_Main_perform__write34  instR6 (zll_main_perform_write19_out, zll_main_perform_write34_out);
  ZLL_Main_perform__read22  instR7 (zll_main_perform_write34_out, zll_main_perform_read22_out);
  ZLL_Main_perform__write33  instR8 (zll_main_perform_read22_out, zll_main_perform_write33_out);
  assign zi21 = {__st0, __st1};
  Main_get__addr__reg  instR9 (zi21, main_get_addr_reg_outR1);
  assign zi22 = main_get_addr_reg_outR1;
  assign zi24 = zi22[204:0];
  assign zi30 = zi22[127:96];
  assign zi32 = zi22[63:32];
  assign zi33 = zi22[31:0];
  ZLL_Main_mem02  instR10 (zi30, zi32, zi33, zll_main_mem02_out);
  assign zi35 = zi22[95:64];
  assign zi36 = zi22[63:32];
  assign zi37 = zi22[31:0];
  ZLL_Main_mem02  instR11 (zi35, zi36, zi37, zll_main_mem02_outR1);
  assign zi40 = zi22[63:32];
  assign zi41 = zi22[31:0];
  ZLL_Main_mem01  instR12 (zi40, zi41, zll_main_mem01_outR1);
  assign zi45 = zi22[31:0];
  ZLL_Main_data__out__reg3  instR13 (zi45, zll_main_data_out_reg3_out);
  assign zi46 = {((zi22[214] == 1'h0) & ((zi22[213] == 1'h0) & ((zi22[212] == 1'h0) & ((zi22[211] == 1'h0) & ((zi22[210] == 1'h0) & ((zi22[209] == 1'h0) & ((zi22[208] == 1'h0) & ((zi22[207] == 1'h0) & ((zi22[206] == 1'h0) & (zi22[205] == 1'h0)))))))))) ? zll_main_mem02_out : (((zi22[214] == 1'h0) & ((zi22[213] == 1'h0) & ((zi22[212] == 1'h0) & ((zi22[211] == 1'h0) & ((zi22[210] == 1'h0) & ((zi22[209] == 1'h0) & ((zi22[208] == 1'h0) & ((zi22[207] == 1'h0) & ((zi22[206] == 1'h0) & (zi22[205] == 1'h1)))))))))) ? zll_main_mem02_outR1 : (((zi22[214] == 1'h0) & ((zi22[213] == 1'h0) & ((zi22[212] == 1'h0) & ((zi22[211] == 1'h0) & ((zi22[210] == 1'h0) & ((zi22[209] == 1'h0) & ((zi22[208] == 1'h0) & ((zi22[207] == 1'h0) & ((zi22[206] == 1'h1) & (zi22[205] == 1'h0)))))))))) ? zll_main_mem01_outR1 : zll_main_data_out_reg3_out)), zi24};
  assign zi47 = zi46[236:205];
  assign zi48 = zi46[204:0];
  ZLL_Main_set__data__out__reg6  instR14 (zi47, zi48, zll_main_set_data_out_reg6_out);
  ZLL_Main_perform__write19  instR15 (zll_main_set_data_out_reg6_out, zll_main_perform_write19_outR1);
  ZLL_Main_perform__write34  instR16 (zll_main_perform_write19_outR1, zll_main_perform_write34_outR1);
  ZLL_Main_perform__read22  instR17 (zll_main_perform_write34_outR1, zll_main_perform_read22_outR1);
  ZLL_Main_perform__write33  instR18 (zll_main_perform_read22_outR1, zll_main_perform_write33_outR1);
  assign zi49 = {__st0, __st1};
  Main_go  instR19 (__in0, main_go_out);
  Main_rnw  instR20 (__in0, main_rnw_out);
  assign zi50 = {zi49, __in0, main_go_out, main_rnw_out};
  assign zi51 = zi50[252:48];
  assign zi52 = zi50[47:2];
  Main_partition__in  instR21 (zi52, main_partition_in_out);
  assign zi53 = main_partition_in_out;
  Main_addr__in  instR22 (zi52, main_addr_in_out);
  assign zi54 = main_addr_in_out;
  Mainzuzlzazg  instR23 (zi53, zi54, mainzuzlzazgzuout);
  Main_set__addr__reg  instR24 (mainzuzlzazgzuout, zi51, main_set_addr_reg_out);
  ZLL_Main_idle75  instR25 (zi53, main_set_addr_reg_out, zll_main_idle75_out);
  ZLL_Main_perform__write34  instR26 (zll_main_idle75_out, zll_main_perform_write34_outR2);
  ZLL_Main_perform__read22  instR27 (zll_main_perform_write34_outR2, zll_main_perform_read22_outR2);
  assign zi58 = zll_main_perform_read22_outR2;
  assign zi59 = zi58[409:205];
  ZLL_Main_idle115  instR28 (zi59, zll_main_idle115_out);
  assign zi61 = zll_main_idle115_out;
  assign zi62 = zi61[204:0];
  ZLL_Main_connect4  instR29 (zi59, zll_main_connect4_out);
  ZLL_Main_while__addr__reg__023  instR30 (zll_main_connect4_out, zll_main_while_addr_reg_023_out);
  Main_go  instR31 (__in0, main_go_outR1);
  Main_rnw  instR32 (__in0, main_rnw_outR1);
  assign zi63 = {zi49, __in0, main_go_outR1, main_rnw_outR1};
  assign zi64 = zi63[252:48];
  assign zi65 = zi63[47:2];
  Main_partition__in  instR33 (zi65, main_partition_in_outR1);
  assign zi66 = main_partition_in_outR1;
  Main_addr__in  instR34 (zi65, main_addr_in_outR1);
  assign zi67 = main_addr_in_outR1;
  assign zi68 = zi63[47:16];
  assign zi73 = zi68;
  Mainzuzlzazg  instR35 (zi66, zi67, mainzuzlzazgzuoutR1);
  Main_set__addr__reg  instR36 (mainzuzlzazgzuoutR1, zi64, main_set_addr_reg_outR1);
  assign zi78 = main_set_addr_reg_outR1;
  assign zi83 = zi78[127:0];
  assign zi84 = zi78[204:203];
  assign zi85 = zi78[202:193];
  assign zi87 = zi78[160];
  assign zi88 = zi78[159:128];
  ZLL_Main_idle75  instR37 (zi66, {{zi84, zi85, zi73, zi87, zi88}, zi83}, zll_main_idle75_outR1);
  ZLL_Main_perform__write34  instR38 (zll_main_idle75_outR1, zll_main_perform_write34_outR3);
  ZLL_Main_perform__read22  instR39 (zll_main_perform_write34_outR3, zll_main_perform_read22_outR3);
  assign zi89 = zll_main_perform_read22_outR3;
  assign zi90 = zi89[409:205];
  ZLL_Main_idle115  instR40 (zi90, zll_main_idle115_outR1);
  assign zi92 = zll_main_idle115_outR1;
  assign zi93 = zi92[204:0];
  ZLL_Main_connect4  instR41 (zi90, zll_main_connect4_outR1);
  ZLL_Main_while__addr__reg__023  instR42 (zll_main_connect4_outR1, zll_main_while_addr_reg_023_outR1);
  Main_set__ack__reg  instR43 (zi49, main_set_ack_reg_out);
  assign zi94 = main_set_ack_reg_out;
  ZLL_Main_idle102  instR44 (zi94, zll_main_idle102_out);
  ZLL_Main_perform__write34  instR45 (zll_main_idle102_out, zll_main_perform_write34_outR4);
  ZLL_Main_perform__read22  instR46 (zll_main_perform_write34_outR4, zll_main_perform_read22_outR4);
  ZLL_Main_perform__write33  instR47 (zll_main_perform_read22_outR4, zll_main_perform_write33_outR2);
  assign zi95 = {__st0, __st1};
  Main_get__addr__reg  instR48 (zi95, main_get_addr_reg_outR2);
  assign zi96 = main_get_addr_reg_outR2;
  assign zi97 = zi96[214:205];
  assign zi98 = zi96[204:0];
  ZLL_Main_setloc3  instR49 ({zi97, {6'h20{1'h1}}}, zi98, zll_main_setloc3_outR1);
  assign zi99 = zll_main_setloc3_outR1;
  assign zi100 = zi96[213];
  assign zi101 = zi96[212];
  assign zi102 = zi96[211];
  assign zi103 = zi96[210];
  assign zi104 = zi96[209];
  assign zi105 = zi96[208];
  assign zi106 = zi96[207];
  assign zi107 = zi96[206];
  assign zi108 = zi96[205];
  assign zi120 = zi96[212];
  assign zi121 = zi96[211];
  assign zi122 = zi96[210];
  assign zi123 = zi96[209];
  assign zi124 = zi96[208];
  assign zi125 = zi96[207];
  assign zi126 = zi96[206];
  assign zi127 = zi96[205];
  assign zi139 = zi96[211];
  assign zi140 = zi96[210];
  assign zi141 = zi96[209];
  assign zi142 = zi96[208];
  assign zi143 = zi96[207];
  assign zi144 = zi96[206];
  assign zi145 = zi96[205];
  assign zi157 = zi96[210];
  assign zi158 = zi96[209];
  assign zi159 = zi96[208];
  assign zi160 = zi96[207];
  assign zi161 = zi96[206];
  assign zi162 = zi96[205];
  assign zi174 = zi96[209];
  assign zi175 = zi96[208];
  assign zi176 = zi96[207];
  assign zi177 = zi96[206];
  assign zi178 = zi96[205];
  assign zi190 = zi96[208];
  assign zi191 = zi96[207];
  assign zi192 = zi96[206];
  assign zi193 = zi96[205];
  assign zi205 = zi96[207];
  assign zi206 = zi96[206];
  assign zi207 = zi96[205];
  assign zi219 = zi96[206];
  assign zi220 = zi96[205];
  Main_set__addr__reg  instR50 ((zi96[214] == 1'h1) ? {1'h0, zi100, zi101, zi102, zi103, zi104, zi105, zi106, zi107, zi108} : (((zi96[214] == 1'h0) & (zi96[213] == 1'h1)) ? {2'h2, zi120, zi121, zi122, zi123, zi124, zi125, zi126, zi127} : (((zi96[214] == 1'h0) & ((zi96[213] == 1'h0) & (zi96[212] == 1'h1))) ? {3'h6, zi139, zi140, zi141, zi142, zi143, zi144, zi145} : (((zi96[214] == 1'h0) & ((zi96[213] == 1'h0) & ((zi96[212] == 1'h0) & (zi96[211] == 1'h1)))) ? {4'he, zi157, zi158, zi159, zi160, zi161, zi162} : (((zi96[214] == 1'h0) & ((zi96[213] == 1'h0) & ((zi96[212] == 1'h0) & ((zi96[211] == 1'h0) & (zi96[210] == 1'h1))))) ? {5'h1e, zi174, zi175, zi176, zi177, zi178} : (((zi96[214] == 1'h0) & ((zi96[213] == 1'h0) & ((zi96[212] == 1'h0) & ((zi96[211] == 1'h0) & ((zi96[210] == 1'h0) & (zi96[209] == 1'h1)))))) ? {6'h3e, zi190, zi191, zi192, zi193} : (((zi96[214] == 1'h0) & ((zi96[213] == 1'h0) & ((zi96[212] == 1'h0) & ((zi96[211] == 1'h0) & ((zi96[210] == 1'h0) & ((zi96[209] == 1'h0) & (zi96[208] == 1'h1))))))) ? {7'h7e, zi205, zi206, zi207} : (((zi96[214] == 1'h0) & ((zi96[213] == 1'h0) & ((zi96[212] == 1'h0) & ((zi96[211] == 1'h0) & ((zi96[210] == 1'h0) & ((zi96[209] == 1'h0) & ((zi96[208] == 1'h0) & (zi96[207] == 1'h1)))))))) ? {8'hfe, zi219, zi220} : 10'h3fe))))))), zi99, main_set_addr_reg_outR2);
  ZLL_Main_perform__write34  instR51 (main_set_addr_reg_outR2, zll_main_perform_write34_outR5);
  ZLL_Main_perform__read22  instR52 (zll_main_perform_write34_outR5, zll_main_perform_read22_outR5);
  assign zi232 = zll_main_perform_read22_outR5;
  assign zi233 = zi232[409:205];
  assign zi234 = zi232[204:0];
  Main_while__addr__reg__0  instR53 (zi233, zi234, main_while_addr_reg_0_outR1);
  assign zres = (__resumption_tag[207:205] == 3'h1) ? main_while_addr_reg_0_out : ((__resumption_tag[207:205] == 3'h2) ? zll_main_perform_write33_out : ((__resumption_tag[207:205] == 3'h3) ? zll_main_perform_write33_outR1 : ((__resumption_tag[207:205] == 3'h4) ? (((zi50[1] == 1'h1) & (zi50[0] == 1'h1)) ? {1'h1, zll_main_while_addr_reg_023_out, {3'h3, {8'hcd{1'h0}}}, zi62} : (((zi63[1] == 1'h1) & (zi63[0] == 1'h0)) ? {1'h1, zll_main_while_addr_reg_023_outR1, {2'h1, {8'hce{1'h0}}}, zi93} : zll_main_perform_write33_outR2)) : main_while_addr_reg_0_outR1)));
  assign __resumption_tag_next = zres[412:205];
  assign __st0_next = zres[204:128];
  assign __st1_next = zres[127:0];
  assign __out0 = zres[445:413];
  initial {__resumption_tag, __st0, __st1} = 413'h7fffffffffeffffffffffffffffffffffffffffffffffffffff;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 413'h7fffffffffeffffffffffffffffffffffffffffffffffffffff;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_idle115 (input logic [204:0] arg0,
  output logic [446:0] res);
  assign res = {{36'h1, {8'hce{1'h0}}}, arg0};
endmodule

module ZLL_Main_perform__write34 (input logic [204:0] arg0,
  output logic [409:0] res);
  assign res = {arg0, arg0};
endmodule

module ZLL_Main_perform__write33 (input logic [446:0] arg0,
  output logic [446:0] res);
  logic [204:0] zi0;
  logic [446:0] zll_main_idle115_out;
  logic [446:0] zi2;
  logic [204:0] zi3;
  logic [76:0] zll_main_connect4_out;
  logic [32:0] zll_main_while_addr_reg_023_out;
  assign zi0 = arg0[409:205];
  ZLL_Main_idle115  inst (zi0, zll_main_idle115_out);
  assign zi2 = zll_main_idle115_out;
  assign zi3 = zi2[204:0];
  ZLL_Main_connect4  instR1 (zi0, zll_main_connect4_out);
  ZLL_Main_while__addr__reg__023  instR2 (zll_main_connect4_out, zll_main_while_addr_reg_023_out);
  assign res = {1'h1, zll_main_while_addr_reg_023_out, {1'h1, {8'hcf{1'h0}}}, zi3};
endmodule

module ZLL_Main_data__out__reg3 (input logic [31:0] arg0,
  output logic [31:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_connect4 (input logic [204:0] arg0,
  output logic [76:0] res);
  logic [76:0] zi0;
  assign zi0 = arg0[204:128];
  assign res = zi0;
endmodule

module ZLL_Main_idle102 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] main_set_data_out_reg_out;
  Main_set__data__out__reg  inst (arg0, main_set_data_out_reg_out);
  assign res = main_set_data_out_reg_out;
endmodule

module ZLL_Main_perform__read22 (input logic [409:0] arg0,
  output logic [446:0] res);
  logic [204:0] zi0;
  logic [204:0] zi1;
  assign zi0 = arg0[409:205];
  assign zi1 = arg0[204:0];
  assign res = {37'h1, zi0, zi1};
endmodule

module Main_set__ack__reg (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] zll_main_set_ack_reg10_out;
  ZLL_Main_set__ack__reg10  inst (1'h0, arg0, zll_main_set_ack_reg10_out);
  assign res = zll_main_set_ack_reg10_out;
endmodule

module ZLL_Main_set__ack__reg10 (input logic [0:0] arg0,
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

module Main_set__addr__reg (input logic [9:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi4;
  logic [1:0] zi5;
  logic [31:0] zi7;
  logic [0:0] zi8;
  logic [31:0] zi9;
  assign zi4 = arg1[127:0];
  assign zi5 = arg1[204:203];
  assign zi7 = arg1[192:161];
  assign zi8 = arg1[160];
  assign zi9 = arg1[159:128];
  assign res = {{zi5, arg0, zi7, zi8, zi9}, zi4};
endmodule

module ZLL_Main_idle75 (input logic [1:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [127:0] zi4;
  logic [9:0] zi6;
  logic [31:0] zi7;
  logic [0:0] zi8;
  logic [31:0] zi9;
  logic [204:0] zi10;
  logic [204:0] main_set_ack_reg_out;
  logic [204:0] zi11;
  logic [204:0] zll_main_idle102_out;
  assign zi4 = arg1[127:0];
  assign zi6 = arg1[202:193];
  assign zi7 = arg1[192:161];
  assign zi8 = arg1[160];
  assign zi9 = arg1[159:128];
  assign zi10 = {{arg0, zi6, zi7, zi8, zi9}, zi4};
  Main_set__ack__reg  inst (zi10, main_set_ack_reg_out);
  assign zi11 = main_set_ack_reg_out;
  ZLL_Main_idle102  instR1 (zi11, zll_main_idle102_out);
  assign res = zll_main_idle102_out;
endmodule

module ZLL_Main_while__addr__reg__023 (input logic [76:0] arg0,
  output logic [32:0] res);
  logic [0:0] zi3;
  logic [31:0] zi9;
  logic [31:0] zll_main_data_out_reg3_out;
  assign zi3 = arg0[32];
  assign zi9 = arg0[31:0];
  ZLL_Main_data__out__reg3  inst (zi9, zll_main_data_out_reg3_out);
  assign res = {zi3, zll_main_data_out_reg3_out};
endmodule

module Main_set__data__out__reg (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] zll_main_set_data_out_reg6_out;
  ZLL_Main_set__data__out__reg6  inst ({6'h20{1'h1}}, arg0, zll_main_set_data_out_reg6_out);
  assign res = zll_main_set_data_out_reg6_out;
endmodule

module Main_get__addr__reg (input logic [204:0] arg0,
  output logic [214:0] res);
  logic [9:0] zi6;
  assign zi6 = arg0[202:193];
  assign res = {zi6, arg0};
endmodule

module ZLL_Main_mem02 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  input logic [31:0] arg2,
  output logic [31:0] res);
  logic [31:0] zll_main_mem01_out;
  ZLL_Main_mem01  inst (arg0, arg2, zll_main_mem01_out);
  assign res = zll_main_mem01_out;
endmodule

module ZLL_Main_perform__write19 (input logic [204:0] arg0,
  output logic [204:0] res);
  logic [204:0] zll_main_set_ack_reg10_out;
  ZLL_Main_set__ack__reg10  inst (1'h1, arg0, zll_main_set_ack_reg10_out);
  assign res = zll_main_set_ack_reg10_out;
endmodule

module ZLL_Main_mem01 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  output logic [31:0] res);
  assign res = arg0;
endmodule

module ZLL_Main_go2 (input logic [0:0] arg0,
  input logic [1:0] arg1,
  output logic [0:0] res);
  assign res = arg0;
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
  logic [0:0] zll_main_go2_out;
  assign zi3 = arg0[2];
  assign zi4 = arg0[1:0];
  ZLL_Main_go2  inst (zi3, zi4, zll_main_go2_out);
  assign res = zll_main_go2_out;
endmodule

module Main_while__addr__reg__0 (input logic [204:0] arg0,
  input logic [204:0] arg1,
  output logic [446:0] res);
  logic [214:0] main_get_addr_reg_out;
  logic [214:0] zi0;
  logic [9:0] zi1;
  logic [204:0] zi2;
  logic [446:0] zi3;
  logic [9:0] zi4;
  logic [204:0] zi5;
  logic [0:0] main_positivew10_out;
  logic [410:0] zi6;
  logic [204:0] zi8;
  logic [446:0] zll_main_idle115_out;
  logic [446:0] zi9;
  logic [204:0] zi10;
  logic [76:0] zll_main_connect4_out;
  logic [32:0] zll_main_while_addr_reg_023_out;
  logic [0:0] main_positivew10_outR1;
  logic [205:0] zi11;
  logic [204:0] zi12;
  logic [446:0] zll_main_perform_read22_out;
  logic [446:0] zi13;
  logic [204:0] zi15;
  logic [76:0] zi16;
  logic [32:0] zll_main_while_addr_reg_023_outR1;
  Main_get__addr__reg  inst (arg1, main_get_addr_reg_out);
  assign zi0 = main_get_addr_reg_out;
  assign zi1 = zi0[214:205];
  assign zi2 = zi0[204:0];
  assign zi3 = {{8'he8{1'h0}}, zi1, zi2};
  assign zi4 = zi3[214:205];
  assign zi5 = zi3[204:0];
  Main_positiveW10  instR1 (zi4, main_positivew10_out);
  assign zi6 = {zi5, arg0, main_positivew10_out};
  assign zi8 = zi6[205:1];
  ZLL_Main_idle115  instR2 (zi8, zll_main_idle115_out);
  assign zi9 = zll_main_idle115_out;
  assign zi10 = zi9[204:0];
  ZLL_Main_connect4  instR3 (zi8, zll_main_connect4_out);
  ZLL_Main_while__addr__reg__023  instR4 (zll_main_connect4_out, zll_main_while_addr_reg_023_out);
  Main_positiveW10  instR5 (zi4, main_positivew10_outR1);
  assign zi11 = {zi5, main_positivew10_outR1};
  assign zi12 = zi11[205:1];
  ZLL_Main_perform__read22  instR6 ({zi12, zi12}, zll_main_perform_read22_out);
  assign zi13 = zll_main_perform_read22_out;
  assign zi15 = zi13[204:0];
  assign zi16 = zi13[409:333];
  ZLL_Main_while__addr__reg__023  instR7 (zi16, zll_main_while_addr_reg_023_outR1);
  assign res = (zi6[0] == 1'h1) ? {1'h1, zll_main_while_addr_reg_023_out, 3'h1, zi8, zi10} : {1'h1, zll_main_while_addr_reg_023_outR1, {1'h1, {8'hcf{1'h0}}}, zi15};
endmodule

module ZLL_Main_setloc3 (input logic [41:0] arg0,
  input logic [204:0] arg1,
  output logic [204:0] res);
  logic [31:0] zi1;
  logic [76:0] zi5;
  logic [31:0] zi11;
  logic [31:0] zi12;
  logic [31:0] zi13;
  logic [31:0] zi17;
  logic [31:0] zi19;
  logic [31:0] zi20;
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
  assign zi17 = arg1[127:96];
  assign zi19 = arg1[63:32];
  assign zi20 = arg1[31:0];
  assign zi24 = arg1[127:96];
  assign zi25 = arg1[95:64];
  assign zi27 = arg1[31:0];
  assign zi31 = arg1[127:96];
  assign zi32 = arg1[95:64];
  assign zi33 = arg1[63:32];
  assign res = {zi5, ((arg0[41] == 1'h0) & ((arg0[40] == 1'h0) & ((arg0[39] == 1'h0) & ((arg0[38] == 1'h0) & ((arg0[37] == 1'h0) & ((arg0[36] == 1'h0) & ((arg0[35] == 1'h0) & ((arg0[34] == 1'h0) & ((arg0[33] == 1'h0) & (arg0[32] == 1'h0)))))))))) ? {zi1, zi11, zi12, zi13} : (((arg0[41] == 1'h0) & ((arg0[40] == 1'h0) & ((arg0[39] == 1'h0) & ((arg0[38] == 1'h0) & ((arg0[37] == 1'h0) & ((arg0[36] == 1'h0) & ((arg0[35] == 1'h0) & ((arg0[34] == 1'h0) & ((arg0[33] == 1'h0) & (arg0[32] == 1'h1)))))))))) ? {zi17, zi1, zi19, zi20} : (((arg0[41] == 1'h0) & ((arg0[40] == 1'h0) & ((arg0[39] == 1'h0) & ((arg0[38] == 1'h0) & ((arg0[37] == 1'h0) & ((arg0[36] == 1'h0) & ((arg0[35] == 1'h0) & ((arg0[34] == 1'h0) & ((arg0[33] == 1'h1) & (arg0[32] == 1'h0)))))))))) ? {zi24, zi25, zi1, zi27} : {zi31, zi32, zi33, zi1}))};
endmodule

module Main_positiveW10 (input logic [9:0] arg0,
  output logic [0:0] res);
  assign res = (arg0[0] == 1'h1) ? 1'h1 : 1'h0;
endmodule

module ZLL_Main_set__data__out__reg6 (input logic [31:0] arg0,
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

module Main_go (input logic [45:0] arg0,
  output logic [0:0] res);
  logic [0:0] zi2;
  logic [1:0] zi4;
  logic [0:0] zll_main_go2_out;
  assign zi2 = arg0[3];
  assign zi4 = arg0[1:0];
  ZLL_Main_go2  inst (zi2, zi4, zll_main_go2_out);
  assign res = zll_main_go2_out;
endmodule

module Main_addr__in (input logic [45:0] arg0,
  output logic [9:0] res);
  logic [9:0] zi1;
  assign zi1 = arg0[13:4];
  assign res = zi1;
endmodule