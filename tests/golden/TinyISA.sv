module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [16:0] __in0,
  output logic [14:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [69:0] __st0;
  logic [69:0] __st0_next;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [5:0] zi6;
  logic [14:0] zi7;
  logic [69:0] zll_l_s89_out;
  logic [87:0] zl_main_getins196_out;
  logic [7:0] zi9;
  logic [7:0] zi10;
  logic [7:0] zi11;
  logic [7:0] zi12;
  logic [5:0] zi13;
  logic [14:0] zi14;
  logic [69:0] zll_l_s89_outR1;
  logic [69:0] zi15;
  logic [16:0] main_inputs_out;
  logic [16:0] zi16;
  logic [7:0] zi17;
  logic [7:0] zi18;
  logic [7:0] zi19;
  logic [7:0] zi20;
  logic [7:0] zi21;
  logic [5:0] zi22;
  logic [16:0] zi23;
  logic [14:0] zi24;
  logic [69:0] zll_l_s89_outR2;
  logic [87:0] zl_main_getins196_outR1;
  logic [7:0] zi26;
  logic [7:0] zi27;
  logic [7:0] zi28;
  logic [7:0] zi29;
  logic [5:0] zi30;
  logic [14:0] zi31;
  logic [69:0] zll_l_s89_outR3;
  logic [69:0] zi32;
  logic [5:0] main_pc_out;
  logic [5:0] zi33;
  logic [5:0] zi34;
  logic [7:0] zi35;
  logic [7:0] zi36;
  logic [7:0] zi37;
  logic [7:0] zi38;
  logic [16:0] zi39;
  logic [14:0] zi40;
  logic [69:0] zll_l_s89_outR4;
  logic [69:0] zi41;
  logic [5:0] main_pc_outR1;
  logic [5:0] zi42;
  logic [14:0] main_outputs_out;
  logic [14:0] zi43;
  logic [0:0] zi44;
  logic [7:0] zi45;
  logic [14:0] zll_l_o2321_out;
  logic [14:0] zi46;
  logic [7:0] zi47;
  logic [7:0] zi48;
  logic [7:0] zi49;
  logic [7:0] zi50;
  logic [5:0] zi51;
  logic [16:0] zi52;
  logic [69:0] zll_l_s89_outR5;
  logic [69:0] zi53;
  logic [14:0] main_outputs_outR1;
  logic [14:0] zi54;
  logic [5:0] zi55;
  logic [7:0] zi56;
  logic [14:0] zll_l_o2321_outR1;
  logic [14:0] zi57;
  logic [7:0] zi58;
  logic [7:0] zi59;
  logic [7:0] zi60;
  logic [7:0] zi61;
  logic [5:0] zi62;
  logic [16:0] zi63;
  logic [69:0] zll_l_s89_outR6;
  logic [69:0] zi64;
  logic [14:0] main_outputs_outR2;
  logic [14:0] zi65;
  logic [7:0] zi67;
  logic [7:0] zi68;
  logic [7:0] zi69;
  logic [7:0] zi70;
  logic [5:0] zi71;
  logic [14:0] zi72;
  logic [69:0] zll_l_s89_outR7;
  logic [87:0] zl_main_getpc40_out;
  logic [7:0] zi74;
  logic [7:0] zi75;
  logic [7:0] zi76;
  logic [7:0] zi77;
  logic [5:0] zi78;
  logic [14:0] zi79;
  logic [69:0] zll_l_s89_outR8;
  logic [87:0] zl_main_getpc34_out;
  logic [87:0] zres;
  assign zi2 = __st0[69:62];
  assign zi3 = __st0[61:54];
  assign zi4 = __st0[53:46];
  assign zi5 = __st0[45:38];
  assign zi6 = __st0[37:32];
  assign zi7 = __st0[14:0];
  ZLL_L_s89  inst ({zi2, zi3, zi4, zi5, zi6, __in0, zi7}, zll_l_s89_out);
  ZL_Main_getIns196  instR1 (zll_l_s89_out, zl_main_getins196_out);
  assign zi9 = __st0[69:62];
  assign zi10 = __st0[61:54];
  assign zi11 = __st0[53:46];
  assign zi12 = __st0[45:38];
  assign zi13 = __st0[37:32];
  assign zi14 = __st0[14:0];
  ZLL_L_s89  instR2 ({zi9, zi10, zi11, zi12, zi13, __in0, zi14}, zll_l_s89_outR1);
  assign zi15 = zll_l_s89_outR1;
  Main_inputs  instR3 (zi15, main_inputs_out);
  assign zi16 = main_inputs_out;
  assign zi17 = zi16[7:0];
  assign zi18 = zi17;
  assign zi19 = zi15[61:54];
  assign zi20 = zi15[53:46];
  assign zi21 = zi15[45:38];
  assign zi22 = zi15[37:32];
  assign zi23 = zi15[31:15];
  assign zi24 = zi15[14:0];
  ZLL_L_s89  instR4 ({zi18, zi19, zi20, zi21, zi22, zi23, zi24}, zll_l_s89_outR2);
  ZL_Main_getIns196  instR5 (zll_l_s89_outR2, zl_main_getins196_outR1);
  assign zi26 = __st0[69:62];
  assign zi27 = __st0[61:54];
  assign zi28 = __st0[53:46];
  assign zi29 = __st0[45:38];
  assign zi30 = __st0[37:32];
  assign zi31 = __st0[14:0];
  ZLL_L_s89  instR6 ({zi26, zi27, zi28, zi29, zi30, __in0, zi31}, zll_l_s89_outR3);
  assign zi32 = zll_l_s89_outR3;
  Main_pc  instR7 (zi32, main_pc_out);
  assign zi33 = main_pc_out;
  assign zi34 = zi33 + 6'h1;
  assign zi35 = zi32[69:62];
  assign zi36 = zi32[61:54];
  assign zi37 = zi32[53:46];
  assign zi38 = zi32[45:38];
  assign zi39 = zi32[31:15];
  assign zi40 = zi32[14:0];
  ZLL_L_s89  instR8 ({zi35, zi36, zi37, zi38, zi34, zi39, zi40}, zll_l_s89_outR4);
  assign zi41 = zll_l_s89_outR4;
  Main_pc  instR9 (zi41, main_pc_outR1);
  assign zi42 = main_pc_outR1;
  Main_outputs  instR10 (zi41, main_outputs_out);
  assign zi43 = main_outputs_out;
  assign zi44 = zi43[14];
  assign zi45 = zi43[7:0];
  ZLL_L_o2321  instR11 ({zi44, zi42, zi45}, zll_l_o2321_out);
  assign zi46 = zll_l_o2321_out;
  assign zi47 = zi41[69:62];
  assign zi48 = zi41[61:54];
  assign zi49 = zi41[53:46];
  assign zi50 = zi41[45:38];
  assign zi51 = zi41[37:32];
  assign zi52 = zi41[31:15];
  ZLL_L_s89  instR12 ({zi47, zi48, zi49, zi50, zi51, zi52, zi46}, zll_l_s89_outR5);
  assign zi53 = zll_l_s89_outR5;
  Main_outputs  instR13 (zi53, main_outputs_outR1);
  assign zi54 = main_outputs_outR1;
  assign zi55 = zi54[13:8];
  assign zi56 = zi54[7:0];
  ZLL_L_o2321  instR14 ({1'h0, zi55, zi56}, zll_l_o2321_outR1);
  assign zi57 = zll_l_o2321_outR1;
  assign zi58 = zi53[69:62];
  assign zi59 = zi53[61:54];
  assign zi60 = zi53[53:46];
  assign zi61 = zi53[45:38];
  assign zi62 = zi53[37:32];
  assign zi63 = zi53[31:15];
  ZLL_L_s89  instR15 ({zi58, zi59, zi60, zi61, zi62, zi63, zi57}, zll_l_s89_outR6);
  assign zi64 = zll_l_s89_outR6;
  Main_outputs  instR16 (zi64, main_outputs_outR2);
  assign zi65 = main_outputs_outR2;
  assign zi67 = __st0[69:62];
  assign zi68 = __st0[61:54];
  assign zi69 = __st0[53:46];
  assign zi70 = __st0[45:38];
  assign zi71 = __st0[37:32];
  assign zi72 = __st0[14:0];
  ZLL_L_s89  instR17 ({zi67, zi68, zi69, zi70, zi71, __in0, zi72}, zll_l_s89_outR7);
  ZL_Main_getPC40  instR18 (zll_l_s89_outR7, zl_main_getpc40_out);
  assign zi74 = __st0[69:62];
  assign zi75 = __st0[61:54];
  assign zi76 = __st0[53:46];
  assign zi77 = __st0[45:38];
  assign zi78 = __st0[37:32];
  assign zi79 = __st0[14:0];
  ZLL_L_s89  instR19 ({zi74, zi75, zi76, zi77, zi78, __in0, zi79}, zll_l_s89_outR8);
  ZL_Main_getPC34  instR20 (zll_l_s89_outR8, zl_main_getpc34_out);
  assign zres = (__resumption_tag == 3'h0) ? zl_main_getins196_out : ((__resumption_tag == 3'h1) ? zl_main_getins196_outR1 : ((__resumption_tag == 3'h2) ? {zi65, 3'h1, zi64} : ((__resumption_tag == 3'h3) ? zl_main_getpc40_out : zl_main_getpc34_out)));
  assign __resumption_tag_next = zres[72:70];
  assign __st0_next = zres[69:0];
  assign __out0 = zres[87:73];
  initial {__resumption_tag, __st0} = {1'h1, {7'h48{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {1'h1, {7'h48{1'h0}}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_L_o2321 (input logic [14:0] arg0,
  output logic [14:0] res);
  assign res = arg0;
endmodule

module ZLL_L_s89 (input logic [69:0] arg0,
  output logic [69:0] res);
  assign res = arg0;
endmodule

module ZLL_L_x177 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [69:0] arg2,
  input logic [7:0] arg3,
  output logic [87:0] res);
  logic [87:0] zl_main_getreg165_out;
  ZL_Main_getReg165  inst (arg1, arg3, arg0, arg2, zl_main_getreg165_out);
  assign res = zl_main_getreg165_out;
endmodule

module ZL_Main_putPC38 (input logic [5:0] arg0,
  input logic [69:0] arg1,
  output logic [87:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [16:0] zi4;
  logic [14:0] zi5;
  logic [69:0] zll_l_s89_out;
  logic [87:0] zl_main_getpc34_out;
  assign zi0 = arg1[69:62];
  assign zi1 = arg1[61:54];
  assign zi2 = arg1[53:46];
  assign zi3 = arg1[45:38];
  assign zi4 = arg1[31:15];
  assign zi5 = arg1[14:0];
  ZLL_L_s89  inst ({zi0, zi1, zi2, zi3, arg0, zi4, zi5}, zll_l_s89_out);
  ZL_Main_getPC34  instR1 (zll_l_s89_out, zl_main_getpc34_out);
  assign res = zl_main_getpc34_out;
endmodule

module Main_pc (input logic [69:0] arg0,
  output logic [5:0] res);
  logic [5:0] ds5;
  assign ds5 = arg0[37:32];
  assign res = ds5;
endmodule

module ZLL_L_x172 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [69:0] arg2,
  input logic [7:0] arg3,
  output logic [87:0] res);
  logic [87:0] zl_main_putreg156_out;
  ZL_Main_putReg156  inst (arg0, ~(arg1 & arg3), arg2, zl_main_putreg156_out);
  assign res = zl_main_putreg156_out;
endmodule

module Main_inputs (input logic [69:0] arg0,
  output logic [16:0] res);
  logic [16:0] ds6;
  assign ds6 = arg0[31:15];
  assign res = ds6;
endmodule

module Main_r0 (input logic [69:0] arg0,
  output logic [7:0] res);
  logic [7:0] ds1;
  assign ds1 = arg0[69:62];
  assign res = ds1;
endmodule

module Main_r1 (input logic [69:0] arg0,
  output logic [7:0] res);
  logic [7:0] ds2;
  assign ds2 = arg0[61:54];
  assign res = ds2;
endmodule

module ZL_Main_getPC34 (input logic [69:0] arg0,
  output logic [87:0] res);
  logic [5:0] main_pc_out;
  logic [5:0] zi0;
  logic [14:0] main_outputs_out;
  logic [14:0] zi1;
  logic [0:0] zi2;
  logic [7:0] zi3;
  logic [14:0] zll_l_o2321_out;
  logic [14:0] zi4;
  logic [7:0] zi5;
  logic [7:0] zi6;
  logic [7:0] zi7;
  logic [7:0] zi8;
  logic [5:0] zi9;
  logic [16:0] zi10;
  logic [69:0] zll_l_s89_out;
  logic [69:0] zi11;
  logic [14:0] main_outputs_outR1;
  logic [14:0] zi12;
  logic [5:0] zi13;
  logic [7:0] zi14;
  logic [14:0] zll_l_o2321_outR1;
  logic [14:0] zi15;
  logic [7:0] zi16;
  logic [7:0] zi17;
  logic [7:0] zi18;
  logic [7:0] zi19;
  logic [5:0] zi20;
  logic [16:0] zi21;
  logic [69:0] zll_l_s89_outR1;
  logic [69:0] zi22;
  logic [14:0] main_outputs_outR2;
  logic [14:0] zi23;
  Main_pc  inst (arg0, main_pc_out);
  assign zi0 = main_pc_out;
  Main_outputs  instR1 (arg0, main_outputs_out);
  assign zi1 = main_outputs_out;
  assign zi2 = zi1[14];
  assign zi3 = zi1[7:0];
  ZLL_L_o2321  instR2 ({zi2, zi0, zi3}, zll_l_o2321_out);
  assign zi4 = zll_l_o2321_out;
  assign zi5 = arg0[69:62];
  assign zi6 = arg0[61:54];
  assign zi7 = arg0[53:46];
  assign zi8 = arg0[45:38];
  assign zi9 = arg0[37:32];
  assign zi10 = arg0[31:15];
  ZLL_L_s89  instR3 ({zi5, zi6, zi7, zi8, zi9, zi10, zi4}, zll_l_s89_out);
  assign zi11 = zll_l_s89_out;
  Main_outputs  instR4 (zi11, main_outputs_outR1);
  assign zi12 = main_outputs_outR1;
  assign zi13 = zi12[13:8];
  assign zi14 = zi12[7:0];
  ZLL_L_o2321  instR5 ({1'h0, zi13, zi14}, zll_l_o2321_outR1);
  assign zi15 = zll_l_o2321_outR1;
  assign zi16 = zi11[69:62];
  assign zi17 = zi11[61:54];
  assign zi18 = zi11[53:46];
  assign zi19 = zi11[45:38];
  assign zi20 = zi11[37:32];
  assign zi21 = zi11[31:15];
  ZLL_L_s89  instR6 ({zi16, zi17, zi18, zi19, zi20, zi21, zi15}, zll_l_s89_outR1);
  assign zi22 = zll_l_s89_outR1;
  Main_outputs  instR7 (zi22, main_outputs_outR2);
  assign zi23 = main_outputs_outR2;
  assign res = {zi23, 3'h0, zi22};
endmodule

module Main_r3 (input logic [69:0] arg0,
  output logic [7:0] res);
  logic [7:0] ds4;
  assign ds4 = arg0[45:38];
  assign res = ds4;
endmodule

module Main_r2 (input logic [69:0] arg0,
  output logic [7:0] res);
  logic [7:0] ds3;
  assign ds3 = arg0[53:46];
  assign res = ds3;
endmodule

module ZL_Main_putReg156 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [69:0] arg2,
  output logic [87:0] res);
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [5:0] zi3;
  logic [16:0] zi4;
  logic [14:0] zi5;
  logic [69:0] zll_l_s89_out;
  logic [87:0] zl_main_getpc40_out;
  logic [7:0] zi6;
  logic [7:0] zi7;
  logic [7:0] zi8;
  logic [5:0] zi9;
  logic [16:0] zi10;
  logic [14:0] zi11;
  logic [69:0] zll_l_s89_outR1;
  logic [87:0] zl_main_getpc40_outR1;
  logic [7:0] zi12;
  logic [7:0] zi13;
  logic [7:0] zi14;
  logic [5:0] zi15;
  logic [16:0] zi16;
  logic [14:0] zi17;
  logic [69:0] zll_l_s89_outR2;
  logic [87:0] zl_main_getpc40_outR2;
  logic [7:0] zi18;
  logic [7:0] zi19;
  logic [7:0] zi20;
  logic [5:0] zi21;
  logic [16:0] zi22;
  logic [14:0] zi23;
  logic [69:0] zll_l_s89_outR3;
  logic [87:0] zl_main_getpc40_outR3;
  assign zi0 = arg2[61:54];
  assign zi1 = arg2[53:46];
  assign zi2 = arg2[45:38];
  assign zi3 = arg2[37:32];
  assign zi4 = arg2[31:15];
  assign zi5 = arg2[14:0];
  ZLL_L_s89  inst ({arg1, zi0, zi1, zi2, zi3, zi4, zi5}, zll_l_s89_out);
  ZL_Main_getPC40  instR1 (zll_l_s89_out, zl_main_getpc40_out);
  assign zi6 = arg2[69:62];
  assign zi7 = arg2[53:46];
  assign zi8 = arg2[45:38];
  assign zi9 = arg2[37:32];
  assign zi10 = arg2[31:15];
  assign zi11 = arg2[14:0];
  ZLL_L_s89  instR2 ({zi6, arg1, zi7, zi8, zi9, zi10, zi11}, zll_l_s89_outR1);
  ZL_Main_getPC40  instR3 (zll_l_s89_outR1, zl_main_getpc40_outR1);
  assign zi12 = arg2[69:62];
  assign zi13 = arg2[61:54];
  assign zi14 = arg2[45:38];
  assign zi15 = arg2[37:32];
  assign zi16 = arg2[31:15];
  assign zi17 = arg2[14:0];
  ZLL_L_s89  instR4 ({zi12, zi13, arg1, zi14, zi15, zi16, zi17}, zll_l_s89_outR2);
  ZL_Main_getPC40  instR5 (zll_l_s89_outR2, zl_main_getpc40_outR2);
  assign zi18 = arg2[69:62];
  assign zi19 = arg2[61:54];
  assign zi20 = arg2[53:46];
  assign zi21 = arg2[37:32];
  assign zi22 = arg2[31:15];
  assign zi23 = arg2[14:0];
  ZLL_L_s89  instR6 ({zi18, zi19, zi20, arg1, zi21, zi22, zi23}, zll_l_s89_outR3);
  ZL_Main_getPC40  instR7 (zll_l_s89_outR3, zl_main_getpc40_outR3);
  assign res = (arg0 == 2'h0) ? zl_main_getpc40_out : ((arg0 == 2'h1) ? zl_main_getpc40_outR1 : ((arg0 == 2'h2) ? zl_main_getpc40_outR2 : zl_main_getpc40_outR3));
endmodule

module ZL_Main_getPC40 (input logic [69:0] arg0,
  output logic [87:0] res);
  logic [5:0] main_pc_out;
  logic [5:0] zi0;
  logic [87:0] zl_main_putpc38_out;
  Main_pc  inst (arg0, main_pc_out);
  assign zi0 = main_pc_out;
  ZL_Main_putPC38  instR1 (zi0 + 6'h1, arg0, zl_main_putpc38_out);
  assign res = zl_main_putpc38_out;
endmodule

module Main_outputs (input logic [69:0] arg0,
  output logic [14:0] res);
  logic [14:0] ds7;
  assign ds7 = arg0[14:0];
  assign res = ds7;
endmodule

module ZL_Main_getIns196 (input logic [69:0] arg0,
  output logic [87:0] res);
  logic [16:0] main_inputs_out;
  logic [16:0] zi0;
  logic [8:0] zi1;
  logic [8:0] zi2;
  logic [87:0] zl_main_getpc40_out;
  logic [5:0] zi3;
  logic [14:0] main_outputs_out;
  logic [14:0] zi4;
  logic [0:0] zi5;
  logic [7:0] zi6;
  logic [14:0] zll_l_o2321_out;
  logic [14:0] zi7;
  logic [7:0] zi8;
  logic [7:0] zi9;
  logic [7:0] zi10;
  logic [7:0] zi11;
  logic [5:0] zi12;
  logic [16:0] zi13;
  logic [69:0] zll_l_s89_out;
  logic [69:0] zi14;
  logic [14:0] main_outputs_outR1;
  logic [14:0] zi15;
  logic [5:0] zi16;
  logic [7:0] zi17;
  logic [14:0] zll_l_o2321_outR1;
  logic [14:0] zi18;
  logic [7:0] zi19;
  logic [7:0] zi20;
  logic [7:0] zi21;
  logic [7:0] zi22;
  logic [5:0] zi23;
  logic [16:0] zi24;
  logic [69:0] zll_l_s89_outR1;
  logic [69:0] zi25;
  logic [14:0] main_outputs_outR2;
  logic [14:0] zi26;
  logic [5:0] zi27;
  logic [7:0] main_r0_out;
  logic [7:0] zi28;
  logic [14:0] main_outputs_outR3;
  logic [14:0] zi29;
  logic [0:0] zi30;
  logic [7:0] zi31;
  logic [14:0] zll_l_o2321_outR2;
  logic [14:0] zi32;
  logic [7:0] zi33;
  logic [7:0] zi34;
  logic [7:0] zi35;
  logic [7:0] zi36;
  logic [5:0] zi37;
  logic [16:0] zi38;
  logic [69:0] zll_l_s89_outR2;
  logic [69:0] zi39;
  logic [14:0] main_outputs_outR4;
  logic [14:0] zi40;
  logic [0:0] zi41;
  logic [5:0] zi42;
  logic [14:0] zll_l_o2321_outR3;
  logic [14:0] zi43;
  logic [7:0] zi44;
  logic [7:0] zi45;
  logic [7:0] zi46;
  logic [7:0] zi47;
  logic [5:0] zi48;
  logic [16:0] zi49;
  logic [69:0] zll_l_s89_outR3;
  logic [69:0] zi50;
  logic [14:0] main_outputs_outR5;
  logic [14:0] zi51;
  logic [5:0] zi52;
  logic [7:0] zi53;
  logic [14:0] zll_l_o2321_outR4;
  logic [14:0] zi54;
  logic [7:0] zi55;
  logic [7:0] zi56;
  logic [7:0] zi57;
  logic [7:0] zi58;
  logic [5:0] zi59;
  logic [16:0] zi60;
  logic [69:0] zll_l_s89_outR4;
  logic [69:0] zi61;
  logic [14:0] main_outputs_outR6;
  logic [14:0] zi62;
  logic [1:0] zi63;
  logic [1:0] zi64;
  logic [1:0] zi65;
  logic [7:0] main_r0_outR1;
  logic [87:0] zll_l_x177_out;
  logic [7:0] main_r1_out;
  logic [87:0] zll_l_x177_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi66;
  logic [87:0] zl_main_getreg165_out;
  logic [7:0] main_r3_out;
  logic [7:0] zi67;
  logic [87:0] zl_main_getreg165_outR1;
  logic [5:0] zi68;
  logic [7:0] main_r0_outR2;
  logic [7:0] zi69;
  logic [0:0] zi70;
  logic [87:0] zl_main_putpc38_out;
  logic [87:0] zl_main_getpc40_outR1;
  Main_inputs  inst (arg0, main_inputs_out);
  assign zi0 = main_inputs_out;
  assign zi1 = zi0[16:8];
  assign zi2 = zi1;
  ZL_Main_getPC40  instR1 (arg0, zl_main_getpc40_out);
  assign zi3 = zi2[5:0];
  Main_outputs  instR2 (arg0, main_outputs_out);
  assign zi4 = main_outputs_out;
  assign zi5 = zi4[14];
  assign zi6 = zi4[7:0];
  ZLL_L_o2321  instR3 ({zi5, zi3, zi6}, zll_l_o2321_out);
  assign zi7 = zll_l_o2321_out;
  assign zi8 = arg0[69:62];
  assign zi9 = arg0[61:54];
  assign zi10 = arg0[53:46];
  assign zi11 = arg0[45:38];
  assign zi12 = arg0[37:32];
  assign zi13 = arg0[31:15];
  ZLL_L_s89  instR4 ({zi8, zi9, zi10, zi11, zi12, zi13, zi7}, zll_l_s89_out);
  assign zi14 = zll_l_s89_out;
  Main_outputs  instR5 (zi14, main_outputs_outR1);
  assign zi15 = main_outputs_outR1;
  assign zi16 = zi15[13:8];
  assign zi17 = zi15[7:0];
  ZLL_L_o2321  instR6 ({1'h0, zi16, zi17}, zll_l_o2321_outR1);
  assign zi18 = zll_l_o2321_outR1;
  assign zi19 = zi14[69:62];
  assign zi20 = zi14[61:54];
  assign zi21 = zi14[53:46];
  assign zi22 = zi14[45:38];
  assign zi23 = zi14[37:32];
  assign zi24 = zi14[31:15];
  ZLL_L_s89  instR7 ({zi19, zi20, zi21, zi22, zi23, zi24, zi18}, zll_l_s89_outR1);
  assign zi25 = zll_l_s89_outR1;
  Main_outputs  instR8 (zi25, main_outputs_outR2);
  assign zi26 = main_outputs_outR2;
  assign zi27 = zi2[5:0];
  Main_r0  instR9 (arg0, main_r0_out);
  assign zi28 = main_r0_out;
  Main_outputs  instR10 (arg0, main_outputs_outR3);
  assign zi29 = main_outputs_outR3;
  assign zi30 = zi29[14];
  assign zi31 = zi29[7:0];
  ZLL_L_o2321  instR11 ({zi30, zi27, zi31}, zll_l_o2321_outR2);
  assign zi32 = zll_l_o2321_outR2;
  assign zi33 = arg0[69:62];
  assign zi34 = arg0[61:54];
  assign zi35 = arg0[53:46];
  assign zi36 = arg0[45:38];
  assign zi37 = arg0[37:32];
  assign zi38 = arg0[31:15];
  ZLL_L_s89  instR12 ({zi33, zi34, zi35, zi36, zi37, zi38, zi32}, zll_l_s89_outR2);
  assign zi39 = zll_l_s89_outR2;
  Main_outputs  instR13 (zi39, main_outputs_outR4);
  assign zi40 = main_outputs_outR4;
  assign zi41 = zi40[14];
  assign zi42 = zi40[13:8];
  ZLL_L_o2321  instR14 ({zi41, zi42, zi28}, zll_l_o2321_outR3);
  assign zi43 = zll_l_o2321_outR3;
  assign zi44 = zi39[69:62];
  assign zi45 = zi39[61:54];
  assign zi46 = zi39[53:46];
  assign zi47 = zi39[45:38];
  assign zi48 = zi39[37:32];
  assign zi49 = zi39[31:15];
  ZLL_L_s89  instR15 ({zi44, zi45, zi46, zi47, zi48, zi49, zi43}, zll_l_s89_outR3);
  assign zi50 = zll_l_s89_outR3;
  Main_outputs  instR16 (zi50, main_outputs_outR5);
  assign zi51 = main_outputs_outR5;
  assign zi52 = zi51[13:8];
  assign zi53 = zi51[7:0];
  ZLL_L_o2321  instR17 ({1'h1, zi52, zi53}, zll_l_o2321_outR4);
  assign zi54 = zll_l_o2321_outR4;
  assign zi55 = zi50[69:62];
  assign zi56 = zi50[61:54];
  assign zi57 = zi50[53:46];
  assign zi58 = zi50[45:38];
  assign zi59 = zi50[37:32];
  assign zi60 = zi50[31:15];
  ZLL_L_s89  instR18 ({zi55, zi56, zi57, zi58, zi59, zi60, zi54}, zll_l_s89_outR4);
  assign zi61 = zll_l_s89_outR4;
  Main_outputs  instR19 (zi61, main_outputs_outR6);
  assign zi62 = main_outputs_outR6;
  assign zi63 = zi2[5:4];
  assign zi64 = zi2[3:2];
  assign zi65 = zi2[1:0];
  Main_r0  instR20 (arg0, main_r0_outR1);
  ZLL_L_x177  instR21 (zi65, zi63, arg0, main_r0_outR1, zll_l_x177_out);
  Main_r1  instR22 (arg0, main_r1_out);
  ZLL_L_x177  instR23 (zi65, zi63, arg0, main_r1_out, zll_l_x177_outR1);
  Main_r2  instR24 (arg0, main_r2_out);
  assign zi66 = main_r2_out;
  ZL_Main_getReg165  instR25 (zi63, zi66, zi65, arg0, zl_main_getreg165_out);
  Main_r3  instR26 (arg0, main_r3_out);
  assign zi67 = main_r3_out;
  ZL_Main_getReg165  instR27 (zi63, zi67, zi65, arg0, zl_main_getreg165_outR1);
  assign zi68 = zi2[5:0];
  Main_r0  instR28 (arg0, main_r0_outR2);
  assign zi69 = main_r0_outR2;
  assign zi70 = zi69 == 8'h0;
  ZL_Main_putPC38  instR29 (zi68, arg0, zl_main_putpc38_out);
  ZL_Main_getPC40  instR30 (arg0, zl_main_getpc40_outR1);
  assign res = (zi2[8:6] == 3'h0) ? zl_main_getpc40_out : ((zi2[8:6] == 3'h1) ? {zi26, 3'h2, zi25} : ((zi2[8:6] == 3'h2) ? {zi62, 3'h3, zi61} : ((zi2[8:6] == 3'h3) ? ((zi64 == 2'h0) ? zll_l_x177_out : ((zi64 == 2'h1) ? zll_l_x177_outR1 : ((zi64 == 2'h2) ? zl_main_getreg165_out : zl_main_getreg165_outR1))) : ((zi70 == 1'h0) ? zl_main_putpc38_out : zl_main_getpc40_outR1))));
endmodule

module ZL_Main_getReg165 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [1:0] arg2,
  input logic [69:0] arg3,
  output logic [87:0] res);
  logic [7:0] main_r0_out;
  logic [87:0] zll_l_x172_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi0;
  logic [87:0] zl_main_putreg156_out;
  logic [7:0] main_r2_out;
  logic [7:0] zi1;
  logic [87:0] zl_main_putreg156_outR1;
  logic [7:0] main_r3_out;
  logic [87:0] zll_l_x172_outR1;
  Main_r0  inst (arg3, main_r0_out);
  ZLL_L_x172  instR1 (arg0, arg1, arg3, main_r0_out, zll_l_x172_out);
  Main_r1  instR2 (arg3, main_r1_out);
  assign zi0 = main_r1_out;
  ZL_Main_putReg156  instR3 (arg0, ~(arg1 & zi0), arg3, zl_main_putreg156_out);
  Main_r2  instR4 (arg3, main_r2_out);
  assign zi1 = main_r2_out;
  ZL_Main_putReg156  instR5 (arg0, ~(arg1 & zi1), arg3, zl_main_putreg156_outR1);
  Main_r3  instR6 (arg3, main_r3_out);
  ZLL_L_x172  instR7 (arg0, arg1, arg3, main_r3_out, zll_l_x172_outR1);
  assign res = (arg2 == 2'h0) ? zll_l_x172_out : ((arg2 == 2'h1) ? zl_main_putreg156_out : ((arg2 == 2'h2) ? zl_main_putreg156_outR1 : zll_l_x172_outR1));
endmodule