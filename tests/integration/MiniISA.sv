module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  input logic [0:0] __in1,
  input logic [0:0] __in2,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [0:0] __out2,
  output logic [0:0] __out3);
  logic [6:0] __resumption_tag;
  logic [6:0] __resumption_tag_next;
  logic [80:0] __st0;
  logic [80:0] __st0_next;
  logic [9:0] zi1;
  logic [0:0] zi2;
  logic [1:0] zi3;
  logic [80:0] main_setinputs_out;
  logic [80:0] zi5;
  logic [105:0] zl_main_loop3_out;
  logic [9:0] main_inputs_out;
  logic [9:0] zi6;
  logic [7:0] main_datain_out;
  logic [7:0] zi7;
  logic [80:0] main_setr0_out;
  logic [105:0] zl_main_loop3_outR1;
  logic [80:0] main_setr1_out;
  logic [105:0] zl_main_loop3_outR2;
  logic [80:0] main_setr2_out;
  logic [105:0] zl_main_loop3_outR3;
  logic [80:0] main_setr3_out;
  logic [105:0] zl_main_loop3_outR4;
  logic [0:0] zi8;
  logic [0:0] zi9;
  logic [1:0] zi10;
  logic [80:0] main_setinputs_outR1;
  logic [80:0] zi12;
  logic [9:0] main_inputs_outR1;
  logic [9:0] zi13;
  logic [7:0] main_datain_outR1;
  logic [7:0] zi14;
  logic [17:0] main_outputs_out;
  logic [17:0] zi15;
  logic [17:0] main_setaddrout_out;
  logic [17:0] zi16;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi17;
  logic [17:0] main_outputs_outR1;
  logic [17:0] zi18;
  logic [17:0] main_setweout_out;
  logic [17:0] zi19;
  logic [80:0] main_setoutputs_outR1;
  logic [80:0] zi20;
  logic [105:0] zl__unused14_out;
  logic [7:0] main_r0_out;
  logic [7:0] zi21;
  logic [105:0] zl_d36_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi22;
  logic [105:0] zl_d36_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zll_l_s44_out;
  logic [7:0] main_r3_out;
  logic [105:0] zll_l_s44_outR1;
  logic [0:0] zi23;
  logic [0:0] zi24;
  logic [80:0] main_setinputs_outR2;
  logic [80:0] zi26;
  logic [9:0] main_inputs_outR2;
  logic [9:0] zi27;
  logic [7:0] main_datain_outR2;
  logic [7:0] zi28;
  logic [1:0] main_mkreg_out;
  logic [1:0] zi29;
  logic [80:0] main_setr0_outR1;
  logic [105:0] zl_main_loop3_outR5;
  logic [80:0] main_setr1_outR1;
  logic [105:0] zl_main_loop3_outR6;
  logic [80:0] main_setr2_outR1;
  logic [105:0] zl_main_loop3_outR7;
  logic [80:0] main_setr3_outR1;
  logic [105:0] zl_main_loop3_outR8;
  logic [105:0] zl_i139_out;
  logic [105:0] zl_i139_outR1;
  logic [105:0] zres;
  assign zi1 = {__in0, __in1, __in2};
  assign zi2 = __resumption_tag[2];
  assign zi3 = __resumption_tag[1:0];
  Main_setInputs  inst (__st0, zi1, main_setinputs_out);
  assign zi5 = main_setinputs_out;
  ZL_Main_loop3  instR1 (zi5, zl_main_loop3_out);
  Main_inputs  instR2 (zi5, main_inputs_out);
  assign zi6 = main_inputs_out;
  Main_dataIn  instR3 (zi6, main_datain_out);
  assign zi7 = main_datain_out;
  Main_setR0  instR4 (zi5, zi7, main_setr0_out);
  ZL_Main_loop3  instR5 (main_setr0_out, zl_main_loop3_outR1);
  Main_setR1  instR6 (zi5, zi7, main_setr1_out);
  ZL_Main_loop3  instR7 (main_setr1_out, zl_main_loop3_outR2);
  Main_setR2  instR8 (zi5, zi7, main_setr2_out);
  ZL_Main_loop3  instR9 (main_setr2_out, zl_main_loop3_outR3);
  Main_setR3  instR10 (zi5, zi7, main_setr3_out);
  ZL_Main_loop3  instR11 (main_setr3_out, zl_main_loop3_outR4);
  assign zi8 = __resumption_tag[3];
  assign zi9 = __resumption_tag[2];
  assign zi10 = __resumption_tag[1:0];
  Main_setInputs  instR12 (__st0, zi1, main_setinputs_outR1);
  assign zi12 = main_setinputs_outR1;
  Main_inputs  instR13 (zi12, main_inputs_outR1);
  assign zi13 = main_inputs_outR1;
  Main_dataIn  instR14 (zi13, main_datain_outR1);
  assign zi14 = main_datain_outR1;
  Main_outputs  instR15 (zi12, main_outputs_out);
  assign zi15 = main_outputs_out;
  Main_setAddrOut  instR16 (zi15, zi14, main_setaddrout_out);
  assign zi16 = main_setaddrout_out;
  Main_setOutputs  instR17 (zi12, zi16, main_setoutputs_out);
  assign zi17 = main_setoutputs_out;
  Main_outputs  instR18 (zi17, main_outputs_outR1);
  assign zi18 = main_outputs_outR1;
  Main_setWeOut  instR19 (zi18, zi9, main_setweout_out);
  assign zi19 = main_setweout_out;
  Main_setOutputs  instR20 (zi17, zi19, main_setoutputs_outR1);
  assign zi20 = main_setoutputs_outR1;
  ZL___unused14  instR21 (zi8, zi10, zi20, zl__unused14_out);
  Main_r0  instR22 (zi20, main_r0_out);
  assign zi21 = main_r0_out;
  ZL_d36  instR23 (zi8, zi10, zi21, zi20, zl_d36_out);
  Main_r1  instR24 (zi20, main_r1_out);
  assign zi22 = main_r1_out;
  ZL_d36  instR25 (zi8, zi10, zi22, zi20, zl_d36_outR1);
  Main_r2  instR26 (zi20, main_r2_out);
  ZLL_L_s44  instR27 (zi10, zi20, zi8, main_r2_out, zll_l_s44_out);
  Main_r3  instR28 (zi20, main_r3_out);
  ZLL_L_s44  instR29 (zi10, zi20, zi8, main_r3_out, zll_l_s44_outR1);
  assign zi23 = __resumption_tag[1];
  assign zi24 = __resumption_tag[0];
  Main_setInputs  instR30 (__st0, zi1, main_setinputs_outR2);
  assign zi26 = main_setinputs_outR2;
  Main_inputs  instR31 (zi26, main_inputs_outR2);
  assign zi27 = main_inputs_outR2;
  Main_dataIn  instR32 (zi27, main_datain_outR2);
  assign zi28 = main_datain_outR2;
  Main_mkReg  instR33 (zi23, zi24, main_mkreg_out);
  assign zi29 = main_mkreg_out;
  Main_setR0  instR34 (zi26, zi28, main_setr0_outR1);
  ZL_Main_loop3  instR35 (main_setr0_outR1, zl_main_loop3_outR5);
  Main_setR1  instR36 (zi26, zi28, main_setr1_outR1);
  ZL_Main_loop3  instR37 (main_setr1_outR1, zl_main_loop3_outR6);
  Main_setR2  instR38 (zi26, zi28, main_setr2_outR1);
  ZL_Main_loop3  instR39 (main_setr2_outR1, zl_main_loop3_outR7);
  Main_setR3  instR40 (zi26, zi28, main_setr3_outR1);
  ZL_Main_loop3  instR41 (main_setr3_outR1, zl_main_loop3_outR8);
  ZL_i139  instR42 (zi1, __st0, zl_i139_out);
  ZL_i139  instR43 (zi1, __st0, zl_i139_outR1);
  assign zres = (__resumption_tag[6:4] == 3'h0) ? ((zi2 == 1'h0) ? zl_main_loop3_out : ((zi3 == 2'h0) ? zl_main_loop3_outR1 : ((zi3 == 2'h1) ? zl_main_loop3_outR2 : ((zi3 == 2'h2) ? zl_main_loop3_outR3 : zl_main_loop3_outR4)))) : ((__resumption_tag[6:4] == 3'h1) ? ((zi9 == 1'h0) ? zl__unused14_out : ((zi10 == 2'h0) ? zl_d36_out : ((zi10 == 2'h1) ? zl_d36_outR1 : ((zi10 == 2'h2) ? zll_l_s44_out : zll_l_s44_outR1)))) : ((__resumption_tag[6:4] == 3'h2) ? ((zi29 == 2'h0) ? zl_main_loop3_outR5 : ((zi29 == 2'h1) ? zl_main_loop3_outR6 : ((zi29 == 2'h2) ? zl_main_loop3_outR7 : zl_main_loop3_outR8))) : ((__resumption_tag[6:4] == 3'h3) ? zl_i139_out : zl_i139_outR1)));
  assign __resumption_tag_next = zres[87:81];
  assign __st0_next = zres[80:0];
  assign __out0 = zres[105:98];
  assign __out1 = zres[97:90];
  assign __out2 = zres[89];
  assign __out3 = zres[88];
  initial {__resumption_tag, __st0} = {3'h3, {7'h55{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {3'h3, {7'h55{1'h0}}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module Main_setOutputs (input logic [80:0] arg0,
  input logic [17:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [0:0] z;
  logic [0:0] c;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r2;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign z = arg0[52];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {i, arg1, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

module Main_setDataOut (input logic [17:0] arg0,
  input logic [7:0] arg1,
  output logic [17:0] res);
  logic [7:0] a_o;
  logic [0:0] we_o;
  logic [0:0] iack_o;
  assign a_o = arg0[17:10];
  assign we_o = arg0[1];
  assign iack_o = arg0[0];
  assign res = {a_o, arg1, we_o, iack_o};
endmodule

module ZL_vS135 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [8:0] zll_main_pluscw8$s2_out;
  logic [105:0] zll_l_cin240_out;
  ZLL_Main_plusCW8$s2  inst (({1'h0, arg1} + {1'h0, arg2}) + 9'h0, zll_main_pluscw8$s2_out);
  ZLL_L_cin240  instR1 (arg0, arg3, zll_main_pluscw8$s2_out, zll_l_cin240_out);
  assign res = zll_l_cin240_out;
endmodule

module ZLL_L_s446 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setpc_out;
  logic [105:0] zl__unused103_out;
  Main_setPC  inst (arg0, arg1, main_setpc_out);
  ZL___unused103  instR1 (main_setpc_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module ZLL_L_s664 (input logic [1:0] arg0,
  input logic [80:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_v650_out;
  ZL_v650  inst (arg0, arg2, arg1, zl_v650_out);
  assign res = zl_v650_out;
endmodule

module ZL_arm315 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [105:0] zl__unused299_out;
  Main_setR3  inst (arg1, arg0, main_setr3_out);
  ZL___unused299  instR1 (arg0, main_setr3_out, zl__unused299_out);
  assign res = zl__unused299_out;
endmodule

module ZLL_L_v69512 (input logic [8:0] arg0,
  output logic [8:0] res);
  assign res = arg0;
endmodule

module Main_setAddrOut (input logic [17:0] arg0,
  input logic [7:0] arg1,
  output logic [17:0] res);
  logic [7:0] d_o;
  logic [0:0] we_o;
  logic [0:0] iack_o;
  assign d_o = arg0[9:2];
  assign we_o = arg0[1];
  assign iack_o = arg0[0];
  assign res = {arg1, d_o, we_o, iack_o};
endmodule

module ZLL_L_vS205 (input logic [80:0] arg0,
  input logic [1:0] arg1,
  input logic [8:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_s150_out;
  ZL_s150  inst (arg1, arg2, arg0, arg0, zl_s150_out);
  assign res = zl_s150_out;
endmodule

module ZLL_L_s44 (input logic [1:0] arg0,
  input logic [80:0] arg1,
  input logic [0:0] arg2,
  input logic [7:0] arg3,
  output logic [105:0] res);
  logic [105:0] zl_d36_out;
  ZL_d36  inst (arg2, arg0, arg3, arg1, zl_d36_out);
  assign res = zl_d36_out;
endmodule

module ZLL_L_s616 (input logic [1:0] arg0,
  input logic [80:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [8:0] main_pluscw8$s1_out;
  logic [8:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [105:0] zl_arm607_out;
  logic [105:0] zl_arm609_out;
  logic [105:0] zl_arm611_out;
  logic [105:0] zl_arm613_out;
  Main_plusCW8$s1  inst (arg2, main_pluscw8$s1_out);
  assign zi0 = main_pluscw8$s1_out;
  assign zi1 = zi0[7:0];
  assign zi2 = zi1;
  ZL_arm607  instR1 (zi0, zi2, arg1, zl_arm607_out);
  ZL_arm609  instR2 (zi0, zi2, arg1, zl_arm609_out);
  ZL_arm611  instR3 (zi0, zi2, arg1, zl_arm611_out);
  ZL_arm613  instR4 (zi0, zi2, arg1, zl_arm613_out);
  assign res = (arg0 == 2'h0) ? zl_arm607_out : ((arg0 == 2'h1) ? zl_arm609_out : ((arg0 == 2'h2) ? zl_arm611_out : zl_arm613_out));
endmodule

module Main_setR0 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] c;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r1;
  logic [7:0] r2;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, arg1, r1, r2, r3};
endmodule

module ZLL_Main_plusCW8$s2 (input logic [8:0] arg0,
  output logic [8:0] res);
  assign res = {arg0[8], arg0[7:0]};
endmodule

module ZL_vD369 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] zl_vs370_out;
  logic [7:0] main_r1_out;
  logic [105:0] zl_vs370_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zl_vs370_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zl_vs370_outR3;
  Main_r0  inst (arg3, main_r0_out);
  ZL_vS370  instR1 (arg0, arg2, main_r0_out, arg3, zl_vs370_out);
  Main_r1  instR2 (arg3, main_r1_out);
  ZL_vS370  instR3 (arg0, arg2, main_r1_out, arg3, zl_vs370_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  ZL_vS370  instR5 (arg0, arg2, main_r2_out, arg3, zl_vs370_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  ZL_vS370  instR7 (arg0, arg2, main_r3_out, arg3, zl_vs370_outR3);
  assign res = (arg1 == 2'h0) ? zl_vs370_out : ((arg1 == 2'h1) ? zl_vs370_outR1 : ((arg1 == 2'h2) ? zl_vs370_outR2 : zl_vs370_outR3));
endmodule

module ZLL_L_s427 (input logic [80:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [7:0] arg3,
  output logic [105:0] res);
  logic [105:0] zl_vd405_out;
  ZL_vD405  inst (arg2, arg1, arg3, arg0, zl_vd405_out);
  assign res = zl_vd405_out;
endmodule

module ZLL_L_s643 (input logic [80:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_v623_out;
  ZL_v623  inst (arg1, arg2, arg0, zl_v623_out);
  assign res = zl_v623_out;
endmodule

module ZL_vD168 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] zl_vs169_out;
  logic [7:0] main_r1_out;
  logic [105:0] zl_vs169_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zl_vs169_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zl_vs169_outR3;
  Main_r0  inst (arg3, main_r0_out);
  ZL_vS169  instR1 (arg0, arg2, main_r0_out, arg3, zl_vs169_out);
  Main_r1  instR2 (arg3, main_r1_out);
  ZL_vS169  instR3 (arg0, arg2, main_r1_out, arg3, zl_vs169_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  ZL_vS169  instR5 (arg0, arg2, main_r2_out, arg3, zl_vs169_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  ZL_vS169  instR7 (arg0, arg2, main_r3_out, arg3, zl_vs169_outR3);
  assign res = (arg1 == 2'h0) ? zl_vs169_out : ((arg1 == 2'h1) ? zl_vs169_outR1 : ((arg1 == 2'h2) ? zl_vs169_outR2 : zl_vs169_outR3));
endmodule

module ZLL_L_v6721 (input logic [1:0] arg0,
  input logic [80:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_arm566_out;
  logic [105:0] zl_arm568_out;
  logic [105:0] zl_arm570_out;
  logic [105:0] zl_arm572_out;
  ZL_arm566  inst (arg2, arg1, zl_arm566_out);
  ZL_arm568  instR1 (arg2, arg1, zl_arm568_out);
  ZL_arm570  instR2 (arg2, arg1, zl_arm570_out);
  ZL_arm572  instR3 (arg2, arg1, zl_arm572_out);
  assign res = (arg0 == 2'h0) ? zl_arm566_out : ((arg0 == 2'h1) ? zl_arm568_out : ((arg0 == 2'h2) ? zl_arm570_out : zl_arm572_out));
endmodule

module ZLL_L_s577 (input logic [80:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_v559_out;
  ZL_v559  inst (arg1, arg2, arg0, zl_v559_out);
  assign res = zl_v559_out;
endmodule

module ZLL_L_s421 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_vs406_out;
  ZL_vS406  inst (arg1, arg2, arg0, zl_vs406_out);
  assign res = zl_vs406_out;
endmodule

module ZL_d36 (input logic [0:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  logic [17:0] main_setdataout_out;
  logic [17:0] zi1;
  logic [80:0] main_setoutputs_out;
  logic [105:0] zl__unused14_out;
  Main_outputs  inst (arg3, main_outputs_out);
  assign zi0 = main_outputs_out;
  Main_setDataOut  instR1 (zi0, arg2, main_setdataout_out);
  assign zi1 = main_setdataout_out;
  Main_setOutputs  instR2 (arg3, zi1, main_setoutputs_out);
  ZL___unused14  instR3 (arg0, arg1, main_setoutputs_out, zl__unused14_out);
  assign res = zl__unused14_out;
endmodule

module ZL_vS334 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] zi0;
  logic [105:0] zl_arm309_out;
  logic [105:0] zl_arm311_out;
  logic [105:0] zl_arm313_out;
  logic [105:0] zl_arm315_out;
  assign zi0 = arg1 & arg2;
  ZL_arm309  inst (zi0, arg3, zl_arm309_out);
  ZL_arm311  instR1 (zi0, arg3, zl_arm311_out);
  ZL_arm313  instR2 (zi0, arg3, zl_arm313_out);
  ZL_arm315  instR3 (zi0, arg3, zl_arm315_out);
  assign res = (arg0 == 2'h0) ? zl_arm309_out : ((arg0 == 2'h1) ? zl_arm311_out : ((arg0 == 2'h2) ? zl_arm313_out : zl_arm315_out));
endmodule

module ZL_arm566 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [105:0] zl__unused103_out;
  Main_setR0  inst (arg1, arg0, main_setr0_out);
  ZL___unused103  instR1 (main_setr0_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module Main_setIEFlag (input logic [80:0] arg0,
  input logic [0:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] c;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r2;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign c = arg0[51];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {i, o, z, c, arg1, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

module ZLL_L_s515 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setpc_out;
  logic [105:0] zl__unused103_out;
  Main_setPC  inst (arg0, arg1, main_setpc_out);
  ZL___unused103  instR1 (main_setpc_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module Main_setCFlag (input logic [80:0] arg0,
  input logic [0:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r2;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {i, o, z, arg1, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

module ZLL_L_s90 (input logic [0:0] arg0,
  input logic [80:0] arg1,
  input logic [0:0] arg2,
  input logic [7:0] arg3,
  output logic [105:0] res);
  logic [105:0] zl_a65_out;
  ZL_a65  inst (arg2, arg0, arg3, arg1, zl_a65_out);
  assign res = zl_a65_out;
endmodule

module ZLL_L_s688 (input logic [1:0] arg0,
  input logic [80:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_v672_out;
  ZL_v672  inst (arg0, arg2, arg1, zl_v672_out);
  assign res = zl_v672_out;
endmodule

module ZL_arm311 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [105:0] zl__unused299_out;
  Main_setR1  inst (arg1, arg0, main_setr1_out);
  ZL___unused299  instR1 (arg0, main_setr1_out, zl__unused299_out);
  assign res = zl__unused299_out;
endmodule

module ZL_Main_loop3 (input logic [80:0] arg0,
  output logic [105:0] res);
  logic [9:0] main_inputs_out;
  logic [9:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  logic [0:0] zi6;
  logic [80:0] main_setieflag_out;
  logic [80:0] zi7;
  logic [7:0] main_pc_out;
  logic [7:0] zi8;
  logic [0:0] main_zflag_out;
  logic [0:0] zi9;
  logic [0:0] main_cflag_out;
  logic [0:0] zi10;
  logic [9:0] zi11;
  logic [17:0] zi12;
  logic [0:0] zi13;
  logic [0:0] zi14;
  logic [0:0] zi15;
  logic [7:0] zi16;
  logic [0:0] zi17;
  logic [0:0] zi18;
  logic [7:0] zi19;
  logic [7:0] zi20;
  logic [7:0] zi21;
  logic [7:0] zi22;
  logic [80:0] zi23;
  logic [9:0] zi24;
  logic [17:0] zi25;
  logic [0:0] zi26;
  logic [0:0] zi27;
  logic [0:0] zi28;
  logic [7:0] zi29;
  logic [0:0] zi30;
  logic [7:0] zi31;
  logic [7:0] zi32;
  logic [7:0] zi33;
  logic [7:0] zi34;
  logic [7:0] zi35;
  logic [80:0] zi36;
  logic [9:0] zi37;
  logic [17:0] zi38;
  logic [0:0] zi39;
  logic [0:0] zi40;
  logic [0:0] zi41;
  logic [7:0] zi42;
  logic [0:0] zi43;
  logic [7:0] zi44;
  logic [7:0] zi45;
  logic [7:0] zi46;
  logic [7:0] zi47;
  logic [7:0] zi48;
  logic [105:0] zl__unused103_out;
  logic [105:0] zl_fail7_out;
  logic [105:0] zl_fail7_outR1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi49;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi50;
  logic [80:0] main_setoutputs_out;
  logic [105:0] zl__unused103_outR1;
  Main_inputs  inst (arg0, main_inputs_out);
  assign zi0 = main_inputs_out;
  assign zi1 = zi0[1];
  assign zi2 = zi1;
  assign zi3 = arg0[50];
  assign zi4 = zi3;
  assign zi5 = zi0[0];
  assign zi6 = zi5;
  Main_setIEFlag  instR1 (arg0, 1'h0, main_setieflag_out);
  assign zi7 = main_setieflag_out;
  Main_pc  instR2 (zi7, main_pc_out);
  assign zi8 = main_pc_out;
  Main_zFlag  instR3 (zi7, main_zflag_out);
  assign zi9 = main_zflag_out;
  Main_cFlag  instR4 (zi7, main_cflag_out);
  assign zi10 = main_cflag_out;
  assign zi11 = zi7[80:71];
  assign zi12 = zi7[70:53];
  assign zi13 = zi7[52];
  assign zi14 = zi7[51];
  assign zi15 = zi7[50];
  assign zi16 = zi7[49:42];
  assign zi17 = zi7[41];
  assign zi18 = zi7[40];
  assign zi19 = zi7[31:24];
  assign zi20 = zi7[23:16];
  assign zi21 = zi7[15:8];
  assign zi22 = zi7[7:0];
  assign zi23 = {zi11, zi12, zi13, zi14, zi15, zi16, zi17, zi18, zi8, zi19, zi20, zi21, zi22};
  assign zi24 = zi23[80:71];
  assign zi25 = zi23[70:53];
  assign zi26 = zi23[52];
  assign zi27 = zi23[51];
  assign zi28 = zi23[50];
  assign zi29 = zi23[49:42];
  assign zi30 = zi23[40];
  assign zi31 = zi23[39:32];
  assign zi32 = zi23[31:24];
  assign zi33 = zi23[23:16];
  assign zi34 = zi23[15:8];
  assign zi35 = zi23[7:0];
  assign zi36 = {zi24, zi25, zi26, zi27, zi28, zi29, zi9, zi30, zi31, zi32, zi33, zi34, zi35};
  assign zi37 = zi36[80:71];
  assign zi38 = zi36[70:53];
  assign zi39 = zi36[52];
  assign zi40 = zi36[51];
  assign zi41 = zi36[50];
  assign zi42 = zi36[49:42];
  assign zi43 = zi36[41];
  assign zi44 = zi36[39:32];
  assign zi45 = zi36[31:24];
  assign zi46 = zi36[23:16];
  assign zi47 = zi36[15:8];
  assign zi48 = zi36[7:0];
  ZL___unused103  instR5 ({zi37, zi38, zi39, zi40, zi41, zi42, zi43, zi10, zi44, zi45, zi46, zi47, zi48}, zl__unused103_out);
  ZL_fail7  instR6 (zi0, arg0, zl_fail7_out);
  ZL_fail7  instR7 (zi0, arg0, zl_fail7_outR1);
  Main_setCFlag  instR8 (arg0, 1'h0, main_setcflag_out);
  assign zi49 = main_setcflag_out;
  Main_setZFlag  instR9 (zi49, 1'h0, main_setzflag_out);
  assign zi50 = main_setzflag_out;
  Main_setOutputs  instR10 (zi50, 18'h0, main_setoutputs_out);
  ZL___unused103  instR11 (main_setoutputs_out, zl__unused103_outR1);
  assign res = (zi2 == 1'h0) ? ((zi4 == 1'h1) ? ((zi6 == 1'h1) ? zl__unused103_out : zl_fail7_out) : zl_fail7_outR1) : zl__unused103_outR1;
endmodule

module Main_setZFlag (input logic [80:0] arg0,
  input logic [0:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] c;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r2;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {i, o, arg1, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

module ZL_arm568 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [105:0] zl__unused103_out;
  Main_setR1  inst (arg1, arg0, main_setr1_out);
  ZL___unused103  instR1 (main_setr1_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module ZL_arm309 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [105:0] zl__unused299_out;
  Main_setR0  inst (arg1, arg0, main_setr0_out);
  ZL___unused299  instR1 (arg0, main_setr0_out, zl__unused299_out);
  assign res = zl__unused299_out;
endmodule

module Main_msbW8 (input logic [7:0] arg0,
  output logic [0:0] res);
  assign res = arg0[7];
endmodule

module ZLL_L_s692 (input logic [80:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_v672_out;
  ZL_v672  inst (arg1, arg2, arg0, zl_v672_out);
  assign res = zl_v672_out;
endmodule

module Main_pc (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] pc;
  assign pc = arg0[49:42];
  assign res = pc;
endmodule

module ZL_v695 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [1:0] arg2,
  input logic [7:0] arg3,
  input logic [80:0] arg4,
  output logic [105:0] res);
  logic [0:0] main_msbw8_out;
  logic [0:0] zi0;
  logic [0:0] main_msbw8_outR1;
  logic [8:0] zll_l_v69512_out;
  logic [0:0] main_msbw8_outR2;
  logic [8:0] zll_l_v69512_outR1;
  logic [8:0] zll_l_v69512_outR2;
  logic [0:0] main_lsbw8_out;
  logic [0:0] zi1;
  logic [0:0] main_lsbw8_outR1;
  logic [8:0] zll_l_v69512_outR3;
  logic [0:0] main_lsbw8_outR2;
  logic [8:0] zll_l_v69512_outR4;
  logic [8:0] zll_l_v69512_outR5;
  logic [8:0] zi2;
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [105:0] zl_arm607_out;
  logic [105:0] zl_arm609_out;
  logic [105:0] zl_arm611_out;
  logic [105:0] zl_arm613_out;
  Main_msbW8  inst (arg3, main_msbw8_out);
  assign zi0 = main_msbw8_out;
  Main_msbW8  instR1 (arg3, main_msbw8_outR1);
  ZLL_L_v69512  instR2 ({main_msbw8_outR1, (arg3 << 8'h1) | {7'h0, zi0}}, zll_l_v69512_out);
  Main_msbW8  instR3 (arg3, main_msbw8_outR2);
  ZLL_L_v69512  instR4 ({main_msbw8_outR2, (arg3 << 8'h1) | 8'h0}, zll_l_v69512_outR1);
  ZLL_L_v69512  instR5 ((arg0 == 1'h0) ? zll_l_v69512_out : zll_l_v69512_outR1, zll_l_v69512_outR2);
  Main_lsbW8  instR6 (arg3, main_lsbw8_out);
  assign zi1 = main_lsbw8_out;
  Main_lsbW8  instR7 (arg3, main_lsbw8_outR1);
  ZLL_L_v69512  instR8 ({main_lsbw8_outR1, (arg3 >> 8'h1) | ({7'h0, zi1} << 8'h7)}, zll_l_v69512_outR3);
  Main_lsbW8  instR9 (arg3, main_lsbw8_outR2);
  ZLL_L_v69512  instR10 ({main_lsbw8_outR2, (arg3 >> 8'h1) | 8'h0}, zll_l_v69512_outR4);
  ZLL_L_v69512  instR11 ((arg0 == 1'h0) ? zll_l_v69512_outR3 : zll_l_v69512_outR4, zll_l_v69512_outR5);
  assign zi2 = (arg1 == 1'h0) ? zll_l_v69512_outR2 : zll_l_v69512_outR5;
  assign zi3 = zi2[7:0];
  assign zi4 = zi3;
  ZL_arm607  instR12 (zi2, zi4, arg4, zl_arm607_out);
  ZL_arm609  instR13 (zi2, zi4, arg4, zl_arm609_out);
  ZL_arm611  instR14 (zi2, zi4, arg4, zl_arm611_out);
  ZL_arm613  instR15 (zi2, zi4, arg4, zl_arm613_out);
  assign res = (arg2 == 2'h0) ? zl_arm607_out : ((arg2 == 2'h1) ? zl_arm609_out : ((arg2 == 2'h2) ? zl_arm611_out : zl_arm613_out));
endmodule

module Main_setPC (input logic [80:0] arg0,
  input logic [7:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] c;
  logic [0:0] ie;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r2;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {i, o, z, c, ie, arg1, zs, cs, pcs, r0, r1, r2, r3};
endmodule

module ZL_v672 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [105:0] zll_l_v6721_out;
  ZLL_L_v6721  inst (arg0, arg2, (arg1 >> 8'h1) | (arg1 << 8'h7), zll_l_v6721_out);
  assign res = zll_l_v6721_out;
endmodule

module ZL_x274 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] zi0;
  logic [80:0] main_setr0_out;
  logic [105:0] zl__unused103_out;
  logic [80:0] main_setr1_out;
  logic [105:0] zl__unused103_outR1;
  logic [80:0] main_setr2_out;
  logic [105:0] zl__unused103_outR2;
  logic [80:0] main_setr3_out;
  logic [105:0] zl__unused103_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign zi0 = main_mkreg_out;
  Main_setR0  instR1 (arg3, arg2, main_setr0_out);
  ZL___unused103  instR2 (main_setr0_out, zl__unused103_out);
  Main_setR1  instR3 (arg3, arg2, main_setr1_out);
  ZL___unused103  instR4 (main_setr1_out, zl__unused103_outR1);
  Main_setR2  instR5 (arg3, arg2, main_setr2_out);
  ZL___unused103  instR6 (main_setr2_out, zl__unused103_outR2);
  Main_setR3  instR7 (arg3, arg2, main_setr3_out);
  ZL___unused103  instR8 (main_setr3_out, zl__unused103_outR3);
  assign res = (zi0 == 2'h0) ? zl__unused103_out : ((zi0 == 2'h1) ? zl__unused103_outR1 : ((zi0 == 2'h2) ? zl__unused103_outR2 : zl__unused103_outR3));
endmodule

module Main_setR2 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] c;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r3 = arg0[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, arg1, r3};
endmodule

module ZLL_L_cin240 (input logic [1:0] arg0,
  input logic [80:0] arg1,
  input logic [8:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_s150_out;
  ZL_s150  inst (arg0, arg2, arg1, arg1, zl_s150_out);
  assign res = zl_s150_out;
endmodule

module ZL_vS406 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] main_minuscw8$s1_out;
  logic [8:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi3;
  logic [7:0] zi4;
  logic [7:0] zi5;
  logic [80:0] main_setzflag_out;
  logic [105:0] zl__unused103_out;
  Main_minusCW8$s1  inst (arg0, arg1, main_minuscw8$s1_out);
  assign zi0 = main_minuscw8$s1_out;
  assign zi1 = zi0[8];
  assign zi2 = zi1;
  Main_setCFlag  instR1 (arg2, zi2, main_setcflag_out);
  assign zi3 = main_setcflag_out;
  assign zi4 = zi0[7:0];
  assign zi5 = zi4;
  Main_setZFlag  instR2 (zi3, zi5 == 8'h0, main_setzflag_out);
  ZL___unused103  instR3 (main_setzflag_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module ZL_vD333 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] zl_vs334_out;
  logic [7:0] main_r1_out;
  logic [105:0] zl_vs334_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zl_vs334_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zl_vs334_outR3;
  Main_r0  inst (arg3, main_r0_out);
  ZL_vS334  instR1 (arg0, arg2, main_r0_out, arg3, zl_vs334_out);
  Main_r1  instR2 (arg3, main_r1_out);
  ZL_vS334  instR3 (arg0, arg2, main_r1_out, arg3, zl_vs334_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  ZL_vS334  instR5 (arg0, arg2, main_r2_out, arg3, zl_vs334_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  ZL_vS334  instR7 (arg0, arg2, main_r3_out, arg3, zl_vs334_outR3);
  assign res = (arg1 == 2'h0) ? zl_vs334_out : ((arg1 == 2'h1) ? zl_vs334_outR1 : ((arg1 == 2'h2) ? zl_vs334_outR2 : zl_vs334_outR3));
endmodule

module ZL___unused597 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi2;
  logic [80:0] main_setzflag_out;
  logic [105:0] zl__unused103_out;
  assign zi0 = arg0[8];
  assign zi1 = zi0;
  Main_setCFlag  inst (arg2, zi1, main_setcflag_out);
  assign zi2 = main_setcflag_out;
  Main_setZFlag  instR1 (zi2, arg1 == 8'h0, main_setzflag_out);
  ZL___unused103  instR2 (main_setzflag_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module ZLL_L_s123 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_v100_out;
  ZL_v100  inst (arg1, arg2, arg0, zl_v100_out);
  assign res = zl_v100_out;
endmodule

module Main_setR1 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] c;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r2;
  logic [7:0] r3;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, arg1, r2, r3};
endmodule

module Main_inputs (input logic [80:0] arg0,
  output logic [9:0] res);
  logic [9:0] i;
  assign i = arg0[80:71];
  assign res = i;
endmodule

module ZL_v623 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] zll_main_pluscw8$s2_out;
  logic [8:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [105:0] zl_arm607_out;
  logic [105:0] zl_arm609_out;
  logic [105:0] zl_arm611_out;
  logic [105:0] zl_arm613_out;
  ZLL_Main_plusCW8$s2  inst (({1'h0, arg1} - 9'h1) - 9'h0, zll_main_pluscw8$s2_out);
  assign zi0 = zll_main_pluscw8$s2_out;
  assign zi1 = zi0[7:0];
  assign zi2 = zi1;
  ZL_arm607  instR1 (zi0, zi2, arg2, zl_arm607_out);
  ZL_arm609  instR2 (zi0, zi2, arg2, zl_arm609_out);
  ZL_arm611  instR3 (zi0, zi2, arg2, zl_arm611_out);
  ZL_arm613  instR4 (zi0, zi2, arg2, zl_arm613_out);
  assign res = (arg0 == 2'h0) ? zl_arm607_out : ((arg0 == 2'h1) ? zl_arm609_out : ((arg0 == 2'h2) ? zl_arm611_out : zl_arm613_out));
endmodule

module Main_setInputs (input logic [80:0] arg0,
  input logic [9:0] arg1,
  output logic [80:0] res);
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] c;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r2;
  logic [7:0] r3;
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign r3 = arg0[7:0];
  assign res = {arg1, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

module ZLL_L_vS370 (input logic [80:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_arm309_out;
  logic [105:0] zl_arm311_out;
  logic [105:0] zl_arm313_out;
  logic [105:0] zl_arm315_out;
  ZL_arm309  inst (arg2, arg0, zl_arm309_out);
  ZL_arm311  instR1 (arg2, arg0, zl_arm311_out);
  ZL_arm313  instR2 (arg2, arg0, zl_arm313_out);
  ZL_arm315  instR3 (arg2, arg0, zl_arm315_out);
  assign res = (arg1 == 2'h0) ? zl_arm309_out : ((arg1 == 2'h1) ? zl_arm311_out : ((arg1 == 2'h2) ? zl_arm313_out : zl_arm315_out));
endmodule

module ZLL_L_s666 (input logic [80:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  output logic [105:0] res);
  logic [105:0] zl_v650_out;
  ZL_v650  inst (arg1, arg2, arg0, zl_v650_out);
  assign res = zl_v650_out;
endmodule

module ZL___unused137 (input logic [80:0] arg0,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  Main_outputs  inst (arg0, main_outputs_out);
  assign zi0 = main_outputs_out;
  assign res = {zi0, 7'h40, arg0};
endmodule

module Main_r0 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r0;
  assign r0 = arg0[31:24];
  assign res = r0;
endmodule

module ZL_a99 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] zi0;
  logic [7:0] main_r0_out;
  logic [105:0] zll_l_s123_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] zl_v100_out;
  logic [7:0] main_r2_out;
  logic [105:0] zll_l_s123_outR1;
  logic [7:0] main_r3_out;
  logic [105:0] zll_l_s123_outR2;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign zi0 = main_mkreg_out;
  Main_r0  instR1 (arg3, main_r0_out);
  ZLL_L_s123  instR2 (arg3, arg2, main_r0_out, zll_l_s123_out);
  Main_r1  instR3 (arg3, main_r1_out);
  assign zi1 = main_r1_out;
  ZL_v100  instR4 (arg2, zi1, arg3, zl_v100_out);
  Main_r2  instR5 (arg3, main_r2_out);
  ZLL_L_s123  instR6 (arg3, arg2, main_r2_out, zll_l_s123_outR1);
  Main_r3  instR7 (arg3, main_r3_out);
  ZLL_L_s123  instR8 (arg3, arg2, main_r3_out, zll_l_s123_outR2);
  assign res = (zi0 == 2'h0) ? zll_l_s123_out : ((zi0 == 2'h1) ? zl_v100_out : ((zi0 == 2'h2) ? zll_l_s123_outR1 : zll_l_s123_outR2));
endmodule

module ZL_arm313 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [105:0] zl__unused299_out;
  Main_setR2  inst (arg1, arg0, main_setr2_out);
  ZL___unused299  instR1 (arg0, main_setr2_out, zl__unused299_out);
  assign res = zl__unused299_out;
endmodule

module ZL_v650 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] zi0;
  logic [105:0] zl_arm566_out;
  logic [105:0] zl_arm568_out;
  logic [105:0] zl_arm570_out;
  logic [105:0] zl_arm572_out;
  assign zi0 = (arg1 << 8'h1) | (arg1 >> 8'h7);
  ZL_arm566  inst (zi0, arg2, zl_arm566_out);
  ZL_arm568  instR1 (zi0, arg2, zl_arm568_out);
  ZL_arm570  instR2 (zi0, arg2, zl_arm570_out);
  ZL_arm572  instR3 (zi0, arg2, zl_arm572_out);
  assign res = (arg0 == 2'h0) ? zl_arm566_out : ((arg0 == 2'h1) ? zl_arm568_out : ((arg0 == 2'h2) ? zl_arm570_out : zl_arm572_out));
endmodule

module ZL_i139 (input logic [9:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setinputs_out;
  logic [105:0] zl_main_loop3_out;
  Main_setInputs  inst (arg1, arg0, main_setinputs_out);
  ZL_Main_loop3  instR1 (main_setinputs_out, zl_main_loop3_out);
  assign res = zl_main_loop3_out;
endmodule

module Main_notb (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h0) ? 1'h1 : 1'h0;
endmodule

module Main_dataIn (input logic [9:0] arg0,
  output logic [7:0] res);
  logic [7:0] d_i;
  assign d_i = arg0[9:2];
  assign res = d_i;
endmodule

module ZL_s150 (input logic [1:0] arg0,
  input logic [8:0] arg1,
  input logic [80:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] x;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi0;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [80:0] main_setr0_out;
  logic [105:0] zl__unused137_out;
  logic [80:0] main_setr1_out;
  logic [105:0] zl__unused137_outR1;
  logic [80:0] main_setr2_out;
  logic [105:0] zl__unused137_outR2;
  logic [80:0] main_setr3_out;
  logic [105:0] zl__unused137_outR3;
  assign x = arg1[8];
  Main_setCFlag  inst (arg2, x, main_setcflag_out);
  assign zi0 = main_setcflag_out;
  assign zi1 = arg1[7:0];
  assign zi2 = zi1;
  Main_setR0  instR1 (zi0, zi2, main_setr0_out);
  ZL___unused137  instR2 (main_setr0_out, zl__unused137_out);
  Main_setR1  instR3 (zi0, zi2, main_setr1_out);
  ZL___unused137  instR4 (main_setr1_out, zl__unused137_outR1);
  Main_setR2  instR5 (zi0, zi2, main_setr2_out);
  ZL___unused137  instR6 (main_setr2_out, zl__unused137_outR2);
  Main_setR3  instR7 (zi0, zi2, main_setr3_out);
  ZL___unused137  instR8 (main_setr3_out, zl__unused137_outR3);
  assign res = (arg0 == 2'h0) ? zl__unused137_out : ((arg0 == 2'h1) ? zl__unused137_outR1 : ((arg0 == 2'h2) ? zl__unused137_outR2 : zl__unused137_outR3));
endmodule

module ZL_arm572 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [105:0] zl__unused103_out;
  Main_setR3  inst (arg1, arg0, main_setr3_out);
  ZL___unused103  instR1 (main_setr3_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module ZL___unused299 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setcflag_out;
  logic [80:0] zi0;
  logic [80:0] main_setzflag_out;
  logic [105:0] zl__unused137_out;
  Main_setCFlag  inst (arg1, 1'h0, main_setcflag_out);
  assign zi0 = main_setcflag_out;
  Main_setZFlag  instR1 (zi0, arg0 == 8'h0, main_setzflag_out);
  ZL___unused137  instR2 (main_setzflag_out, zl__unused137_out);
  assign res = zl__unused137_out;
endmodule

module Main_lsbW8 (input logic [7:0] arg0,
  output logic [0:0] res);
  assign res = arg0[0];
endmodule

module Main_r1 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r1;
  assign r1 = arg0[23:16];
  assign res = r1;
endmodule

module ZLL_L_s537 (input logic [80:0] arg0,
  input logic [17:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setoutputs_out;
  logic [105:0] zl__unused103_out;
  Main_setOutputs  inst (arg0, arg1, main_setoutputs_out);
  ZL___unused103  instR1 (main_setoutputs_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module Main_minusCW8$s1 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [8:0] res);
  logic [8:0] zll_main_pluscw8$s2_out;
  ZLL_Main_plusCW8$s2  inst (({1'h0, arg0} - {1'h0, arg1}) - 9'h0, zll_main_pluscw8$s2_out);
  assign res = zll_main_pluscw8$s2_out;
endmodule

module Main_setR3 (input logic [80:0] arg0,
  input logic [7:0] arg1,
  output logic [80:0] res);
  logic [9:0] i;
  logic [17:0] o;
  logic [0:0] z;
  logic [0:0] c;
  logic [0:0] ie;
  logic [7:0] pc;
  logic [0:0] zs;
  logic [0:0] cs;
  logic [7:0] pcs;
  logic [7:0] r0;
  logic [7:0] r1;
  logic [7:0] r2;
  assign i = arg0[80:71];
  assign o = arg0[70:53];
  assign z = arg0[52];
  assign c = arg0[51];
  assign ie = arg0[50];
  assign pc = arg0[49:42];
  assign zs = arg0[41];
  assign cs = arg0[40];
  assign pcs = arg0[39:32];
  assign r0 = arg0[31:24];
  assign r1 = arg0[23:16];
  assign r2 = arg0[15:8];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, arg1};
endmodule

module ZLL_L_z451 (input logic [80:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [105:0] res);
  logic [105:0] zl__unused103_out;
  logic [105:0] zl_arm448_out;
  ZL___unused103  inst (arg0, zl__unused103_out);
  ZL_arm448  instR1 (arg2, arg1, arg0, zl_arm448_out);
  assign res = (arg3 == 1'h0) ? zl__unused103_out : zl_arm448_out;
endmodule

module ZL_arm611 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [105:0] zl__unused597_out;
  Main_setR2  inst (arg2, arg1, main_setr2_out);
  ZL___unused597  instR1 (arg0, arg1, main_setr2_out, zl__unused597_out);
  assign res = zl__unused597_out;
endmodule

module ZL_vD238 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] zl_vs239_out;
  logic [7:0] main_r1_out;
  logic [105:0] zl_vs239_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zl_vs239_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zl_vs239_outR3;
  Main_r0  inst (arg3, main_r0_out);
  ZL_vS239  instR1 (arg0, arg2, main_r0_out, arg3, zl_vs239_out);
  Main_r1  instR2 (arg3, main_r1_out);
  ZL_vS239  instR3 (arg0, arg2, main_r1_out, arg3, zl_vs239_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  ZL_vS239  instR5 (arg0, arg2, main_r2_out, arg3, zl_vs239_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  ZL_vS239  instR7 (arg0, arg2, main_r3_out, arg3, zl_vs239_outR3);
  assign res = (arg1 == 2'h0) ? zl_vs239_out : ((arg1 == 2'h1) ? zl_vs239_outR1 : ((arg1 == 2'h2) ? zl_vs239_outR2 : zl_vs239_outR3));
endmodule

module ZL_arm448 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] zi0;
  logic [7:0] main_r0_out;
  logic [105:0] zll_l_s446_out;
  logic [7:0] main_r1_out;
  logic [105:0] zll_l_s446_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zll_l_s446_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zll_l_s446_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign zi0 = main_mkreg_out;
  Main_r0  instR1 (arg2, main_r0_out);
  ZLL_L_s446  instR2 (arg2, main_r0_out, zll_l_s446_out);
  Main_r1  instR3 (arg2, main_r1_out);
  ZLL_L_s446  instR4 (arg2, main_r1_out, zll_l_s446_outR1);
  Main_r2  instR5 (arg2, main_r2_out);
  ZLL_L_s446  instR6 (arg2, main_r2_out, zll_l_s446_outR2);
  Main_r3  instR7 (arg2, main_r3_out);
  ZLL_L_s446  instR8 (arg2, main_r3_out, zll_l_s446_outR3);
  assign res = (zi0 == 2'h0) ? zll_l_s446_out : ((zi0 == 2'h1) ? zll_l_s446_outR1 : ((zi0 == 2'h2) ? zll_l_s446_outR2 : zll_l_s446_outR3));
endmodule

module ZL_arm570 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [105:0] zl__unused103_out;
  Main_setR2  inst (arg1, arg0, main_setr2_out);
  ZL___unused103  instR1 (main_setr2_out, zl__unused103_out);
  assign res = zl__unused103_out;
endmodule

module Main_r3 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r3;
  assign r3 = arg0[7:0];
  assign res = r3;
endmodule

module Main_r2 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r2;
  assign r2 = arg0[15:8];
  assign res = r2;
endmodule

module ZL___unused103 (input logic [80:0] arg0,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  Main_outputs  inst (arg0, main_outputs_out);
  assign zi0 = main_outputs_out;
  assign res = {zi0, 7'h30, arg0};
endmodule

module Main_plusCW8$s1 (input logic [7:0] arg0,
  output logic [8:0] res);
  logic [8:0] zll_main_pluscw8$s2_out;
  ZLL_Main_plusCW8$s2  inst (({1'h0, arg0} + 9'h1) + 9'h0, zll_main_pluscw8$s2_out);
  assign res = zll_main_pluscw8$s2_out;
endmodule

module ZL_a65 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  logic [17:0] main_setweout_out;
  logic [17:0] zi1;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi2;
  logic [17:0] main_outputs_outR1;
  logic [17:0] zi3;
  logic [17:0] main_setaddrout_out;
  logic [17:0] zi4;
  logic [80:0] main_setoutputs_outR1;
  logic [80:0] zi5;
  logic [17:0] main_outputs_outR2;
  logic [17:0] zi6;
  Main_outputs  inst (arg3, main_outputs_out);
  assign zi0 = main_outputs_out;
  Main_setWeOut  instR1 (zi0, 1'h0, main_setweout_out);
  assign zi1 = main_setweout_out;
  Main_setOutputs  instR2 (arg3, zi1, main_setoutputs_out);
  assign zi2 = main_setoutputs_out;
  Main_outputs  instR3 (zi2, main_outputs_outR1);
  assign zi3 = main_outputs_outR1;
  Main_setAddrOut  instR4 (zi3, arg2, main_setaddrout_out);
  assign zi4 = main_setaddrout_out;
  Main_setOutputs  instR5 (zi2, zi4, main_setoutputs_outR1);
  assign zi5 = main_setoutputs_outR1;
  Main_outputs  instR6 (zi5, main_outputs_outR2);
  assign zi6 = main_outputs_outR2;
  assign res = {zi6, 5'h8, arg0, arg1, zi5};
endmodule

module ZL_fail7 (input logic [9:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [7:0] main_datain_out;
  logic [7:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [0:0] zi3;
  logic [0:0] zi4;
  logic [0:0] zi5;
  logic [0:0] zi6;
  logic [0:0] zi7;
  logic [0:0] zi8;
  logic [1:0] main_mkreg_out;
  logic [1:0] zi9;
  logic [7:0] main_pc_out;
  logic [7:0] zi10;
  logic [17:0] main_outputs_out;
  logic [17:0] zi11;
  logic [17:0] main_setaddrout_out;
  logic [17:0] zi12;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi13;
  logic [17:0] main_outputs_outR1;
  logic [17:0] zi14;
  logic [1:0] main_mkreg_outR1;
  logic [1:0] zi15;
  logic [7:0] main_r0_out;
  logic [105:0] zll_l_s90_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi16;
  logic [105:0] zl_a65_out;
  logic [7:0] main_r2_out;
  logic [7:0] zi17;
  logic [105:0] zl_a65_outR1;
  logic [7:0] main_r3_out;
  logic [105:0] zll_l_s90_outR1;
  logic [1:0] main_mkreg_outR2;
  logic [1:0] zi18;
  logic [7:0] main_r0_outR1;
  logic [7:0] zi19;
  logic [105:0] zl_a99_out;
  logic [7:0] main_r1_outR1;
  logic [7:0] zi20;
  logic [105:0] zl_a99_outR1;
  logic [7:0] main_r2_outR1;
  logic [7:0] zi21;
  logic [105:0] zl_a99_outR2;
  logic [7:0] main_r3_outR1;
  logic [7:0] zi22;
  logic [105:0] zl_a99_outR3;
  logic [1:0] main_mkreg_outR3;
  logic [1:0] zi23;
  logic [1:0] main_mkreg_outR4;
  logic [1:0] zi24;
  logic [7:0] main_r0_outR2;
  logic [105:0] zl_vd134_out;
  logic [7:0] main_r1_outR2;
  logic [105:0] zl_vd134_outR1;
  logic [7:0] main_r2_outR2;
  logic [105:0] zl_vd134_outR2;
  logic [7:0] main_r3_outR2;
  logic [105:0] zl_vd134_outR3;
  logic [1:0] main_mkreg_outR5;
  logic [1:0] zi25;
  logic [1:0] main_mkreg_outR6;
  logic [1:0] zi26;
  logic [7:0] main_r0_outR3;
  logic [105:0] zl_vd168_out;
  logic [7:0] main_r1_outR3;
  logic [105:0] zl_vd168_outR1;
  logic [7:0] main_r2_outR3;
  logic [105:0] zl_vd168_outR2;
  logic [7:0] main_r3_outR3;
  logic [105:0] zl_vd168_outR3;
  logic [1:0] main_mkreg_outR7;
  logic [1:0] zi27;
  logic [1:0] main_mkreg_outR8;
  logic [1:0] zi28;
  logic [7:0] main_r0_outR4;
  logic [105:0] zl_vd204_out;
  logic [7:0] main_r1_outR4;
  logic [105:0] zl_vd204_outR1;
  logic [7:0] main_r2_outR4;
  logic [105:0] zl_vd204_outR2;
  logic [7:0] main_r3_outR4;
  logic [105:0] zl_vd204_outR3;
  logic [1:0] main_mkreg_outR9;
  logic [1:0] zi29;
  logic [1:0] main_mkreg_outR10;
  logic [1:0] zi30;
  logic [7:0] main_r0_outR5;
  logic [105:0] zl_vd238_out;
  logic [7:0] main_r1_outR5;
  logic [105:0] zl_vd238_outR1;
  logic [7:0] main_r2_outR5;
  logic [105:0] zl_vd238_outR2;
  logic [7:0] main_r3_outR5;
  logic [105:0] zl_vd238_outR3;
  logic [1:0] main_mkreg_outR11;
  logic [1:0] zi31;
  logic [7:0] main_r0_outR6;
  logic [7:0] zi32;
  logic [105:0] zl_x274_out;
  logic [7:0] main_r1_outR6;
  logic [7:0] zi33;
  logic [105:0] zl_x274_outR1;
  logic [7:0] main_r2_outR6;
  logic [7:0] zi34;
  logic [105:0] zl_x274_outR2;
  logic [7:0] main_r3_outR6;
  logic [7:0] zi35;
  logic [105:0] zl_x274_outR3;
  logic [1:0] main_mkreg_outR12;
  logic [1:0] zi36;
  logic [1:0] main_mkreg_outR13;
  logic [1:0] zi37;
  logic [7:0] main_r0_outR7;
  logic [105:0] zl_vd297_out;
  logic [7:0] main_r1_outR7;
  logic [105:0] zl_vd297_outR1;
  logic [7:0] main_r2_outR7;
  logic [105:0] zl_vd297_outR2;
  logic [7:0] main_r3_outR7;
  logic [105:0] zl_vd297_outR3;
  logic [1:0] main_mkreg_outR14;
  logic [1:0] zi38;
  logic [1:0] main_mkreg_outR15;
  logic [1:0] zi39;
  logic [7:0] main_r0_outR8;
  logic [105:0] zl_vd333_out;
  logic [7:0] main_r1_outR8;
  logic [105:0] zl_vd333_outR1;
  logic [7:0] main_r2_outR8;
  logic [105:0] zl_vd333_outR2;
  logic [7:0] main_r3_outR8;
  logic [105:0] zl_vd333_outR3;
  logic [1:0] main_mkreg_outR16;
  logic [1:0] zi40;
  logic [1:0] main_mkreg_outR17;
  logic [1:0] zi41;
  logic [7:0] main_r0_outR9;
  logic [105:0] zl_vd369_out;
  logic [7:0] main_r1_outR9;
  logic [105:0] zl_vd369_outR1;
  logic [7:0] main_r2_outR9;
  logic [105:0] zl_vd369_outR2;
  logic [7:0] main_r3_outR9;
  logic [105:0] zl_vd369_outR3;
  logic [1:0] main_mkreg_outR18;
  logic [1:0] zi42;
  logic [7:0] main_r0_outR10;
  logic [7:0] zi43;
  logic [105:0] zl_vd405_out;
  logic [7:0] main_r1_outR10;
  logic [105:0] zll_l_s427_out;
  logic [7:0] main_r2_outR10;
  logic [105:0] zll_l_s427_outR1;
  logic [7:0] main_r3_outR10;
  logic [105:0] zll_l_s427_outR2;
  logic [0:0] main_zflag_out;
  logic [0:0] zi44;
  logic [105:0] zl__unused103_out;
  logic [105:0] zl_arm448_out;
  logic [0:0] main_zflag_outR1;
  logic [0:0] zi45;
  logic [0:0] main_notb_out;
  logic [105:0] zll_l_z451_out;
  logic [0:0] main_cflag_out;
  logic [0:0] zi46;
  logic [105:0] zl__unused103_outR1;
  logic [105:0] zl_arm448_outR1;
  logic [0:0] main_cflag_outR1;
  logic [0:0] zi47;
  logic [0:0] main_notb_outR1;
  logic [105:0] zll_l_z451_outR1;
  logic [1:0] main_mkreg_outR19;
  logic [1:0] zi48;
  logic [7:0] main_r0_outR11;
  logic [105:0] zll_l_s515_out;
  logic [7:0] main_r1_outR11;
  logic [105:0] zll_l_s515_outR1;
  logic [7:0] main_r2_outR11;
  logic [105:0] zll_l_s515_outR2;
  logic [7:0] main_r3_outR11;
  logic [105:0] zll_l_s515_outR3;
  logic [80:0] main_setieflag_out;
  logic [105:0] zl__unused103_outR2;
  logic [17:0] main_outputs_outR2;
  logic [17:0] zi49;
  logic [7:0] zi50;
  logic [7:0] zi51;
  logic [0:0] zi52;
  logic [105:0] zll_l_s537_out;
  logic [80:0] main_setieflag_outR1;
  logic [80:0] zi53;
  logic [7:0] zi54;
  logic [7:0] zi55;
  logic [80:0] main_setpc_out;
  logic [80:0] zi56;
  logic [0:0] zi57;
  logic [0:0] zi58;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi59;
  logic [0:0] zi60;
  logic [0:0] zi61;
  logic [80:0] main_setcflag_out;
  logic [105:0] zl__unused103_outR3;
  logic [1:0] main_mkreg_outR20;
  logic [1:0] zi62;
  logic [7:0] main_r0_outR12;
  logic [105:0] zll_l_s577_out;
  logic [7:0] main_r1_outR12;
  logic [7:0] zi63;
  logic [105:0] zl_v559_out;
  logic [7:0] main_r2_outR12;
  logic [105:0] zll_l_s577_outR1;
  logic [7:0] main_r3_outR12;
  logic [105:0] zll_l_s577_outR2;
  logic [1:0] main_mkreg_outR21;
  logic [1:0] zi64;
  logic [80:0] main_setr0_out;
  logic [105:0] zl__unused103_outR4;
  logic [80:0] main_setr1_out;
  logic [105:0] zl__unused103_outR5;
  logic [80:0] main_setr2_out;
  logic [105:0] zl__unused103_outR6;
  logic [80:0] main_setr3_out;
  logic [105:0] zl__unused103_outR7;
  logic [1:0] main_mkreg_outR22;
  logic [1:0] zi65;
  logic [7:0] main_r0_outR13;
  logic [105:0] zll_l_s616_out;
  logic [7:0] main_r1_outR13;
  logic [105:0] zll_l_s616_outR1;
  logic [7:0] main_r2_outR13;
  logic [105:0] zll_l_s616_outR2;
  logic [7:0] main_r3_outR13;
  logic [105:0] zll_l_s616_outR3;
  logic [1:0] main_mkreg_outR23;
  logic [1:0] zi66;
  logic [7:0] main_r0_outR14;
  logic [105:0] zll_l_s643_out;
  logic [7:0] main_r1_outR14;
  logic [105:0] zll_l_s643_outR1;
  logic [7:0] main_r2_outR14;
  logic [7:0] zi67;
  logic [105:0] zl_v623_out;
  logic [7:0] main_r3_outR14;
  logic [105:0] zll_l_s643_outR2;
  logic [1:0] main_mkreg_outR24;
  logic [1:0] zi68;
  logic [7:0] main_r0_outR15;
  logic [105:0] zll_l_s664_out;
  logic [7:0] main_r1_outR15;
  logic [105:0] zll_l_s666_out;
  logic [7:0] main_r2_outR15;
  logic [105:0] zll_l_s664_outR1;
  logic [7:0] main_r3_outR15;
  logic [105:0] zll_l_s666_outR1;
  logic [7:0] main_r0_outR16;
  logic [105:0] zll_l_s688_out;
  logic [7:0] main_r1_outR16;
  logic [105:0] zll_l_s688_outR1;
  logic [7:0] main_r2_outR16;
  logic [105:0] zll_l_s692_out;
  logic [7:0] main_r3_outR16;
  logic [105:0] zll_l_s692_outR1;
  logic [1:0] main_mkreg_outR25;
  logic [1:0] zi69;
  logic [7:0] main_r0_outR17;
  logic [7:0] zi70;
  logic [105:0] zl_v695_out;
  logic [7:0] main_r1_outR17;
  logic [7:0] zi71;
  logic [105:0] zl_v695_outR1;
  logic [7:0] main_r2_outR17;
  logic [7:0] zi72;
  logic [105:0] zl_v695_outR2;
  logic [7:0] main_r3_outR17;
  logic [7:0] zi73;
  logic [105:0] zl_v695_outR3;
  Main_dataIn  inst (arg0, main_datain_out);
  assign zi0 = main_datain_out;
  assign zi1 = zi0[6];
  assign zi2 = zi0[5];
  assign zi3 = zi0[4];
  assign zi4 = zi0[3];
  assign zi5 = zi0[2];
  assign zi6 = zi0[1];
  assign zi7 = zi0[0];
  assign zi8 = zi0[7];
  Main_mkReg  instR1 (zi6, zi7, main_mkreg_out);
  assign zi9 = main_mkreg_out;
  Main_pc  instR2 (arg1, main_pc_out);
  assign zi10 = main_pc_out;
  Main_outputs  instR3 (arg1, main_outputs_out);
  assign zi11 = main_outputs_out;
  Main_setAddrOut  instR4 (zi11, zi10, main_setaddrout_out);
  assign zi12 = main_setaddrout_out;
  Main_setOutputs  instR5 (arg1, zi12, main_setoutputs_out);
  assign zi13 = main_setoutputs_out;
  Main_outputs  instR6 (zi13, main_outputs_outR1);
  assign zi14 = main_outputs_outR1;
  Main_mkReg  instR7 (zi6, zi7, main_mkreg_outR1);
  assign zi15 = main_mkreg_outR1;
  Main_r0  instR8 (arg1, main_r0_out);
  ZLL_L_s90  instR9 (zi5, arg1, zi4, main_r0_out, zll_l_s90_out);
  Main_r1  instR10 (arg1, main_r1_out);
  assign zi16 = main_r1_out;
  ZL_a65  instR11 (zi4, zi5, zi16, arg1, zl_a65_out);
  Main_r2  instR12 (arg1, main_r2_out);
  assign zi17 = main_r2_out;
  ZL_a65  instR13 (zi4, zi5, zi17, arg1, zl_a65_outR1);
  Main_r3  instR14 (arg1, main_r3_out);
  ZLL_L_s90  instR15 (zi5, arg1, zi4, main_r3_out, zll_l_s90_outR1);
  Main_mkReg  instR16 (zi6, zi7, main_mkreg_outR2);
  assign zi18 = main_mkreg_outR2;
  Main_r0  instR17 (arg1, main_r0_outR1);
  assign zi19 = main_r0_outR1;
  ZL_a99  instR18 (zi4, zi5, zi19, arg1, zl_a99_out);
  Main_r1  instR19 (arg1, main_r1_outR1);
  assign zi20 = main_r1_outR1;
  ZL_a99  instR20 (zi4, zi5, zi20, arg1, zl_a99_outR1);
  Main_r2  instR21 (arg1, main_r2_outR1);
  assign zi21 = main_r2_outR1;
  ZL_a99  instR22 (zi4, zi5, zi21, arg1, zl_a99_outR2);
  Main_r3  instR23 (arg1, main_r3_outR1);
  assign zi22 = main_r3_outR1;
  ZL_a99  instR24 (zi4, zi5, zi22, arg1, zl_a99_outR3);
  Main_mkReg  instR25 (zi4, zi5, main_mkreg_outR3);
  assign zi23 = main_mkreg_outR3;
  Main_mkReg  instR26 (zi6, zi7, main_mkreg_outR4);
  assign zi24 = main_mkreg_outR4;
  Main_r0  instR27 (arg1, main_r0_outR2);
  ZL_vD134  instR28 (zi23, zi24, main_r0_outR2, arg1, zl_vd134_out);
  Main_r1  instR29 (arg1, main_r1_outR2);
  ZL_vD134  instR30 (zi23, zi24, main_r1_outR2, arg1, zl_vd134_outR1);
  Main_r2  instR31 (arg1, main_r2_outR2);
  ZL_vD134  instR32 (zi23, zi24, main_r2_outR2, arg1, zl_vd134_outR2);
  Main_r3  instR33 (arg1, main_r3_outR2);
  ZL_vD134  instR34 (zi23, zi24, main_r3_outR2, arg1, zl_vd134_outR3);
  Main_mkReg  instR35 (zi4, zi5, main_mkreg_outR5);
  assign zi25 = main_mkreg_outR5;
  Main_mkReg  instR36 (zi6, zi7, main_mkreg_outR6);
  assign zi26 = main_mkreg_outR6;
  Main_r0  instR37 (arg1, main_r0_outR3);
  ZL_vD168  instR38 (zi25, zi26, main_r0_outR3, arg1, zl_vd168_out);
  Main_r1  instR39 (arg1, main_r1_outR3);
  ZL_vD168  instR40 (zi25, zi26, main_r1_outR3, arg1, zl_vd168_outR1);
  Main_r2  instR41 (arg1, main_r2_outR3);
  ZL_vD168  instR42 (zi25, zi26, main_r2_outR3, arg1, zl_vd168_outR2);
  Main_r3  instR43 (arg1, main_r3_outR3);
  ZL_vD168  instR44 (zi25, zi26, main_r3_outR3, arg1, zl_vd168_outR3);
  Main_mkReg  instR45 (zi4, zi5, main_mkreg_outR7);
  assign zi27 = main_mkreg_outR7;
  Main_mkReg  instR46 (zi6, zi7, main_mkreg_outR8);
  assign zi28 = main_mkreg_outR8;
  Main_r0  instR47 (arg1, main_r0_outR4);
  ZL_vD204  instR48 (zi27, zi28, main_r0_outR4, arg1, zl_vd204_out);
  Main_r1  instR49 (arg1, main_r1_outR4);
  ZL_vD204  instR50 (zi27, zi28, main_r1_outR4, arg1, zl_vd204_outR1);
  Main_r2  instR51 (arg1, main_r2_outR4);
  ZL_vD204  instR52 (zi27, zi28, main_r2_outR4, arg1, zl_vd204_outR2);
  Main_r3  instR53 (arg1, main_r3_outR4);
  ZL_vD204  instR54 (zi27, zi28, main_r3_outR4, arg1, zl_vd204_outR3);
  Main_mkReg  instR55 (zi4, zi5, main_mkreg_outR9);
  assign zi29 = main_mkreg_outR9;
  Main_mkReg  instR56 (zi6, zi7, main_mkreg_outR10);
  assign zi30 = main_mkreg_outR10;
  Main_r0  instR57 (arg1, main_r0_outR5);
  ZL_vD238  instR58 (zi29, zi30, main_r0_outR5, arg1, zl_vd238_out);
  Main_r1  instR59 (arg1, main_r1_outR5);
  ZL_vD238  instR60 (zi29, zi30, main_r1_outR5, arg1, zl_vd238_outR1);
  Main_r2  instR61 (arg1, main_r2_outR5);
  ZL_vD238  instR62 (zi29, zi30, main_r2_outR5, arg1, zl_vd238_outR2);
  Main_r3  instR63 (arg1, main_r3_outR5);
  ZL_vD238  instR64 (zi29, zi30, main_r3_outR5, arg1, zl_vd238_outR3);
  Main_mkReg  instR65 (zi6, zi7, main_mkreg_outR11);
  assign zi31 = main_mkreg_outR11;
  Main_r0  instR66 (arg1, main_r0_outR6);
  assign zi32 = main_r0_outR6;
  ZL_x274  instR67 (zi4, zi5, zi32, arg1, zl_x274_out);
  Main_r1  instR68 (arg1, main_r1_outR6);
  assign zi33 = main_r1_outR6;
  ZL_x274  instR69 (zi4, zi5, zi33, arg1, zl_x274_outR1);
  Main_r2  instR70 (arg1, main_r2_outR6);
  assign zi34 = main_r2_outR6;
  ZL_x274  instR71 (zi4, zi5, zi34, arg1, zl_x274_outR2);
  Main_r3  instR72 (arg1, main_r3_outR6);
  assign zi35 = main_r3_outR6;
  ZL_x274  instR73 (zi4, zi5, zi35, arg1, zl_x274_outR3);
  Main_mkReg  instR74 (zi4, zi5, main_mkreg_outR12);
  assign zi36 = main_mkreg_outR12;
  Main_mkReg  instR75 (zi6, zi7, main_mkreg_outR13);
  assign zi37 = main_mkreg_outR13;
  Main_r0  instR76 (arg1, main_r0_outR7);
  ZL_vD297  instR77 (zi36, zi37, main_r0_outR7, arg1, zl_vd297_out);
  Main_r1  instR78 (arg1, main_r1_outR7);
  ZL_vD297  instR79 (zi36, zi37, main_r1_outR7, arg1, zl_vd297_outR1);
  Main_r2  instR80 (arg1, main_r2_outR7);
  ZL_vD297  instR81 (zi36, zi37, main_r2_outR7, arg1, zl_vd297_outR2);
  Main_r3  instR82 (arg1, main_r3_outR7);
  ZL_vD297  instR83 (zi36, zi37, main_r3_outR7, arg1, zl_vd297_outR3);
  Main_mkReg  instR84 (zi4, zi5, main_mkreg_outR14);
  assign zi38 = main_mkreg_outR14;
  Main_mkReg  instR85 (zi6, zi7, main_mkreg_outR15);
  assign zi39 = main_mkreg_outR15;
  Main_r0  instR86 (arg1, main_r0_outR8);
  ZL_vD333  instR87 (zi38, zi39, main_r0_outR8, arg1, zl_vd333_out);
  Main_r1  instR88 (arg1, main_r1_outR8);
  ZL_vD333  instR89 (zi38, zi39, main_r1_outR8, arg1, zl_vd333_outR1);
  Main_r2  instR90 (arg1, main_r2_outR8);
  ZL_vD333  instR91 (zi38, zi39, main_r2_outR8, arg1, zl_vd333_outR2);
  Main_r3  instR92 (arg1, main_r3_outR8);
  ZL_vD333  instR93 (zi38, zi39, main_r3_outR8, arg1, zl_vd333_outR3);
  Main_mkReg  instR94 (zi4, zi5, main_mkreg_outR16);
  assign zi40 = main_mkreg_outR16;
  Main_mkReg  instR95 (zi6, zi7, main_mkreg_outR17);
  assign zi41 = main_mkreg_outR17;
  Main_r0  instR96 (arg1, main_r0_outR9);
  ZL_vD369  instR97 (zi40, zi41, main_r0_outR9, arg1, zl_vd369_out);
  Main_r1  instR98 (arg1, main_r1_outR9);
  ZL_vD369  instR99 (zi40, zi41, main_r1_outR9, arg1, zl_vd369_outR1);
  Main_r2  instR100 (arg1, main_r2_outR9);
  ZL_vD369  instR101 (zi40, zi41, main_r2_outR9, arg1, zl_vd369_outR2);
  Main_r3  instR102 (arg1, main_r3_outR9);
  ZL_vD369  instR103 (zi40, zi41, main_r3_outR9, arg1, zl_vd369_outR3);
  Main_mkReg  instR104 (zi4, zi5, main_mkreg_outR18);
  assign zi42 = main_mkreg_outR18;
  Main_r0  instR105 (arg1, main_r0_outR10);
  assign zi43 = main_r0_outR10;
  ZL_vD405  instR106 (zi6, zi7, zi43, arg1, zl_vd405_out);
  Main_r1  instR107 (arg1, main_r1_outR10);
  ZLL_L_s427  instR108 (arg1, zi7, zi6, main_r1_outR10, zll_l_s427_out);
  Main_r2  instR109 (arg1, main_r2_outR10);
  ZLL_L_s427  instR110 (arg1, zi7, zi6, main_r2_outR10, zll_l_s427_outR1);
  Main_r3  instR111 (arg1, main_r3_outR10);
  ZLL_L_s427  instR112 (arg1, zi7, zi6, main_r3_outR10, zll_l_s427_outR2);
  Main_zFlag  instR113 (arg1, main_zflag_out);
  assign zi44 = main_zflag_out;
  ZL___unused103  instR114 (arg1, zl__unused103_out);
  ZL_arm448  instR115 (zi6, zi7, arg1, zl_arm448_out);
  Main_zFlag  instR116 (arg1, main_zflag_outR1);
  assign zi45 = main_zflag_outR1;
  Main_notb  instR117 (zi45, main_notb_out);
  ZLL_L_z451  instR118 (arg1, zi7, zi6, main_notb_out, zll_l_z451_out);
  Main_cFlag  instR119 (arg1, main_cflag_out);
  assign zi46 = main_cflag_out;
  ZL___unused103  instR120 (arg1, zl__unused103_outR1);
  ZL_arm448  instR121 (zi6, zi7, arg1, zl_arm448_outR1);
  Main_cFlag  instR122 (arg1, main_cflag_outR1);
  assign zi47 = main_cflag_outR1;
  Main_notb  instR123 (zi47, main_notb_outR1);
  ZLL_L_z451  instR124 (arg1, zi7, zi6, main_notb_outR1, zll_l_z451_outR1);
  Main_mkReg  instR125 (zi6, zi7, main_mkreg_outR19);
  assign zi48 = main_mkreg_outR19;
  Main_r0  instR126 (arg1, main_r0_outR11);
  ZLL_L_s515  instR127 (arg1, main_r0_outR11, zll_l_s515_out);
  Main_r1  instR128 (arg1, main_r1_outR11);
  ZLL_L_s515  instR129 (arg1, main_r1_outR11, zll_l_s515_outR1);
  Main_r2  instR130 (arg1, main_r2_outR11);
  ZLL_L_s515  instR131 (arg1, main_r2_outR11, zll_l_s515_outR2);
  Main_r3  instR132 (arg1, main_r3_outR11);
  ZLL_L_s515  instR133 (arg1, main_r3_outR11, zll_l_s515_outR3);
  Main_setIEFlag  instR134 (arg1, zi7, main_setieflag_out);
  ZL___unused103  instR135 (main_setieflag_out, zl__unused103_outR2);
  Main_outputs  instR136 (arg1, main_outputs_outR2);
  assign zi49 = main_outputs_outR2;
  assign zi50 = zi49[17:10];
  assign zi51 = zi49[9:2];
  assign zi52 = zi49[1];
  ZLL_L_s537  instR137 (arg1, {zi50, zi51, zi52, 1'h1}, zll_l_s537_out);
  Main_setIEFlag  instR138 (arg1, 1'h1, main_setieflag_outR1);
  assign zi53 = main_setieflag_outR1;
  assign zi54 = zi53[39:32];
  assign zi55 = zi54;
  Main_setPC  instR139 (zi53, zi55, main_setpc_out);
  assign zi56 = main_setpc_out;
  assign zi57 = zi56[41];
  assign zi58 = zi57;
  Main_setZFlag  instR140 (zi56, zi58, main_setzflag_out);
  assign zi59 = main_setzflag_out;
  assign zi60 = zi59[40];
  assign zi61 = zi60;
  Main_setCFlag  instR141 (zi59, zi61, main_setcflag_out);
  ZL___unused103  instR142 (main_setcflag_out, zl__unused103_outR3);
  Main_mkReg  instR143 (zi6, zi7, main_mkreg_outR20);
  assign zi62 = main_mkreg_outR20;
  Main_r0  instR144 (arg1, main_r0_outR12);
  ZLL_L_s577  instR145 (arg1, zi62, main_r0_outR12, zll_l_s577_out);
  Main_r1  instR146 (arg1, main_r1_outR12);
  assign zi63 = main_r1_outR12;
  ZL_v559  instR147 (zi62, zi63, arg1, zl_v559_out);
  Main_r2  instR148 (arg1, main_r2_outR12);
  ZLL_L_s577  instR149 (arg1, zi62, main_r2_outR12, zll_l_s577_outR1);
  Main_r3  instR150 (arg1, main_r3_outR12);
  ZLL_L_s577  instR151 (arg1, zi62, main_r3_outR12, zll_l_s577_outR2);
  Main_mkReg  instR152 (zi6, zi7, main_mkreg_outR21);
  assign zi64 = main_mkreg_outR21;
  Main_setR0  instR153 (arg1, 8'h0, main_setr0_out);
  ZL___unused103  instR154 (main_setr0_out, zl__unused103_outR4);
  Main_setR1  instR155 (arg1, 8'h0, main_setr1_out);
  ZL___unused103  instR156 (main_setr1_out, zl__unused103_outR5);
  Main_setR2  instR157 (arg1, 8'h0, main_setr2_out);
  ZL___unused103  instR158 (main_setr2_out, zl__unused103_outR6);
  Main_setR3  instR159 (arg1, 8'h0, main_setr3_out);
  ZL___unused103  instR160 (main_setr3_out, zl__unused103_outR7);
  Main_mkReg  instR161 (zi6, zi7, main_mkreg_outR22);
  assign zi65 = main_mkreg_outR22;
  Main_r0  instR162 (arg1, main_r0_outR13);
  ZLL_L_s616  instR163 (zi65, arg1, main_r0_outR13, zll_l_s616_out);
  Main_r1  instR164 (arg1, main_r1_outR13);
  ZLL_L_s616  instR165 (zi65, arg1, main_r1_outR13, zll_l_s616_outR1);
  Main_r2  instR166 (arg1, main_r2_outR13);
  ZLL_L_s616  instR167 (zi65, arg1, main_r2_outR13, zll_l_s616_outR2);
  Main_r3  instR168 (arg1, main_r3_outR13);
  ZLL_L_s616  instR169 (zi65, arg1, main_r3_outR13, zll_l_s616_outR3);
  Main_mkReg  instR170 (zi6, zi7, main_mkreg_outR23);
  assign zi66 = main_mkreg_outR23;
  Main_r0  instR171 (arg1, main_r0_outR14);
  ZLL_L_s643  instR172 (arg1, zi66, main_r0_outR14, zll_l_s643_out);
  Main_r1  instR173 (arg1, main_r1_outR14);
  ZLL_L_s643  instR174 (arg1, zi66, main_r1_outR14, zll_l_s643_outR1);
  Main_r2  instR175 (arg1, main_r2_outR14);
  assign zi67 = main_r2_outR14;
  ZL_v623  instR176 (zi66, zi67, arg1, zl_v623_out);
  Main_r3  instR177 (arg1, main_r3_outR14);
  ZLL_L_s643  instR178 (arg1, zi66, main_r3_outR14, zll_l_s643_outR2);
  Main_mkReg  instR179 (zi6, zi7, main_mkreg_outR24);
  assign zi68 = main_mkreg_outR24;
  Main_r0  instR180 (arg1, main_r0_outR15);
  ZLL_L_s664  instR181 (zi68, arg1, main_r0_outR15, zll_l_s664_out);
  Main_r1  instR182 (arg1, main_r1_outR15);
  ZLL_L_s666  instR183 (arg1, zi68, main_r1_outR15, zll_l_s666_out);
  Main_r2  instR184 (arg1, main_r2_outR15);
  ZLL_L_s664  instR185 (zi68, arg1, main_r2_outR15, zll_l_s664_outR1);
  Main_r3  instR186 (arg1, main_r3_outR15);
  ZLL_L_s666  instR187 (arg1, zi68, main_r3_outR15, zll_l_s666_outR1);
  Main_r0  instR188 (arg1, main_r0_outR16);
  ZLL_L_s688  instR189 (zi68, arg1, main_r0_outR16, zll_l_s688_out);
  Main_r1  instR190 (arg1, main_r1_outR16);
  ZLL_L_s688  instR191 (zi68, arg1, main_r1_outR16, zll_l_s688_outR1);
  Main_r2  instR192 (arg1, main_r2_outR16);
  ZLL_L_s692  instR193 (arg1, zi68, main_r2_outR16, zll_l_s692_out);
  Main_r3  instR194 (arg1, main_r3_outR16);
  ZLL_L_s692  instR195 (arg1, zi68, main_r3_outR16, zll_l_s692_outR1);
  Main_mkReg  instR196 (zi6, zi7, main_mkreg_outR25);
  assign zi69 = main_mkreg_outR25;
  Main_r0  instR197 (arg1, main_r0_outR17);
  assign zi70 = main_r0_outR17;
  ZL_v695  instR198 (zi4, zi5, zi69, zi70, arg1, zl_v695_out);
  Main_r1  instR199 (arg1, main_r1_outR17);
  assign zi71 = main_r1_outR17;
  ZL_v695  instR200 (zi4, zi5, zi69, zi71, arg1, zl_v695_outR1);
  Main_r2  instR201 (arg1, main_r2_outR17);
  assign zi72 = main_r2_outR17;
  ZL_v695  instR202 (zi4, zi5, zi69, zi72, arg1, zl_v695_outR2);
  Main_r3  instR203 (arg1, main_r3_outR17);
  assign zi73 = main_r3_outR17;
  ZL_v695  instR204 (zi4, zi5, zi69, zi73, arg1, zl_v695_outR3);
  assign res = (zi8 == 1'h0) ? ((zi1 == 1'h0) ? ((zi2 == 1'h0) ? ((zi3 == 1'h0) ? {zi14, 3'h1, zi4, zi5, zi9, zi13} : ((zi15 == 2'h0) ? zll_l_s90_out : ((zi15 == 2'h1) ? zl_a65_out : ((zi15 == 2'h2) ? zl_a65_outR1 : zll_l_s90_outR1)))) : ((zi3 == 1'h0) ? ((zi18 == 2'h0) ? zl_a99_out : ((zi18 == 2'h1) ? zl_a99_outR1 : ((zi18 == 2'h2) ? zl_a99_outR2 : zl_a99_outR3))) : ((zi23 == 2'h0) ? zl_vd134_out : ((zi23 == 2'h1) ? zl_vd134_outR1 : ((zi23 == 2'h2) ? zl_vd134_outR2 : zl_vd134_outR3))))) : ((zi2 == 1'h0) ? ((zi3 == 1'h0) ? ((zi25 == 2'h0) ? zl_vd168_out : ((zi25 == 2'h1) ? zl_vd168_outR1 : ((zi25 == 2'h2) ? zl_vd168_outR2 : zl_vd168_outR3))) : ((zi27 == 2'h0) ? zl_vd204_out : ((zi27 == 2'h1) ? zl_vd204_outR1 : ((zi27 == 2'h2) ? zl_vd204_outR2 : zl_vd204_outR3)))) : ((zi3 == 1'h0) ? ((zi29 == 2'h0) ? zl_vd238_out : ((zi29 == 2'h1) ? zl_vd238_outR1 : ((zi29 == 2'h2) ? zl_vd238_outR2 : zl_vd238_outR3))) : ((zi31 == 2'h0) ? zl_x274_out : ((zi31 == 2'h1) ? zl_x274_outR1 : ((zi31 == 2'h2) ? zl_x274_outR2 : zl_x274_outR3)))))) : ((zi1 == 1'h0) ? ((zi2 == 1'h0) ? ((zi3 == 1'h0) ? ((zi36 == 2'h0) ? zl_vd297_out : ((zi36 == 2'h1) ? zl_vd297_outR1 : ((zi36 == 2'h2) ? zl_vd297_outR2 : zl_vd297_outR3))) : ((zi38 == 2'h0) ? zl_vd333_out : ((zi38 == 2'h1) ? zl_vd333_outR1 : ((zi38 == 2'h2) ? zl_vd333_outR2 : zl_vd333_outR3)))) : ((zi3 == 1'h0) ? ((zi40 == 2'h0) ? zl_vd369_out : ((zi40 == 2'h1) ? zl_vd369_outR1 : ((zi40 == 2'h2) ? zl_vd369_outR2 : zl_vd369_outR3))) : ((zi42 == 2'h0) ? zl_vd405_out : ((zi42 == 2'h1) ? zll_l_s427_out : ((zi42 == 2'h2) ? zll_l_s427_outR1 : zll_l_s427_outR2))))) : ((zi2 == 1'h0) ? ((zi3 == 1'h0) ? ((zi4 == 1'h0) ? ((zi5 == 1'h0) ? ((zi44 == 1'h0) ? zl__unused103_out : zl_arm448_out) : zll_l_z451_out) : ((zi5 == 1'h0) ? ((zi46 == 1'h0) ? zl__unused103_outR1 : zl_arm448_outR1) : zll_l_z451_outR1)) : ((zi4 == 1'h0) ? ((zi5 == 1'h0) ? ((zi48 == 2'h0) ? zll_l_s515_out : ((zi48 == 2'h1) ? zll_l_s515_outR1 : ((zi48 == 2'h2) ? zll_l_s515_outR2 : zll_l_s515_outR3))) : ((zi6 == 1'h0) ? zl__unused103_outR2 : ((zi7 == 1'h0) ? zll_l_s537_out : zl__unused103_outR3))) : ((zi5 == 1'h0) ? ((zi62 == 2'h0) ? zll_l_s577_out : ((zi62 == 2'h1) ? zl_v559_out : ((zi62 == 2'h2) ? zll_l_s577_outR1 : zll_l_s577_outR2))) : ((zi64 == 2'h0) ? zl__unused103_outR4 : ((zi64 == 2'h1) ? zl__unused103_outR5 : ((zi64 == 2'h2) ? zl__unused103_outR6 : zl__unused103_outR7)))))) : ((zi3 == 1'h0) ? ((zi4 == 1'h0) ? ((zi5 == 1'h0) ? ((zi65 == 2'h0) ? zll_l_s616_out : ((zi65 == 2'h1) ? zll_l_s616_outR1 : ((zi65 == 2'h2) ? zll_l_s616_outR2 : zll_l_s616_outR3))) : ((zi66 == 2'h0) ? zll_l_s643_out : ((zi66 == 2'h1) ? zll_l_s643_outR1 : ((zi66 == 2'h2) ? zl_v623_out : zll_l_s643_outR2)))) : ((zi5 == 1'h0) ? ((zi68 == 2'h0) ? zll_l_s664_out : ((zi68 == 2'h1) ? zll_l_s666_out : ((zi68 == 2'h2) ? zll_l_s664_outR1 : zll_l_s666_outR1))) : ((zi68 == 2'h0) ? zll_l_s688_out : ((zi68 == 2'h1) ? zll_l_s688_outR1 : ((zi68 == 2'h2) ? zll_l_s692_out : zll_l_s692_outR1))))) : ((zi69 == 2'h0) ? zl_v695_out : ((zi69 == 2'h1) ? zl_v695_outR1 : ((zi69 == 2'h2) ? zl_v695_outR2 : zl_v695_outR3))))));
endmodule

module ZL_arm613 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [105:0] zl__unused597_out;
  Main_setR3  inst (arg2, arg1, main_setr3_out);
  ZL___unused597  instR1 (arg0, arg1, main_setr3_out, zl__unused597_out);
  assign res = zl__unused597_out;
endmodule

module ZL_arm609 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [105:0] zl__unused597_out;
  Main_setR1  inst (arg2, arg1, main_setr1_out);
  ZL___unused597  instR1 (arg0, arg1, main_setr1_out, zl__unused597_out);
  assign res = zl__unused597_out;
endmodule

module Main_cFlag (input logic [80:0] arg0,
  output logic [0:0] res);
  logic [0:0] c;
  assign c = arg0[51];
  assign res = c;
endmodule

module Main_setWeOut (input logic [17:0] arg0,
  input logic [0:0] arg1,
  output logic [17:0] res);
  logic [7:0] a_o;
  logic [7:0] d_o;
  logic [0:0] iack_o;
  assign a_o = arg0[17:10];
  assign d_o = arg0[9:2];
  assign iack_o = arg0[0];
  assign res = {a_o, d_o, arg1, iack_o};
endmodule

module ZL_vD297 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] zl_vs298_out;
  logic [7:0] main_r1_out;
  logic [105:0] zl_vs298_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zl_vs298_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zl_vs298_outR3;
  Main_r0  inst (arg3, main_r0_out);
  ZL_vS298  instR1 (arg0, arg2, main_r0_out, arg3, zl_vs298_out);
  Main_r1  instR2 (arg3, main_r1_out);
  ZL_vS298  instR3 (arg0, arg2, main_r1_out, arg3, zl_vs298_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  ZL_vS298  instR5 (arg0, arg2, main_r2_out, arg3, zl_vs298_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  ZL_vS298  instR7 (arg0, arg2, main_r3_out, arg3, zl_vs298_outR3);
  assign res = (arg1 == 2'h0) ? zl_vs298_out : ((arg1 == 2'h1) ? zl_vs298_outR1 : ((arg1 == 2'h2) ? zl_vs298_outR2 : zl_vs298_outR3));
endmodule

module ZL___unused14 (input logic [0:0] arg0,
  input logic [1:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] main_pc_out;
  logic [7:0] zi0;
  logic [8:0] main_pluscw8$s1_out;
  logic [8:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [80:0] main_setpc_out;
  logic [80:0] zi4;
  logic [17:0] main_outputs_out;
  logic [17:0] zi5;
  Main_pc  inst (arg2, main_pc_out);
  assign zi0 = main_pc_out;
  Main_plusCW8$s1  instR1 (zi0, main_pluscw8$s1_out);
  assign zi1 = main_pluscw8$s1_out;
  assign zi2 = zi1[7:0];
  assign zi3 = zi2;
  Main_setPC  instR2 (arg2, zi3, main_setpc_out);
  assign zi4 = main_setpc_out;
  Main_outputs  instR3 (zi4, main_outputs_out);
  assign zi5 = main_outputs_out;
  assign res = {zi5, 4'h0, arg0, arg1, zi4};
endmodule

module ZL_vD204 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] zl_vs205_out;
  logic [7:0] main_r1_out;
  logic [105:0] zl_vs205_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zl_vs205_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zl_vs205_outR3;
  Main_r0  inst (arg3, main_r0_out);
  ZL_vS205  instR1 (arg0, arg2, main_r0_out, arg3, zl_vs205_out);
  Main_r1  instR2 (arg3, main_r1_out);
  ZL_vS205  instR3 (arg0, arg2, main_r1_out, arg3, zl_vs205_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  ZL_vS205  instR5 (arg0, arg2, main_r2_out, arg3, zl_vs205_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  ZL_vS205  instR7 (arg0, arg2, main_r3_out, arg3, zl_vs205_outR3);
  assign res = (arg1 == 2'h0) ? zl_vs205_out : ((arg1 == 2'h1) ? zl_vs205_outR1 : ((arg1 == 2'h2) ? zl_vs205_outR2 : zl_vs205_outR3));
endmodule

module ZL_vS298 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [105:0] zll_l_vs370_out;
  ZLL_L_vS370  inst (arg3, arg0, arg1 | arg2, zll_l_vs370_out);
  assign res = zll_l_vs370_out;
endmodule

module ZL_vS239 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] main_cflag_out;
  logic [0:0] zi0;
  logic [8:0] zll_main_pluscw8$s2_out;
  logic [105:0] zll_l_cin240_out;
  Main_cFlag  inst (arg3, main_cflag_out);
  assign zi0 = main_cflag_out;
  ZLL_Main_plusCW8$s2  instR1 (({1'h0, arg1} - {1'h0, arg2}) - {8'h0, zi0}, zll_main_pluscw8$s2_out);
  ZLL_L_cin240  instR2 (arg0, arg3, zll_main_pluscw8$s2_out, zll_l_cin240_out);
  assign res = zll_l_cin240_out;
endmodule

module ZL_vD134 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] zl_vs135_out;
  logic [7:0] main_r1_out;
  logic [105:0] zl_vs135_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] zl_vs135_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] zl_vs135_outR3;
  Main_r0  inst (arg3, main_r0_out);
  ZL_vS135  instR1 (arg0, arg2, main_r0_out, arg3, zl_vs135_out);
  Main_r1  instR2 (arg3, main_r1_out);
  ZL_vS135  instR3 (arg0, arg2, main_r1_out, arg3, zl_vs135_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  ZL_vS135  instR5 (arg0, arg2, main_r2_out, arg3, zl_vs135_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  ZL_vS135  instR7 (arg0, arg2, main_r3_out, arg3, zl_vs135_outR3);
  assign res = (arg1 == 2'h0) ? zl_vs135_out : ((arg1 == 2'h1) ? zl_vs135_outR1 : ((arg1 == 2'h2) ? zl_vs135_outR2 : zl_vs135_outR3));
endmodule

module ZL_vD405 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] zi0;
  logic [7:0] main_r0_out;
  logic [105:0] zll_l_s421_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] zl_vs406_out;
  logic [7:0] main_r2_out;
  logic [105:0] zll_l_s421_outR1;
  logic [7:0] main_r3_out;
  logic [105:0] zll_l_s421_outR2;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign zi0 = main_mkreg_out;
  Main_r0  instR1 (arg3, main_r0_out);
  ZLL_L_s421  instR2 (arg3, arg2, main_r0_out, zll_l_s421_out);
  Main_r1  instR3 (arg3, main_r1_out);
  assign zi1 = main_r1_out;
  ZL_vS406  instR4 (arg2, zi1, arg3, zl_vs406_out);
  Main_r2  instR5 (arg3, main_r2_out);
  ZLL_L_s421  instR6 (arg3, arg2, main_r2_out, zll_l_s421_outR1);
  Main_r3  instR7 (arg3, main_r3_out);
  ZLL_L_s421  instR8 (arg3, arg2, main_r3_out, zll_l_s421_outR2);
  assign res = (zi0 == 2'h0) ? zll_l_s421_out : ((zi0 == 2'h1) ? zl_vs406_out : ((zi0 == 2'h2) ? zll_l_s421_outR1 : zll_l_s421_outR2));
endmodule

module ZL_vS205 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [8:0] main_minuscw8$s1_out;
  logic [105:0] zll_l_vs205_out;
  Main_minusCW8$s1  inst (arg1, arg2, main_minuscw8$s1_out);
  ZLL_L_vS205  instR1 (arg3, arg0, main_minuscw8$s1_out, zll_l_vs205_out);
  assign res = zll_l_vs205_out;
endmodule

module ZL_vS370 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [105:0] zll_l_vs370_out;
  ZLL_L_vS370  inst (arg3, arg0, arg1 ^ arg2, zll_l_vs370_out);
  assign res = zll_l_vs370_out;
endmodule

module Main_zFlag (input logic [80:0] arg0,
  output logic [0:0] res);
  logic [0:0] z;
  assign z = arg0[52];
  assign res = z;
endmodule

module ZL_vS169 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] main_cflag_out;
  logic [0:0] zi0;
  logic [8:0] zll_main_pluscw8$s2_out;
  logic [105:0] zll_l_vs205_out;
  Main_cFlag  inst (arg3, main_cflag_out);
  assign zi0 = main_cflag_out;
  ZLL_Main_plusCW8$s2  instR1 (({1'h0, arg1} + {1'h0, arg2}) + {8'h0, zi0}, zll_main_pluscw8$s2_out);
  ZLL_L_vS205  instR2 (arg3, arg0, zll_main_pluscw8$s2_out, zll_l_vs205_out);
  assign res = zll_l_vs205_out;
endmodule

module ZL_arm607 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [105:0] zl__unused597_out;
  Main_setR0  inst (arg2, arg1, main_setr0_out);
  ZL___unused597  instR1 (arg0, arg1, main_setr0_out, zl__unused597_out);
  assign res = zl__unused597_out;
endmodule

module Main_outputs (input logic [80:0] arg0,
  output logic [17:0] res);
  logic [17:0] o;
  assign o = arg0[70:53];
  assign res = o;
endmodule

module ZL_v100 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  logic [17:0] main_setweout_out;
  logic [17:0] zi1;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi2;
  logic [17:0] main_outputs_outR1;
  logic [17:0] zi3;
  logic [17:0] main_setdataout_out;
  logic [17:0] zi4;
  logic [80:0] main_setoutputs_outR1;
  logic [80:0] zi5;
  logic [17:0] main_outputs_outR2;
  logic [17:0] zi6;
  logic [17:0] main_setaddrout_out;
  logic [105:0] zll_l_s537_out;
  Main_outputs  inst (arg2, main_outputs_out);
  assign zi0 = main_outputs_out;
  Main_setWeOut  instR1 (zi0, 1'h1, main_setweout_out);
  assign zi1 = main_setweout_out;
  Main_setOutputs  instR2 (arg2, zi1, main_setoutputs_out);
  assign zi2 = main_setoutputs_out;
  Main_outputs  instR3 (zi2, main_outputs_outR1);
  assign zi3 = main_outputs_outR1;
  Main_setDataOut  instR4 (zi3, arg1, main_setdataout_out);
  assign zi4 = main_setdataout_out;
  Main_setOutputs  instR5 (zi2, zi4, main_setoutputs_outR1);
  assign zi5 = main_setoutputs_outR1;
  Main_outputs  instR6 (zi5, main_outputs_outR2);
  assign zi6 = main_outputs_outR2;
  Main_setAddrOut  instR7 (zi6, arg0, main_setaddrout_out);
  ZLL_L_s537  instR8 (zi5, main_setaddrout_out, zll_l_s537_out);
  assign res = zll_l_s537_out;
endmodule

module ZL_v559 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [105:0] zll_l_v6721_out;
  ZLL_L_v6721  inst (arg0, arg2, ~arg1, zll_l_v6721_out);
  assign res = zll_l_v6721_out;
endmodule

module Main_mkReg (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  assign res = (arg0 == 1'h0) ? ((arg1 == 1'h0) ? 2'h0 : 2'h1) : ((arg1 == 1'h0) ? 2'h2 : 2'h3);
endmodule