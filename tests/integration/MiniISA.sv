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
  logic [105:0] main_$l_main_loop3$6359_out;
  logic [9:0] main_inputs_out;
  logic [9:0] zi6;
  logic [7:0] main_datain_out;
  logic [7:0] zi7;
  logic [80:0] main_setr0_out;
  logic [80:0] zi8;
  logic [105:0] main_$l_main_loop3$6359_outR1;
  logic [80:0] main_setr1_out;
  logic [80:0] zi9;
  logic [105:0] main_$l_main_loop3$6359_outR2;
  logic [80:0] main_setr2_out;
  logic [80:0] zi10;
  logic [105:0] main_$l_main_loop3$6359_outR3;
  logic [80:0] main_setr3_out;
  logic [80:0] zi11;
  logic [105:0] main_$l_main_loop3$6359_outR4;
  logic [0:0] zi12;
  logic [0:0] zi13;
  logic [1:0] zi14;
  logic [80:0] main_setinputs_outR1;
  logic [80:0] zi16;
  logic [9:0] main_inputs_outR1;
  logic [9:0] zi17;
  logic [7:0] main_datain_outR1;
  logic [7:0] zi18;
  logic [17:0] main_outputs_out;
  logic [17:0] zi19;
  logic [17:0] main_setaddrout_out;
  logic [17:0] zi20;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi21;
  logic [17:0] main_outputs_outR1;
  logic [17:0] zi22;
  logic [17:0] main_setweout_out;
  logic [17:0] zi23;
  logic [80:0] main_setoutputs_outR1;
  logic [80:0] zi24;
  logic [105:0] main_$l__unused14$6370_out;
  logic [7:0] main_r0_out;
  logic [7:0] zi25;
  logic [105:0] main_$l_d36$6401_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi26;
  logic [105:0] main_$l_d36$6401_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi27;
  logic [105:0] main_$l_d36$6401_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi28;
  logic [105:0] main_$l_d36$6401_outR3;
  logic [0:0] zi29;
  logic [0:0] zi30;
  logic [80:0] main_setinputs_outR2;
  logic [80:0] zi32;
  logic [9:0] main_inputs_outR2;
  logic [9:0] zi33;
  logic [7:0] main_datain_outR2;
  logic [7:0] zi34;
  logic [1:0] main_mkreg_out;
  logic [1:0] zi35;
  logic [80:0] main_setr0_outR1;
  logic [80:0] zi36;
  logic [105:0] main_$l_main_loop3$6359_outR5;
  logic [80:0] main_setr1_outR1;
  logic [80:0] zi37;
  logic [105:0] main_$l_main_loop3$6359_outR6;
  logic [80:0] main_setr2_outR1;
  logic [80:0] zi38;
  logic [105:0] main_$l_main_loop3$6359_outR7;
  logic [80:0] main_setr3_outR1;
  logic [80:0] zi39;
  logic [105:0] main_$l_main_loop3$6359_outR8;
  logic [80:0] main_setinputs_outR3;
  logic [80:0] zi41;
  logic [105:0] main_$l_main_loop3$6359_outR9;
  logic [80:0] main_setinputs_outR4;
  logic [80:0] zi43;
  logic [105:0] main_$l_main_loop3$6359_outR10;
  logic [105:0] zres;
  assign zi1 = {__in0, __in1, __in2};
  assign zi2 = __resumption_tag[2];
  assign zi3 = __resumption_tag[1:0];
  Main_setInputs  inst (__st0, zi1, main_setinputs_out);
  assign zi5 = main_setinputs_out;
  main_$L_Main_loop3$6359  instR1 (zi5, main_$l_main_loop3$6359_out);
  Main_inputs  instR2 (zi5, main_inputs_out);
  assign zi6 = main_inputs_out;
  Main_dataIn  instR3 (zi6, main_datain_out);
  assign zi7 = main_datain_out;
  Main_setR0  instR4 (zi5, zi7, main_setr0_out);
  assign zi8 = main_setr0_out;
  main_$L_Main_loop3$6359  instR5 (zi8, main_$l_main_loop3$6359_outR1);
  Main_setR1  instR6 (zi5, zi7, main_setr1_out);
  assign zi9 = main_setr1_out;
  main_$L_Main_loop3$6359  instR7 (zi9, main_$l_main_loop3$6359_outR2);
  Main_setR2  instR8 (zi5, zi7, main_setr2_out);
  assign zi10 = main_setr2_out;
  main_$L_Main_loop3$6359  instR9 (zi10, main_$l_main_loop3$6359_outR3);
  Main_setR3  instR10 (zi5, zi7, main_setr3_out);
  assign zi11 = main_setr3_out;
  main_$L_Main_loop3$6359  instR11 (zi11, main_$l_main_loop3$6359_outR4);
  assign zi12 = __resumption_tag[3];
  assign zi13 = __resumption_tag[2];
  assign zi14 = __resumption_tag[1:0];
  Main_setInputs  instR12 (__st0, zi1, main_setinputs_outR1);
  assign zi16 = main_setinputs_outR1;
  Main_inputs  instR13 (zi16, main_inputs_outR1);
  assign zi17 = main_inputs_outR1;
  Main_dataIn  instR14 (zi17, main_datain_outR1);
  assign zi18 = main_datain_outR1;
  Main_outputs  instR15 (zi16, main_outputs_out);
  assign zi19 = main_outputs_out;
  Main_setAddrOut  instR16 (zi19, zi18, main_setaddrout_out);
  assign zi20 = main_setaddrout_out;
  Main_setOutputs  instR17 (zi16, zi20, main_setoutputs_out);
  assign zi21 = main_setoutputs_out;
  Main_outputs  instR18 (zi21, main_outputs_outR1);
  assign zi22 = main_outputs_outR1;
  Main_setWeOut  instR19 (zi22, zi13, main_setweout_out);
  assign zi23 = main_setweout_out;
  Main_setOutputs  instR20 (zi21, zi23, main_setoutputs_outR1);
  assign zi24 = main_setoutputs_outR1;
  main_$L___unused14$6370  instR21 (zi12, zi14, zi24, main_$l__unused14$6370_out);
  Main_r0  instR22 (zi24, main_r0_out);
  assign zi25 = main_r0_out;
  main_$L_d36$6401  instR23 (zi12, zi14, zi25, zi24, main_$l_d36$6401_out);
  Main_r1  instR24 (zi24, main_r1_out);
  assign zi26 = main_r1_out;
  main_$L_d36$6401  instR25 (zi12, zi14, zi26, zi24, main_$l_d36$6401_outR1);
  Main_r2  instR26 (zi24, main_r2_out);
  assign zi27 = main_r2_out;
  main_$L_d36$6401  instR27 (zi12, zi14, zi27, zi24, main_$l_d36$6401_outR2);
  Main_r3  instR28 (zi24, main_r3_out);
  assign zi28 = main_r3_out;
  main_$L_d36$6401  instR29 (zi12, zi14, zi28, zi24, main_$l_d36$6401_outR3);
  assign zi29 = __resumption_tag[1];
  assign zi30 = __resumption_tag[0];
  Main_setInputs  instR30 (__st0, zi1, main_setinputs_outR2);
  assign zi32 = main_setinputs_outR2;
  Main_inputs  instR31 (zi32, main_inputs_outR2);
  assign zi33 = main_inputs_outR2;
  Main_dataIn  instR32 (zi33, main_datain_outR2);
  assign zi34 = main_datain_outR2;
  Main_mkReg  instR33 (zi29, zi30, main_mkreg_out);
  assign zi35 = main_mkreg_out;
  Main_setR0  instR34 (zi32, zi34, main_setr0_outR1);
  assign zi36 = main_setr0_outR1;
  main_$L_Main_loop3$6359  instR35 (zi36, main_$l_main_loop3$6359_outR5);
  Main_setR1  instR36 (zi32, zi34, main_setr1_outR1);
  assign zi37 = main_setr1_outR1;
  main_$L_Main_loop3$6359  instR37 (zi37, main_$l_main_loop3$6359_outR6);
  Main_setR2  instR38 (zi32, zi34, main_setr2_outR1);
  assign zi38 = main_setr2_outR1;
  main_$L_Main_loop3$6359  instR39 (zi38, main_$l_main_loop3$6359_outR7);
  Main_setR3  instR40 (zi32, zi34, main_setr3_outR1);
  assign zi39 = main_setr3_outR1;
  main_$L_Main_loop3$6359  instR41 (zi39, main_$l_main_loop3$6359_outR8);
  Main_setInputs  instR42 (__st0, zi1, main_setinputs_outR3);
  assign zi41 = main_setinputs_outR3;
  main_$L_Main_loop3$6359  instR43 (zi41, main_$l_main_loop3$6359_outR9);
  Main_setInputs  instR44 (__st0, zi1, main_setinputs_outR4);
  assign zi43 = main_setinputs_outR4;
  main_$L_Main_loop3$6359  instR45 (zi43, main_$l_main_loop3$6359_outR10);
  assign zres = (__resumption_tag[6:4] == 3'h0) ? ((zi2 == 1'h0) ? main_$l_main_loop3$6359_out : ((zi3 == 2'h0) ? main_$l_main_loop3$6359_outR1 : ((zi3 == 2'h1) ? main_$l_main_loop3$6359_outR2 : ((zi3 == 2'h2) ? main_$l_main_loop3$6359_outR3 : main_$l_main_loop3$6359_outR4)))) : ((__resumption_tag[6:4] == 3'h1) ? ((zi13 == 1'h0) ? main_$l__unused14$6370_out : ((zi14 == 2'h0) ? main_$l_d36$6401_out : ((zi14 == 2'h1) ? main_$l_d36$6401_outR1 : ((zi14 == 2'h2) ? main_$l_d36$6401_outR2 : main_$l_d36$6401_outR3)))) : ((__resumption_tag[6:4] == 3'h2) ? ((zi35 == 2'h0) ? main_$l_main_loop3$6359_outR5 : ((zi35 == 2'h1) ? main_$l_main_loop3$6359_outR6 : ((zi35 == 2'h2) ? main_$l_main_loop3$6359_outR7 : main_$l_main_loop3$6359_outR8))) : ((__resumption_tag[6:4] == 3'h3) ? main_$l_main_loop3$6359_outR9 : main_$l_main_loop3$6359_outR10)));
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

module main_$L___unused14$6370 (input logic [0:0] arg0,
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

module main_$L_d36$6401 (input logic [0:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  logic [17:0] main_setdataout_out;
  logic [17:0] zi1;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi2;
  logic [105:0] main_$l__unused14$6370_out;
  Main_outputs  inst (arg3, main_outputs_out);
  assign zi0 = main_outputs_out;
  Main_setDataOut  instR1 (zi0, arg2, main_setdataout_out);
  assign zi1 = main_setdataout_out;
  Main_setOutputs  instR2 (arg3, zi1, main_setoutputs_out);
  assign zi2 = main_setoutputs_out;
  main_$L___unused14$6370  instR3 (arg0, arg1, zi2, main_$l__unused14$6370_out);
  assign res = main_$l__unused14$6370_out;
endmodule

module main_$L_a65$6446 (input logic [0:0] arg0,
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

module main_$L___unused103$6499 (input logic [80:0] arg0,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  Main_outputs  inst (arg0, main_outputs_out);
  assign zi0 = main_outputs_out;
  assign res = {zi0, 7'h30, arg0};
endmodule

module main_$L_v100$6496 (input logic [7:0] arg0,
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
  logic [17:0] zi7;
  logic [80:0] main_setoutputs_outR2;
  logic [80:0] zi8;
  logic [105:0] main_$l__unused103$6499_out;
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
  assign zi7 = main_setaddrout_out;
  Main_setOutputs  instR8 (zi5, zi7, main_setoutputs_outR2);
  assign zi8 = main_setoutputs_outR2;
  main_$L___unused103$6499  instR9 (zi8, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_a99$6495 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [7:0] main_r0_out;
  logic [7:0] zi0;
  logic [105:0] main_$l_v100$6496_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] main_$l_v100$6496_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi2;
  logic [105:0] main_$l_v100$6496_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi3;
  logic [105:0] main_$l_v100$6496_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_r0  instR1 (arg3, main_r0_out);
  assign zi0 = main_r0_out;
  main_$L_v100$6496  instR2 (arg2, zi0, arg3, main_$l_v100$6496_out);
  Main_r1  instR3 (arg3, main_r1_out);
  assign zi1 = main_r1_out;
  main_$L_v100$6496  instR4 (arg2, zi1, arg3, main_$l_v100$6496_outR1);
  Main_r2  instR5 (arg3, main_r2_out);
  assign zi2 = main_r2_out;
  main_$L_v100$6496  instR6 (arg2, zi2, arg3, main_$l_v100$6496_outR2);
  Main_r3  instR7 (arg3, main_r3_out);
  assign zi3 = main_r3_out;
  main_$L_v100$6496  instR8 (arg2, zi3, arg3, main_$l_v100$6496_outR3);
  assign res = (za == 2'h0) ? main_$l_v100$6496_out : ((za == 2'h1) ? main_$l_v100$6496_outR1 : ((za == 2'h2) ? main_$l_v100$6496_outR2 : main_$l_v100$6496_outR3));
endmodule

module main_$L___unused137$6549 (input logic [80:0] arg0,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  Main_outputs  inst (arg0, main_outputs_out);
  assign zi0 = main_outputs_out;
  assign res = {zi0, 7'h40, arg0};
endmodule

module main_$L_s150$6568 (input logic [1:0] arg0,
  input logic [8:0] arg1,
  input logic [80:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] x;
  logic [80:0] main_setcflag_out;
  logic [80:0] s01;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [80:0] main_setr0_out;
  logic [80:0] zi2;
  logic [105:0] main_$l__unused137$6549_out;
  logic [80:0] main_setr1_out;
  logic [80:0] zi3;
  logic [105:0] main_$l__unused137$6549_outR1;
  logic [80:0] main_setr2_out;
  logic [80:0] zi4;
  logic [105:0] main_$l__unused137$6549_outR2;
  logic [80:0] main_setr3_out;
  logic [80:0] zi5;
  logic [105:0] main_$l__unused137$6549_outR3;
  assign x = arg1[8];
  Main_setCFlag  inst (arg2, x, main_setcflag_out);
  assign s01 = main_setcflag_out;
  assign zi0 = arg1[7:0];
  assign zi1 = zi0;
  Main_setR0  instR1 (s01, zi1, main_setr0_out);
  assign zi2 = main_setr0_out;
  main_$L___unused137$6549  instR2 (zi2, main_$l__unused137$6549_out);
  Main_setR1  instR3 (s01, zi1, main_setr1_out);
  assign zi3 = main_setr1_out;
  main_$L___unused137$6549  instR4 (zi3, main_$l__unused137$6549_outR1);
  Main_setR2  instR5 (s01, zi1, main_setr2_out);
  assign zi4 = main_setr2_out;
  main_$L___unused137$6549  instR6 (zi4, main_$l__unused137$6549_outR2);
  Main_setR3  instR7 (s01, zi1, main_setr3_out);
  assign zi5 = main_setr3_out;
  main_$L___unused137$6549  instR8 (zi5, main_$l__unused137$6549_outR3);
  assign res = (arg0 == 2'h0) ? main_$l__unused137$6549_out : ((arg0 == 2'h1) ? main_$l__unused137$6549_outR1 : ((arg0 == 2'h2) ? main_$l__unused137$6549_outR2 : main_$l__unused137$6549_outR3));
endmodule

module main_$L_vS135$6547 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [8:0] zi0;
  logic [8:0] p;
  logic [105:0] main_$l_s150$6568_out;
  assign zi0 = ({1'h0, arg1} + {1'h0, arg2}) + 9'h0;
  assign p = {zi0[8], zi0[7:0]};
  main_$L_s150$6568  inst (arg0, p, arg3, arg3, main_$l_s150$6568_out);
  assign res = main_$l_s150$6568_out;
endmodule

module main_$L_vD134$6546 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_$l_vs135$6547_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_$l_vs135$6547_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_$l_vs135$6547_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_$l_vs135$6547_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_$L_vS135$6547  instR1 (arg0, arg2, main_r0_out, arg3, main_$l_vs135$6547_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_$L_vS135$6547  instR3 (arg0, arg2, main_r1_out, arg3, main_$l_vs135$6547_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_$L_vS135$6547  instR5 (arg0, arg2, main_r2_out, arg3, main_$l_vs135$6547_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_$L_vS135$6547  instR7 (arg0, arg2, main_r3_out, arg3, main_$l_vs135$6547_outR3);
  assign res = (arg1 == 2'h0) ? main_$l_vs135$6547_out : ((arg1 == 2'h1) ? main_$l_vs135$6547_outR1 : ((arg1 == 2'h2) ? main_$l_vs135$6547_outR2 : main_$l_vs135$6547_outR3));
endmodule

module main_$L_vS169$6596 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] main_cflag_out;
  logic [0:0] zi0;
  logic [8:0] zi1;
  logic [8:0] zi2;
  logic [105:0] main_$l_s150$6568_out;
  Main_cFlag  inst (arg3, main_cflag_out);
  assign zi0 = main_cflag_out;
  assign zi1 = ({1'h0, arg1} + {1'h0, arg2}) + {8'h0, zi0};
  assign zi2 = {zi1[8], zi1[7:0]};
  main_$L_s150$6568  instR1 (arg0, zi2, arg3, arg3, main_$l_s150$6568_out);
  assign res = main_$l_s150$6568_out;
endmodule

module main_$L_vD168$6595 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_$l_vs169$6596_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_$l_vs169$6596_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_$l_vs169$6596_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_$l_vs169$6596_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_$L_vS169$6596  instR1 (arg0, arg2, main_r0_out, arg3, main_$l_vs169$6596_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_$L_vS169$6596  instR3 (arg0, arg2, main_r1_out, arg3, main_$l_vs169$6596_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_$L_vS169$6596  instR5 (arg0, arg2, main_r2_out, arg3, main_$l_vs169$6596_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_$L_vS169$6596  instR7 (arg0, arg2, main_r3_out, arg3, main_$l_vs169$6596_outR3);
  assign res = (arg1 == 2'h0) ? main_$l_vs169$6596_out : ((arg1 == 2'h1) ? main_$l_vs169$6596_outR1 : ((arg1 == 2'h2) ? main_$l_vs169$6596_outR2 : main_$l_vs169$6596_outR3));
endmodule

module main_$L_vS205$6648 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [8:0] main_minuscw8$s1_out;
  logic [8:0] p;
  logic [105:0] main_$l_s150$6568_out;
  Main_minusCW8$s1  inst (arg1, arg2, main_minuscw8$s1_out);
  assign p = main_minuscw8$s1_out;
  main_$L_s150$6568  instR1 (arg0, p, arg3, arg3, main_$l_s150$6568_out);
  assign res = main_$l_s150$6568_out;
endmodule

module main_$L_vD204$6647 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_$l_vs205$6648_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_$l_vs205$6648_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_$l_vs205$6648_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_$l_vs205$6648_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_$L_vS205$6648  instR1 (arg0, arg2, main_r0_out, arg3, main_$l_vs205$6648_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_$L_vS205$6648  instR3 (arg0, arg2, main_r1_out, arg3, main_$l_vs205$6648_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_$L_vS205$6648  instR5 (arg0, arg2, main_r2_out, arg3, main_$l_vs205$6648_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_$L_vS205$6648  instR7 (arg0, arg2, main_r3_out, arg3, main_$l_vs205$6648_outR3);
  assign res = (arg1 == 2'h0) ? main_$l_vs205$6648_out : ((arg1 == 2'h1) ? main_$l_vs205$6648_outR1 : ((arg1 == 2'h2) ? main_$l_vs205$6648_outR2 : main_$l_vs205$6648_outR3));
endmodule

module main_$L_vS239$6697 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] main_cflag_out;
  logic [0:0] zi0;
  logic [8:0] zi1;
  logic [8:0] zi2;
  logic [105:0] main_$l_s150$6568_out;
  Main_cFlag  inst (arg3, main_cflag_out);
  assign zi0 = main_cflag_out;
  assign zi1 = ({1'h0, arg1} - {1'h0, arg2}) - {8'h0, zi0};
  assign zi2 = {zi1[8], zi1[7:0]};
  main_$L_s150$6568  instR1 (arg0, zi2, arg3, arg3, main_$l_s150$6568_out);
  assign res = main_$l_s150$6568_out;
endmodule

module main_$L_vD238$6696 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_$l_vs239$6697_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_$l_vs239$6697_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_$l_vs239$6697_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_$l_vs239$6697_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_$L_vS239$6697  instR1 (arg0, arg2, main_r0_out, arg3, main_$l_vs239$6697_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_$L_vS239$6697  instR3 (arg0, arg2, main_r1_out, arg3, main_$l_vs239$6697_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_$L_vS239$6697  instR5 (arg0, arg2, main_r2_out, arg3, main_$l_vs239$6697_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_$L_vS239$6697  instR7 (arg0, arg2, main_r3_out, arg3, main_$l_vs239$6697_outR3);
  assign res = (arg1 == 2'h0) ? main_$l_vs239$6697_out : ((arg1 == 2'h1) ? main_$l_vs239$6697_outR1 : ((arg1 == 2'h2) ? main_$l_vs239$6697_outR2 : main_$l_vs239$6697_outR3));
endmodule

module main_$L_x274$6748 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused103$6499_out;
  logic [80:0] main_setr1_out;
  logic [80:0] zi1;
  logic [105:0] main_$l__unused103$6499_outR1;
  logic [80:0] main_setr2_out;
  logic [80:0] zi2;
  logic [105:0] main_$l__unused103$6499_outR2;
  logic [80:0] main_setr3_out;
  logic [80:0] zi3;
  logic [105:0] main_$l__unused103$6499_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_setR0  instR1 (arg3, arg2, main_setr0_out);
  assign zi0 = main_setr0_out;
  main_$L___unused103$6499  instR2 (zi0, main_$l__unused103$6499_out);
  Main_setR1  instR3 (arg3, arg2, main_setr1_out);
  assign zi1 = main_setr1_out;
  main_$L___unused103$6499  instR4 (zi1, main_$l__unused103$6499_outR1);
  Main_setR2  instR5 (arg3, arg2, main_setr2_out);
  assign zi2 = main_setr2_out;
  main_$L___unused103$6499  instR6 (zi2, main_$l__unused103$6499_outR2);
  Main_setR3  instR7 (arg3, arg2, main_setr3_out);
  assign zi3 = main_setr3_out;
  main_$L___unused103$6499  instR8 (zi3, main_$l__unused103$6499_outR3);
  assign res = (za == 2'h0) ? main_$l__unused103$6499_out : ((za == 2'h1) ? main_$l__unused103$6499_outR1 : ((za == 2'h2) ? main_$l__unused103$6499_outR2 : main_$l__unused103$6499_outR3));
endmodule

module main_$L___unused299$6783 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setcflag_out;
  logic [80:0] zi0;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi1;
  logic [105:0] main_$l__unused137$6549_out;
  Main_setCFlag  inst (arg1, 1'h0, main_setcflag_out);
  assign zi0 = main_setcflag_out;
  Main_setZFlag  instR1 (zi0, arg0 == 8'h0, main_setzflag_out);
  assign zi1 = main_setzflag_out;
  main_$L___unused137$6549  instR2 (zi1, main_$l__unused137$6549_out);
  assign res = main_$l__unused137$6549_out;
endmodule

module main_$L_arm309$6798 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused299$6783_out;
  Main_setR0  inst (arg1, arg0, main_setr0_out);
  assign zi0 = main_setr0_out;
  main_$L___unused299$6783  instR1 (arg0, zi0, main_$l__unused299$6783_out);
  assign res = main_$l__unused299$6783_out;
endmodule

module main_$L_arm311$6801 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused299$6783_out;
  Main_setR1  inst (arg1, arg0, main_setr1_out);
  assign zi0 = main_setr1_out;
  main_$L___unused299$6783  instR1 (arg0, zi0, main_$l__unused299$6783_out);
  assign res = main_$l__unused299$6783_out;
endmodule

module main_$L_arm313$6804 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused299$6783_out;
  Main_setR2  inst (arg1, arg0, main_setr2_out);
  assign zi0 = main_setr2_out;
  main_$L___unused299$6783  instR1 (arg0, zi0, main_$l__unused299$6783_out);
  assign res = main_$l__unused299$6783_out;
endmodule

module main_$L_arm315$6807 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused299$6783_out;
  Main_setR3  inst (arg1, arg0, main_setr3_out);
  assign zi0 = main_setr3_out;
  main_$L___unused299$6783  instR1 (arg0, zi0, main_$l__unused299$6783_out);
  assign res = main_$l__unused299$6783_out;
endmodule

module main_$L_vS298$6782 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] vd$;
  logic [105:0] main_$l_arm309$6798_out;
  logic [105:0] main_$l_arm311$6801_out;
  logic [105:0] main_$l_arm313$6804_out;
  logic [105:0] main_$l_arm315$6807_out;
  assign vd$ = arg1 | arg2;
  main_$L_arm309$6798  inst (vd$, arg3, main_$l_arm309$6798_out);
  main_$L_arm311$6801  instR1 (vd$, arg3, main_$l_arm311$6801_out);
  main_$L_arm313$6804  instR2 (vd$, arg3, main_$l_arm313$6804_out);
  main_$L_arm315$6807  instR3 (vd$, arg3, main_$l_arm315$6807_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm309$6798_out : ((arg0 == 2'h1) ? main_$l_arm311$6801_out : ((arg0 == 2'h2) ? main_$l_arm313$6804_out : main_$l_arm315$6807_out));
endmodule

module main_$L_vD297$6781 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_$l_vs298$6782_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_$l_vs298$6782_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_$l_vs298$6782_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_$l_vs298$6782_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_$L_vS298$6782  instR1 (arg0, arg2, main_r0_out, arg3, main_$l_vs298$6782_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_$L_vS298$6782  instR3 (arg0, arg2, main_r1_out, arg3, main_$l_vs298$6782_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_$L_vS298$6782  instR5 (arg0, arg2, main_r2_out, arg3, main_$l_vs298$6782_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_$L_vS298$6782  instR7 (arg0, arg2, main_r3_out, arg3, main_$l_vs298$6782_outR3);
  assign res = (arg1 == 2'h0) ? main_$l_vs298$6782_out : ((arg1 == 2'h1) ? main_$l_vs298$6782_outR1 : ((arg1 == 2'h2) ? main_$l_vs298$6782_outR2 : main_$l_vs298$6782_outR3));
endmodule

module main_$L_vS334$6834 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] vd$;
  logic [105:0] main_$l_arm309$6798_out;
  logic [105:0] main_$l_arm311$6801_out;
  logic [105:0] main_$l_arm313$6804_out;
  logic [105:0] main_$l_arm315$6807_out;
  assign vd$ = arg1 & arg2;
  main_$L_arm309$6798  inst (vd$, arg3, main_$l_arm309$6798_out);
  main_$L_arm311$6801  instR1 (vd$, arg3, main_$l_arm311$6801_out);
  main_$L_arm313$6804  instR2 (vd$, arg3, main_$l_arm313$6804_out);
  main_$L_arm315$6807  instR3 (vd$, arg3, main_$l_arm315$6807_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm309$6798_out : ((arg0 == 2'h1) ? main_$l_arm311$6801_out : ((arg0 == 2'h2) ? main_$l_arm313$6804_out : main_$l_arm315$6807_out));
endmodule

module main_$L_vD333$6833 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_$l_vs334$6834_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_$l_vs334$6834_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_$l_vs334$6834_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_$l_vs334$6834_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_$L_vS334$6834  instR1 (arg0, arg2, main_r0_out, arg3, main_$l_vs334$6834_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_$L_vS334$6834  instR3 (arg0, arg2, main_r1_out, arg3, main_$l_vs334$6834_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_$L_vS334$6834  instR5 (arg0, arg2, main_r2_out, arg3, main_$l_vs334$6834_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_$L_vS334$6834  instR7 (arg0, arg2, main_r3_out, arg3, main_$l_vs334$6834_outR3);
  assign res = (arg1 == 2'h0) ? main_$l_vs334$6834_out : ((arg1 == 2'h1) ? main_$l_vs334$6834_outR1 : ((arg1 == 2'h2) ? main_$l_vs334$6834_outR2 : main_$l_vs334$6834_outR3));
endmodule

module main_$L_vS370$6886 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] vd$;
  logic [105:0] main_$l_arm309$6798_out;
  logic [105:0] main_$l_arm311$6801_out;
  logic [105:0] main_$l_arm313$6804_out;
  logic [105:0] main_$l_arm315$6807_out;
  assign vd$ = arg1 ^ arg2;
  main_$L_arm309$6798  inst (vd$, arg3, main_$l_arm309$6798_out);
  main_$L_arm311$6801  instR1 (vd$, arg3, main_$l_arm311$6801_out);
  main_$L_arm313$6804  instR2 (vd$, arg3, main_$l_arm313$6804_out);
  main_$L_arm315$6807  instR3 (vd$, arg3, main_$l_arm315$6807_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm309$6798_out : ((arg0 == 2'h1) ? main_$l_arm311$6801_out : ((arg0 == 2'h2) ? main_$l_arm313$6804_out : main_$l_arm315$6807_out));
endmodule

module main_$L_vD369$6885 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_$l_vs370$6886_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_$l_vs370$6886_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_$l_vs370$6886_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_$l_vs370$6886_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_$L_vS370$6886  instR1 (arg0, arg2, main_r0_out, arg3, main_$l_vs370$6886_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_$L_vS370$6886  instR3 (arg0, arg2, main_r1_out, arg3, main_$l_vs370$6886_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_$L_vS370$6886  instR5 (arg0, arg2, main_r2_out, arg3, main_$l_vs370$6886_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_$L_vS370$6886  instR7 (arg0, arg2, main_r3_out, arg3, main_$l_vs370$6886_outR3);
  assign res = (arg1 == 2'h0) ? main_$l_vs370$6886_out : ((arg1 == 2'h1) ? main_$l_vs370$6886_outR1 : ((arg1 == 2'h2) ? main_$l_vs370$6886_outR2 : main_$l_vs370$6886_outR3));
endmodule

module main_$L_vS406$6938 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] main_minuscw8$s1_out;
  logic [8:0] p;
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi2;
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi5;
  logic [105:0] main_$l__unused103$6499_out;
  Main_minusCW8$s1  inst (arg0, arg1, main_minuscw8$s1_out);
  assign p = main_minuscw8$s1_out;
  assign zi0 = p[8];
  assign zi1 = zi0;
  Main_setCFlag  instR1 (arg2, zi1, main_setcflag_out);
  assign zi2 = main_setcflag_out;
  assign zi3 = p[7:0];
  assign zi4 = zi3;
  Main_setZFlag  instR2 (zi2, zi4 == 8'h0, main_setzflag_out);
  assign zi5 = main_setzflag_out;
  main_$L___unused103$6499  instR3 (zi5, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_vD405$6937 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [7:0] main_r0_out;
  logic [7:0] zi0;
  logic [105:0] main_$l_vs406$6938_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] main_$l_vs406$6938_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi2;
  logic [105:0] main_$l_vs406$6938_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi3;
  logic [105:0] main_$l_vs406$6938_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_r0  instR1 (arg3, main_r0_out);
  assign zi0 = main_r0_out;
  main_$L_vS406$6938  instR2 (arg2, zi0, arg3, main_$l_vs406$6938_out);
  Main_r1  instR3 (arg3, main_r1_out);
  assign zi1 = main_r1_out;
  main_$L_vS406$6938  instR4 (arg2, zi1, arg3, main_$l_vs406$6938_outR1);
  Main_r2  instR5 (arg3, main_r2_out);
  assign zi2 = main_r2_out;
  main_$L_vS406$6938  instR6 (arg2, zi2, arg3, main_$l_vs406$6938_outR2);
  Main_r3  instR7 (arg3, main_r3_out);
  assign zi3 = main_r3_out;
  main_$L_vS406$6938  instR8 (arg2, zi3, arg3, main_$l_vs406$6938_outR3);
  assign res = (za == 2'h0) ? main_$l_vs406$6938_out : ((za == 2'h1) ? main_$l_vs406$6938_outR1 : ((za == 2'h2) ? main_$l_vs406$6938_outR2 : main_$l_vs406$6938_outR3));
endmodule

module main_$L_pc438$6984 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setpc_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused103$6499_out;
  Main_setPC  inst (arg1, arg0, main_setpc_out);
  assign zi0 = main_setpc_out;
  main_$L___unused103$6499  instR1 (zi0, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_arm448$6999 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [7:0] main_r0_out;
  logic [7:0] zi0;
  logic [105:0] main_$l_pc438$6984_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] main_$l_pc438$6984_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi2;
  logic [105:0] main_$l_pc438$6984_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi3;
  logic [105:0] main_$l_pc438$6984_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_r0  instR1 (arg2, main_r0_out);
  assign zi0 = main_r0_out;
  main_$L_pc438$6984  instR2 (zi0, arg2, main_$l_pc438$6984_out);
  Main_r1  instR3 (arg2, main_r1_out);
  assign zi1 = main_r1_out;
  main_$L_pc438$6984  instR4 (zi1, arg2, main_$l_pc438$6984_outR1);
  Main_r2  instR5 (arg2, main_r2_out);
  assign zi2 = main_r2_out;
  main_$L_pc438$6984  instR6 (zi2, arg2, main_$l_pc438$6984_outR2);
  Main_r3  instR7 (arg2, main_r3_out);
  assign zi3 = main_r3_out;
  main_$L_pc438$6984  instR8 (zi3, arg2, main_$l_pc438$6984_outR3);
  assign res = (za == 2'h0) ? main_$l_pc438$6984_out : ((za == 2'h1) ? main_$l_pc438$6984_outR1 : ((za == 2'h2) ? main_$l_pc438$6984_outR2 : main_$l_pc438$6984_outR3));
endmodule

module main_$L_x508$7084 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setpc_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused103$6499_out;
  Main_setPC  inst (arg1, arg0, main_setpc_out);
  assign zi0 = main_setpc_out;
  main_$L___unused103$6499  instR1 (zi0, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_arm566$7168 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused103$6499_out;
  Main_setR0  inst (arg1, arg0, main_setr0_out);
  assign zi0 = main_setr0_out;
  main_$L___unused103$6499  instR1 (zi0, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_arm568$7171 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused103$6499_out;
  Main_setR1  inst (arg1, arg0, main_setr1_out);
  assign zi0 = main_setr1_out;
  main_$L___unused103$6499  instR1 (zi0, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_arm570$7174 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused103$6499_out;
  Main_setR2  inst (arg1, arg0, main_setr2_out);
  assign zi0 = main_setr2_out;
  main_$L___unused103$6499  instR1 (zi0, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_arm572$7177 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused103$6499_out;
  Main_setR3  inst (arg1, arg0, main_setr3_out);
  assign zi0 = main_setr3_out;
  main_$L___unused103$6499  instR1 (zi0, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_v559$7158 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] v1;
  logic [105:0] main_$l_arm566$7168_out;
  logic [105:0] main_$l_arm568$7171_out;
  logic [105:0] main_$l_arm570$7174_out;
  logic [105:0] main_$l_arm572$7177_out;
  assign v1 = ~arg1;
  main_$L_arm566$7168  inst (v1, arg2, main_$l_arm566$7168_out);
  main_$L_arm568$7171  instR1 (v1, arg2, main_$l_arm568$7171_out);
  main_$L_arm570$7174  instR2 (v1, arg2, main_$l_arm570$7174_out);
  main_$L_arm572$7177  instR3 (v1, arg2, main_$l_arm572$7177_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm566$7168_out : ((arg0 == 2'h1) ? main_$l_arm568$7171_out : ((arg0 == 2'h2) ? main_$l_arm570$7174_out : main_$l_arm572$7177_out));
endmodule

module main_$L___unused597$7212 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi2;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi3;
  logic [105:0] main_$l__unused103$6499_out;
  assign zi0 = arg0[8];
  assign zi1 = zi0;
  Main_setCFlag  inst (arg2, zi1, main_setcflag_out);
  assign zi2 = main_setcflag_out;
  Main_setZFlag  instR1 (zi2, arg1 == 8'h0, main_setzflag_out);
  assign zi3 = main_setzflag_out;
  main_$L___unused103$6499  instR2 (zi3, main_$l__unused103$6499_out);
  assign res = main_$l__unused103$6499_out;
endmodule

module main_$L_arm607$7227 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused597$7212_out;
  Main_setR0  inst (arg2, arg1, main_setr0_out);
  assign zi0 = main_setr0_out;
  main_$L___unused597$7212  instR1 (arg0, arg1, zi0, main_$l__unused597$7212_out);
  assign res = main_$l__unused597$7212_out;
endmodule

module main_$L_arm609$7230 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused597$7212_out;
  Main_setR1  inst (arg2, arg1, main_setr1_out);
  assign zi0 = main_setr1_out;
  main_$L___unused597$7212  instR1 (arg0, arg1, zi0, main_$l__unused597$7212_out);
  assign res = main_$l__unused597$7212_out;
endmodule

module main_$L_arm611$7233 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused597$7212_out;
  Main_setR2  inst (arg2, arg1, main_setr2_out);
  assign zi0 = main_setr2_out;
  main_$L___unused597$7212  instR1 (arg0, arg1, zi0, main_$l__unused597$7212_out);
  assign res = main_$l__unused597$7212_out;
endmodule

module main_$L_arm613$7236 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [80:0] zi0;
  logic [105:0] main_$l__unused597$7212_out;
  Main_setR3  inst (arg2, arg1, main_setr3_out);
  assign zi0 = main_setr3_out;
  main_$L___unused597$7212  instR1 (arg0, arg1, zi0, main_$l__unused597$7212_out);
  assign res = main_$l__unused597$7212_out;
endmodule

module main_$L_v596$7211 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] main_pluscw8$s1_out;
  logic [8:0] p;
  logic [7:0] y;
  logic [7:0] v$;
  logic [105:0] main_$l_arm607$7227_out;
  logic [105:0] main_$l_arm609$7230_out;
  logic [105:0] main_$l_arm611$7233_out;
  logic [105:0] main_$l_arm613$7236_out;
  Main_plusCW8$s1  inst (arg1, main_pluscw8$s1_out);
  assign p = main_pluscw8$s1_out;
  assign y = p[7:0];
  assign v$ = y;
  main_$L_arm607$7227  instR1 (p, v$, arg2, main_$l_arm607$7227_out);
  main_$L_arm609$7230  instR2 (p, v$, arg2, main_$l_arm609$7230_out);
  main_$L_arm611$7233  instR3 (p, v$, arg2, main_$l_arm611$7233_out);
  main_$L_arm613$7236  instR4 (p, v$, arg2, main_$l_arm613$7236_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm607$7227_out : ((arg0 == 2'h1) ? main_$l_arm609$7230_out : ((arg0 == 2'h2) ? main_$l_arm611$7233_out : main_$l_arm613$7236_out));
endmodule

module main_$L_v623$7250 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] zi0;
  logic [8:0] p;
  logic [7:0] y;
  logic [7:0] v$;
  logic [105:0] main_$l_arm607$7227_out;
  logic [105:0] main_$l_arm609$7230_out;
  logic [105:0] main_$l_arm611$7233_out;
  logic [105:0] main_$l_arm613$7236_out;
  assign zi0 = ({1'h0, arg1} - 9'h1) - 9'h0;
  assign p = {zi0[8], zi0[7:0]};
  assign y = p[7:0];
  assign v$ = y;
  main_$L_arm607$7227  inst (p, v$, arg2, main_$l_arm607$7227_out);
  main_$L_arm609$7230  instR1 (p, v$, arg2, main_$l_arm609$7230_out);
  main_$L_arm611$7233  instR2 (p, v$, arg2, main_$l_arm611$7233_out);
  main_$L_arm613$7236  instR3 (p, v$, arg2, main_$l_arm613$7236_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm607$7227_out : ((arg0 == 2'h1) ? main_$l_arm609$7230_out : ((arg0 == 2'h2) ? main_$l_arm611$7233_out : main_$l_arm613$7236_out));
endmodule

module main_$L_v650$7289 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] v1;
  logic [105:0] main_$l_arm566$7168_out;
  logic [105:0] main_$l_arm568$7171_out;
  logic [105:0] main_$l_arm570$7174_out;
  logic [105:0] main_$l_arm572$7177_out;
  assign v1 = (arg1 << 8'h1) | (arg1 >> 8'h7);
  main_$L_arm566$7168  inst (v1, arg2, main_$l_arm566$7168_out);
  main_$L_arm568$7171  instR1 (v1, arg2, main_$l_arm568$7171_out);
  main_$L_arm570$7174  instR2 (v1, arg2, main_$l_arm570$7174_out);
  main_$L_arm572$7177  instR3 (v1, arg2, main_$l_arm572$7177_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm566$7168_out : ((arg0 == 2'h1) ? main_$l_arm568$7171_out : ((arg0 == 2'h2) ? main_$l_arm570$7174_out : main_$l_arm572$7177_out));
endmodule

module main_$L_v672$7321 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] v1;
  logic [105:0] main_$l_arm566$7168_out;
  logic [105:0] main_$l_arm568$7171_out;
  logic [105:0] main_$l_arm570$7174_out;
  logic [105:0] main_$l_arm572$7177_out;
  assign v1 = (arg1 >> 8'h1) | (arg1 << 8'h7);
  main_$L_arm566$7168  inst (v1, arg2, main_$l_arm566$7168_out);
  main_$L_arm568$7171  instR1 (v1, arg2, main_$l_arm568$7171_out);
  main_$L_arm570$7174  instR2 (v1, arg2, main_$l_arm570$7174_out);
  main_$L_arm572$7177  instR3 (v1, arg2, main_$l_arm572$7177_out);
  assign res = (arg0 == 2'h0) ? main_$l_arm566$7168_out : ((arg0 == 2'h1) ? main_$l_arm568$7171_out : ((arg0 == 2'h2) ? main_$l_arm570$7174_out : main_$l_arm572$7177_out));
endmodule

module main_$L_v695$7354 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [1:0] arg2,
  input logic [7:0] arg3,
  input logic [80:0] arg4,
  output logic [105:0] res);
  logic [0:0] main_msbw8_out;
  logic [0:0] za;
  logic [0:0] main_msbw8_outR1;
  logic [8:0] za1;
  logic [0:0] main_msbw8_outR2;
  logic [8:0] zaR1;
  logic [8:0] zaR2;
  logic [0:0] main_lsbw8_out;
  logic [0:0] zaR3;
  logic [0:0] main_lsbw8_outR1;
  logic [8:0] za1R1;
  logic [0:0] main_lsbw8_outR2;
  logic [8:0] zaR4;
  logic [8:0] zaR5;
  logic [8:0] p;
  logic [7:0] y;
  logic [7:0] v$;
  logic [105:0] main_$l_arm607$7227_out;
  logic [105:0] main_$l_arm609$7230_out;
  logic [105:0] main_$l_arm611$7233_out;
  logic [105:0] main_$l_arm613$7236_out;
  Main_msbW8  inst (arg3, main_msbw8_out);
  assign za = main_msbw8_out;
  Main_msbW8  instR1 (arg3, main_msbw8_outR1);
  assign za1 = {main_msbw8_outR1, (arg3 << 8'h1) | {7'h0, za}};
  Main_msbW8  instR2 (arg3, main_msbw8_outR2);
  assign zaR1 = {main_msbw8_outR2, (arg3 << 8'h1) | 8'h0};
  assign zaR2 = (arg0 == 1'h0) ? za1 : zaR1;
  Main_lsbW8  instR3 (arg3, main_lsbw8_out);
  assign zaR3 = main_lsbw8_out;
  Main_lsbW8  instR4 (arg3, main_lsbw8_outR1);
  assign za1R1 = {main_lsbw8_outR1, (arg3 >> 8'h1) | ({7'h0, zaR3} << 8'h7)};
  Main_lsbW8  instR5 (arg3, main_lsbw8_outR2);
  assign zaR4 = {main_lsbw8_outR2, (arg3 >> 8'h1) | 8'h0};
  assign zaR5 = (arg0 == 1'h0) ? za1R1 : zaR4;
  assign p = (arg1 == 1'h0) ? zaR2 : zaR5;
  assign y = p[7:0];
  assign v$ = y;
  main_$L_arm607$7227  instR6 (p, v$, arg4, main_$l_arm607$7227_out);
  main_$L_arm609$7230  instR7 (p, v$, arg4, main_$l_arm609$7230_out);
  main_$L_arm611$7233  instR8 (p, v$, arg4, main_$l_arm611$7233_out);
  main_$L_arm613$7236  instR9 (p, v$, arg4, main_$l_arm613$7236_out);
  assign res = (arg2 == 2'h0) ? main_$l_arm607$7227_out : ((arg2 == 2'h1) ? main_$l_arm609$7230_out : ((arg2 == 2'h2) ? main_$l_arm611$7233_out : main_$l_arm613$7236_out));
endmodule

module main_$L_fail7$6363 (input logic [9:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [7:0] main_datain_out;
  logic [7:0] i;
  logic [0:0] ds1;
  logic [0:0] ds2;
  logic [0:0] ds3;
  logic [0:0] ren;
  logic [0:0] wen;
  logic [0:0] b0;
  logic [0:0] b1;
  logic [0:0] za;
  logic [1:0] main_mkreg_out;
  logic [1:0] zi0;
  logic [7:0] main_pc_out;
  logic [7:0] zi1;
  logic [17:0] main_outputs_out;
  logic [17:0] zi2;
  logic [17:0] main_setaddrout_out;
  logic [17:0] zi3;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi4;
  logic [17:0] main_outputs_outR1;
  logic [17:0] zi5;
  logic [1:0] main_mkreg_outR1;
  logic [1:0] zi6;
  logic [7:0] main_r0_out;
  logic [7:0] zi7;
  logic [105:0] main_$l_a65$6446_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi8;
  logic [105:0] main_$l_a65$6446_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi9;
  logic [105:0] main_$l_a65$6446_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi10;
  logic [105:0] main_$l_a65$6446_outR3;
  logic [1:0] main_mkreg_outR2;
  logic [1:0] zi11;
  logic [7:0] main_r0_outR1;
  logic [7:0] zi12;
  logic [105:0] main_$l_a99$6495_out;
  logic [7:0] main_r1_outR1;
  logic [7:0] zi13;
  logic [105:0] main_$l_a99$6495_outR1;
  logic [7:0] main_r2_outR1;
  logic [7:0] zi14;
  logic [105:0] main_$l_a99$6495_outR2;
  logic [7:0] main_r3_outR1;
  logic [7:0] zi15;
  logic [105:0] main_$l_a99$6495_outR3;
  logic [1:0] main_mkreg_outR3;
  logic [1:0] zi16;
  logic [1:0] main_mkreg_outR4;
  logic [1:0] zi17;
  logic [7:0] main_r0_outR2;
  logic [105:0] main_$l_vd134$6546_out;
  logic [7:0] main_r1_outR2;
  logic [105:0] main_$l_vd134$6546_outR1;
  logic [7:0] main_r2_outR2;
  logic [105:0] main_$l_vd134$6546_outR2;
  logic [7:0] main_r3_outR2;
  logic [105:0] main_$l_vd134$6546_outR3;
  logic [1:0] main_mkreg_outR5;
  logic [1:0] zi18;
  logic [1:0] main_mkreg_outR6;
  logic [1:0] zi19;
  logic [7:0] main_r0_outR3;
  logic [105:0] main_$l_vd168$6595_out;
  logic [7:0] main_r1_outR3;
  logic [105:0] main_$l_vd168$6595_outR1;
  logic [7:0] main_r2_outR3;
  logic [105:0] main_$l_vd168$6595_outR2;
  logic [7:0] main_r3_outR3;
  logic [105:0] main_$l_vd168$6595_outR3;
  logic [1:0] main_mkreg_outR7;
  logic [1:0] zi20;
  logic [1:0] main_mkreg_outR8;
  logic [1:0] zi21;
  logic [7:0] main_r0_outR4;
  logic [105:0] main_$l_vd204$6647_out;
  logic [7:0] main_r1_outR4;
  logic [105:0] main_$l_vd204$6647_outR1;
  logic [7:0] main_r2_outR4;
  logic [105:0] main_$l_vd204$6647_outR2;
  logic [7:0] main_r3_outR4;
  logic [105:0] main_$l_vd204$6647_outR3;
  logic [1:0] main_mkreg_outR9;
  logic [1:0] zi22;
  logic [1:0] main_mkreg_outR10;
  logic [1:0] zi23;
  logic [7:0] main_r0_outR5;
  logic [105:0] main_$l_vd238$6696_out;
  logic [7:0] main_r1_outR5;
  logic [105:0] main_$l_vd238$6696_outR1;
  logic [7:0] main_r2_outR5;
  logic [105:0] main_$l_vd238$6696_outR2;
  logic [7:0] main_r3_outR5;
  logic [105:0] main_$l_vd238$6696_outR3;
  logic [1:0] main_mkreg_outR11;
  logic [1:0] zi24;
  logic [7:0] main_r0_outR6;
  logic [7:0] zi25;
  logic [105:0] main_$l_x274$6748_out;
  logic [7:0] main_r1_outR6;
  logic [7:0] zi26;
  logic [105:0] main_$l_x274$6748_outR1;
  logic [7:0] main_r2_outR6;
  logic [7:0] zi27;
  logic [105:0] main_$l_x274$6748_outR2;
  logic [7:0] main_r3_outR6;
  logic [7:0] zi28;
  logic [105:0] main_$l_x274$6748_outR3;
  logic [1:0] main_mkreg_outR12;
  logic [1:0] zi29;
  logic [1:0] main_mkreg_outR13;
  logic [1:0] zi30;
  logic [7:0] main_r0_outR7;
  logic [105:0] main_$l_vd297$6781_out;
  logic [7:0] main_r1_outR7;
  logic [105:0] main_$l_vd297$6781_outR1;
  logic [7:0] main_r2_outR7;
  logic [105:0] main_$l_vd297$6781_outR2;
  logic [7:0] main_r3_outR7;
  logic [105:0] main_$l_vd297$6781_outR3;
  logic [1:0] main_mkreg_outR14;
  logic [1:0] zi31;
  logic [1:0] main_mkreg_outR15;
  logic [1:0] zi32;
  logic [7:0] main_r0_outR8;
  logic [105:0] main_$l_vd333$6833_out;
  logic [7:0] main_r1_outR8;
  logic [105:0] main_$l_vd333$6833_outR1;
  logic [7:0] main_r2_outR8;
  logic [105:0] main_$l_vd333$6833_outR2;
  logic [7:0] main_r3_outR8;
  logic [105:0] main_$l_vd333$6833_outR3;
  logic [1:0] main_mkreg_outR16;
  logic [1:0] zi33;
  logic [1:0] main_mkreg_outR17;
  logic [1:0] zi34;
  logic [7:0] main_r0_outR9;
  logic [105:0] main_$l_vd369$6885_out;
  logic [7:0] main_r1_outR9;
  logic [105:0] main_$l_vd369$6885_outR1;
  logic [7:0] main_r2_outR9;
  logic [105:0] main_$l_vd369$6885_outR2;
  logic [7:0] main_r3_outR9;
  logic [105:0] main_$l_vd369$6885_outR3;
  logic [1:0] main_mkreg_outR18;
  logic [1:0] zi35;
  logic [7:0] main_r0_outR10;
  logic [7:0] zi36;
  logic [105:0] main_$l_vd405$6937_out;
  logic [7:0] main_r1_outR10;
  logic [7:0] zi37;
  logic [105:0] main_$l_vd405$6937_outR1;
  logic [7:0] main_r2_outR10;
  logic [7:0] zi38;
  logic [105:0] main_$l_vd405$6937_outR2;
  logic [7:0] main_r3_outR10;
  logic [7:0] zi39;
  logic [105:0] main_$l_vd405$6937_outR3;
  logic [0:0] main_zflag_out;
  logic [0:0] zi40;
  logic [105:0] main_$l__unused103$6499_out;
  logic [105:0] main_$l_arm448$6999_out;
  logic [0:0] main_zflag_outR1;
  logic [0:0] zi41;
  logic [0:0] main_notb_out;
  logic [0:0] zi42;
  logic [105:0] main_$l__unused103$6499_outR1;
  logic [105:0] main_$l_arm448$6999_outR1;
  logic [0:0] main_cflag_out;
  logic [0:0] zi43;
  logic [105:0] main_$l__unused103$6499_outR2;
  logic [105:0] main_$l_arm448$6999_outR2;
  logic [0:0] main_cflag_outR1;
  logic [0:0] zi44;
  logic [0:0] main_notb_outR1;
  logic [0:0] zi45;
  logic [105:0] main_$l__unused103$6499_outR3;
  logic [105:0] main_$l_arm448$6999_outR3;
  logic [1:0] main_mkreg_outR19;
  logic [1:0] zi46;
  logic [7:0] main_r0_outR11;
  logic [7:0] zi47;
  logic [105:0] main_$l_x508$7084_out;
  logic [7:0] main_r1_outR11;
  logic [7:0] zi48;
  logic [105:0] main_$l_x508$7084_outR1;
  logic [7:0] main_r2_outR11;
  logic [7:0] zi49;
  logic [105:0] main_$l_x508$7084_outR2;
  logic [7:0] main_r3_outR11;
  logic [7:0] zi50;
  logic [105:0] main_$l_x508$7084_outR3;
  logic [80:0] main_setieflag_out;
  logic [80:0] zi51;
  logic [105:0] main_$l__unused103$6499_outR4;
  logic [17:0] main_outputs_outR2;
  logic [17:0] zi52;
  logic [7:0] zi53;
  logic [7:0] zi54;
  logic [0:0] zi55;
  logic [17:0] zi56;
  logic [80:0] main_setoutputs_outR1;
  logic [80:0] zi57;
  logic [105:0] main_$l__unused103$6499_outR5;
  logic [80:0] main_setieflag_outR1;
  logic [80:0] zi58;
  logic [7:0] zi59;
  logic [7:0] zi60;
  logic [80:0] main_setpc_out;
  logic [80:0] zi61;
  logic [0:0] zi62;
  logic [0:0] zi63;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi64;
  logic [0:0] zi65;
  logic [0:0] zi66;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi67;
  logic [105:0] main_$l__unused103$6499_outR6;
  logic [1:0] main_mkreg_outR20;
  logic [1:0] zi68;
  logic [7:0] main_r0_outR12;
  logic [7:0] zi69;
  logic [105:0] main_$l_v559$7158_out;
  logic [7:0] main_r1_outR12;
  logic [7:0] zi70;
  logic [105:0] main_$l_v559$7158_outR1;
  logic [7:0] main_r2_outR12;
  logic [7:0] zi71;
  logic [105:0] main_$l_v559$7158_outR2;
  logic [7:0] main_r3_outR12;
  logic [7:0] zi72;
  logic [105:0] main_$l_v559$7158_outR3;
  logic [1:0] main_mkreg_outR21;
  logic [1:0] zi73;
  logic [80:0] main_setr0_out;
  logic [80:0] zi74;
  logic [105:0] main_$l__unused103$6499_outR7;
  logic [80:0] main_setr1_out;
  logic [80:0] zi75;
  logic [105:0] main_$l__unused103$6499_outR8;
  logic [80:0] main_setr2_out;
  logic [80:0] zi76;
  logic [105:0] main_$l__unused103$6499_outR9;
  logic [80:0] main_setr3_out;
  logic [80:0] zi77;
  logic [105:0] main_$l__unused103$6499_outR10;
  logic [1:0] main_mkreg_outR22;
  logic [1:0] zi78;
  logic [7:0] main_r0_outR13;
  logic [7:0] zi79;
  logic [105:0] main_$l_v596$7211_out;
  logic [7:0] main_r1_outR13;
  logic [7:0] zi80;
  logic [105:0] main_$l_v596$7211_outR1;
  logic [7:0] main_r2_outR13;
  logic [7:0] zi81;
  logic [105:0] main_$l_v596$7211_outR2;
  logic [7:0] main_r3_outR13;
  logic [7:0] zi82;
  logic [105:0] main_$l_v596$7211_outR3;
  logic [1:0] main_mkreg_outR23;
  logic [1:0] zi83;
  logic [7:0] main_r0_outR14;
  logic [7:0] zi84;
  logic [105:0] main_$l_v623$7250_out;
  logic [7:0] main_r1_outR14;
  logic [7:0] zi85;
  logic [105:0] main_$l_v623$7250_outR1;
  logic [7:0] main_r2_outR14;
  logic [7:0] zi86;
  logic [105:0] main_$l_v623$7250_outR2;
  logic [7:0] main_r3_outR14;
  logic [7:0] zi87;
  logic [105:0] main_$l_v623$7250_outR3;
  logic [1:0] main_mkreg_outR24;
  logic [1:0] zi88;
  logic [7:0] main_r0_outR15;
  logic [7:0] zi89;
  logic [105:0] main_$l_v650$7289_out;
  logic [7:0] main_r1_outR15;
  logic [7:0] zi90;
  logic [105:0] main_$l_v650$7289_outR1;
  logic [7:0] main_r2_outR15;
  logic [7:0] zi91;
  logic [105:0] main_$l_v650$7289_outR2;
  logic [7:0] main_r3_outR15;
  logic [7:0] zi92;
  logic [105:0] main_$l_v650$7289_outR3;
  logic [7:0] main_r0_outR16;
  logic [7:0] zi93;
  logic [105:0] main_$l_v672$7321_out;
  logic [7:0] main_r1_outR16;
  logic [7:0] zi94;
  logic [105:0] main_$l_v672$7321_outR1;
  logic [7:0] main_r2_outR16;
  logic [7:0] zi95;
  logic [105:0] main_$l_v672$7321_outR2;
  logic [7:0] main_r3_outR16;
  logic [7:0] zi96;
  logic [105:0] main_$l_v672$7321_outR3;
  logic [1:0] main_mkreg_outR25;
  logic [1:0] zi97;
  logic [7:0] main_r0_outR17;
  logic [7:0] zi98;
  logic [105:0] main_$l_v695$7354_out;
  logic [7:0] main_r1_outR17;
  logic [7:0] zi99;
  logic [105:0] main_$l_v695$7354_outR1;
  logic [7:0] main_r2_outR17;
  logic [7:0] zi100;
  logic [105:0] main_$l_v695$7354_outR2;
  logic [7:0] main_r3_outR17;
  logic [7:0] zi101;
  logic [105:0] main_$l_v695$7354_outR3;
  Main_dataIn  inst (arg0, main_datain_out);
  assign i = main_datain_out;
  assign ds1 = i[6];
  assign ds2 = i[5];
  assign ds3 = i[4];
  assign ren = i[3];
  assign wen = i[2];
  assign b0 = i[1];
  assign b1 = i[0];
  assign za = i[7];
  Main_mkReg  instR1 (b0, b1, main_mkreg_out);
  assign zi0 = main_mkreg_out;
  Main_pc  instR2 (arg1, main_pc_out);
  assign zi1 = main_pc_out;
  Main_outputs  instR3 (arg1, main_outputs_out);
  assign zi2 = main_outputs_out;
  Main_setAddrOut  instR4 (zi2, zi1, main_setaddrout_out);
  assign zi3 = main_setaddrout_out;
  Main_setOutputs  instR5 (arg1, zi3, main_setoutputs_out);
  assign zi4 = main_setoutputs_out;
  Main_outputs  instR6 (zi4, main_outputs_outR1);
  assign zi5 = main_outputs_outR1;
  Main_mkReg  instR7 (b0, b1, main_mkreg_outR1);
  assign zi6 = main_mkreg_outR1;
  Main_r0  instR8 (arg1, main_r0_out);
  assign zi7 = main_r0_out;
  main_$L_a65$6446  instR9 (ren, wen, zi7, arg1, main_$l_a65$6446_out);
  Main_r1  instR10 (arg1, main_r1_out);
  assign zi8 = main_r1_out;
  main_$L_a65$6446  instR11 (ren, wen, zi8, arg1, main_$l_a65$6446_outR1);
  Main_r2  instR12 (arg1, main_r2_out);
  assign zi9 = main_r2_out;
  main_$L_a65$6446  instR13 (ren, wen, zi9, arg1, main_$l_a65$6446_outR2);
  Main_r3  instR14 (arg1, main_r3_out);
  assign zi10 = main_r3_out;
  main_$L_a65$6446  instR15 (ren, wen, zi10, arg1, main_$l_a65$6446_outR3);
  Main_mkReg  instR16 (b0, b1, main_mkreg_outR2);
  assign zi11 = main_mkreg_outR2;
  Main_r0  instR17 (arg1, main_r0_outR1);
  assign zi12 = main_r0_outR1;
  main_$L_a99$6495  instR18 (ren, wen, zi12, arg1, main_$l_a99$6495_out);
  Main_r1  instR19 (arg1, main_r1_outR1);
  assign zi13 = main_r1_outR1;
  main_$L_a99$6495  instR20 (ren, wen, zi13, arg1, main_$l_a99$6495_outR1);
  Main_r2  instR21 (arg1, main_r2_outR1);
  assign zi14 = main_r2_outR1;
  main_$L_a99$6495  instR22 (ren, wen, zi14, arg1, main_$l_a99$6495_outR2);
  Main_r3  instR23 (arg1, main_r3_outR1);
  assign zi15 = main_r3_outR1;
  main_$L_a99$6495  instR24 (ren, wen, zi15, arg1, main_$l_a99$6495_outR3);
  Main_mkReg  instR25 (ren, wen, main_mkreg_outR3);
  assign zi16 = main_mkreg_outR3;
  Main_mkReg  instR26 (b0, b1, main_mkreg_outR4);
  assign zi17 = main_mkreg_outR4;
  Main_r0  instR27 (arg1, main_r0_outR2);
  main_$L_vD134$6546  instR28 (zi16, zi17, main_r0_outR2, arg1, main_$l_vd134$6546_out);
  Main_r1  instR29 (arg1, main_r1_outR2);
  main_$L_vD134$6546  instR30 (zi16, zi17, main_r1_outR2, arg1, main_$l_vd134$6546_outR1);
  Main_r2  instR31 (arg1, main_r2_outR2);
  main_$L_vD134$6546  instR32 (zi16, zi17, main_r2_outR2, arg1, main_$l_vd134$6546_outR2);
  Main_r3  instR33 (arg1, main_r3_outR2);
  main_$L_vD134$6546  instR34 (zi16, zi17, main_r3_outR2, arg1, main_$l_vd134$6546_outR3);
  Main_mkReg  instR35 (ren, wen, main_mkreg_outR5);
  assign zi18 = main_mkreg_outR5;
  Main_mkReg  instR36 (b0, b1, main_mkreg_outR6);
  assign zi19 = main_mkreg_outR6;
  Main_r0  instR37 (arg1, main_r0_outR3);
  main_$L_vD168$6595  instR38 (zi18, zi19, main_r0_outR3, arg1, main_$l_vd168$6595_out);
  Main_r1  instR39 (arg1, main_r1_outR3);
  main_$L_vD168$6595  instR40 (zi18, zi19, main_r1_outR3, arg1, main_$l_vd168$6595_outR1);
  Main_r2  instR41 (arg1, main_r2_outR3);
  main_$L_vD168$6595  instR42 (zi18, zi19, main_r2_outR3, arg1, main_$l_vd168$6595_outR2);
  Main_r3  instR43 (arg1, main_r3_outR3);
  main_$L_vD168$6595  instR44 (zi18, zi19, main_r3_outR3, arg1, main_$l_vd168$6595_outR3);
  Main_mkReg  instR45 (ren, wen, main_mkreg_outR7);
  assign zi20 = main_mkreg_outR7;
  Main_mkReg  instR46 (b0, b1, main_mkreg_outR8);
  assign zi21 = main_mkreg_outR8;
  Main_r0  instR47 (arg1, main_r0_outR4);
  main_$L_vD204$6647  instR48 (zi20, zi21, main_r0_outR4, arg1, main_$l_vd204$6647_out);
  Main_r1  instR49 (arg1, main_r1_outR4);
  main_$L_vD204$6647  instR50 (zi20, zi21, main_r1_outR4, arg1, main_$l_vd204$6647_outR1);
  Main_r2  instR51 (arg1, main_r2_outR4);
  main_$L_vD204$6647  instR52 (zi20, zi21, main_r2_outR4, arg1, main_$l_vd204$6647_outR2);
  Main_r3  instR53 (arg1, main_r3_outR4);
  main_$L_vD204$6647  instR54 (zi20, zi21, main_r3_outR4, arg1, main_$l_vd204$6647_outR3);
  Main_mkReg  instR55 (ren, wen, main_mkreg_outR9);
  assign zi22 = main_mkreg_outR9;
  Main_mkReg  instR56 (b0, b1, main_mkreg_outR10);
  assign zi23 = main_mkreg_outR10;
  Main_r0  instR57 (arg1, main_r0_outR5);
  main_$L_vD238$6696  instR58 (zi22, zi23, main_r0_outR5, arg1, main_$l_vd238$6696_out);
  Main_r1  instR59 (arg1, main_r1_outR5);
  main_$L_vD238$6696  instR60 (zi22, zi23, main_r1_outR5, arg1, main_$l_vd238$6696_outR1);
  Main_r2  instR61 (arg1, main_r2_outR5);
  main_$L_vD238$6696  instR62 (zi22, zi23, main_r2_outR5, arg1, main_$l_vd238$6696_outR2);
  Main_r3  instR63 (arg1, main_r3_outR5);
  main_$L_vD238$6696  instR64 (zi22, zi23, main_r3_outR5, arg1, main_$l_vd238$6696_outR3);
  Main_mkReg  instR65 (b0, b1, main_mkreg_outR11);
  assign zi24 = main_mkreg_outR11;
  Main_r0  instR66 (arg1, main_r0_outR6);
  assign zi25 = main_r0_outR6;
  main_$L_x274$6748  instR67 (ren, wen, zi25, arg1, main_$l_x274$6748_out);
  Main_r1  instR68 (arg1, main_r1_outR6);
  assign zi26 = main_r1_outR6;
  main_$L_x274$6748  instR69 (ren, wen, zi26, arg1, main_$l_x274$6748_outR1);
  Main_r2  instR70 (arg1, main_r2_outR6);
  assign zi27 = main_r2_outR6;
  main_$L_x274$6748  instR71 (ren, wen, zi27, arg1, main_$l_x274$6748_outR2);
  Main_r3  instR72 (arg1, main_r3_outR6);
  assign zi28 = main_r3_outR6;
  main_$L_x274$6748  instR73 (ren, wen, zi28, arg1, main_$l_x274$6748_outR3);
  Main_mkReg  instR74 (ren, wen, main_mkreg_outR12);
  assign zi29 = main_mkreg_outR12;
  Main_mkReg  instR75 (b0, b1, main_mkreg_outR13);
  assign zi30 = main_mkreg_outR13;
  Main_r0  instR76 (arg1, main_r0_outR7);
  main_$L_vD297$6781  instR77 (zi29, zi30, main_r0_outR7, arg1, main_$l_vd297$6781_out);
  Main_r1  instR78 (arg1, main_r1_outR7);
  main_$L_vD297$6781  instR79 (zi29, zi30, main_r1_outR7, arg1, main_$l_vd297$6781_outR1);
  Main_r2  instR80 (arg1, main_r2_outR7);
  main_$L_vD297$6781  instR81 (zi29, zi30, main_r2_outR7, arg1, main_$l_vd297$6781_outR2);
  Main_r3  instR82 (arg1, main_r3_outR7);
  main_$L_vD297$6781  instR83 (zi29, zi30, main_r3_outR7, arg1, main_$l_vd297$6781_outR3);
  Main_mkReg  instR84 (ren, wen, main_mkreg_outR14);
  assign zi31 = main_mkreg_outR14;
  Main_mkReg  instR85 (b0, b1, main_mkreg_outR15);
  assign zi32 = main_mkreg_outR15;
  Main_r0  instR86 (arg1, main_r0_outR8);
  main_$L_vD333$6833  instR87 (zi31, zi32, main_r0_outR8, arg1, main_$l_vd333$6833_out);
  Main_r1  instR88 (arg1, main_r1_outR8);
  main_$L_vD333$6833  instR89 (zi31, zi32, main_r1_outR8, arg1, main_$l_vd333$6833_outR1);
  Main_r2  instR90 (arg1, main_r2_outR8);
  main_$L_vD333$6833  instR91 (zi31, zi32, main_r2_outR8, arg1, main_$l_vd333$6833_outR2);
  Main_r3  instR92 (arg1, main_r3_outR8);
  main_$L_vD333$6833  instR93 (zi31, zi32, main_r3_outR8, arg1, main_$l_vd333$6833_outR3);
  Main_mkReg  instR94 (ren, wen, main_mkreg_outR16);
  assign zi33 = main_mkreg_outR16;
  Main_mkReg  instR95 (b0, b1, main_mkreg_outR17);
  assign zi34 = main_mkreg_outR17;
  Main_r0  instR96 (arg1, main_r0_outR9);
  main_$L_vD369$6885  instR97 (zi33, zi34, main_r0_outR9, arg1, main_$l_vd369$6885_out);
  Main_r1  instR98 (arg1, main_r1_outR9);
  main_$L_vD369$6885  instR99 (zi33, zi34, main_r1_outR9, arg1, main_$l_vd369$6885_outR1);
  Main_r2  instR100 (arg1, main_r2_outR9);
  main_$L_vD369$6885  instR101 (zi33, zi34, main_r2_outR9, arg1, main_$l_vd369$6885_outR2);
  Main_r3  instR102 (arg1, main_r3_outR9);
  main_$L_vD369$6885  instR103 (zi33, zi34, main_r3_outR9, arg1, main_$l_vd369$6885_outR3);
  Main_mkReg  instR104 (ren, wen, main_mkreg_outR18);
  assign zi35 = main_mkreg_outR18;
  Main_r0  instR105 (arg1, main_r0_outR10);
  assign zi36 = main_r0_outR10;
  main_$L_vD405$6937  instR106 (b0, b1, zi36, arg1, main_$l_vd405$6937_out);
  Main_r1  instR107 (arg1, main_r1_outR10);
  assign zi37 = main_r1_outR10;
  main_$L_vD405$6937  instR108 (b0, b1, zi37, arg1, main_$l_vd405$6937_outR1);
  Main_r2  instR109 (arg1, main_r2_outR10);
  assign zi38 = main_r2_outR10;
  main_$L_vD405$6937  instR110 (b0, b1, zi38, arg1, main_$l_vd405$6937_outR2);
  Main_r3  instR111 (arg1, main_r3_outR10);
  assign zi39 = main_r3_outR10;
  main_$L_vD405$6937  instR112 (b0, b1, zi39, arg1, main_$l_vd405$6937_outR3);
  Main_zFlag  instR113 (arg1, main_zflag_out);
  assign zi40 = main_zflag_out;
  main_$L___unused103$6499  instR114 (arg1, main_$l__unused103$6499_out);
  main_$L_arm448$6999  instR115 (b0, b1, arg1, main_$l_arm448$6999_out);
  Main_zFlag  instR116 (arg1, main_zflag_outR1);
  assign zi41 = main_zflag_outR1;
  Main_notb  instR117 (zi41, main_notb_out);
  assign zi42 = main_notb_out;
  main_$L___unused103$6499  instR118 (arg1, main_$l__unused103$6499_outR1);
  main_$L_arm448$6999  instR119 (b0, b1, arg1, main_$l_arm448$6999_outR1);
  Main_cFlag  instR120 (arg1, main_cflag_out);
  assign zi43 = main_cflag_out;
  main_$L___unused103$6499  instR121 (arg1, main_$l__unused103$6499_outR2);
  main_$L_arm448$6999  instR122 (b0, b1, arg1, main_$l_arm448$6999_outR2);
  Main_cFlag  instR123 (arg1, main_cflag_outR1);
  assign zi44 = main_cflag_outR1;
  Main_notb  instR124 (zi44, main_notb_outR1);
  assign zi45 = main_notb_outR1;
  main_$L___unused103$6499  instR125 (arg1, main_$l__unused103$6499_outR3);
  main_$L_arm448$6999  instR126 (b0, b1, arg1, main_$l_arm448$6999_outR3);
  Main_mkReg  instR127 (b0, b1, main_mkreg_outR19);
  assign zi46 = main_mkreg_outR19;
  Main_r0  instR128 (arg1, main_r0_outR11);
  assign zi47 = main_r0_outR11;
  main_$L_x508$7084  instR129 (zi47, arg1, main_$l_x508$7084_out);
  Main_r1  instR130 (arg1, main_r1_outR11);
  assign zi48 = main_r1_outR11;
  main_$L_x508$7084  instR131 (zi48, arg1, main_$l_x508$7084_outR1);
  Main_r2  instR132 (arg1, main_r2_outR11);
  assign zi49 = main_r2_outR11;
  main_$L_x508$7084  instR133 (zi49, arg1, main_$l_x508$7084_outR2);
  Main_r3  instR134 (arg1, main_r3_outR11);
  assign zi50 = main_r3_outR11;
  main_$L_x508$7084  instR135 (zi50, arg1, main_$l_x508$7084_outR3);
  Main_setIEFlag  instR136 (arg1, b1, main_setieflag_out);
  assign zi51 = main_setieflag_out;
  main_$L___unused103$6499  instR137 (zi51, main_$l__unused103$6499_outR4);
  Main_outputs  instR138 (arg1, main_outputs_outR2);
  assign zi52 = main_outputs_outR2;
  assign zi53 = zi52[17:10];
  assign zi54 = zi52[9:2];
  assign zi55 = zi52[1];
  assign zi56 = {zi53, zi54, zi55, 1'h1};
  Main_setOutputs  instR139 (arg1, zi56, main_setoutputs_outR1);
  assign zi57 = main_setoutputs_outR1;
  main_$L___unused103$6499  instR140 (zi57, main_$l__unused103$6499_outR5);
  Main_setIEFlag  instR141 (arg1, 1'h1, main_setieflag_outR1);
  assign zi58 = main_setieflag_outR1;
  assign zi59 = zi58[39:32];
  assign zi60 = zi59;
  Main_setPC  instR142 (zi58, zi60, main_setpc_out);
  assign zi61 = main_setpc_out;
  assign zi62 = zi61[41];
  assign zi63 = zi62;
  Main_setZFlag  instR143 (zi61, zi63, main_setzflag_out);
  assign zi64 = main_setzflag_out;
  assign zi65 = zi64[40];
  assign zi66 = zi65;
  Main_setCFlag  instR144 (zi64, zi66, main_setcflag_out);
  assign zi67 = main_setcflag_out;
  main_$L___unused103$6499  instR145 (zi67, main_$l__unused103$6499_outR6);
  Main_mkReg  instR146 (b0, b1, main_mkreg_outR20);
  assign zi68 = main_mkreg_outR20;
  Main_r0  instR147 (arg1, main_r0_outR12);
  assign zi69 = main_r0_outR12;
  main_$L_v559$7158  instR148 (zi68, zi69, arg1, main_$l_v559$7158_out);
  Main_r1  instR149 (arg1, main_r1_outR12);
  assign zi70 = main_r1_outR12;
  main_$L_v559$7158  instR150 (zi68, zi70, arg1, main_$l_v559$7158_outR1);
  Main_r2  instR151 (arg1, main_r2_outR12);
  assign zi71 = main_r2_outR12;
  main_$L_v559$7158  instR152 (zi68, zi71, arg1, main_$l_v559$7158_outR2);
  Main_r3  instR153 (arg1, main_r3_outR12);
  assign zi72 = main_r3_outR12;
  main_$L_v559$7158  instR154 (zi68, zi72, arg1, main_$l_v559$7158_outR3);
  Main_mkReg  instR155 (b0, b1, main_mkreg_outR21);
  assign zi73 = main_mkreg_outR21;
  Main_setR0  instR156 (arg1, 8'h0, main_setr0_out);
  assign zi74 = main_setr0_out;
  main_$L___unused103$6499  instR157 (zi74, main_$l__unused103$6499_outR7);
  Main_setR1  instR158 (arg1, 8'h0, main_setr1_out);
  assign zi75 = main_setr1_out;
  main_$L___unused103$6499  instR159 (zi75, main_$l__unused103$6499_outR8);
  Main_setR2  instR160 (arg1, 8'h0, main_setr2_out);
  assign zi76 = main_setr2_out;
  main_$L___unused103$6499  instR161 (zi76, main_$l__unused103$6499_outR9);
  Main_setR3  instR162 (arg1, 8'h0, main_setr3_out);
  assign zi77 = main_setr3_out;
  main_$L___unused103$6499  instR163 (zi77, main_$l__unused103$6499_outR10);
  Main_mkReg  instR164 (b0, b1, main_mkreg_outR22);
  assign zi78 = main_mkreg_outR22;
  Main_r0  instR165 (arg1, main_r0_outR13);
  assign zi79 = main_r0_outR13;
  main_$L_v596$7211  instR166 (zi78, zi79, arg1, main_$l_v596$7211_out);
  Main_r1  instR167 (arg1, main_r1_outR13);
  assign zi80 = main_r1_outR13;
  main_$L_v596$7211  instR168 (zi78, zi80, arg1, main_$l_v596$7211_outR1);
  Main_r2  instR169 (arg1, main_r2_outR13);
  assign zi81 = main_r2_outR13;
  main_$L_v596$7211  instR170 (zi78, zi81, arg1, main_$l_v596$7211_outR2);
  Main_r3  instR171 (arg1, main_r3_outR13);
  assign zi82 = main_r3_outR13;
  main_$L_v596$7211  instR172 (zi78, zi82, arg1, main_$l_v596$7211_outR3);
  Main_mkReg  instR173 (b0, b1, main_mkreg_outR23);
  assign zi83 = main_mkreg_outR23;
  Main_r0  instR174 (arg1, main_r0_outR14);
  assign zi84 = main_r0_outR14;
  main_$L_v623$7250  instR175 (zi83, zi84, arg1, main_$l_v623$7250_out);
  Main_r1  instR176 (arg1, main_r1_outR14);
  assign zi85 = main_r1_outR14;
  main_$L_v623$7250  instR177 (zi83, zi85, arg1, main_$l_v623$7250_outR1);
  Main_r2  instR178 (arg1, main_r2_outR14);
  assign zi86 = main_r2_outR14;
  main_$L_v623$7250  instR179 (zi83, zi86, arg1, main_$l_v623$7250_outR2);
  Main_r3  instR180 (arg1, main_r3_outR14);
  assign zi87 = main_r3_outR14;
  main_$L_v623$7250  instR181 (zi83, zi87, arg1, main_$l_v623$7250_outR3);
  Main_mkReg  instR182 (b0, b1, main_mkreg_outR24);
  assign zi88 = main_mkreg_outR24;
  Main_r0  instR183 (arg1, main_r0_outR15);
  assign zi89 = main_r0_outR15;
  main_$L_v650$7289  instR184 (zi88, zi89, arg1, main_$l_v650$7289_out);
  Main_r1  instR185 (arg1, main_r1_outR15);
  assign zi90 = main_r1_outR15;
  main_$L_v650$7289  instR186 (zi88, zi90, arg1, main_$l_v650$7289_outR1);
  Main_r2  instR187 (arg1, main_r2_outR15);
  assign zi91 = main_r2_outR15;
  main_$L_v650$7289  instR188 (zi88, zi91, arg1, main_$l_v650$7289_outR2);
  Main_r3  instR189 (arg1, main_r3_outR15);
  assign zi92 = main_r3_outR15;
  main_$L_v650$7289  instR190 (zi88, zi92, arg1, main_$l_v650$7289_outR3);
  Main_r0  instR191 (arg1, main_r0_outR16);
  assign zi93 = main_r0_outR16;
  main_$L_v672$7321  instR192 (zi88, zi93, arg1, main_$l_v672$7321_out);
  Main_r1  instR193 (arg1, main_r1_outR16);
  assign zi94 = main_r1_outR16;
  main_$L_v672$7321  instR194 (zi88, zi94, arg1, main_$l_v672$7321_outR1);
  Main_r2  instR195 (arg1, main_r2_outR16);
  assign zi95 = main_r2_outR16;
  main_$L_v672$7321  instR196 (zi88, zi95, arg1, main_$l_v672$7321_outR2);
  Main_r3  instR197 (arg1, main_r3_outR16);
  assign zi96 = main_r3_outR16;
  main_$L_v672$7321  instR198 (zi88, zi96, arg1, main_$l_v672$7321_outR3);
  Main_mkReg  instR199 (b0, b1, main_mkreg_outR25);
  assign zi97 = main_mkreg_outR25;
  Main_r0  instR200 (arg1, main_r0_outR17);
  assign zi98 = main_r0_outR17;
  main_$L_v695$7354  instR201 (ren, wen, zi97, zi98, arg1, main_$l_v695$7354_out);
  Main_r1  instR202 (arg1, main_r1_outR17);
  assign zi99 = main_r1_outR17;
  main_$L_v695$7354  instR203 (ren, wen, zi97, zi99, arg1, main_$l_v695$7354_outR1);
  Main_r2  instR204 (arg1, main_r2_outR17);
  assign zi100 = main_r2_outR17;
  main_$L_v695$7354  instR205 (ren, wen, zi97, zi100, arg1, main_$l_v695$7354_outR2);
  Main_r3  instR206 (arg1, main_r3_outR17);
  assign zi101 = main_r3_outR17;
  main_$L_v695$7354  instR207 (ren, wen, zi97, zi101, arg1, main_$l_v695$7354_outR3);
  assign res = (za == 1'h0) ? ((ds1 == 1'h0) ? ((ds2 == 1'h0) ? ((ds3 == 1'h0) ? {zi5, 3'h1, ren, wen, zi0, zi4} : ((zi6 == 2'h0) ? main_$l_a65$6446_out : ((zi6 == 2'h1) ? main_$l_a65$6446_outR1 : ((zi6 == 2'h2) ? main_$l_a65$6446_outR2 : main_$l_a65$6446_outR3)))) : ((ds3 == 1'h0) ? ((zi11 == 2'h0) ? main_$l_a99$6495_out : ((zi11 == 2'h1) ? main_$l_a99$6495_outR1 : ((zi11 == 2'h2) ? main_$l_a99$6495_outR2 : main_$l_a99$6495_outR3))) : ((zi16 == 2'h0) ? main_$l_vd134$6546_out : ((zi16 == 2'h1) ? main_$l_vd134$6546_outR1 : ((zi16 == 2'h2) ? main_$l_vd134$6546_outR2 : main_$l_vd134$6546_outR3))))) : ((ds2 == 1'h0) ? ((ds3 == 1'h0) ? ((zi18 == 2'h0) ? main_$l_vd168$6595_out : ((zi18 == 2'h1) ? main_$l_vd168$6595_outR1 : ((zi18 == 2'h2) ? main_$l_vd168$6595_outR2 : main_$l_vd168$6595_outR3))) : ((zi20 == 2'h0) ? main_$l_vd204$6647_out : ((zi20 == 2'h1) ? main_$l_vd204$6647_outR1 : ((zi20 == 2'h2) ? main_$l_vd204$6647_outR2 : main_$l_vd204$6647_outR3)))) : ((ds3 == 1'h0) ? ((zi22 == 2'h0) ? main_$l_vd238$6696_out : ((zi22 == 2'h1) ? main_$l_vd238$6696_outR1 : ((zi22 == 2'h2) ? main_$l_vd238$6696_outR2 : main_$l_vd238$6696_outR3))) : ((zi24 == 2'h0) ? main_$l_x274$6748_out : ((zi24 == 2'h1) ? main_$l_x274$6748_outR1 : ((zi24 == 2'h2) ? main_$l_x274$6748_outR2 : main_$l_x274$6748_outR3)))))) : ((ds1 == 1'h0) ? ((ds2 == 1'h0) ? ((ds3 == 1'h0) ? ((zi29 == 2'h0) ? main_$l_vd297$6781_out : ((zi29 == 2'h1) ? main_$l_vd297$6781_outR1 : ((zi29 == 2'h2) ? main_$l_vd297$6781_outR2 : main_$l_vd297$6781_outR3))) : ((zi31 == 2'h0) ? main_$l_vd333$6833_out : ((zi31 == 2'h1) ? main_$l_vd333$6833_outR1 : ((zi31 == 2'h2) ? main_$l_vd333$6833_outR2 : main_$l_vd333$6833_outR3)))) : ((ds3 == 1'h0) ? ((zi33 == 2'h0) ? main_$l_vd369$6885_out : ((zi33 == 2'h1) ? main_$l_vd369$6885_outR1 : ((zi33 == 2'h2) ? main_$l_vd369$6885_outR2 : main_$l_vd369$6885_outR3))) : ((zi35 == 2'h0) ? main_$l_vd405$6937_out : ((zi35 == 2'h1) ? main_$l_vd405$6937_outR1 : ((zi35 == 2'h2) ? main_$l_vd405$6937_outR2 : main_$l_vd405$6937_outR3))))) : ((ds2 == 1'h0) ? ((ds3 == 1'h0) ? ((ren == 1'h0) ? ((wen == 1'h0) ? ((zi40 == 1'h0) ? main_$l__unused103$6499_out : main_$l_arm448$6999_out) : ((zi42 == 1'h0) ? main_$l__unused103$6499_outR1 : main_$l_arm448$6999_outR1)) : ((wen == 1'h0) ? ((zi43 == 1'h0) ? main_$l__unused103$6499_outR2 : main_$l_arm448$6999_outR2) : ((zi45 == 1'h0) ? main_$l__unused103$6499_outR3 : main_$l_arm448$6999_outR3))) : ((ren == 1'h0) ? ((wen == 1'h0) ? ((zi46 == 2'h0) ? main_$l_x508$7084_out : ((zi46 == 2'h1) ? main_$l_x508$7084_outR1 : ((zi46 == 2'h2) ? main_$l_x508$7084_outR2 : main_$l_x508$7084_outR3))) : ((b0 == 1'h0) ? main_$l__unused103$6499_outR4 : ((b1 == 1'h0) ? main_$l__unused103$6499_outR5 : main_$l__unused103$6499_outR6))) : ((wen == 1'h0) ? ((zi68 == 2'h0) ? main_$l_v559$7158_out : ((zi68 == 2'h1) ? main_$l_v559$7158_outR1 : ((zi68 == 2'h2) ? main_$l_v559$7158_outR2 : main_$l_v559$7158_outR3))) : ((zi73 == 2'h0) ? main_$l__unused103$6499_outR7 : ((zi73 == 2'h1) ? main_$l__unused103$6499_outR8 : ((zi73 == 2'h2) ? main_$l__unused103$6499_outR9 : main_$l__unused103$6499_outR10)))))) : ((ds3 == 1'h0) ? ((ren == 1'h0) ? ((wen == 1'h0) ? ((zi78 == 2'h0) ? main_$l_v596$7211_out : ((zi78 == 2'h1) ? main_$l_v596$7211_outR1 : ((zi78 == 2'h2) ? main_$l_v596$7211_outR2 : main_$l_v596$7211_outR3))) : ((zi83 == 2'h0) ? main_$l_v623$7250_out : ((zi83 == 2'h1) ? main_$l_v623$7250_outR1 : ((zi83 == 2'h2) ? main_$l_v623$7250_outR2 : main_$l_v623$7250_outR3)))) : ((wen == 1'h0) ? ((zi88 == 2'h0) ? main_$l_v650$7289_out : ((zi88 == 2'h1) ? main_$l_v650$7289_outR1 : ((zi88 == 2'h2) ? main_$l_v650$7289_outR2 : main_$l_v650$7289_outR3))) : ((zi88 == 2'h0) ? main_$l_v672$7321_out : ((zi88 == 2'h1) ? main_$l_v672$7321_outR1 : ((zi88 == 2'h2) ? main_$l_v672$7321_outR2 : main_$l_v672$7321_outR3))))) : ((zi97 == 2'h0) ? main_$l_v695$7354_out : ((zi97 == 2'h1) ? main_$l_v695$7354_outR1 : ((zi97 == 2'h2) ? main_$l_v695$7354_outR2 : main_$l_v695$7354_outR3))))));
endmodule

module main_$L_Main_loop3$6359 (input logic [80:0] arg0,
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
  logic [80:0] zi49;
  logic [105:0] main_$l__unused103$6499_out;
  logic [105:0] main_$l_fail7$6363_out;
  logic [105:0] main_$l_fail7$6363_outR1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi50;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi51;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi52;
  logic [105:0] main_$l__unused103$6499_outR1;
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
  assign zi49 = {zi37, zi38, zi39, zi40, zi41, zi42, zi43, zi10, zi44, zi45, zi46, zi47, zi48};
  main_$L___unused103$6499  instR5 (zi49, main_$l__unused103$6499_out);
  main_$L_fail7$6363  instR6 (zi0, arg0, main_$l_fail7$6363_out);
  main_$L_fail7$6363  instR7 (zi0, arg0, main_$l_fail7$6363_outR1);
  Main_setCFlag  instR8 (arg0, 1'h0, main_setcflag_out);
  assign zi50 = main_setcflag_out;
  Main_setZFlag  instR9 (zi50, 1'h0, main_setzflag_out);
  assign zi51 = main_setzflag_out;
  Main_setOutputs  instR10 (zi51, 18'h0, main_setoutputs_out);
  assign zi52 = main_setoutputs_out;
  main_$L___unused103$6499  instR11 (zi52, main_$l__unused103$6499_outR1);
  assign res = (zi2 == 1'h0) ? ((zi4 == 1'h1) ? ((zi6 == 1'h1) ? main_$l__unused103$6499_out : main_$l_fail7$6363_out) : main_$l_fail7$6363_outR1) : main_$l__unused103$6499_outR1;
endmodule

module Main_notb (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = (arg0 == 1'h0) ? 1'h1 : 1'h0;
endmodule

module Main_mkReg (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  assign res = (arg0 == 1'h0) ? ((arg1 == 1'h0) ? 2'h0 : 2'h1) : ((arg1 == 1'h0) ? 2'h2 : 2'h3);
endmodule

module Main_outputs (input logic [80:0] arg0,
  output logic [17:0] res);
  logic [17:0] o;
  assign o = arg0[70:53];
  assign res = o;
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

module Main_pc (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] pc;
  assign pc = arg0[49:42];
  assign res = pc;
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

module Main_r0 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r0;
  assign r0 = arg0[31:24];
  assign res = r0;
endmodule

module Main_r1 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r1;
  assign r1 = arg0[23:16];
  assign res = r1;
endmodule

module Main_r2 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r2;
  assign r2 = arg0[15:8];
  assign res = r2;
endmodule

module Main_r3 (input logic [80:0] arg0,
  output logic [7:0] res);
  logic [7:0] r3;
  assign r3 = arg0[7:0];
  assign res = r3;
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

module Main_zFlag (input logic [80:0] arg0,
  output logic [0:0] res);
  logic [0:0] z;
  assign z = arg0[52];
  assign res = z;
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

module Main_cFlag (input logic [80:0] arg0,
  output logic [0:0] res);
  logic [0:0] c;
  assign c = arg0[51];
  assign res = c;
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

module Main_inputs (input logic [80:0] arg0,
  output logic [9:0] res);
  logic [9:0] i;
  assign i = arg0[80:71];
  assign res = i;
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

module Main_dataIn (input logic [9:0] arg0,
  output logic [7:0] res);
  logic [7:0] d_i;
  assign d_i = arg0[9:2];
  assign res = d_i;
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

module Main_lsbW8 (input logic [7:0] arg0,
  output logic [0:0] res);
  assign res = arg0[0];
endmodule

module Main_msbW8 (input logic [7:0] arg0,
  output logic [0:0] res);
  assign res = arg0[7];
endmodule

module Main_plusCW8$s1 (input logic [7:0] arg0,
  output logic [8:0] res);
  logic [8:0] s;
  assign s = ({1'h0, arg0} + 9'h1) + 9'h0;
  assign res = {s[8], s[7:0]};
endmodule

module Main_minusCW8$s1 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [8:0] res);
  logic [8:0] s;
  assign s = ({1'h0, arg0} - {1'h0, arg1}) - 9'h0;
  assign res = {s[8], s[7:0]};
endmodule