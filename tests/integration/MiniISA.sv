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
  logic [105:0] main_loop_out;
  logic [9:0] main_inputs_out;
  logic [9:0] zi6;
  logic [7:0] main_datain_out;
  logic [7:0] zi7;
  logic [80:0] main_setr0_out;
  logic [80:0] zi8;
  logic [105:0] main_loop_outR1;
  logic [80:0] main_setr1_out;
  logic [80:0] zi9;
  logic [105:0] main_loop_outR2;
  logic [80:0] main_setr2_out;
  logic [80:0] zi10;
  logic [105:0] main_loop_outR3;
  logic [80:0] main_setr3_out;
  logic [80:0] zi11;
  logic [105:0] main_loop_outR4;
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
  logic [105:0] main__unused7_out;
  logic [7:0] main_r0_out;
  logic [7:0] zi25;
  logic [105:0] main_d_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi26;
  logic [105:0] main_d_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi27;
  logic [105:0] main_d_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi28;
  logic [105:0] main_d_outR3;
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
  logic [105:0] main_loop_outR5;
  logic [80:0] main_setr1_outR1;
  logic [80:0] zi37;
  logic [105:0] main_loop_outR6;
  logic [80:0] main_setr2_outR1;
  logic [80:0] zi38;
  logic [105:0] main_loop_outR7;
  logic [80:0] main_setr3_outR1;
  logic [80:0] zi39;
  logic [105:0] main_loop_outR8;
  logic [80:0] main_setinputs_outR3;
  logic [80:0] zi41;
  logic [105:0] main_loop_outR9;
  logic [80:0] main_setinputs_outR4;
  logic [80:0] zi43;
  logic [105:0] main_loop_outR10;
  logic [105:0] zres;
  assign zi1 = {__in0, __in1, __in2};
  assign zi2 = __resumption_tag[2];
  assign zi3 = __resumption_tag[1:0];
  Main_setInputs  inst (__st0, zi1, main_setinputs_out);
  assign zi5 = main_setinputs_out;
  main_loop  instR1 (zi5, main_loop_out);
  Main_inputs  instR2 (zi5, main_inputs_out);
  assign zi6 = main_inputs_out;
  Main_dataIn  instR3 (zi6, main_datain_out);
  assign zi7 = main_datain_out;
  Main_setR0  instR4 (zi5, zi7, main_setr0_out);
  assign zi8 = main_setr0_out;
  main_loop  instR5 (zi8, main_loop_outR1);
  Main_setR1  instR6 (zi5, zi7, main_setr1_out);
  assign zi9 = main_setr1_out;
  main_loop  instR7 (zi9, main_loop_outR2);
  Main_setR2  instR8 (zi5, zi7, main_setr2_out);
  assign zi10 = main_setr2_out;
  main_loop  instR9 (zi10, main_loop_outR3);
  Main_setR3  instR10 (zi5, zi7, main_setr3_out);
  assign zi11 = main_setr3_out;
  main_loop  instR11 (zi11, main_loop_outR4);
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
  main___unused7  instR21 (zi12, zi14, zi24, main__unused7_out);
  Main_r0  instR22 (zi24, main_r0_out);
  assign zi25 = main_r0_out;
  main_d  instR23 (zi12, zi14, zi25, zi24, main_d_out);
  Main_r1  instR24 (zi24, main_r1_out);
  assign zi26 = main_r1_out;
  main_d  instR25 (zi12, zi14, zi26, zi24, main_d_outR1);
  Main_r2  instR26 (zi24, main_r2_out);
  assign zi27 = main_r2_out;
  main_d  instR27 (zi12, zi14, zi27, zi24, main_d_outR2);
  Main_r3  instR28 (zi24, main_r3_out);
  assign zi28 = main_r3_out;
  main_d  instR29 (zi12, zi14, zi28, zi24, main_d_outR3);
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
  main_loop  instR35 (zi36, main_loop_outR5);
  Main_setR1  instR36 (zi32, zi34, main_setr1_outR1);
  assign zi37 = main_setr1_outR1;
  main_loop  instR37 (zi37, main_loop_outR6);
  Main_setR2  instR38 (zi32, zi34, main_setr2_outR1);
  assign zi38 = main_setr2_outR1;
  main_loop  instR39 (zi38, main_loop_outR7);
  Main_setR3  instR40 (zi32, zi34, main_setr3_outR1);
  assign zi39 = main_setr3_outR1;
  main_loop  instR41 (zi39, main_loop_outR8);
  Main_setInputs  instR42 (__st0, zi1, main_setinputs_outR3);
  assign zi41 = main_setinputs_outR3;
  main_loop  instR43 (zi41, main_loop_outR9);
  Main_setInputs  instR44 (__st0, zi1, main_setinputs_outR4);
  assign zi43 = main_setinputs_outR4;
  main_loop  instR45 (zi43, main_loop_outR10);
  assign zres = (__resumption_tag[6:4] == 3'h0) ? ((~zi2) ? main_loop_out : ((zi3 == 2'h0) ? main_loop_outR1 : ((zi3 == 2'h1) ? main_loop_outR2 : ((zi3 == 2'h2) ? main_loop_outR3 : main_loop_outR4)))) : ((__resumption_tag[6:4] == 3'h1) ? ((~zi13) ? main__unused7_out : ((zi14 == 2'h0) ? main_d_out : ((zi14 == 2'h1) ? main_d_outR1 : ((zi14 == 2'h2) ? main_d_outR2 : main_d_outR3)))) : ((__resumption_tag[6:4] == 3'h2) ? ((zi35 == 2'h0) ? main_loop_outR5 : ((zi35 == 2'h1) ? main_loop_outR6 : ((zi35 == 2'h2) ? main_loop_outR7 : main_loop_outR8))) : ((__resumption_tag[6:4] == 3'h3) ? main_loop_outR9 : main_loop_outR10)));
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

module main___unused7 (input logic [0:0] arg0,
  input logic [1:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] main_pc_out;
  logic [7:0] zi0;
  logic [8:0] main_pluscw8$smain_onew8_false_bool_out;
  logic [8:0] zi1;
  logic [7:0] zi2;
  logic [7:0] zi3;
  logic [80:0] main_setpc_out;
  logic [80:0] zi4;
  logic [17:0] main_outputs_out;
  logic [17:0] zi5;
  Main_pc  inst (arg2, main_pc_out);
  assign zi0 = main_pc_out;
  Main_plusCW8$sMain__oneW8__False__Bool  instR1 (zi0, main_pluscw8$smain_onew8_false_bool_out);
  assign zi1 = main_pluscw8$smain_onew8_false_bool_out;
  assign zi2 = zi1[7:0];
  assign zi3 = zi2;
  Main_setPC  instR2 (arg2, zi3, main_setpc_out);
  assign zi4 = main_setpc_out;
  Main_outputs  instR3 (zi4, main_outputs_out);
  assign zi5 = main_outputs_out;
  assign res = {zi5, 4'h0, arg0, arg1, zi4};
endmodule

module main_d (input logic [0:0] arg0,
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
  logic [105:0] main__unused7_out;
  Main_outputs  inst (arg3, main_outputs_out);
  assign zi0 = main_outputs_out;
  Main_setDataOut  instR1 (zi0, arg2, main_setdataout_out);
  assign zi1 = main_setdataout_out;
  Main_setOutputs  instR2 (arg3, zi1, main_setoutputs_out);
  assign zi2 = main_setoutputs_out;
  main___unused7  instR3 (arg0, arg1, zi2, main__unused7_out);
  assign res = main__unused7_out;
endmodule

module main_a2 (input logic [0:0] arg0,
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

module main_v2 (input logic [7:0] arg0,
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
  logic [105:0] main_zzz_out;
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
  main_zzz  instR9 (zi8, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_a3 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [7:0] main_r0_out;
  logic [7:0] zi0;
  logic [105:0] main_v2_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] main_v2_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi2;
  logic [105:0] main_v2_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi3;
  logic [105:0] main_v2_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_r0  instR1 (arg3, main_r0_out);
  assign zi0 = main_r0_out;
  main_v2  instR2 (arg2, zi0, arg3, main_v2_out);
  Main_r1  instR3 (arg3, main_r1_out);
  assign zi1 = main_r1_out;
  main_v2  instR4 (arg2, zi1, arg3, main_v2_outR1);
  Main_r2  instR5 (arg3, main_r2_out);
  assign zi2 = main_r2_out;
  main_v2  instR6 (arg2, zi2, arg3, main_v2_outR2);
  Main_r3  instR7 (arg3, main_r3_out);
  assign zi3 = main_r3_out;
  main_v2  instR8 (arg2, zi3, arg3, main_v2_outR3);
  assign res = (za == 2'h0) ? main_v2_out : ((za == 2'h1) ? main_v2_outR1 : ((za == 2'h2) ? main_v2_outR2 : main_v2_outR3));
endmodule

module main___unused17 (input logic [80:0] arg0,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  Main_outputs  inst (arg0, main_outputs_out);
  assign zi0 = main_outputs_out;
  assign res = {zi0, 7'h40, arg0};
endmodule

module main_s63 (input logic [1:0] arg0,
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
  logic [105:0] main__unused17_out;
  logic [80:0] main_setr1_out;
  logic [80:0] zi3;
  logic [105:0] main__unused17_outR1;
  logic [80:0] main_setr2_out;
  logic [80:0] zi4;
  logic [105:0] main__unused17_outR2;
  logic [80:0] main_setr3_out;
  logic [80:0] zi5;
  logic [105:0] main__unused17_outR3;
  assign x = arg1[8];
  Main_setCFlag  inst (arg2, x, main_setcflag_out);
  assign s01 = main_setcflag_out;
  assign zi0 = arg1[7:0];
  assign zi1 = zi0;
  Main_setR0  instR1 (s01, zi1, main_setr0_out);
  assign zi2 = main_setr0_out;
  main___unused17  instR2 (zi2, main__unused17_out);
  Main_setR1  instR3 (s01, zi1, main_setr1_out);
  assign zi3 = main_setr1_out;
  main___unused17  instR4 (zi3, main__unused17_outR1);
  Main_setR2  instR5 (s01, zi1, main_setr2_out);
  assign zi4 = main_setr2_out;
  main___unused17  instR6 (zi4, main__unused17_outR2);
  Main_setR3  instR7 (s01, zi1, main_setr3_out);
  assign zi5 = main_setr3_out;
  main___unused17  instR8 (zi5, main__unused17_outR3);
  assign res = (arg0 == 2'h0) ? main__unused17_out : ((arg0 == 2'h1) ? main__unused17_outR1 : ((arg0 == 2'h2) ? main__unused17_outR2 : main__unused17_outR3));
endmodule

module main_vS (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [8:0] zi0;
  logic [8:0] p;
  logic [105:0] main_s63_out;
  assign zi0 = ({1'h0, arg1} + {1'h0, arg2}) + 9'h0;
  assign p = {zi0[8], zi0[7:0]};
  main_s63  inst (arg0, p, arg3, arg3, main_s63_out);
  assign res = main_s63_out;
endmodule

module main_vD (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_vs_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_vs_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_vs_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_vs_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_vS  instR1 (arg0, arg2, main_r0_out, arg3, main_vs_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_vS  instR3 (arg0, arg2, main_r1_out, arg3, main_vs_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_vS  instR5 (arg0, arg2, main_r2_out, arg3, main_vs_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_vS  instR7 (arg0, arg2, main_r3_out, arg3, main_vs_outR3);
  assign res = (arg1 == 2'h0) ? main_vs_out : ((arg1 == 2'h1) ? main_vs_outR1 : ((arg1 == 2'h2) ? main_vs_outR2 : main_vs_outR3));
endmodule

module main_vS2 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] main_cflag_out;
  logic [0:0] zi0;
  logic [8:0] zi1;
  logic [8:0] zi2;
  logic [105:0] main_s63_out;
  Main_cFlag  inst (arg3, main_cflag_out);
  assign zi0 = main_cflag_out;
  assign zi1 = ({1'h0, arg1} + {1'h0, arg2}) + {8'h0, zi0};
  assign zi2 = {zi1[8], zi1[7:0]};
  main_s63  instR1 (arg0, zi2, arg3, arg3, main_s63_out);
  assign res = main_s63_out;
endmodule

module main_vD2 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_vs2_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_vs2_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_vs2_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_vs2_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_vS2  instR1 (arg0, arg2, main_r0_out, arg3, main_vs2_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_vS2  instR3 (arg0, arg2, main_r1_out, arg3, main_vs2_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_vS2  instR5 (arg0, arg2, main_r2_out, arg3, main_vs2_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_vS2  instR7 (arg0, arg2, main_r3_out, arg3, main_vs2_outR3);
  assign res = (arg1 == 2'h0) ? main_vs2_out : ((arg1 == 2'h1) ? main_vs2_outR1 : ((arg1 == 2'h2) ? main_vs2_outR2 : main_vs2_outR3));
endmodule

module main_vS3 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [8:0] main_minuscw8$sfalse_bool_out;
  logic [8:0] p;
  logic [105:0] main_s63_out;
  Main_minusCW8$sFalse__Bool  inst (arg1, arg2, main_minuscw8$sfalse_bool_out);
  assign p = main_minuscw8$sfalse_bool_out;
  main_s63  instR1 (arg0, p, arg3, arg3, main_s63_out);
  assign res = main_s63_out;
endmodule

module main_vD3 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_vs3_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_vs3_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_vs3_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_vs3_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_vS3  instR1 (arg0, arg2, main_r0_out, arg3, main_vs3_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_vS3  instR3 (arg0, arg2, main_r1_out, arg3, main_vs3_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_vS3  instR5 (arg0, arg2, main_r2_out, arg3, main_vs3_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_vS3  instR7 (arg0, arg2, main_r3_out, arg3, main_vs3_outR3);
  assign res = (arg1 == 2'h0) ? main_vs3_out : ((arg1 == 2'h1) ? main_vs3_outR1 : ((arg1 == 2'h2) ? main_vs3_outR2 : main_vs3_outR3));
endmodule

module main_vS4 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [0:0] main_cflag_out;
  logic [0:0] zi0;
  logic [8:0] zi1;
  logic [8:0] zi2;
  logic [105:0] main_s63_out;
  Main_cFlag  inst (arg3, main_cflag_out);
  assign zi0 = main_cflag_out;
  assign zi1 = ({1'h0, arg1} - {1'h0, arg2}) - {8'h0, zi0};
  assign zi2 = {zi1[8], zi1[7:0]};
  main_s63  instR1 (arg0, zi2, arg3, arg3, main_s63_out);
  assign res = main_s63_out;
endmodule

module main_vD4 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_vs4_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_vs4_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_vs4_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_vs4_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_vS4  instR1 (arg0, arg2, main_r0_out, arg3, main_vs4_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_vS4  instR3 (arg0, arg2, main_r1_out, arg3, main_vs4_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_vS4  instR5 (arg0, arg2, main_r2_out, arg3, main_vs4_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_vS4  instR7 (arg0, arg2, main_r3_out, arg3, main_vs4_outR3);
  assign res = (arg1 == 2'h0) ? main_vs4_out : ((arg1 == 2'h1) ? main_vs4_outR1 : ((arg1 == 2'h2) ? main_vs4_outR2 : main_vs4_outR3));
endmodule

module main_zzz (input logic [80:0] arg0,
  output logic [105:0] res);
  logic [17:0] main_outputs_out;
  logic [17:0] zi0;
  Main_outputs  inst (arg0, main_outputs_out);
  assign zi0 = main_outputs_out;
  assign res = {zi0, 7'h30, arg0};
endmodule

module main_x2 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main_zzz_out;
  logic [80:0] main_setr1_out;
  logic [80:0] zi1;
  logic [105:0] main_zzz_outR1;
  logic [80:0] main_setr2_out;
  logic [80:0] zi2;
  logic [105:0] main_zzz_outR2;
  logic [80:0] main_setr3_out;
  logic [80:0] zi3;
  logic [105:0] main_zzz_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_setR0  instR1 (arg3, arg2, main_setr0_out);
  assign zi0 = main_setr0_out;
  main_zzz  instR2 (zi0, main_zzz_out);
  Main_setR1  instR3 (arg3, arg2, main_setr1_out);
  assign zi1 = main_setr1_out;
  main_zzz  instR4 (zi1, main_zzz_outR1);
  Main_setR2  instR5 (arg3, arg2, main_setr2_out);
  assign zi2 = main_setr2_out;
  main_zzz  instR6 (zi2, main_zzz_outR2);
  Main_setR3  instR7 (arg3, arg2, main_setr3_out);
  assign zi3 = main_setr3_out;
  main_zzz  instR8 (zi3, main_zzz_outR3);
  assign res = (za == 2'h0) ? main_zzz_out : ((za == 2'h1) ? main_zzz_outR1 : ((za == 2'h2) ? main_zzz_outR2 : main_zzz_outR3));
endmodule

module main___unused24 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setcflag_out;
  logic [80:0] zi0;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi1;
  logic [105:0] main__unused17_out;
  Main_setCFlag  inst (arg1, 1'h0, main_setcflag_out);
  assign zi0 = main_setcflag_out;
  Main_setZFlag  instR1 (zi0, arg0 == 8'h0, main_setzflag_out);
  assign zi1 = main_setzflag_out;
  main___unused17  instR2 (zi1, main__unused17_out);
  assign res = main__unused17_out;
endmodule

module main_arm90 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main__unused24_out;
  Main_setR0  inst (arg1, arg0, main_setr0_out);
  assign zi0 = main_setr0_out;
  main___unused24  instR1 (arg0, zi0, main__unused24_out);
  assign res = main__unused24_out;
endmodule

module main_arm91 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [80:0] zi0;
  logic [105:0] main__unused24_out;
  Main_setR1  inst (arg1, arg0, main_setr1_out);
  assign zi0 = main_setr1_out;
  main___unused24  instR1 (arg0, zi0, main__unused24_out);
  assign res = main__unused24_out;
endmodule

module main_arm92 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [80:0] zi0;
  logic [105:0] main__unused24_out;
  Main_setR2  inst (arg1, arg0, main_setr2_out);
  assign zi0 = main_setr2_out;
  main___unused24  instR1 (arg0, zi0, main__unused24_out);
  assign res = main__unused24_out;
endmodule

module main_arm93 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [80:0] zi0;
  logic [105:0] main__unused24_out;
  Main_setR3  inst (arg1, arg0, main_setr3_out);
  assign zi0 = main_setr3_out;
  main___unused24  instR1 (arg0, zi0, main__unused24_out);
  assign res = main__unused24_out;
endmodule

module main_vS5 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] vd$;
  logic [105:0] main_arm90_out;
  logic [105:0] main_arm91_out;
  logic [105:0] main_arm92_out;
  logic [105:0] main_arm93_out;
  assign vd$ = arg1 | arg2;
  main_arm90  inst (vd$, arg3, main_arm90_out);
  main_arm91  instR1 (vd$, arg3, main_arm91_out);
  main_arm92  instR2 (vd$, arg3, main_arm92_out);
  main_arm93  instR3 (vd$, arg3, main_arm93_out);
  assign res = (arg0 == 2'h0) ? main_arm90_out : ((arg0 == 2'h1) ? main_arm91_out : ((arg0 == 2'h2) ? main_arm92_out : main_arm93_out));
endmodule

module main_vD5 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_vs5_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_vs5_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_vs5_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_vs5_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_vS5  instR1 (arg0, arg2, main_r0_out, arg3, main_vs5_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_vS5  instR3 (arg0, arg2, main_r1_out, arg3, main_vs5_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_vS5  instR5 (arg0, arg2, main_r2_out, arg3, main_vs5_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_vS5  instR7 (arg0, arg2, main_r3_out, arg3, main_vs5_outR3);
  assign res = (arg1 == 2'h0) ? main_vs5_out : ((arg1 == 2'h1) ? main_vs5_outR1 : ((arg1 == 2'h2) ? main_vs5_outR2 : main_vs5_outR3));
endmodule

module main_vS6 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] vd$;
  logic [105:0] main_arm90_out;
  logic [105:0] main_arm91_out;
  logic [105:0] main_arm92_out;
  logic [105:0] main_arm93_out;
  assign vd$ = arg1 & arg2;
  main_arm90  inst (vd$, arg3, main_arm90_out);
  main_arm91  instR1 (vd$, arg3, main_arm91_out);
  main_arm92  instR2 (vd$, arg3, main_arm92_out);
  main_arm93  instR3 (vd$, arg3, main_arm93_out);
  assign res = (arg0 == 2'h0) ? main_arm90_out : ((arg0 == 2'h1) ? main_arm91_out : ((arg0 == 2'h2) ? main_arm92_out : main_arm93_out));
endmodule

module main_vD6 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_vs6_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_vs6_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_vs6_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_vs6_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_vS6  instR1 (arg0, arg2, main_r0_out, arg3, main_vs6_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_vS6  instR3 (arg0, arg2, main_r1_out, arg3, main_vs6_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_vS6  instR5 (arg0, arg2, main_r2_out, arg3, main_vs6_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_vS6  instR7 (arg0, arg2, main_r3_out, arg3, main_vs6_outR3);
  assign res = (arg1 == 2'h0) ? main_vs6_out : ((arg1 == 2'h1) ? main_vs6_outR1 : ((arg1 == 2'h2) ? main_vs6_outR2 : main_vs6_outR3));
endmodule

module main_vS7 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] vd$;
  logic [105:0] main_arm90_out;
  logic [105:0] main_arm91_out;
  logic [105:0] main_arm92_out;
  logic [105:0] main_arm93_out;
  assign vd$ = arg1 ^ arg2;
  main_arm90  inst (vd$, arg3, main_arm90_out);
  main_arm91  instR1 (vd$, arg3, main_arm91_out);
  main_arm92  instR2 (vd$, arg3, main_arm92_out);
  main_arm93  instR3 (vd$, arg3, main_arm93_out);
  assign res = (arg0 == 2'h0) ? main_arm90_out : ((arg0 == 2'h1) ? main_arm91_out : ((arg0 == 2'h2) ? main_arm92_out : main_arm93_out));
endmodule

module main_vD7 (input logic [1:0] arg0,
  input logic [1:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [7:0] main_r0_out;
  logic [105:0] main_vs7_out;
  logic [7:0] main_r1_out;
  logic [105:0] main_vs7_outR1;
  logic [7:0] main_r2_out;
  logic [105:0] main_vs7_outR2;
  logic [7:0] main_r3_out;
  logic [105:0] main_vs7_outR3;
  Main_r0  inst (arg3, main_r0_out);
  main_vS7  instR1 (arg0, arg2, main_r0_out, arg3, main_vs7_out);
  Main_r1  instR2 (arg3, main_r1_out);
  main_vS7  instR3 (arg0, arg2, main_r1_out, arg3, main_vs7_outR1);
  Main_r2  instR4 (arg3, main_r2_out);
  main_vS7  instR5 (arg0, arg2, main_r2_out, arg3, main_vs7_outR2);
  Main_r3  instR6 (arg3, main_r3_out);
  main_vS7  instR7 (arg0, arg2, main_r3_out, arg3, main_vs7_outR3);
  assign res = (arg1 == 2'h0) ? main_vs7_out : ((arg1 == 2'h1) ? main_vs7_outR1 : ((arg1 == 2'h2) ? main_vs7_outR2 : main_vs7_outR3));
endmodule

module main_vS8 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] main_minuscw8$sfalse_bool_out;
  logic [8:0] p;
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi2;
  logic [7:0] zi3;
  logic [7:0] zi4;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi5;
  logic [105:0] main_zzz_out;
  Main_minusCW8$sFalse__Bool  inst (arg0, arg1, main_minuscw8$sfalse_bool_out);
  assign p = main_minuscw8$sfalse_bool_out;
  assign zi0 = p[8];
  assign zi1 = zi0;
  Main_setCFlag  instR1 (arg2, zi1, main_setcflag_out);
  assign zi2 = main_setcflag_out;
  assign zi3 = p[7:0];
  assign zi4 = zi3;
  Main_setZFlag  instR2 (zi2, zi4 == 8'h0, main_setzflag_out);
  assign zi5 = main_setzflag_out;
  main_zzz  instR3 (zi5, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_vD8 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [7:0] arg2,
  input logic [80:0] arg3,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [7:0] main_r0_out;
  logic [7:0] zi0;
  logic [105:0] main_vs8_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] main_vs8_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi2;
  logic [105:0] main_vs8_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi3;
  logic [105:0] main_vs8_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_r0  instR1 (arg3, main_r0_out);
  assign zi0 = main_r0_out;
  main_vS8  instR2 (arg2, zi0, arg3, main_vs8_out);
  Main_r1  instR3 (arg3, main_r1_out);
  assign zi1 = main_r1_out;
  main_vS8  instR4 (arg2, zi1, arg3, main_vs8_outR1);
  Main_r2  instR5 (arg3, main_r2_out);
  assign zi2 = main_r2_out;
  main_vS8  instR6 (arg2, zi2, arg3, main_vs8_outR2);
  Main_r3  instR7 (arg3, main_r3_out);
  assign zi3 = main_r3_out;
  main_vS8  instR8 (arg2, zi3, arg3, main_vs8_outR3);
  assign res = (za == 2'h0) ? main_vs8_out : ((za == 2'h1) ? main_vs8_outR1 : ((za == 2'h2) ? main_vs8_outR2 : main_vs8_outR3));
endmodule

module main_pc3 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setpc_out;
  logic [80:0] zi0;
  logic [105:0] main_zzz_out;
  Main_setPC  inst (arg1, arg0, main_setpc_out);
  assign zi0 = main_setpc_out;
  main_zzz  instR1 (zi0, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_arm142 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [1:0] main_mkreg_out;
  logic [1:0] za;
  logic [7:0] main_r0_out;
  logic [7:0] zi0;
  logic [105:0] main_pc3_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi1;
  logic [105:0] main_pc3_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi2;
  logic [105:0] main_pc3_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi3;
  logic [105:0] main_pc3_outR3;
  Main_mkReg  inst (arg0, arg1, main_mkreg_out);
  assign za = main_mkreg_out;
  Main_r0  instR1 (arg2, main_r0_out);
  assign zi0 = main_r0_out;
  main_pc3  instR2 (zi0, arg2, main_pc3_out);
  Main_r1  instR3 (arg2, main_r1_out);
  assign zi1 = main_r1_out;
  main_pc3  instR4 (zi1, arg2, main_pc3_outR1);
  Main_r2  instR5 (arg2, main_r2_out);
  assign zi2 = main_r2_out;
  main_pc3  instR6 (zi2, arg2, main_pc3_outR2);
  Main_r3  instR7 (arg2, main_r3_out);
  assign zi3 = main_r3_out;
  main_pc3  instR8 (zi3, arg2, main_pc3_outR3);
  assign res = (za == 2'h0) ? main_pc3_out : ((za == 2'h1) ? main_pc3_outR1 : ((za == 2'h2) ? main_pc3_outR2 : main_pc3_outR3));
endmodule

module main_x3 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setpc_out;
  logic [80:0] zi0;
  logic [105:0] main_zzz_out;
  Main_setPC  inst (arg1, arg0, main_setpc_out);
  assign zi0 = main_setpc_out;
  main_zzz  instR1 (zi0, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_arm170 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main_zzz_out;
  Main_setR0  inst (arg1, arg0, main_setr0_out);
  assign zi0 = main_setr0_out;
  main_zzz  instR1 (zi0, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_arm171 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [80:0] zi0;
  logic [105:0] main_zzz_out;
  Main_setR1  inst (arg1, arg0, main_setr1_out);
  assign zi0 = main_setr1_out;
  main_zzz  instR1 (zi0, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_arm172 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [80:0] zi0;
  logic [105:0] main_zzz_out;
  Main_setR2  inst (arg1, arg0, main_setr2_out);
  assign zi0 = main_setr2_out;
  main_zzz  instR1 (zi0, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_arm173 (input logic [7:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [80:0] zi0;
  logic [105:0] main_zzz_out;
  Main_setR3  inst (arg1, arg0, main_setr3_out);
  assign zi0 = main_setr3_out;
  main_zzz  instR1 (zi0, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_v3 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] v1;
  logic [105:0] main_arm170_out;
  logic [105:0] main_arm171_out;
  logic [105:0] main_arm172_out;
  logic [105:0] main_arm173_out;
  assign v1 = ~arg1;
  main_arm170  inst (v1, arg2, main_arm170_out);
  main_arm171  instR1 (v1, arg2, main_arm171_out);
  main_arm172  instR2 (v1, arg2, main_arm172_out);
  main_arm173  instR3 (v1, arg2, main_arm173_out);
  assign res = (arg0 == 2'h0) ? main_arm170_out : ((arg0 == 2'h1) ? main_arm171_out : ((arg0 == 2'h2) ? main_arm172_out : main_arm173_out));
endmodule

module main___unused44 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi2;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi3;
  logic [105:0] main_zzz_out;
  assign zi0 = arg0[8];
  assign zi1 = zi0;
  Main_setCFlag  inst (arg2, zi1, main_setcflag_out);
  assign zi2 = main_setcflag_out;
  Main_setZFlag  instR1 (zi2, arg1 == 8'h0, main_setzflag_out);
  assign zi3 = main_setzflag_out;
  main_zzz  instR2 (zi3, main_zzz_out);
  assign res = main_zzz_out;
endmodule

module main_arm184 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr0_out;
  logic [80:0] zi0;
  logic [105:0] main__unused44_out;
  Main_setR0  inst (arg2, arg1, main_setr0_out);
  assign zi0 = main_setr0_out;
  main___unused44  instR1 (arg0, arg1, zi0, main__unused44_out);
  assign res = main__unused44_out;
endmodule

module main_arm185 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr1_out;
  logic [80:0] zi0;
  logic [105:0] main__unused44_out;
  Main_setR1  inst (arg2, arg1, main_setr1_out);
  assign zi0 = main_setr1_out;
  main___unused44  instR1 (arg0, arg1, zi0, main__unused44_out);
  assign res = main__unused44_out;
endmodule

module main_arm186 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr2_out;
  logic [80:0] zi0;
  logic [105:0] main__unused44_out;
  Main_setR2  inst (arg2, arg1, main_setr2_out);
  assign zi0 = main_setr2_out;
  main___unused44  instR1 (arg0, arg1, zi0, main__unused44_out);
  assign res = main__unused44_out;
endmodule

module main_arm187 (input logic [8:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [80:0] main_setr3_out;
  logic [80:0] zi0;
  logic [105:0] main__unused44_out;
  Main_setR3  inst (arg2, arg1, main_setr3_out);
  assign zi0 = main_setr3_out;
  main___unused44  instR1 (arg0, arg1, zi0, main__unused44_out);
  assign res = main__unused44_out;
endmodule

module main_v4 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] main_pluscw8$smain_onew8_false_bool_out;
  logic [8:0] p;
  logic [7:0] y;
  logic [7:0] v$;
  logic [105:0] main_arm184_out;
  logic [105:0] main_arm185_out;
  logic [105:0] main_arm186_out;
  logic [105:0] main_arm187_out;
  Main_plusCW8$sMain__oneW8__False__Bool  inst (arg1, main_pluscw8$smain_onew8_false_bool_out);
  assign p = main_pluscw8$smain_onew8_false_bool_out;
  assign y = p[7:0];
  assign v$ = y;
  main_arm184  instR1 (p, v$, arg2, main_arm184_out);
  main_arm185  instR2 (p, v$, arg2, main_arm185_out);
  main_arm186  instR3 (p, v$, arg2, main_arm186_out);
  main_arm187  instR4 (p, v$, arg2, main_arm187_out);
  assign res = (arg0 == 2'h0) ? main_arm184_out : ((arg0 == 2'h1) ? main_arm185_out : ((arg0 == 2'h2) ? main_arm186_out : main_arm187_out));
endmodule

module main_v5 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [8:0] zi0;
  logic [8:0] p;
  logic [7:0] y;
  logic [7:0] v$;
  logic [105:0] main_arm184_out;
  logic [105:0] main_arm185_out;
  logic [105:0] main_arm186_out;
  logic [105:0] main_arm187_out;
  assign zi0 = ({1'h0, arg1} - 9'h1) - 9'h0;
  assign p = {zi0[8], zi0[7:0]};
  assign y = p[7:0];
  assign v$ = y;
  main_arm184  inst (p, v$, arg2, main_arm184_out);
  main_arm185  instR1 (p, v$, arg2, main_arm185_out);
  main_arm186  instR2 (p, v$, arg2, main_arm186_out);
  main_arm187  instR3 (p, v$, arg2, main_arm187_out);
  assign res = (arg0 == 2'h0) ? main_arm184_out : ((arg0 == 2'h1) ? main_arm185_out : ((arg0 == 2'h2) ? main_arm186_out : main_arm187_out));
endmodule

module main_v6 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] v1;
  logic [105:0] main_arm170_out;
  logic [105:0] main_arm171_out;
  logic [105:0] main_arm172_out;
  logic [105:0] main_arm173_out;
  assign v1 = (arg1 << 8'h1) | (arg1 >> 8'h7);
  main_arm170  inst (v1, arg2, main_arm170_out);
  main_arm171  instR1 (v1, arg2, main_arm171_out);
  main_arm172  instR2 (v1, arg2, main_arm172_out);
  main_arm173  instR3 (v1, arg2, main_arm173_out);
  assign res = (arg0 == 2'h0) ? main_arm170_out : ((arg0 == 2'h1) ? main_arm171_out : ((arg0 == 2'h2) ? main_arm172_out : main_arm173_out));
endmodule

module main_v7 (input logic [1:0] arg0,
  input logic [7:0] arg1,
  input logic [80:0] arg2,
  output logic [105:0] res);
  logic [7:0] v1;
  logic [105:0] main_arm170_out;
  logic [105:0] main_arm171_out;
  logic [105:0] main_arm172_out;
  logic [105:0] main_arm173_out;
  assign v1 = (arg1 >> 8'h1) | (arg1 << 8'h7);
  main_arm170  inst (v1, arg2, main_arm170_out);
  main_arm171  instR1 (v1, arg2, main_arm171_out);
  main_arm172  instR2 (v1, arg2, main_arm172_out);
  main_arm173  instR3 (v1, arg2, main_arm173_out);
  assign res = (arg0 == 2'h0) ? main_arm170_out : ((arg0 == 2'h1) ? main_arm171_out : ((arg0 == 2'h2) ? main_arm172_out : main_arm173_out));
endmodule

module main_v8 (input logic [0:0] arg0,
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
  logic [105:0] main_arm184_out;
  logic [105:0] main_arm185_out;
  logic [105:0] main_arm186_out;
  logic [105:0] main_arm187_out;
  Main_msbW8  inst (arg3, main_msbw8_out);
  assign za = main_msbw8_out;
  Main_msbW8  instR1 (arg3, main_msbw8_outR1);
  assign za1 = {main_msbw8_outR1, (arg3 << 8'h1) | {7'h0, za}};
  Main_msbW8  instR2 (arg3, main_msbw8_outR2);
  assign zaR1 = {main_msbw8_outR2, (arg3 << 8'h1) | 8'h0};
  assign zaR2 = (~arg0) ? za1 : zaR1;
  Main_lsbW8  instR3 (arg3, main_lsbw8_out);
  assign zaR3 = main_lsbw8_out;
  Main_lsbW8  instR4 (arg3, main_lsbw8_outR1);
  assign za1R1 = {main_lsbw8_outR1, (arg3 >> 8'h1) | ({7'h0, zaR3} << 8'h7)};
  Main_lsbW8  instR5 (arg3, main_lsbw8_outR2);
  assign zaR4 = {main_lsbw8_outR2, (arg3 >> 8'h1) | 8'h0};
  assign zaR5 = (~arg0) ? za1R1 : zaR4;
  assign p = (~arg1) ? zaR2 : zaR5;
  assign y = p[7:0];
  assign v$ = y;
  main_arm184  instR6 (p, v$, arg4, main_arm184_out);
  main_arm185  instR7 (p, v$, arg4, main_arm185_out);
  main_arm186  instR8 (p, v$, arg4, main_arm186_out);
  main_arm187  instR9 (p, v$, arg4, main_arm187_out);
  assign res = (arg2 == 2'h0) ? main_arm184_out : ((arg2 == 2'h1) ? main_arm185_out : ((arg2 == 2'h2) ? main_arm186_out : main_arm187_out));
endmodule

module main_$fail (input logic [9:0] arg0,
  input logic [80:0] arg1,
  output logic [105:0] res);
  logic [7:0] main_datain_out;
  logic [7:0] i;
  logic [0:0] zds1;
  logic [0:0] zds2;
  logic [0:0] zds3;
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
  logic [105:0] main_a2_out;
  logic [7:0] main_r1_out;
  logic [7:0] zi8;
  logic [105:0] main_a2_outR1;
  logic [7:0] main_r2_out;
  logic [7:0] zi9;
  logic [105:0] main_a2_outR2;
  logic [7:0] main_r3_out;
  logic [7:0] zi10;
  logic [105:0] main_a2_outR3;
  logic [1:0] main_mkreg_outR2;
  logic [1:0] zi11;
  logic [7:0] main_r0_outR1;
  logic [7:0] zi12;
  logic [105:0] main_a3_out;
  logic [7:0] main_r1_outR1;
  logic [7:0] zi13;
  logic [105:0] main_a3_outR1;
  logic [7:0] main_r2_outR1;
  logic [7:0] zi14;
  logic [105:0] main_a3_outR2;
  logic [7:0] main_r3_outR1;
  logic [7:0] zi15;
  logic [105:0] main_a3_outR3;
  logic [1:0] main_mkreg_outR3;
  logic [1:0] zi16;
  logic [1:0] main_mkreg_outR4;
  logic [1:0] zi17;
  logic [7:0] main_r0_outR2;
  logic [105:0] main_vd_out;
  logic [7:0] main_r1_outR2;
  logic [105:0] main_vd_outR1;
  logic [7:0] main_r2_outR2;
  logic [105:0] main_vd_outR2;
  logic [7:0] main_r3_outR2;
  logic [105:0] main_vd_outR3;
  logic [1:0] main_mkreg_outR5;
  logic [1:0] zi18;
  logic [1:0] main_mkreg_outR6;
  logic [1:0] zi19;
  logic [7:0] main_r0_outR3;
  logic [105:0] main_vd2_out;
  logic [7:0] main_r1_outR3;
  logic [105:0] main_vd2_outR1;
  logic [7:0] main_r2_outR3;
  logic [105:0] main_vd2_outR2;
  logic [7:0] main_r3_outR3;
  logic [105:0] main_vd2_outR3;
  logic [1:0] main_mkreg_outR7;
  logic [1:0] zi20;
  logic [1:0] main_mkreg_outR8;
  logic [1:0] zi21;
  logic [7:0] main_r0_outR4;
  logic [105:0] main_vd3_out;
  logic [7:0] main_r1_outR4;
  logic [105:0] main_vd3_outR1;
  logic [7:0] main_r2_outR4;
  logic [105:0] main_vd3_outR2;
  logic [7:0] main_r3_outR4;
  logic [105:0] main_vd3_outR3;
  logic [1:0] main_mkreg_outR9;
  logic [1:0] zi22;
  logic [1:0] main_mkreg_outR10;
  logic [1:0] zi23;
  logic [7:0] main_r0_outR5;
  logic [105:0] main_vd4_out;
  logic [7:0] main_r1_outR5;
  logic [105:0] main_vd4_outR1;
  logic [7:0] main_r2_outR5;
  logic [105:0] main_vd4_outR2;
  logic [7:0] main_r3_outR5;
  logic [105:0] main_vd4_outR3;
  logic [1:0] main_mkreg_outR11;
  logic [1:0] zi24;
  logic [7:0] main_r0_outR6;
  logic [7:0] zi25;
  logic [105:0] main_x2_out;
  logic [7:0] main_r1_outR6;
  logic [7:0] zi26;
  logic [105:0] main_x2_outR1;
  logic [7:0] main_r2_outR6;
  logic [7:0] zi27;
  logic [105:0] main_x2_outR2;
  logic [7:0] main_r3_outR6;
  logic [7:0] zi28;
  logic [105:0] main_x2_outR3;
  logic [1:0] main_mkreg_outR12;
  logic [1:0] zi29;
  logic [1:0] main_mkreg_outR13;
  logic [1:0] zi30;
  logic [7:0] main_r0_outR7;
  logic [105:0] main_vd5_out;
  logic [7:0] main_r1_outR7;
  logic [105:0] main_vd5_outR1;
  logic [7:0] main_r2_outR7;
  logic [105:0] main_vd5_outR2;
  logic [7:0] main_r3_outR7;
  logic [105:0] main_vd5_outR3;
  logic [1:0] main_mkreg_outR14;
  logic [1:0] zi31;
  logic [1:0] main_mkreg_outR15;
  logic [1:0] zi32;
  logic [7:0] main_r0_outR8;
  logic [105:0] main_vd6_out;
  logic [7:0] main_r1_outR8;
  logic [105:0] main_vd6_outR1;
  logic [7:0] main_r2_outR8;
  logic [105:0] main_vd6_outR2;
  logic [7:0] main_r3_outR8;
  logic [105:0] main_vd6_outR3;
  logic [1:0] main_mkreg_outR16;
  logic [1:0] zi33;
  logic [1:0] main_mkreg_outR17;
  logic [1:0] zi34;
  logic [7:0] main_r0_outR9;
  logic [105:0] main_vd7_out;
  logic [7:0] main_r1_outR9;
  logic [105:0] main_vd7_outR1;
  logic [7:0] main_r2_outR9;
  logic [105:0] main_vd7_outR2;
  logic [7:0] main_r3_outR9;
  logic [105:0] main_vd7_outR3;
  logic [1:0] main_mkreg_outR18;
  logic [1:0] zi35;
  logic [7:0] main_r0_outR10;
  logic [7:0] zi36;
  logic [105:0] main_vd8_out;
  logic [7:0] main_r1_outR10;
  logic [7:0] zi37;
  logic [105:0] main_vd8_outR1;
  logic [7:0] main_r2_outR10;
  logic [7:0] zi38;
  logic [105:0] main_vd8_outR2;
  logic [7:0] main_r3_outR10;
  logic [7:0] zi39;
  logic [105:0] main_vd8_outR3;
  logic [0:0] main_zflag_out;
  logic [0:0] zi40;
  logic [105:0] main_zzz_out;
  logic [105:0] main_arm142_out;
  logic [0:0] main_zflag_outR1;
  logic [0:0] zi41;
  logic [0:0] main_notb_out;
  logic [0:0] zi42;
  logic [105:0] main_zzz_outR1;
  logic [105:0] main_arm142_outR1;
  logic [0:0] main_cflag_out;
  logic [0:0] zi43;
  logic [105:0] main_zzz_outR2;
  logic [105:0] main_arm142_outR2;
  logic [0:0] main_cflag_outR1;
  logic [0:0] zi44;
  logic [0:0] main_notb_outR1;
  logic [0:0] zi45;
  logic [105:0] main_zzz_outR3;
  logic [105:0] main_arm142_outR3;
  logic [1:0] main_mkreg_outR19;
  logic [1:0] zi46;
  logic [7:0] main_r0_outR11;
  logic [7:0] zi47;
  logic [105:0] main_x3_out;
  logic [7:0] main_r1_outR11;
  logic [7:0] zi48;
  logic [105:0] main_x3_outR1;
  logic [7:0] main_r2_outR11;
  logic [7:0] zi49;
  logic [105:0] main_x3_outR2;
  logic [7:0] main_r3_outR11;
  logic [7:0] zi50;
  logic [105:0] main_x3_outR3;
  logic [80:0] main_setieflag_out;
  logic [80:0] zi51;
  logic [105:0] main_zzz_outR4;
  logic [17:0] main_outputs_outR2;
  logic [17:0] zi52;
  logic [7:0] zi53;
  logic [7:0] zi54;
  logic [0:0] zi55;
  logic [17:0] zi56;
  logic [80:0] main_setoutputs_outR1;
  logic [80:0] zi57;
  logic [105:0] main_zzz_outR5;
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
  logic [105:0] main_zzz_outR6;
  logic [1:0] main_mkreg_outR20;
  logic [1:0] zi68;
  logic [7:0] main_r0_outR12;
  logic [7:0] zi69;
  logic [105:0] main_v3_out;
  logic [7:0] main_r1_outR12;
  logic [7:0] zi70;
  logic [105:0] main_v3_outR1;
  logic [7:0] main_r2_outR12;
  logic [7:0] zi71;
  logic [105:0] main_v3_outR2;
  logic [7:0] main_r3_outR12;
  logic [7:0] zi72;
  logic [105:0] main_v3_outR3;
  logic [1:0] main_mkreg_outR21;
  logic [1:0] zi73;
  logic [80:0] main_setr0_out;
  logic [80:0] zi74;
  logic [105:0] main_zzz_outR7;
  logic [80:0] main_setr1_out;
  logic [80:0] zi75;
  logic [105:0] main_zzz_outR8;
  logic [80:0] main_setr2_out;
  logic [80:0] zi76;
  logic [105:0] main_zzz_outR9;
  logic [80:0] main_setr3_out;
  logic [80:0] zi77;
  logic [105:0] main_zzz_outR10;
  logic [1:0] main_mkreg_outR22;
  logic [1:0] zi78;
  logic [7:0] main_r0_outR13;
  logic [7:0] zi79;
  logic [105:0] main_v4_out;
  logic [7:0] main_r1_outR13;
  logic [7:0] zi80;
  logic [105:0] main_v4_outR1;
  logic [7:0] main_r2_outR13;
  logic [7:0] zi81;
  logic [105:0] main_v4_outR2;
  logic [7:0] main_r3_outR13;
  logic [7:0] zi82;
  logic [105:0] main_v4_outR3;
  logic [1:0] main_mkreg_outR23;
  logic [1:0] zi83;
  logic [7:0] main_r0_outR14;
  logic [7:0] zi84;
  logic [105:0] main_v5_out;
  logic [7:0] main_r1_outR14;
  logic [7:0] zi85;
  logic [105:0] main_v5_outR1;
  logic [7:0] main_r2_outR14;
  logic [7:0] zi86;
  logic [105:0] main_v5_outR2;
  logic [7:0] main_r3_outR14;
  logic [7:0] zi87;
  logic [105:0] main_v5_outR3;
  logic [1:0] main_mkreg_outR24;
  logic [1:0] zi88;
  logic [7:0] main_r0_outR15;
  logic [7:0] zi89;
  logic [105:0] main_v6_out;
  logic [7:0] main_r1_outR15;
  logic [7:0] zi90;
  logic [105:0] main_v6_outR1;
  logic [7:0] main_r2_outR15;
  logic [7:0] zi91;
  logic [105:0] main_v6_outR2;
  logic [7:0] main_r3_outR15;
  logic [7:0] zi92;
  logic [105:0] main_v6_outR3;
  logic [7:0] main_r0_outR16;
  logic [7:0] zi93;
  logic [105:0] main_v7_out;
  logic [7:0] main_r1_outR16;
  logic [7:0] zi94;
  logic [105:0] main_v7_outR1;
  logic [7:0] main_r2_outR16;
  logic [7:0] zi95;
  logic [105:0] main_v7_outR2;
  logic [7:0] main_r3_outR16;
  logic [7:0] zi96;
  logic [105:0] main_v7_outR3;
  logic [1:0] main_mkreg_outR25;
  logic [1:0] zi97;
  logic [7:0] main_r0_outR17;
  logic [7:0] zi98;
  logic [105:0] main_v8_out;
  logic [7:0] main_r1_outR17;
  logic [7:0] zi99;
  logic [105:0] main_v8_outR1;
  logic [7:0] main_r2_outR17;
  logic [7:0] zi100;
  logic [105:0] main_v8_outR2;
  logic [7:0] main_r3_outR17;
  logic [7:0] zi101;
  logic [105:0] main_v8_outR3;
  Main_dataIn  inst (arg0, main_datain_out);
  assign i = main_datain_out;
  assign zds1 = i[6];
  assign zds2 = i[5];
  assign zds3 = i[4];
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
  main_a2  instR9 (ren, wen, zi7, arg1, main_a2_out);
  Main_r1  instR10 (arg1, main_r1_out);
  assign zi8 = main_r1_out;
  main_a2  instR11 (ren, wen, zi8, arg1, main_a2_outR1);
  Main_r2  instR12 (arg1, main_r2_out);
  assign zi9 = main_r2_out;
  main_a2  instR13 (ren, wen, zi9, arg1, main_a2_outR2);
  Main_r3  instR14 (arg1, main_r3_out);
  assign zi10 = main_r3_out;
  main_a2  instR15 (ren, wen, zi10, arg1, main_a2_outR3);
  Main_mkReg  instR16 (b0, b1, main_mkreg_outR2);
  assign zi11 = main_mkreg_outR2;
  Main_r0  instR17 (arg1, main_r0_outR1);
  assign zi12 = main_r0_outR1;
  main_a3  instR18 (ren, wen, zi12, arg1, main_a3_out);
  Main_r1  instR19 (arg1, main_r1_outR1);
  assign zi13 = main_r1_outR1;
  main_a3  instR20 (ren, wen, zi13, arg1, main_a3_outR1);
  Main_r2  instR21 (arg1, main_r2_outR1);
  assign zi14 = main_r2_outR1;
  main_a3  instR22 (ren, wen, zi14, arg1, main_a3_outR2);
  Main_r3  instR23 (arg1, main_r3_outR1);
  assign zi15 = main_r3_outR1;
  main_a3  instR24 (ren, wen, zi15, arg1, main_a3_outR3);
  Main_mkReg  instR25 (ren, wen, main_mkreg_outR3);
  assign zi16 = main_mkreg_outR3;
  Main_mkReg  instR26 (b0, b1, main_mkreg_outR4);
  assign zi17 = main_mkreg_outR4;
  Main_r0  instR27 (arg1, main_r0_outR2);
  main_vD  instR28 (zi16, zi17, main_r0_outR2, arg1, main_vd_out);
  Main_r1  instR29 (arg1, main_r1_outR2);
  main_vD  instR30 (zi16, zi17, main_r1_outR2, arg1, main_vd_outR1);
  Main_r2  instR31 (arg1, main_r2_outR2);
  main_vD  instR32 (zi16, zi17, main_r2_outR2, arg1, main_vd_outR2);
  Main_r3  instR33 (arg1, main_r3_outR2);
  main_vD  instR34 (zi16, zi17, main_r3_outR2, arg1, main_vd_outR3);
  Main_mkReg  instR35 (ren, wen, main_mkreg_outR5);
  assign zi18 = main_mkreg_outR5;
  Main_mkReg  instR36 (b0, b1, main_mkreg_outR6);
  assign zi19 = main_mkreg_outR6;
  Main_r0  instR37 (arg1, main_r0_outR3);
  main_vD2  instR38 (zi18, zi19, main_r0_outR3, arg1, main_vd2_out);
  Main_r1  instR39 (arg1, main_r1_outR3);
  main_vD2  instR40 (zi18, zi19, main_r1_outR3, arg1, main_vd2_outR1);
  Main_r2  instR41 (arg1, main_r2_outR3);
  main_vD2  instR42 (zi18, zi19, main_r2_outR3, arg1, main_vd2_outR2);
  Main_r3  instR43 (arg1, main_r3_outR3);
  main_vD2  instR44 (zi18, zi19, main_r3_outR3, arg1, main_vd2_outR3);
  Main_mkReg  instR45 (ren, wen, main_mkreg_outR7);
  assign zi20 = main_mkreg_outR7;
  Main_mkReg  instR46 (b0, b1, main_mkreg_outR8);
  assign zi21 = main_mkreg_outR8;
  Main_r0  instR47 (arg1, main_r0_outR4);
  main_vD3  instR48 (zi20, zi21, main_r0_outR4, arg1, main_vd3_out);
  Main_r1  instR49 (arg1, main_r1_outR4);
  main_vD3  instR50 (zi20, zi21, main_r1_outR4, arg1, main_vd3_outR1);
  Main_r2  instR51 (arg1, main_r2_outR4);
  main_vD3  instR52 (zi20, zi21, main_r2_outR4, arg1, main_vd3_outR2);
  Main_r3  instR53 (arg1, main_r3_outR4);
  main_vD3  instR54 (zi20, zi21, main_r3_outR4, arg1, main_vd3_outR3);
  Main_mkReg  instR55 (ren, wen, main_mkreg_outR9);
  assign zi22 = main_mkreg_outR9;
  Main_mkReg  instR56 (b0, b1, main_mkreg_outR10);
  assign zi23 = main_mkreg_outR10;
  Main_r0  instR57 (arg1, main_r0_outR5);
  main_vD4  instR58 (zi22, zi23, main_r0_outR5, arg1, main_vd4_out);
  Main_r1  instR59 (arg1, main_r1_outR5);
  main_vD4  instR60 (zi22, zi23, main_r1_outR5, arg1, main_vd4_outR1);
  Main_r2  instR61 (arg1, main_r2_outR5);
  main_vD4  instR62 (zi22, zi23, main_r2_outR5, arg1, main_vd4_outR2);
  Main_r3  instR63 (arg1, main_r3_outR5);
  main_vD4  instR64 (zi22, zi23, main_r3_outR5, arg1, main_vd4_outR3);
  Main_mkReg  instR65 (b0, b1, main_mkreg_outR11);
  assign zi24 = main_mkreg_outR11;
  Main_r0  instR66 (arg1, main_r0_outR6);
  assign zi25 = main_r0_outR6;
  main_x2  instR67 (ren, wen, zi25, arg1, main_x2_out);
  Main_r1  instR68 (arg1, main_r1_outR6);
  assign zi26 = main_r1_outR6;
  main_x2  instR69 (ren, wen, zi26, arg1, main_x2_outR1);
  Main_r2  instR70 (arg1, main_r2_outR6);
  assign zi27 = main_r2_outR6;
  main_x2  instR71 (ren, wen, zi27, arg1, main_x2_outR2);
  Main_r3  instR72 (arg1, main_r3_outR6);
  assign zi28 = main_r3_outR6;
  main_x2  instR73 (ren, wen, zi28, arg1, main_x2_outR3);
  Main_mkReg  instR74 (ren, wen, main_mkreg_outR12);
  assign zi29 = main_mkreg_outR12;
  Main_mkReg  instR75 (b0, b1, main_mkreg_outR13);
  assign zi30 = main_mkreg_outR13;
  Main_r0  instR76 (arg1, main_r0_outR7);
  main_vD5  instR77 (zi29, zi30, main_r0_outR7, arg1, main_vd5_out);
  Main_r1  instR78 (arg1, main_r1_outR7);
  main_vD5  instR79 (zi29, zi30, main_r1_outR7, arg1, main_vd5_outR1);
  Main_r2  instR80 (arg1, main_r2_outR7);
  main_vD5  instR81 (zi29, zi30, main_r2_outR7, arg1, main_vd5_outR2);
  Main_r3  instR82 (arg1, main_r3_outR7);
  main_vD5  instR83 (zi29, zi30, main_r3_outR7, arg1, main_vd5_outR3);
  Main_mkReg  instR84 (ren, wen, main_mkreg_outR14);
  assign zi31 = main_mkreg_outR14;
  Main_mkReg  instR85 (b0, b1, main_mkreg_outR15);
  assign zi32 = main_mkreg_outR15;
  Main_r0  instR86 (arg1, main_r0_outR8);
  main_vD6  instR87 (zi31, zi32, main_r0_outR8, arg1, main_vd6_out);
  Main_r1  instR88 (arg1, main_r1_outR8);
  main_vD6  instR89 (zi31, zi32, main_r1_outR8, arg1, main_vd6_outR1);
  Main_r2  instR90 (arg1, main_r2_outR8);
  main_vD6  instR91 (zi31, zi32, main_r2_outR8, arg1, main_vd6_outR2);
  Main_r3  instR92 (arg1, main_r3_outR8);
  main_vD6  instR93 (zi31, zi32, main_r3_outR8, arg1, main_vd6_outR3);
  Main_mkReg  instR94 (ren, wen, main_mkreg_outR16);
  assign zi33 = main_mkreg_outR16;
  Main_mkReg  instR95 (b0, b1, main_mkreg_outR17);
  assign zi34 = main_mkreg_outR17;
  Main_r0  instR96 (arg1, main_r0_outR9);
  main_vD7  instR97 (zi33, zi34, main_r0_outR9, arg1, main_vd7_out);
  Main_r1  instR98 (arg1, main_r1_outR9);
  main_vD7  instR99 (zi33, zi34, main_r1_outR9, arg1, main_vd7_outR1);
  Main_r2  instR100 (arg1, main_r2_outR9);
  main_vD7  instR101 (zi33, zi34, main_r2_outR9, arg1, main_vd7_outR2);
  Main_r3  instR102 (arg1, main_r3_outR9);
  main_vD7  instR103 (zi33, zi34, main_r3_outR9, arg1, main_vd7_outR3);
  Main_mkReg  instR104 (ren, wen, main_mkreg_outR18);
  assign zi35 = main_mkreg_outR18;
  Main_r0  instR105 (arg1, main_r0_outR10);
  assign zi36 = main_r0_outR10;
  main_vD8  instR106 (b0, b1, zi36, arg1, main_vd8_out);
  Main_r1  instR107 (arg1, main_r1_outR10);
  assign zi37 = main_r1_outR10;
  main_vD8  instR108 (b0, b1, zi37, arg1, main_vd8_outR1);
  Main_r2  instR109 (arg1, main_r2_outR10);
  assign zi38 = main_r2_outR10;
  main_vD8  instR110 (b0, b1, zi38, arg1, main_vd8_outR2);
  Main_r3  instR111 (arg1, main_r3_outR10);
  assign zi39 = main_r3_outR10;
  main_vD8  instR112 (b0, b1, zi39, arg1, main_vd8_outR3);
  Main_zFlag  instR113 (arg1, main_zflag_out);
  assign zi40 = main_zflag_out;
  main_zzz  instR114 (arg1, main_zzz_out);
  main_arm142  instR115 (b0, b1, arg1, main_arm142_out);
  Main_zFlag  instR116 (arg1, main_zflag_outR1);
  assign zi41 = main_zflag_outR1;
  Main_notb  instR117 (zi41, main_notb_out);
  assign zi42 = main_notb_out;
  main_zzz  instR118 (arg1, main_zzz_outR1);
  main_arm142  instR119 (b0, b1, arg1, main_arm142_outR1);
  Main_cFlag  instR120 (arg1, main_cflag_out);
  assign zi43 = main_cflag_out;
  main_zzz  instR121 (arg1, main_zzz_outR2);
  main_arm142  instR122 (b0, b1, arg1, main_arm142_outR2);
  Main_cFlag  instR123 (arg1, main_cflag_outR1);
  assign zi44 = main_cflag_outR1;
  Main_notb  instR124 (zi44, main_notb_outR1);
  assign zi45 = main_notb_outR1;
  main_zzz  instR125 (arg1, main_zzz_outR3);
  main_arm142  instR126 (b0, b1, arg1, main_arm142_outR3);
  Main_mkReg  instR127 (b0, b1, main_mkreg_outR19);
  assign zi46 = main_mkreg_outR19;
  Main_r0  instR128 (arg1, main_r0_outR11);
  assign zi47 = main_r0_outR11;
  main_x3  instR129 (zi47, arg1, main_x3_out);
  Main_r1  instR130 (arg1, main_r1_outR11);
  assign zi48 = main_r1_outR11;
  main_x3  instR131 (zi48, arg1, main_x3_outR1);
  Main_r2  instR132 (arg1, main_r2_outR11);
  assign zi49 = main_r2_outR11;
  main_x3  instR133 (zi49, arg1, main_x3_outR2);
  Main_r3  instR134 (arg1, main_r3_outR11);
  assign zi50 = main_r3_outR11;
  main_x3  instR135 (zi50, arg1, main_x3_outR3);
  Main_setIEFlag  instR136 (arg1, b1, main_setieflag_out);
  assign zi51 = main_setieflag_out;
  main_zzz  instR137 (zi51, main_zzz_outR4);
  Main_outputs  instR138 (arg1, main_outputs_outR2);
  assign zi52 = main_outputs_outR2;
  assign zi53 = zi52[17:10];
  assign zi54 = zi52[9:2];
  assign zi55 = zi52[1];
  assign zi56 = {zi53, zi54, zi55, 1'h1};
  Main_setOutputs  instR139 (arg1, zi56, main_setoutputs_outR1);
  assign zi57 = main_setoutputs_outR1;
  main_zzz  instR140 (zi57, main_zzz_outR5);
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
  main_zzz  instR145 (zi67, main_zzz_outR6);
  Main_mkReg  instR146 (b0, b1, main_mkreg_outR20);
  assign zi68 = main_mkreg_outR20;
  Main_r0  instR147 (arg1, main_r0_outR12);
  assign zi69 = main_r0_outR12;
  main_v3  instR148 (zi68, zi69, arg1, main_v3_out);
  Main_r1  instR149 (arg1, main_r1_outR12);
  assign zi70 = main_r1_outR12;
  main_v3  instR150 (zi68, zi70, arg1, main_v3_outR1);
  Main_r2  instR151 (arg1, main_r2_outR12);
  assign zi71 = main_r2_outR12;
  main_v3  instR152 (zi68, zi71, arg1, main_v3_outR2);
  Main_r3  instR153 (arg1, main_r3_outR12);
  assign zi72 = main_r3_outR12;
  main_v3  instR154 (zi68, zi72, arg1, main_v3_outR3);
  Main_mkReg  instR155 (b0, b1, main_mkreg_outR21);
  assign zi73 = main_mkreg_outR21;
  Main_setR0  instR156 (arg1, 8'h0, main_setr0_out);
  assign zi74 = main_setr0_out;
  main_zzz  instR157 (zi74, main_zzz_outR7);
  Main_setR1  instR158 (arg1, 8'h0, main_setr1_out);
  assign zi75 = main_setr1_out;
  main_zzz  instR159 (zi75, main_zzz_outR8);
  Main_setR2  instR160 (arg1, 8'h0, main_setr2_out);
  assign zi76 = main_setr2_out;
  main_zzz  instR161 (zi76, main_zzz_outR9);
  Main_setR3  instR162 (arg1, 8'h0, main_setr3_out);
  assign zi77 = main_setr3_out;
  main_zzz  instR163 (zi77, main_zzz_outR10);
  Main_mkReg  instR164 (b0, b1, main_mkreg_outR22);
  assign zi78 = main_mkreg_outR22;
  Main_r0  instR165 (arg1, main_r0_outR13);
  assign zi79 = main_r0_outR13;
  main_v4  instR166 (zi78, zi79, arg1, main_v4_out);
  Main_r1  instR167 (arg1, main_r1_outR13);
  assign zi80 = main_r1_outR13;
  main_v4  instR168 (zi78, zi80, arg1, main_v4_outR1);
  Main_r2  instR169 (arg1, main_r2_outR13);
  assign zi81 = main_r2_outR13;
  main_v4  instR170 (zi78, zi81, arg1, main_v4_outR2);
  Main_r3  instR171 (arg1, main_r3_outR13);
  assign zi82 = main_r3_outR13;
  main_v4  instR172 (zi78, zi82, arg1, main_v4_outR3);
  Main_mkReg  instR173 (b0, b1, main_mkreg_outR23);
  assign zi83 = main_mkreg_outR23;
  Main_r0  instR174 (arg1, main_r0_outR14);
  assign zi84 = main_r0_outR14;
  main_v5  instR175 (zi83, zi84, arg1, main_v5_out);
  Main_r1  instR176 (arg1, main_r1_outR14);
  assign zi85 = main_r1_outR14;
  main_v5  instR177 (zi83, zi85, arg1, main_v5_outR1);
  Main_r2  instR178 (arg1, main_r2_outR14);
  assign zi86 = main_r2_outR14;
  main_v5  instR179 (zi83, zi86, arg1, main_v5_outR2);
  Main_r3  instR180 (arg1, main_r3_outR14);
  assign zi87 = main_r3_outR14;
  main_v5  instR181 (zi83, zi87, arg1, main_v5_outR3);
  Main_mkReg  instR182 (b0, b1, main_mkreg_outR24);
  assign zi88 = main_mkreg_outR24;
  Main_r0  instR183 (arg1, main_r0_outR15);
  assign zi89 = main_r0_outR15;
  main_v6  instR184 (zi88, zi89, arg1, main_v6_out);
  Main_r1  instR185 (arg1, main_r1_outR15);
  assign zi90 = main_r1_outR15;
  main_v6  instR186 (zi88, zi90, arg1, main_v6_outR1);
  Main_r2  instR187 (arg1, main_r2_outR15);
  assign zi91 = main_r2_outR15;
  main_v6  instR188 (zi88, zi91, arg1, main_v6_outR2);
  Main_r3  instR189 (arg1, main_r3_outR15);
  assign zi92 = main_r3_outR15;
  main_v6  instR190 (zi88, zi92, arg1, main_v6_outR3);
  Main_r0  instR191 (arg1, main_r0_outR16);
  assign zi93 = main_r0_outR16;
  main_v7  instR192 (zi88, zi93, arg1, main_v7_out);
  Main_r1  instR193 (arg1, main_r1_outR16);
  assign zi94 = main_r1_outR16;
  main_v7  instR194 (zi88, zi94, arg1, main_v7_outR1);
  Main_r2  instR195 (arg1, main_r2_outR16);
  assign zi95 = main_r2_outR16;
  main_v7  instR196 (zi88, zi95, arg1, main_v7_outR2);
  Main_r3  instR197 (arg1, main_r3_outR16);
  assign zi96 = main_r3_outR16;
  main_v7  instR198 (zi88, zi96, arg1, main_v7_outR3);
  Main_mkReg  instR199 (b0, b1, main_mkreg_outR25);
  assign zi97 = main_mkreg_outR25;
  Main_r0  instR200 (arg1, main_r0_outR17);
  assign zi98 = main_r0_outR17;
  main_v8  instR201 (ren, wen, zi97, zi98, arg1, main_v8_out);
  Main_r1  instR202 (arg1, main_r1_outR17);
  assign zi99 = main_r1_outR17;
  main_v8  instR203 (ren, wen, zi97, zi99, arg1, main_v8_outR1);
  Main_r2  instR204 (arg1, main_r2_outR17);
  assign zi100 = main_r2_outR17;
  main_v8  instR205 (ren, wen, zi97, zi100, arg1, main_v8_outR2);
  Main_r3  instR206 (arg1, main_r3_outR17);
  assign zi101 = main_r3_outR17;
  main_v8  instR207 (ren, wen, zi97, zi101, arg1, main_v8_outR3);
  assign res = (~za) ? ((~zds1) ? ((~zds2) ? ((~zds3) ? {zi5, 3'h1, ren, wen, zi0, zi4} : ((zi6 == 2'h0) ? main_a2_out : ((zi6 == 2'h1) ? main_a2_outR1 : ((zi6 == 2'h2) ? main_a2_outR2 : main_a2_outR3)))) : ((~zds3) ? ((zi11 == 2'h0) ? main_a3_out : ((zi11 == 2'h1) ? main_a3_outR1 : ((zi11 == 2'h2) ? main_a3_outR2 : main_a3_outR3))) : ((zi16 == 2'h0) ? main_vd_out : ((zi16 == 2'h1) ? main_vd_outR1 : ((zi16 == 2'h2) ? main_vd_outR2 : main_vd_outR3))))) : ((~zds2) ? ((~zds3) ? ((zi18 == 2'h0) ? main_vd2_out : ((zi18 == 2'h1) ? main_vd2_outR1 : ((zi18 == 2'h2) ? main_vd2_outR2 : main_vd2_outR3))) : ((zi20 == 2'h0) ? main_vd3_out : ((zi20 == 2'h1) ? main_vd3_outR1 : ((zi20 == 2'h2) ? main_vd3_outR2 : main_vd3_outR3)))) : ((~zds3) ? ((zi22 == 2'h0) ? main_vd4_out : ((zi22 == 2'h1) ? main_vd4_outR1 : ((zi22 == 2'h2) ? main_vd4_outR2 : main_vd4_outR3))) : ((zi24 == 2'h0) ? main_x2_out : ((zi24 == 2'h1) ? main_x2_outR1 : ((zi24 == 2'h2) ? main_x2_outR2 : main_x2_outR3)))))) : ((~zds1) ? ((~zds2) ? ((~zds3) ? ((zi29 == 2'h0) ? main_vd5_out : ((zi29 == 2'h1) ? main_vd5_outR1 : ((zi29 == 2'h2) ? main_vd5_outR2 : main_vd5_outR3))) : ((zi31 == 2'h0) ? main_vd6_out : ((zi31 == 2'h1) ? main_vd6_outR1 : ((zi31 == 2'h2) ? main_vd6_outR2 : main_vd6_outR3)))) : ((~zds3) ? ((zi33 == 2'h0) ? main_vd7_out : ((zi33 == 2'h1) ? main_vd7_outR1 : ((zi33 == 2'h2) ? main_vd7_outR2 : main_vd7_outR3))) : ((zi35 == 2'h0) ? main_vd8_out : ((zi35 == 2'h1) ? main_vd8_outR1 : ((zi35 == 2'h2) ? main_vd8_outR2 : main_vd8_outR3))))) : ((~zds2) ? ((~zds3) ? ((~ren) ? ((~wen) ? ((~zi40) ? main_zzz_out : main_arm142_out) : ((~zi42) ? main_zzz_outR1 : main_arm142_outR1)) : ((~wen) ? ((~zi43) ? main_zzz_outR2 : main_arm142_outR2) : ((~zi45) ? main_zzz_outR3 : main_arm142_outR3))) : ((~ren) ? ((~wen) ? ((zi46 == 2'h0) ? main_x3_out : ((zi46 == 2'h1) ? main_x3_outR1 : ((zi46 == 2'h2) ? main_x3_outR2 : main_x3_outR3))) : ((~b0) ? main_zzz_outR4 : ((~b1) ? main_zzz_outR5 : main_zzz_outR6))) : ((~wen) ? ((zi68 == 2'h0) ? main_v3_out : ((zi68 == 2'h1) ? main_v3_outR1 : ((zi68 == 2'h2) ? main_v3_outR2 : main_v3_outR3))) : ((zi73 == 2'h0) ? main_zzz_outR7 : ((zi73 == 2'h1) ? main_zzz_outR8 : ((zi73 == 2'h2) ? main_zzz_outR9 : main_zzz_outR10)))))) : ((~zds3) ? ((~ren) ? ((~wen) ? ((zi78 == 2'h0) ? main_v4_out : ((zi78 == 2'h1) ? main_v4_outR1 : ((zi78 == 2'h2) ? main_v4_outR2 : main_v4_outR3))) : ((zi83 == 2'h0) ? main_v5_out : ((zi83 == 2'h1) ? main_v5_outR1 : ((zi83 == 2'h2) ? main_v5_outR2 : main_v5_outR3)))) : ((~wen) ? ((zi88 == 2'h0) ? main_v6_out : ((zi88 == 2'h1) ? main_v6_outR1 : ((zi88 == 2'h2) ? main_v6_outR2 : main_v6_outR3))) : ((zi88 == 2'h0) ? main_v7_out : ((zi88 == 2'h1) ? main_v7_outR1 : ((zi88 == 2'h2) ? main_v7_outR2 : main_v7_outR3))))) : ((zi97 == 2'h0) ? main_v8_out : ((zi97 == 2'h1) ? main_v8_outR1 : ((zi97 == 2'h2) ? main_v8_outR2 : main_v8_outR3))))));
endmodule

module main_loop (input logic [80:0] arg0,
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
  logic [105:0] main_zzz_out;
  logic [105:0] main_$fail_out;
  logic [105:0] main_$fail_outR1;
  logic [80:0] main_setcflag_out;
  logic [80:0] zi50;
  logic [80:0] main_setzflag_out;
  logic [80:0] zi51;
  logic [80:0] main_setoutputs_out;
  logic [80:0] zi52;
  logic [105:0] main_zzz_outR1;
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
  main_zzz  instR5 (zi49, main_zzz_out);
  main_$fail  instR6 (zi0, arg0, main_$fail_out);
  main_$fail  instR7 (zi0, arg0, main_$fail_outR1);
  Main_setCFlag  instR8 (arg0, 1'h0, main_setcflag_out);
  assign zi50 = main_setcflag_out;
  Main_setZFlag  instR9 (zi50, 1'h0, main_setzflag_out);
  assign zi51 = main_setzflag_out;
  Main_setOutputs  instR10 (zi51, 18'h0, main_setoutputs_out);
  assign zi52 = main_setoutputs_out;
  main_zzz  instR11 (zi52, main_zzz_outR1);
  assign res = (~zi2) ? (zi4 ? (zi6 ? main_zzz_out : main_$fail_out) : main_$fail_outR1) : main_zzz_outR1;
endmodule

module Main_notb (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = ~arg0;
endmodule

module Main_mkReg (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  assign res = (~arg0) ? ((~arg1) ? 2'h0 : 2'h1) : ((~arg1) ? 2'h2 : 2'h3);
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

module Main_plusCW8$sMain__oneW8__False__Bool (input logic [7:0] arg0,
  output logic [8:0] res);
  logic [8:0] s;
  assign s = ({1'h0, arg0} + 9'h1) + 9'h0;
  assign res = {s[8], s[7:0]};
endmodule

module Main_minusCW8$sFalse__Bool (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [8:0] res);
  logic [8:0] s;
  assign s = ({1'h0, arg0} - {1'h0, arg1}) - 9'h0;
  assign res = {s[8], s[7:0]};
endmodule