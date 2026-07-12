module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  input logic [0:0] __in1,
  input logic [0:0] __in2,
  output logic [7:0] __out0,
  output logic [7:0] __out1,
  output logic [0:0] __out2,
  output logic [0:0] __out3);
  logic [80:0] Main_setInputs_out;
  logic [105:0] main_loop_out;
  logic [9:0] Main_inputs_out;
  logic [7:0] Main_dataIn_out;
  logic [80:0] Main_setR0_out;
  logic [105:0] main_loop_outR1;
  logic [80:0] Main_setR1_out;
  logic [105:0] main_loop_outR2;
  logic [80:0] Main_setR2_out;
  logic [105:0] main_loop_outR3;
  logic [80:0] Main_setR3_out;
  logic [105:0] main_loop_outR4;
  logic [17:0] Main_outputs_out;
  logic [17:0] Main_setAddrOut_out;
  logic [80:0] Main_setOutputs_out;
  logic [17:0] Main_outputs_outR1;
  logic [17:0] Main_setWeOut_out;
  logic [80:0] Main_setOutputs_outR1;
  logic [105:0] main__unused7_out;
  logic [7:0] Main_r0_out;
  logic [105:0] main_d_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_d_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_d_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_d_outR3;
  logic [1:0] Main_mkReg_out;
  logic [105:0] Zres;
  // state registers
  // __resumption_tag: 7 bits, init 0x30
  //   states: 0=i2 1=i4 2=i6 3=i7 4=i8
  // __st0: 81 bits, init 0x0
  logic [6:0] __resumption_tag;
  logic [6:0] __resumption_tag_next;
  logic [80:0] __st0;
  logic [80:0] __st0_next;
  // combinational logic
  wire [9:0] i = {__in0, __in1, __in2};
  wire [0:0] rEn = __resumption_tag[2];
  wire [1:0] r = __resumption_tag[1:0];
  Main_setInputs  setInputs_i (__st0, i, Main_setInputs_out);
  main_loop  loop_i (Main_setInputs_out, main_loop_out);
  Main_inputs  inputs_i (Main_setInputs_out, Main_inputs_out);
  Main_dataIn  dataIn_i (Main_inputs_out, Main_dataIn_out);
  Main_setR0  setR0_i (Main_setInputs_out, Main_dataIn_out, Main_setR0_out);
  main_loop  loop_iR1 (Main_setR0_out, main_loop_outR1);
  Main_setR1  setR1_i (Main_setInputs_out, Main_dataIn_out, Main_setR1_out);
  main_loop  loop_iR2 (Main_setR1_out, main_loop_outR2);
  Main_setR2  setR2_i (Main_setInputs_out, Main_dataIn_out, Main_setR2_out);
  main_loop  loop_iR3 (Main_setR2_out, main_loop_outR3);
  Main_setR3  setR3_i (Main_setInputs_out, Main_dataIn_out, Main_setR3_out);
  main_loop  loop_iR4 (Main_setR3_out, main_loop_outR4);
  wire [0:0] rEnR1 = __resumption_tag[3];
  Main_outputs  outputs_i (Main_setInputs_out, Main_outputs_out);
  Main_setAddrOut  setAddrOut_i (Main_outputs_out, Main_dataIn_out, Main_setAddrOut_out);
  Main_setOutputs  setOutputs_i (Main_setInputs_out, Main_setAddrOut_out, Main_setOutputs_out);
  Main_outputs  outputs_iR1 (Main_setOutputs_out, Main_outputs_outR1);
  Main_setWeOut  setWeOut_i (Main_outputs_outR1, rEn, Main_setWeOut_out);
  Main_setOutputs  setOutputs_iR1 (Main_setOutputs_out, Main_setWeOut_out, Main_setOutputs_outR1);
  main___unused7  _unused7_i (rEnR1, r, Main_setOutputs_outR1, main__unused7_out);
  Main_r0  r0_i (Main_setOutputs_outR1, Main_r0_out);
  main_d  d_i (rEnR1, r, Main_r0_out, Main_setOutputs_outR1, main_d_out);
  Main_r1  r1_i (Main_setOutputs_outR1, Main_r1_out);
  main_d  d_iR1 (rEnR1, r, Main_r1_out, Main_setOutputs_outR1, main_d_outR1);
  Main_r2  r2_i (Main_setOutputs_outR1, Main_r2_out);
  main_d  d_iR2 (rEnR1, r, Main_r2_out, Main_setOutputs_outR1, main_d_outR2);
  Main_r3  r3_i (Main_setOutputs_outR1, Main_r3_out);
  main_d  d_iR3 (rEnR1, r, Main_r3_out, Main_setOutputs_outR1, main_d_outR3);
  wire [0:0] rEnR2 = __resumption_tag[1];
  wire [0:0] wEn = __resumption_tag[0];
  Main_mkReg  mkReg_i (rEnR2, wEn, Main_mkReg_out);
  wire [2:0] scrut = __resumption_tag[6:4];
  always_comb case (scrut)
    3'h0: Zres = (~rEn) ? main_loop_out :
      ((r == 2'h0) ? main_loop_outR1 :
        ((r == 2'h1) ? main_loop_outR2 :
          ((r == 2'h2) ? main_loop_outR3 : main_loop_outR4)));
    3'h1: Zres = (~rEn) ? main__unused7_out :
      ((r == 2'h0) ? main_d_out :
        ((r == 2'h1) ? main_d_outR1 :
          ((r == 2'h2) ? main_d_outR2 : main_d_outR3)));
    3'h2: Zres = (Main_mkReg_out == 2'h0) ? main_loop_outR1 :
      ((Main_mkReg_out == 2'h1) ? main_loop_outR2 :
        ((Main_mkReg_out == 2'h2) ? main_loop_outR3 : main_loop_outR4));
    3'h3: Zres = main_loop_out;
    default: Zres = main_loop_out;
  endcase
  assign __resumption_tag_next = Zres[87:81];
  assign __st0_next = Zres[80:0];
  // outputs
  assign __out0 = Zres[105:98];
  assign __out1 = Zres[97:90];
  assign __out2 = Zres[89];
  assign __out3 = Zres[88];
  // state register update
  initial __resumption_tag = 7'h30;
  initial __st0 = {7'h51{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 7'h30;
      __st0 <= {7'h51{1'h0}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule

// main._unused7
// block '$L._unused7' of process main
module main___unused7 (input logic [0:0] rEn,
  input logic [1:0] r,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_pc_out;
  logic [8:0] Main_plusCW8$sMain_oneW8_False_Bool_out;
  logic [80:0] Main_setPC_out;
  logic [17:0] Main_outputs_out;
  Main_pc  pc_i (s0, Main_pc_out);
  Main_plusCW8$sMain__oneW8__False__Bool  plusCW8$sMain_oneW8_False_Bool_i (Main_pc_out, Main_plusCW8$sMain_oneW8_False_Bool_out);
  wire [7:0] y = Main_plusCW8$sMain_oneW8_False_Bool_out[7:0];
  Main_setPC  setPC_i (s0, y, Main_setPC_out);
  Main_outputs  outputs_i (Main_setPC_out, Main_outputs_out);
  assign res = {Main_outputs_out, 4'h0, rEn, r, Main_setPC_out};
endmodule

// main.d
// block '$L.d' of process main
module main_d (input logic [0:0] rEn,
  input logic [1:0] r,
  input logic [7:0] d,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [17:0] Main_outputs_out;
  logic [17:0] Main_setDataOut_out;
  logic [80:0] Main_setOutputs_out;
  logic [105:0] main__unused7_out;
  Main_outputs  outputs_i (s0, Main_outputs_out);
  Main_setDataOut  setDataOut_i (Main_outputs_out, d, Main_setDataOut_out);
  Main_setOutputs  setOutputs_i (s0, Main_setDataOut_out, Main_setOutputs_out);
  main___unused7  _unused7_i (rEn, r, Main_setOutputs_out, main__unused7_out);
  assign res = main__unused7_out;
endmodule

// main.a2
// block '$L.a2' of process main
module main_a2 (input logic [0:0] rEn,
  input logic [0:0] wEn,
  input logic [7:0] a,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [17:0] Main_outputs_out;
  logic [17:0] Main_setWeOut_out;
  logic [80:0] Main_setOutputs_out;
  logic [17:0] Main_outputs_outR1;
  logic [17:0] Main_setAddrOut_out;
  logic [80:0] Main_setOutputs_outR1;
  logic [17:0] Main_outputs_outR2;
  Main_outputs  outputs_i (s0, Main_outputs_out);
  Main_setWeOut  setWeOut_i (Main_outputs_out, 1'h0, Main_setWeOut_out);
  Main_setOutputs  setOutputs_i (s0, Main_setWeOut_out, Main_setOutputs_out);
  Main_outputs  outputs_iR1 (Main_setOutputs_out, Main_outputs_outR1);
  Main_setAddrOut  setAddrOut_i (Main_outputs_outR1, a, Main_setAddrOut_out);
  Main_setOutputs  setOutputs_iR1 (Main_setOutputs_out, Main_setAddrOut_out, Main_setOutputs_outR1);
  Main_outputs  outputs_iR2 (Main_setOutputs_outR1, Main_outputs_outR2);
  assign res = {Main_outputs_outR2, 5'h8, rEn, wEn, Main_setOutputs_outR1};
endmodule

// main.v2
// block '$L.v2' of process main
module main_v2 (input logic [7:0] a,
  input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [17:0] Main_outputs_out;
  logic [17:0] Main_setWeOut_out;
  logic [80:0] Main_setOutputs_out;
  logic [17:0] Main_outputs_outR1;
  logic [17:0] Main_setDataOut_out;
  logic [80:0] Main_setOutputs_outR1;
  logic [17:0] Main_outputs_outR2;
  logic [17:0] Main_setAddrOut_out;
  logic [80:0] Main_setOutputs_outR2;
  logic [105:0] main_zzz_out;
  Main_outputs  outputs_i (s0, Main_outputs_out);
  Main_setWeOut  setWeOut_i (Main_outputs_out, 1'h1, Main_setWeOut_out);
  Main_setOutputs  setOutputs_i (s0, Main_setWeOut_out, Main_setOutputs_out);
  Main_outputs  outputs_iR1 (Main_setOutputs_out, Main_outputs_outR1);
  Main_setDataOut  setDataOut_i (Main_outputs_outR1, v, Main_setDataOut_out);
  Main_setOutputs  setOutputs_iR1 (Main_setOutputs_out, Main_setDataOut_out, Main_setOutputs_outR1);
  Main_outputs  outputs_iR2 (Main_setOutputs_outR1, Main_outputs_outR2);
  Main_setAddrOut  setAddrOut_i (Main_outputs_outR2, a, Main_setAddrOut_out);
  Main_setOutputs  setOutputs_iR2 (Main_setOutputs_outR1, Main_setAddrOut_out, Main_setOutputs_outR2);
  main_zzz  zzz_i (Main_setOutputs_outR2, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.a3
// block '$L.a3' of process main
module main_a3 (input logic [0:0] rEn,
  input logic [0:0] wEn,
  input logic [7:0] a,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [1:0] Main_mkReg_out;
  logic [7:0] Main_r0_out;
  logic [105:0] main_v2_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_v2_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_v2_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_v2_outR3;
  Main_mkReg  mkReg_i (rEn, wEn, Main_mkReg_out);
  Main_r0  r0_i (s0, Main_r0_out);
  main_v2  v2_i (a, Main_r0_out, s0, main_v2_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_v2  v2_iR1 (a, Main_r1_out, s0, main_v2_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_v2  v2_iR2 (a, Main_r2_out, s0, main_v2_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_v2  v2_iR3 (a, Main_r3_out, s0, main_v2_outR3);
  always_comb case (Main_mkReg_out)
    2'h0: res = main_v2_out;
    2'h1: res = main_v2_outR1;
    2'h2: res = main_v2_outR2;
    default: res = main_v2_outR3;
  endcase
endmodule

// main._unused17
// block '$L._unused17' of process main
module main___unused17 (input logic [80:0] s0,
  output logic [105:0] res);
  logic [17:0] Main_outputs_out;
  Main_outputs  outputs_i (s0, Main_outputs_out);
  assign res = {Main_outputs_out, 7'h40, s0};
endmodule

// main.s63
// block '$L.s63' of process main
module main_s63 (input logic [1:0] rD,
  input logic [8:0] p,
  input logic [80:0] s,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setCFlag_out;
  logic [80:0] Main_setR0_out;
  logic [105:0] main__unused17_out;
  logic [80:0] Main_setR1_out;
  logic [105:0] main__unused17_outR1;
  logic [80:0] Main_setR2_out;
  logic [105:0] main__unused17_outR2;
  logic [80:0] Main_setR3_out;
  logic [105:0] main__unused17_outR3;
  wire [0:0] x = p[8];
  Main_setCFlag  setCFlag_i (s, x, Main_setCFlag_out);
  wire [7:0] y = p[7:0];
  Main_setR0  setR0_i (Main_setCFlag_out, y, Main_setR0_out);
  main___unused17  _unused17_i (Main_setR0_out, main__unused17_out);
  Main_setR1  setR1_i (Main_setCFlag_out, y, Main_setR1_out);
  main___unused17  _unused17_iR1 (Main_setR1_out, main__unused17_outR1);
  Main_setR2  setR2_i (Main_setCFlag_out, y, Main_setR2_out);
  main___unused17  _unused17_iR2 (Main_setR2_out, main__unused17_outR2);
  Main_setR3  setR3_i (Main_setCFlag_out, y, Main_setR3_out);
  main___unused17  _unused17_iR3 (Main_setR3_out, main__unused17_outR3);
  always_comb case (rD)
    2'h0: res = main__unused17_out;
    2'h1: res = main__unused17_outR1;
    2'h2: res = main__unused17_outR2;
    default: res = main__unused17_outR3;
  endcase
endmodule

// main.vS
// block '$L.vS' of process main
module main_vS (input logic [1:0] rD,
  input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_s63_out;
  wire [8:0] s = ({1'h0, vD} + {1'h0, vS}) + 9'h0;
  wire [8:0] p = {s[8], s[7:0]};
  main_s63  s63_i (rD, p, s0, s0, main_s63_out);
  assign res = main_s63_out;
endmodule

// main.vD
// block '$L.vD' of process main
module main_vD (input logic [1:0] rD,
  input logic [1:0] rS,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS  vS_i (rD, vD, Main_r0_out, s0, main_vS_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS  vS_iR1 (rD, vD, Main_r1_out, s0, main_vS_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS  vS_iR2 (rD, vD, Main_r2_out, s0, main_vS_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS  vS_iR3 (rD, vD, Main_r3_out, s0, main_vS_outR3);
  always_comb case (rS)
    2'h0: res = main_vS_out;
    2'h1: res = main_vS_outR1;
    2'h2: res = main_vS_outR2;
    default: res = main_vS_outR3;
  endcase
endmodule

// main.vS2
// block '$L.vS2' of process main
module main_vS2 (input logic [1:0] rD,
  input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [0:0] Main_cFlag_out;
  logic [105:0] main_s63_out;
  Main_cFlag  cFlag_i (s0, Main_cFlag_out);
  wire [8:0] s = ({1'h0, vD} + {1'h0, vS}) + {8'h0, Main_cFlag_out};
  wire [8:0] p = {s[8], s[7:0]};
  main_s63  s63_i (rD, p, s0, s0, main_s63_out);
  assign res = main_s63_out;
endmodule

// main.vD2
// block '$L.vD2' of process main
module main_vD2 (input logic [1:0] rD,
  input logic [1:0] rS,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS2_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS2_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS2_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS2_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS2  vS2_i (rD, vD, Main_r0_out, s0, main_vS2_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS2  vS2_iR1 (rD, vD, Main_r1_out, s0, main_vS2_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS2  vS2_iR2 (rD, vD, Main_r2_out, s0, main_vS2_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS2  vS2_iR3 (rD, vD, Main_r3_out, s0, main_vS2_outR3);
  always_comb case (rS)
    2'h0: res = main_vS2_out;
    2'h1: res = main_vS2_outR1;
    2'h2: res = main_vS2_outR2;
    default: res = main_vS2_outR3;
  endcase
endmodule

// main.vS3
// block '$L.vS3' of process main
module main_vS3 (input logic [1:0] rD,
  input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [8:0] Main_minusCW8$sFalse_Bool_out;
  logic [105:0] main_s63_out;
  Main_minusCW8$sFalse__Bool  minusCW8$sFalse_Bool_i (vD, vS, Main_minusCW8$sFalse_Bool_out);
  main_s63  s63_i (rD, Main_minusCW8$sFalse_Bool_out, s0, s0, main_s63_out);
  assign res = main_s63_out;
endmodule

// main.vD3
// block '$L.vD3' of process main
module main_vD3 (input logic [1:0] rD,
  input logic [1:0] rS,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS3_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS3_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS3_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS3_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS3  vS3_i (rD, vD, Main_r0_out, s0, main_vS3_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS3  vS3_iR1 (rD, vD, Main_r1_out, s0, main_vS3_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS3  vS3_iR2 (rD, vD, Main_r2_out, s0, main_vS3_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS3  vS3_iR3 (rD, vD, Main_r3_out, s0, main_vS3_outR3);
  always_comb case (rS)
    2'h0: res = main_vS3_out;
    2'h1: res = main_vS3_outR1;
    2'h2: res = main_vS3_outR2;
    default: res = main_vS3_outR3;
  endcase
endmodule

// main.vS4
// block '$L.vS4' of process main
module main_vS4 (input logic [1:0] rD,
  input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [0:0] Main_cFlag_out;
  logic [105:0] main_s63_out;
  Main_cFlag  cFlag_i (s0, Main_cFlag_out);
  wire [8:0] s = ({1'h0, vD} - {1'h0, vS}) - {8'h0, Main_cFlag_out};
  wire [8:0] p = {s[8], s[7:0]};
  main_s63  s63_i (rD, p, s0, s0, main_s63_out);
  assign res = main_s63_out;
endmodule

// main.vD4
// block '$L.vD4' of process main
module main_vD4 (input logic [1:0] rD,
  input logic [1:0] rS,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS4_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS4_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS4_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS4_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS4  vS4_i (rD, vD, Main_r0_out, s0, main_vS4_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS4  vS4_iR1 (rD, vD, Main_r1_out, s0, main_vS4_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS4  vS4_iR2 (rD, vD, Main_r2_out, s0, main_vS4_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS4  vS4_iR3 (rD, vD, Main_r3_out, s0, main_vS4_outR3);
  always_comb case (rS)
    2'h0: res = main_vS4_out;
    2'h1: res = main_vS4_outR1;
    2'h2: res = main_vS4_outR2;
    default: res = main_vS4_outR3;
  endcase
endmodule

// main.zzz
// block '$L.zzz' of process main
// also: main._unused15
module main_zzz (input logic [80:0] s0,
  output logic [105:0] res);
  logic [17:0] Main_outputs_out;
  Main_outputs  outputs_i (s0, Main_outputs_out);
  assign res = {Main_outputs_out, 7'h30, s0};
endmodule

// main.x2
// block '$L.x2' of process main
module main_x2 (input logic [0:0] rEn,
  input logic [0:0] wEn,
  input logic [7:0] x,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [1:0] Main_mkReg_out;
  logic [80:0] Main_setR0_out;
  logic [105:0] main_zzz_out;
  logic [80:0] Main_setR1_out;
  logic [105:0] main_zzz_outR1;
  logic [80:0] Main_setR2_out;
  logic [105:0] main_zzz_outR2;
  logic [80:0] Main_setR3_out;
  logic [105:0] main_zzz_outR3;
  Main_mkReg  mkReg_i (rEn, wEn, Main_mkReg_out);
  Main_setR0  setR0_i (s0, x, Main_setR0_out);
  main_zzz  zzz_i (Main_setR0_out, main_zzz_out);
  Main_setR1  setR1_i (s0, x, Main_setR1_out);
  main_zzz  zzz_iR1 (Main_setR1_out, main_zzz_outR1);
  Main_setR2  setR2_i (s0, x, Main_setR2_out);
  main_zzz  zzz_iR2 (Main_setR2_out, main_zzz_outR2);
  Main_setR3  setR3_i (s0, x, Main_setR3_out);
  main_zzz  zzz_iR3 (Main_setR3_out, main_zzz_outR3);
  always_comb case (Main_mkReg_out)
    2'h0: res = main_zzz_out;
    2'h1: res = main_zzz_outR1;
    2'h2: res = main_zzz_outR2;
    default: res = main_zzz_outR3;
  endcase
endmodule

// main._unused24
// block '$L._unused24' of process main
module main___unused24 (input logic [7:0] vD$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setCFlag_out;
  logic [80:0] Main_setZFlag_out;
  logic [105:0] main__unused17_out;
  Main_setCFlag  setCFlag_i (s0, 1'h0, Main_setCFlag_out);
  Main_setZFlag  setZFlag_i (Main_setCFlag_out, vD$ == 8'h0, Main_setZFlag_out);
  main___unused17  _unused17_i (Main_setZFlag_out, main__unused17_out);
  assign res = main__unused17_out;
endmodule

// main.arm90
// block '$L.arm90' of process main
module main_arm90 (input logic [7:0] vD$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR0_out;
  logic [105:0] main__unused24_out;
  Main_setR0  setR0_i (s0, vD$, Main_setR0_out);
  main___unused24  _unused24_i (vD$, Main_setR0_out, main__unused24_out);
  assign res = main__unused24_out;
endmodule

// main.arm91
// block '$L.arm91' of process main
module main_arm91 (input logic [7:0] vD$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR1_out;
  logic [105:0] main__unused24_out;
  Main_setR1  setR1_i (s0, vD$, Main_setR1_out);
  main___unused24  _unused24_i (vD$, Main_setR1_out, main__unused24_out);
  assign res = main__unused24_out;
endmodule

// main.arm92
// block '$L.arm92' of process main
module main_arm92 (input logic [7:0] vD$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR2_out;
  logic [105:0] main__unused24_out;
  Main_setR2  setR2_i (s0, vD$, Main_setR2_out);
  main___unused24  _unused24_i (vD$, Main_setR2_out, main__unused24_out);
  assign res = main__unused24_out;
endmodule

// main.arm93
// block '$L.arm93' of process main
module main_arm93 (input logic [7:0] vD$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR3_out;
  logic [105:0] main__unused24_out;
  Main_setR3  setR3_i (s0, vD$, Main_setR3_out);
  main___unused24  _unused24_i (vD$, Main_setR3_out, main__unused24_out);
  assign res = main__unused24_out;
endmodule

// main.vS5
// block '$L.vS5' of process main
module main_vS5 (input logic [1:0] rD,
  input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_arm90_out;
  logic [105:0] main_arm91_out;
  logic [105:0] main_arm92_out;
  logic [105:0] main_arm93_out;
  wire [7:0] vD$ = vD | vS;
  main_arm90  arm90_i (vD$, s0, main_arm90_out);
  main_arm91  arm91_i (vD$, s0, main_arm91_out);
  main_arm92  arm92_i (vD$, s0, main_arm92_out);
  main_arm93  arm93_i (vD$, s0, main_arm93_out);
  always_comb case (rD)
    2'h0: res = main_arm90_out;
    2'h1: res = main_arm91_out;
    2'h2: res = main_arm92_out;
    default: res = main_arm93_out;
  endcase
endmodule

// main.vD5
// block '$L.vD5' of process main
module main_vD5 (input logic [1:0] rD,
  input logic [1:0] rS,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS5_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS5_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS5_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS5_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS5  vS5_i (rD, vD, Main_r0_out, s0, main_vS5_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS5  vS5_iR1 (rD, vD, Main_r1_out, s0, main_vS5_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS5  vS5_iR2 (rD, vD, Main_r2_out, s0, main_vS5_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS5  vS5_iR3 (rD, vD, Main_r3_out, s0, main_vS5_outR3);
  always_comb case (rS)
    2'h0: res = main_vS5_out;
    2'h1: res = main_vS5_outR1;
    2'h2: res = main_vS5_outR2;
    default: res = main_vS5_outR3;
  endcase
endmodule

// main.vS6
// block '$L.vS6' of process main
module main_vS6 (input logic [1:0] rD,
  input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_arm90_out;
  logic [105:0] main_arm91_out;
  logic [105:0] main_arm92_out;
  logic [105:0] main_arm93_out;
  wire [7:0] vD$ = vD & vS;
  main_arm90  arm90_i (vD$, s0, main_arm90_out);
  main_arm91  arm91_i (vD$, s0, main_arm91_out);
  main_arm92  arm92_i (vD$, s0, main_arm92_out);
  main_arm93  arm93_i (vD$, s0, main_arm93_out);
  always_comb case (rD)
    2'h0: res = main_arm90_out;
    2'h1: res = main_arm91_out;
    2'h2: res = main_arm92_out;
    default: res = main_arm93_out;
  endcase
endmodule

// main.vD6
// block '$L.vD6' of process main
module main_vD6 (input logic [1:0] rD,
  input logic [1:0] rS,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS6_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS6_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS6_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS6_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS6  vS6_i (rD, vD, Main_r0_out, s0, main_vS6_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS6  vS6_iR1 (rD, vD, Main_r1_out, s0, main_vS6_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS6  vS6_iR2 (rD, vD, Main_r2_out, s0, main_vS6_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS6  vS6_iR3 (rD, vD, Main_r3_out, s0, main_vS6_outR3);
  always_comb case (rS)
    2'h0: res = main_vS6_out;
    2'h1: res = main_vS6_outR1;
    2'h2: res = main_vS6_outR2;
    default: res = main_vS6_outR3;
  endcase
endmodule

// main.vS7
// block '$L.vS7' of process main
module main_vS7 (input logic [1:0] rD,
  input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_arm90_out;
  logic [105:0] main_arm91_out;
  logic [105:0] main_arm92_out;
  logic [105:0] main_arm93_out;
  wire [7:0] vD$ = vD ^ vS;
  main_arm90  arm90_i (vD$, s0, main_arm90_out);
  main_arm91  arm91_i (vD$, s0, main_arm91_out);
  main_arm92  arm92_i (vD$, s0, main_arm92_out);
  main_arm93  arm93_i (vD$, s0, main_arm93_out);
  always_comb case (rD)
    2'h0: res = main_arm90_out;
    2'h1: res = main_arm91_out;
    2'h2: res = main_arm92_out;
    default: res = main_arm93_out;
  endcase
endmodule

// main.vD7
// block '$L.vD7' of process main
module main_vD7 (input logic [1:0] rD,
  input logic [1:0] rS,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS7_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS7_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS7_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS7_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS7  vS7_i (rD, vD, Main_r0_out, s0, main_vS7_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS7  vS7_iR1 (rD, vD, Main_r1_out, s0, main_vS7_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS7  vS7_iR2 (rD, vD, Main_r2_out, s0, main_vS7_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS7  vS7_iR3 (rD, vD, Main_r3_out, s0, main_vS7_outR3);
  always_comb case (rS)
    2'h0: res = main_vS7_out;
    2'h1: res = main_vS7_outR1;
    2'h2: res = main_vS7_outR2;
    default: res = main_vS7_outR3;
  endcase
endmodule

// main.vS8
// block '$L.vS8' of process main
module main_vS8 (input logic [7:0] vD,
  input logic [7:0] vS,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [8:0] Main_minusCW8$sFalse_Bool_out;
  logic [80:0] Main_setCFlag_out;
  logic [80:0] Main_setZFlag_out;
  logic [105:0] main_zzz_out;
  Main_minusCW8$sFalse__Bool  minusCW8$sFalse_Bool_i (vD, vS, Main_minusCW8$sFalse_Bool_out);
  wire [0:0] x = Main_minusCW8$sFalse_Bool_out[8];
  Main_setCFlag  setCFlag_i (s0, x, Main_setCFlag_out);
  wire [7:0] y = Main_minusCW8$sFalse_Bool_out[7:0];
  Main_setZFlag  setZFlag_i (Main_setCFlag_out, y == 8'h0, Main_setZFlag_out);
  main_zzz  zzz_i (Main_setZFlag_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.vD8
// block '$L.vD8' of process main
module main_vD8 (input logic [0:0] b0,
  input logic [0:0] b1,
  input logic [7:0] vD,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [1:0] Main_mkReg_out;
  logic [7:0] Main_r0_out;
  logic [105:0] main_vS8_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_vS8_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_vS8_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_vS8_outR3;
  Main_mkReg  mkReg_i (b0, b1, Main_mkReg_out);
  Main_r0  r0_i (s0, Main_r0_out);
  main_vS8  vS8_i (vD, Main_r0_out, s0, main_vS8_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_vS8  vS8_iR1 (vD, Main_r1_out, s0, main_vS8_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_vS8  vS8_iR2 (vD, Main_r2_out, s0, main_vS8_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_vS8  vS8_iR3 (vD, Main_r3_out, s0, main_vS8_outR3);
  always_comb case (Main_mkReg_out)
    2'h0: res = main_vS8_out;
    2'h1: res = main_vS8_outR1;
    2'h2: res = main_vS8_outR2;
    default: res = main_vS8_outR3;
  endcase
endmodule

// main.pc3
// block '$L.pc3' of process main
module main_pc3 (input logic [7:0] pc,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setPC_out;
  logic [105:0] main_zzz_out;
  Main_setPC  setPC_i (s0, pc, Main_setPC_out);
  main_zzz  zzz_i (Main_setPC_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.arm142
// block '$L.arm142' of process main
module main_arm142 (input logic [0:0] b0,
  input logic [0:0] b1,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [1:0] Main_mkReg_out;
  logic [7:0] Main_r0_out;
  logic [105:0] main_pc3_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_pc3_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_pc3_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_pc3_outR3;
  Main_mkReg  mkReg_i (b0, b1, Main_mkReg_out);
  Main_r0  r0_i (s0, Main_r0_out);
  main_pc3  pc3_i (Main_r0_out, s0, main_pc3_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_pc3  pc3_iR1 (Main_r1_out, s0, main_pc3_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_pc3  pc3_iR2 (Main_r2_out, s0, main_pc3_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_pc3  pc3_iR3 (Main_r3_out, s0, main_pc3_outR3);
  always_comb case (Main_mkReg_out)
    2'h0: res = main_pc3_out;
    2'h1: res = main_pc3_outR1;
    2'h2: res = main_pc3_outR2;
    default: res = main_pc3_outR3;
  endcase
endmodule

// main.x3
// block '$L.x3' of process main
module main_x3 (input logic [7:0] x,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setPC_out;
  logic [105:0] main_zzz_out;
  Main_setPC  setPC_i (s0, x, Main_setPC_out);
  main_zzz  zzz_i (Main_setPC_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.arm170
// block '$L.arm170' of process main
module main_arm170 (input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR0_out;
  logic [105:0] main_zzz_out;
  Main_setR0  setR0_i (s0, v, Main_setR0_out);
  main_zzz  zzz_i (Main_setR0_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.arm171
// block '$L.arm171' of process main
module main_arm171 (input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR1_out;
  logic [105:0] main_zzz_out;
  Main_setR1  setR1_i (s0, v, Main_setR1_out);
  main_zzz  zzz_i (Main_setR1_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.arm172
// block '$L.arm172' of process main
module main_arm172 (input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR2_out;
  logic [105:0] main_zzz_out;
  Main_setR2  setR2_i (s0, v, Main_setR2_out);
  main_zzz  zzz_i (Main_setR2_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.arm173
// block '$L.arm173' of process main
module main_arm173 (input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR3_out;
  logic [105:0] main_zzz_out;
  Main_setR3  setR3_i (s0, v, Main_setR3_out);
  main_zzz  zzz_i (Main_setR3_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.v3
// block '$L.v3' of process main
module main_v3 (input logic [1:0] r,
  input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_arm170_out;
  logic [105:0] main_arm171_out;
  logic [105:0] main_arm172_out;
  logic [105:0] main_arm173_out;
  wire [7:0] v1 = ~v;
  main_arm170  arm170_i (v1, s0, main_arm170_out);
  main_arm171  arm171_i (v1, s0, main_arm171_out);
  main_arm172  arm172_i (v1, s0, main_arm172_out);
  main_arm173  arm173_i (v1, s0, main_arm173_out);
  always_comb case (r)
    2'h0: res = main_arm170_out;
    2'h1: res = main_arm171_out;
    2'h2: res = main_arm172_out;
    default: res = main_arm173_out;
  endcase
endmodule

// main._unused44
// block '$L._unused44' of process main
module main___unused44 (input logic [8:0] p,
  input logic [7:0] v$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setCFlag_out;
  logic [80:0] Main_setZFlag_out;
  logic [105:0] main_zzz_out;
  wire [0:0] x = p[8];
  Main_setCFlag  setCFlag_i (s0, x, Main_setCFlag_out);
  Main_setZFlag  setZFlag_i (Main_setCFlag_out, v$ == 8'h0, Main_setZFlag_out);
  main_zzz  zzz_i (Main_setZFlag_out, main_zzz_out);
  assign res = main_zzz_out;
endmodule

// main.arm184
// block '$L.arm184' of process main
module main_arm184 (input logic [8:0] p,
  input logic [7:0] v$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR0_out;
  logic [105:0] main__unused44_out;
  Main_setR0  setR0_i (s0, v$, Main_setR0_out);
  main___unused44  _unused44_i (p, v$, Main_setR0_out, main__unused44_out);
  assign res = main__unused44_out;
endmodule

// main.arm185
// block '$L.arm185' of process main
module main_arm185 (input logic [8:0] p,
  input logic [7:0] v$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR1_out;
  logic [105:0] main__unused44_out;
  Main_setR1  setR1_i (s0, v$, Main_setR1_out);
  main___unused44  _unused44_i (p, v$, Main_setR1_out, main__unused44_out);
  assign res = main__unused44_out;
endmodule

// main.arm186
// block '$L.arm186' of process main
module main_arm186 (input logic [8:0] p,
  input logic [7:0] v$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR2_out;
  logic [105:0] main__unused44_out;
  Main_setR2  setR2_i (s0, v$, Main_setR2_out);
  main___unused44  _unused44_i (p, v$, Main_setR2_out, main__unused44_out);
  assign res = main__unused44_out;
endmodule

// main.arm187
// block '$L.arm187' of process main
module main_arm187 (input logic [8:0] p,
  input logic [7:0] v$,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [80:0] Main_setR3_out;
  logic [105:0] main__unused44_out;
  Main_setR3  setR3_i (s0, v$, Main_setR3_out);
  main___unused44  _unused44_i (p, v$, Main_setR3_out, main__unused44_out);
  assign res = main__unused44_out;
endmodule

// main.v4
// block '$L.v4' of process main
module main_v4 (input logic [1:0] r,
  input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [8:0] Main_plusCW8$sMain_oneW8_False_Bool_out;
  logic [105:0] main_arm184_out;
  logic [105:0] main_arm185_out;
  logic [105:0] main_arm186_out;
  logic [105:0] main_arm187_out;
  Main_plusCW8$sMain__oneW8__False__Bool  plusCW8$sMain_oneW8_False_Bool_i (v, Main_plusCW8$sMain_oneW8_False_Bool_out);
  wire [7:0] y = Main_plusCW8$sMain_oneW8_False_Bool_out[7:0];
  main_arm184  arm184_i (Main_plusCW8$sMain_oneW8_False_Bool_out, y, s0, main_arm184_out);
  main_arm185  arm185_i (Main_plusCW8$sMain_oneW8_False_Bool_out, y, s0, main_arm185_out);
  main_arm186  arm186_i (Main_plusCW8$sMain_oneW8_False_Bool_out, y, s0, main_arm186_out);
  main_arm187  arm187_i (Main_plusCW8$sMain_oneW8_False_Bool_out, y, s0, main_arm187_out);
  always_comb case (r)
    2'h0: res = main_arm184_out;
    2'h1: res = main_arm185_out;
    2'h2: res = main_arm186_out;
    default: res = main_arm187_out;
  endcase
endmodule

// main.v5
// block '$L.v5' of process main
module main_v5 (input logic [1:0] r,
  input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_arm184_out;
  logic [105:0] main_arm185_out;
  logic [105:0] main_arm186_out;
  logic [105:0] main_arm187_out;
  wire [8:0] s = ({1'h0, v} - 9'h1) - 9'h0;
  wire [8:0] p = {s[8], s[7:0]};
  wire [7:0] y = p[7:0];
  main_arm184  arm184_i (p, y, s0, main_arm184_out);
  main_arm185  arm185_i (p, y, s0, main_arm185_out);
  main_arm186  arm186_i (p, y, s0, main_arm186_out);
  main_arm187  arm187_i (p, y, s0, main_arm187_out);
  always_comb case (r)
    2'h0: res = main_arm184_out;
    2'h1: res = main_arm185_out;
    2'h2: res = main_arm186_out;
    default: res = main_arm187_out;
  endcase
endmodule

// main.v6
// block '$L.v6' of process main
module main_v6 (input logic [1:0] r,
  input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_arm170_out;
  logic [105:0] main_arm171_out;
  logic [105:0] main_arm172_out;
  logic [105:0] main_arm173_out;
  wire [7:0] v1 = (v << 8'h1) | (v >> 8'h7);
  main_arm170  arm170_i (v1, s0, main_arm170_out);
  main_arm171  arm171_i (v1, s0, main_arm171_out);
  main_arm172  arm172_i (v1, s0, main_arm172_out);
  main_arm173  arm173_i (v1, s0, main_arm173_out);
  always_comb case (r)
    2'h0: res = main_arm170_out;
    2'h1: res = main_arm171_out;
    2'h2: res = main_arm172_out;
    default: res = main_arm173_out;
  endcase
endmodule

// main.v7
// block '$L.v7' of process main
module main_v7 (input logic [1:0] r,
  input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [105:0] main_arm170_out;
  logic [105:0] main_arm171_out;
  logic [105:0] main_arm172_out;
  logic [105:0] main_arm173_out;
  wire [7:0] v1 = (v >> 8'h1) | (v << 8'h7);
  main_arm170  arm170_i (v1, s0, main_arm170_out);
  main_arm171  arm171_i (v1, s0, main_arm171_out);
  main_arm172  arm172_i (v1, s0, main_arm172_out);
  main_arm173  arm173_i (v1, s0, main_arm173_out);
  always_comb case (r)
    2'h0: res = main_arm170_out;
    2'h1: res = main_arm171_out;
    2'h2: res = main_arm172_out;
    default: res = main_arm173_out;
  endcase
endmodule

// main.v8
// block '$L.v8' of process main
module main_v8 (input logic [0:0] rEn,
  input logic [0:0] wEn,
  input logic [1:0] r,
  input logic [7:0] v,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [0:0] Main_msbW8_out;
  logic [0:0] Main_lsbW8_out;
  logic [105:0] main_arm184_out;
  logic [105:0] main_arm185_out;
  logic [105:0] main_arm186_out;
  logic [105:0] main_arm187_out;
  Main_msbW8  msbW8_i (v, Main_msbW8_out);
  wire [8:0] Za1 = {Main_msbW8_out, (v << 8'h1) | {7'h0, Main_msbW8_out}};
  wire [8:0] Za = {Main_msbW8_out, (v << 8'h1) | 8'h0};
  wire [8:0] ZaR1 = (~rEn) ? Za1 : Za;
  Main_lsbW8  lsbW8_i (v, Main_lsbW8_out);
  wire [8:0] Za1R1 = {Main_lsbW8_out, (v >> 8'h1) | ({7'h0, Main_lsbW8_out} << 8'h7)};
  wire [8:0] ZaR2 = {Main_lsbW8_out, (v >> 8'h1) | 8'h0};
  wire [8:0] ZaR3 = (~rEn) ? Za1R1 : ZaR2;
  wire [8:0] p = (~wEn) ? ZaR1 : ZaR3;
  wire [7:0] y = p[7:0];
  main_arm184  arm184_i (p, y, s0, main_arm184_out);
  main_arm185  arm185_i (p, y, s0, main_arm185_out);
  main_arm186  arm186_i (p, y, s0, main_arm186_out);
  main_arm187  arm187_i (p, y, s0, main_arm187_out);
  always_comb case (r)
    2'h0: res = main_arm184_out;
    2'h1: res = main_arm185_out;
    2'h2: res = main_arm186_out;
    default: res = main_arm187_out;
  endcase
endmodule

// main.$fail
// block '$L.$fail' of process main
module main_$fail (input logic [9:0] inp,
  input logic [80:0] s0,
  output logic [105:0] res);
  logic [7:0] Main_dataIn_out;
  logic [1:0] Main_mkReg_out;
  logic [7:0] Main_pc_out;
  logic [17:0] Main_outputs_out;
  logic [17:0] Main_setAddrOut_out;
  logic [80:0] Main_setOutputs_out;
  logic [17:0] Main_outputs_outR1;
  logic [7:0] Main_r0_out;
  logic [105:0] main_a2_out;
  logic [7:0] Main_r1_out;
  logic [105:0] main_a2_outR1;
  logic [7:0] Main_r2_out;
  logic [105:0] main_a2_outR2;
  logic [7:0] Main_r3_out;
  logic [105:0] main_a2_outR3;
  logic [105:0] main_a3_out;
  logic [105:0] main_a3_outR1;
  logic [105:0] main_a3_outR2;
  logic [105:0] main_a3_outR3;
  logic [1:0] Main_mkReg_outR1;
  logic [105:0] main_vD_out;
  logic [105:0] main_vD_outR1;
  logic [105:0] main_vD_outR2;
  logic [105:0] main_vD_outR3;
  logic [105:0] main_vD2_out;
  logic [105:0] main_vD2_outR1;
  logic [105:0] main_vD2_outR2;
  logic [105:0] main_vD2_outR3;
  logic [105:0] main_vD3_out;
  logic [105:0] main_vD3_outR1;
  logic [105:0] main_vD3_outR2;
  logic [105:0] main_vD3_outR3;
  logic [105:0] main_vD4_out;
  logic [105:0] main_vD4_outR1;
  logic [105:0] main_vD4_outR2;
  logic [105:0] main_vD4_outR3;
  logic [105:0] main_x2_out;
  logic [105:0] main_x2_outR1;
  logic [105:0] main_x2_outR2;
  logic [105:0] main_x2_outR3;
  logic [105:0] main_vD5_out;
  logic [105:0] main_vD5_outR1;
  logic [105:0] main_vD5_outR2;
  logic [105:0] main_vD5_outR3;
  logic [105:0] main_vD6_out;
  logic [105:0] main_vD6_outR1;
  logic [105:0] main_vD6_outR2;
  logic [105:0] main_vD6_outR3;
  logic [105:0] main_vD7_out;
  logic [105:0] main_vD7_outR1;
  logic [105:0] main_vD7_outR2;
  logic [105:0] main_vD7_outR3;
  logic [105:0] main_vD8_out;
  logic [105:0] main_vD8_outR1;
  logic [105:0] main_vD8_outR2;
  logic [105:0] main_vD8_outR3;
  logic [0:0] Main_zFlag_out;
  logic [105:0] main_zzz_out;
  logic [105:0] main_arm142_out;
  logic [0:0] Main_notb_out;
  logic [0:0] Main_cFlag_out;
  logic [0:0] Main_notb_outR1;
  logic [105:0] main_x3_out;
  logic [105:0] main_x3_outR1;
  logic [105:0] main_x3_outR2;
  logic [105:0] main_x3_outR3;
  logic [80:0] Main_setIEFlag_out;
  logic [105:0] main_zzz_outR1;
  logic [80:0] Main_setOutputs_outR1;
  logic [105:0] main_zzz_outR2;
  logic [80:0] Main_setIEFlag_outR1;
  logic [80:0] Main_setPC_out;
  logic [80:0] Main_setZFlag_out;
  logic [80:0] Main_setCFlag_out;
  logic [105:0] main_zzz_outR3;
  logic [105:0] main_v3_out;
  logic [105:0] main_v3_outR1;
  logic [105:0] main_v3_outR2;
  logic [105:0] main_v3_outR3;
  logic [80:0] Main_setR0_out;
  logic [105:0] main_zzz_outR4;
  logic [80:0] Main_setR1_out;
  logic [105:0] main_zzz_outR5;
  logic [80:0] Main_setR2_out;
  logic [105:0] main_zzz_outR6;
  logic [80:0] Main_setR3_out;
  logic [105:0] main_zzz_outR7;
  logic [105:0] main_v4_out;
  logic [105:0] main_v4_outR1;
  logic [105:0] main_v4_outR2;
  logic [105:0] main_v4_outR3;
  logic [105:0] main_v5_out;
  logic [105:0] main_v5_outR1;
  logic [105:0] main_v5_outR2;
  logic [105:0] main_v5_outR3;
  logic [105:0] main_v6_out;
  logic [105:0] main_v6_outR1;
  logic [105:0] main_v6_outR2;
  logic [105:0] main_v6_outR3;
  logic [105:0] main_v7_out;
  logic [105:0] main_v7_outR1;
  logic [105:0] main_v7_outR2;
  logic [105:0] main_v7_outR3;
  logic [105:0] main_v8_out;
  logic [105:0] main_v8_outR1;
  logic [105:0] main_v8_outR2;
  logic [105:0] main_v8_outR3;
  Main_dataIn  dataIn_i (inp, Main_dataIn_out);
  wire [0:0] Zds1 = Main_dataIn_out[6];
  wire [0:0] Zds2 = Main_dataIn_out[5];
  wire [0:0] Zds3 = Main_dataIn_out[4];
  wire [0:0] rEn = Main_dataIn_out[3];
  wire [0:0] wEn = Main_dataIn_out[2];
  wire [0:0] b0 = Main_dataIn_out[1];
  wire [0:0] b1 = Main_dataIn_out[0];
  wire [0:0] Za = Main_dataIn_out[7];
  Main_mkReg  mkReg_i (b0, b1, Main_mkReg_out);
  Main_pc  pc_i (s0, Main_pc_out);
  Main_outputs  outputs_i (s0, Main_outputs_out);
  Main_setAddrOut  setAddrOut_i (Main_outputs_out, Main_pc_out, Main_setAddrOut_out);
  Main_setOutputs  setOutputs_i (s0, Main_setAddrOut_out, Main_setOutputs_out);
  Main_outputs  outputs_iR1 (Main_setOutputs_out, Main_outputs_outR1);
  Main_r0  r0_i (s0, Main_r0_out);
  main_a2  a2_i (rEn, wEn, Main_r0_out, s0, main_a2_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_a2  a2_iR1 (rEn, wEn, Main_r1_out, s0, main_a2_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_a2  a2_iR2 (rEn, wEn, Main_r2_out, s0, main_a2_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_a2  a2_iR3 (rEn, wEn, Main_r3_out, s0, main_a2_outR3);
  main_a3  a3_i (rEn, wEn, Main_r0_out, s0, main_a3_out);
  main_a3  a3_iR1 (rEn, wEn, Main_r1_out, s0, main_a3_outR1);
  main_a3  a3_iR2 (rEn, wEn, Main_r2_out, s0, main_a3_outR2);
  main_a3  a3_iR3 (rEn, wEn, Main_r3_out, s0, main_a3_outR3);
  Main_mkReg  mkReg_iR1 (rEn, wEn, Main_mkReg_outR1);
  main_vD  vD_i (Main_mkReg_outR1, Main_mkReg_out, Main_r0_out, s0, main_vD_out);
  main_vD  vD_iR1 (Main_mkReg_outR1, Main_mkReg_out, Main_r1_out, s0, main_vD_outR1);
  main_vD  vD_iR2 (Main_mkReg_outR1, Main_mkReg_out, Main_r2_out, s0, main_vD_outR2);
  main_vD  vD_iR3 (Main_mkReg_outR1, Main_mkReg_out, Main_r3_out, s0, main_vD_outR3);
  main_vD2  vD2_i (Main_mkReg_outR1, Main_mkReg_out, Main_r0_out, s0, main_vD2_out);
  main_vD2  vD2_iR1 (Main_mkReg_outR1, Main_mkReg_out, Main_r1_out, s0, main_vD2_outR1);
  main_vD2  vD2_iR2 (Main_mkReg_outR1, Main_mkReg_out, Main_r2_out, s0, main_vD2_outR2);
  main_vD2  vD2_iR3 (Main_mkReg_outR1, Main_mkReg_out, Main_r3_out, s0, main_vD2_outR3);
  main_vD3  vD3_i (Main_mkReg_outR1, Main_mkReg_out, Main_r0_out, s0, main_vD3_out);
  main_vD3  vD3_iR1 (Main_mkReg_outR1, Main_mkReg_out, Main_r1_out, s0, main_vD3_outR1);
  main_vD3  vD3_iR2 (Main_mkReg_outR1, Main_mkReg_out, Main_r2_out, s0, main_vD3_outR2);
  main_vD3  vD3_iR3 (Main_mkReg_outR1, Main_mkReg_out, Main_r3_out, s0, main_vD3_outR3);
  main_vD4  vD4_i (Main_mkReg_outR1, Main_mkReg_out, Main_r0_out, s0, main_vD4_out);
  main_vD4  vD4_iR1 (Main_mkReg_outR1, Main_mkReg_out, Main_r1_out, s0, main_vD4_outR1);
  main_vD4  vD4_iR2 (Main_mkReg_outR1, Main_mkReg_out, Main_r2_out, s0, main_vD4_outR2);
  main_vD4  vD4_iR3 (Main_mkReg_outR1, Main_mkReg_out, Main_r3_out, s0, main_vD4_outR3);
  main_x2  x2_i (rEn, wEn, Main_r0_out, s0, main_x2_out);
  main_x2  x2_iR1 (rEn, wEn, Main_r1_out, s0, main_x2_outR1);
  main_x2  x2_iR2 (rEn, wEn, Main_r2_out, s0, main_x2_outR2);
  main_x2  x2_iR3 (rEn, wEn, Main_r3_out, s0, main_x2_outR3);
  main_vD5  vD5_i (Main_mkReg_outR1, Main_mkReg_out, Main_r0_out, s0, main_vD5_out);
  main_vD5  vD5_iR1 (Main_mkReg_outR1, Main_mkReg_out, Main_r1_out, s0, main_vD5_outR1);
  main_vD5  vD5_iR2 (Main_mkReg_outR1, Main_mkReg_out, Main_r2_out, s0, main_vD5_outR2);
  main_vD5  vD5_iR3 (Main_mkReg_outR1, Main_mkReg_out, Main_r3_out, s0, main_vD5_outR3);
  main_vD6  vD6_i (Main_mkReg_outR1, Main_mkReg_out, Main_r0_out, s0, main_vD6_out);
  main_vD6  vD6_iR1 (Main_mkReg_outR1, Main_mkReg_out, Main_r1_out, s0, main_vD6_outR1);
  main_vD6  vD6_iR2 (Main_mkReg_outR1, Main_mkReg_out, Main_r2_out, s0, main_vD6_outR2);
  main_vD6  vD6_iR3 (Main_mkReg_outR1, Main_mkReg_out, Main_r3_out, s0, main_vD6_outR3);
  main_vD7  vD7_i (Main_mkReg_outR1, Main_mkReg_out, Main_r0_out, s0, main_vD7_out);
  main_vD7  vD7_iR1 (Main_mkReg_outR1, Main_mkReg_out, Main_r1_out, s0, main_vD7_outR1);
  main_vD7  vD7_iR2 (Main_mkReg_outR1, Main_mkReg_out, Main_r2_out, s0, main_vD7_outR2);
  main_vD7  vD7_iR3 (Main_mkReg_outR1, Main_mkReg_out, Main_r3_out, s0, main_vD7_outR3);
  main_vD8  vD8_i (b0, b1, Main_r0_out, s0, main_vD8_out);
  main_vD8  vD8_iR1 (b0, b1, Main_r1_out, s0, main_vD8_outR1);
  main_vD8  vD8_iR2 (b0, b1, Main_r2_out, s0, main_vD8_outR2);
  main_vD8  vD8_iR3 (b0, b1, Main_r3_out, s0, main_vD8_outR3);
  Main_zFlag  zFlag_i (s0, Main_zFlag_out);
  main_zzz  zzz_i (s0, main_zzz_out);
  main_arm142  arm142_i (b0, b1, s0, main_arm142_out);
  Main_notb  notb_i (Main_zFlag_out, Main_notb_out);
  Main_cFlag  cFlag_i (s0, Main_cFlag_out);
  Main_notb  notb_iR1 (Main_cFlag_out, Main_notb_outR1);
  main_x3  x3_i (Main_r0_out, s0, main_x3_out);
  main_x3  x3_iR1 (Main_r1_out, s0, main_x3_outR1);
  main_x3  x3_iR2 (Main_r2_out, s0, main_x3_outR2);
  main_x3  x3_iR3 (Main_r3_out, s0, main_x3_outR3);
  Main_setIEFlag  setIEFlag_i (s0, b1, Main_setIEFlag_out);
  main_zzz  zzz_iR1 (Main_setIEFlag_out, main_zzz_outR1);
  wire [7:0] a_o = Main_outputs_out[17:10];
  wire [7:0] d_o = Main_outputs_out[9:2];
  wire [0:0] we_o = Main_outputs_out[1];
  wire [17:0] ZaR1 = {a_o, d_o, we_o, 1'h1};
  Main_setOutputs  setOutputs_iR1 (s0, ZaR1, Main_setOutputs_outR1);
  main_zzz  zzz_iR2 (Main_setOutputs_outR1, main_zzz_outR2);
  Main_setIEFlag  setIEFlag_iR1 (s0, 1'h1, Main_setIEFlag_outR1);
  wire [7:0] pcs = Main_setIEFlag_outR1[39:32];
  Main_setPC  setPC_i (Main_setIEFlag_outR1, pcs, Main_setPC_out);
  wire [0:0] zs = Main_setPC_out[41];
  Main_setZFlag  setZFlag_i (Main_setPC_out, zs, Main_setZFlag_out);
  wire [0:0] cs = Main_setZFlag_out[40];
  Main_setCFlag  setCFlag_i (Main_setZFlag_out, cs, Main_setCFlag_out);
  main_zzz  zzz_iR3 (Main_setCFlag_out, main_zzz_outR3);
  main_v3  v3_i (Main_mkReg_out, Main_r0_out, s0, main_v3_out);
  main_v3  v3_iR1 (Main_mkReg_out, Main_r1_out, s0, main_v3_outR1);
  main_v3  v3_iR2 (Main_mkReg_out, Main_r2_out, s0, main_v3_outR2);
  main_v3  v3_iR3 (Main_mkReg_out, Main_r3_out, s0, main_v3_outR3);
  Main_setR0  setR0_i (s0, 8'h0, Main_setR0_out);
  main_zzz  zzz_iR4 (Main_setR0_out, main_zzz_outR4);
  Main_setR1  setR1_i (s0, 8'h0, Main_setR1_out);
  main_zzz  zzz_iR5 (Main_setR1_out, main_zzz_outR5);
  Main_setR2  setR2_i (s0, 8'h0, Main_setR2_out);
  main_zzz  zzz_iR6 (Main_setR2_out, main_zzz_outR6);
  Main_setR3  setR3_i (s0, 8'h0, Main_setR3_out);
  main_zzz  zzz_iR7 (Main_setR3_out, main_zzz_outR7);
  main_v4  v4_i (Main_mkReg_out, Main_r0_out, s0, main_v4_out);
  main_v4  v4_iR1 (Main_mkReg_out, Main_r1_out, s0, main_v4_outR1);
  main_v4  v4_iR2 (Main_mkReg_out, Main_r2_out, s0, main_v4_outR2);
  main_v4  v4_iR3 (Main_mkReg_out, Main_r3_out, s0, main_v4_outR3);
  main_v5  v5_i (Main_mkReg_out, Main_r0_out, s0, main_v5_out);
  main_v5  v5_iR1 (Main_mkReg_out, Main_r1_out, s0, main_v5_outR1);
  main_v5  v5_iR2 (Main_mkReg_out, Main_r2_out, s0, main_v5_outR2);
  main_v5  v5_iR3 (Main_mkReg_out, Main_r3_out, s0, main_v5_outR3);
  main_v6  v6_i (Main_mkReg_out, Main_r0_out, s0, main_v6_out);
  main_v6  v6_iR1 (Main_mkReg_out, Main_r1_out, s0, main_v6_outR1);
  main_v6  v6_iR2 (Main_mkReg_out, Main_r2_out, s0, main_v6_outR2);
  main_v6  v6_iR3 (Main_mkReg_out, Main_r3_out, s0, main_v6_outR3);
  main_v7  v7_i (Main_mkReg_out, Main_r0_out, s0, main_v7_out);
  main_v7  v7_iR1 (Main_mkReg_out, Main_r1_out, s0, main_v7_outR1);
  main_v7  v7_iR2 (Main_mkReg_out, Main_r2_out, s0, main_v7_outR2);
  main_v7  v7_iR3 (Main_mkReg_out, Main_r3_out, s0, main_v7_outR3);
  main_v8  v8_i (rEn, wEn, Main_mkReg_out, Main_r0_out, s0, main_v8_out);
  main_v8  v8_iR1 (rEn, wEn, Main_mkReg_out, Main_r1_out, s0, main_v8_outR1);
  main_v8  v8_iR2 (rEn, wEn, Main_mkReg_out, Main_r2_out, s0, main_v8_outR2);
  main_v8  v8_iR3 (rEn, wEn, Main_mkReg_out, Main_r3_out, s0, main_v8_outR3);
  assign res = (~Za) ? ((~Zds1) ? ((~Zds2) ? ((~Zds3) ? {Main_outputs_outR1, 3'h1, rEn, wEn, Main_mkReg_out,
    Main_setOutputs_out} :
    ((Main_mkReg_out == 2'h0) ? main_a2_out :
      ((Main_mkReg_out == 2'h1) ? main_a2_outR1 :
        ((Main_mkReg_out == 2'h2) ? main_a2_outR2 : main_a2_outR3)))) :
    ((~Zds3) ? ((Main_mkReg_out == 2'h0) ? main_a3_out :
      ((Main_mkReg_out == 2'h1) ? main_a3_outR1 :
        ((Main_mkReg_out == 2'h2) ? main_a3_outR2 : main_a3_outR3))) :
      ((Main_mkReg_outR1 == 2'h0) ? main_vD_out :
        ((Main_mkReg_outR1 == 2'h1) ? main_vD_outR1 :
          ((Main_mkReg_outR1 == 2'h2) ? main_vD_outR2 : main_vD_outR3))))) :
    ((~Zds2) ? ((~Zds3) ? ((Main_mkReg_outR1 == 2'h0) ? main_vD2_out :
      ((Main_mkReg_outR1 == 2'h1) ? main_vD2_outR1 :
        ((Main_mkReg_outR1 == 2'h2) ? main_vD2_outR2 : main_vD2_outR3))) :
      ((Main_mkReg_outR1 == 2'h0) ? main_vD3_out :
        ((Main_mkReg_outR1 == 2'h1) ? main_vD3_outR1 :
          ((Main_mkReg_outR1 == 2'h2) ? main_vD3_outR2 : main_vD3_outR3)))) :
      ((~Zds3) ? ((Main_mkReg_outR1 == 2'h0) ? main_vD4_out :
        ((Main_mkReg_outR1 == 2'h1) ? main_vD4_outR1 :
          ((Main_mkReg_outR1 == 2'h2) ? main_vD4_outR2 : main_vD4_outR3))) :
        ((Main_mkReg_out == 2'h0) ? main_x2_out :
          ((Main_mkReg_out == 2'h1) ? main_x2_outR1 :
            ((Main_mkReg_out == 2'h2) ? main_x2_outR2 : main_x2_outR3)))))) :
    ((~Zds1) ? ((~Zds2) ? ((~Zds3) ? ((Main_mkReg_outR1 == 2'h0) ? main_vD5_out :
      ((Main_mkReg_outR1 == 2'h1) ? main_vD5_outR1 :
        ((Main_mkReg_outR1 == 2'h2) ? main_vD5_outR2 : main_vD5_outR3))) :
      ((Main_mkReg_outR1 == 2'h0) ? main_vD6_out :
        ((Main_mkReg_outR1 == 2'h1) ? main_vD6_outR1 :
          ((Main_mkReg_outR1 == 2'h2) ? main_vD6_outR2 : main_vD6_outR3)))) :
      ((~Zds3) ? ((Main_mkReg_outR1 == 2'h0) ? main_vD7_out :
        ((Main_mkReg_outR1 == 2'h1) ? main_vD7_outR1 :
          ((Main_mkReg_outR1 == 2'h2) ? main_vD7_outR2 : main_vD7_outR3))) :
        ((Main_mkReg_outR1 == 2'h0) ? main_vD8_out :
          ((Main_mkReg_outR1 == 2'h1) ? main_vD8_outR1 :
            ((Main_mkReg_outR1 == 2'h2) ? main_vD8_outR2 : main_vD8_outR3))))) :
      ((~Zds2) ? ((~Zds3) ? ((~rEn) ? ((~wEn) ? ((~Main_zFlag_out) ? main_zzz_out : main_arm142_out) :
        ((~Main_notb_out) ? main_zzz_out : main_arm142_out)) :
        ((~wEn) ? ((~Main_cFlag_out) ? main_zzz_out : main_arm142_out) :
          ((~Main_notb_outR1) ? main_zzz_out : main_arm142_out))) :
        ((~rEn) ? ((~wEn) ? ((Main_mkReg_out == 2'h0) ? main_x3_out :
          ((Main_mkReg_out == 2'h1) ? main_x3_outR1 :
            ((Main_mkReg_out == 2'h2) ? main_x3_outR2 : main_x3_outR3))) :
          ((~b0) ? main_zzz_outR1 :
            ((~b1) ? main_zzz_outR2 : main_zzz_outR3))) :
          ((~wEn) ? ((Main_mkReg_out == 2'h0) ? main_v3_out :
            ((Main_mkReg_out == 2'h1) ? main_v3_outR1 :
              ((Main_mkReg_out == 2'h2) ? main_v3_outR2 : main_v3_outR3))) :
            ((Main_mkReg_out == 2'h0) ? main_zzz_outR4 :
              ((Main_mkReg_out == 2'h1) ? main_zzz_outR5 :
                ((Main_mkReg_out == 2'h2) ? main_zzz_outR6 : main_zzz_outR7)))))) :
        ((~Zds3) ? ((~rEn) ? ((~wEn) ? ((Main_mkReg_out == 2'h0) ? main_v4_out :
          ((Main_mkReg_out == 2'h1) ? main_v4_outR1 :
            ((Main_mkReg_out == 2'h2) ? main_v4_outR2 : main_v4_outR3))) :
          ((Main_mkReg_out == 2'h0) ? main_v5_out :
            ((Main_mkReg_out == 2'h1) ? main_v5_outR1 :
              ((Main_mkReg_out == 2'h2) ? main_v5_outR2 : main_v5_outR3)))) :
          ((~wEn) ? ((Main_mkReg_out == 2'h0) ? main_v6_out :
            ((Main_mkReg_out == 2'h1) ? main_v6_outR1 :
              ((Main_mkReg_out == 2'h2) ? main_v6_outR2 : main_v6_outR3))) :
            ((Main_mkReg_out == 2'h0) ? main_v7_out :
              ((Main_mkReg_out == 2'h1) ? main_v7_outR1 :
                ((Main_mkReg_out == 2'h2) ? main_v7_outR2 : main_v7_outR3))))) :
          ((Main_mkReg_out == 2'h0) ? main_v8_out :
            ((Main_mkReg_out == 2'h1) ? main_v8_outR1 :
              ((Main_mkReg_out == 2'h2) ? main_v8_outR2 : main_v8_outR3))))));
endmodule

// main.loop
// block '$L.Main.loop' of process main
module main_loop (input logic [80:0] s0,
  output logic [105:0] res);
  logic [9:0] Main_inputs_out;
  logic [80:0] Main_setIEFlag_out;
  logic [7:0] Main_pc_out;
  logic [0:0] Main_zFlag_out;
  logic [0:0] Main_cFlag_out;
  logic [105:0] main_zzz_out;
  logic [105:0] main_$fail_out;
  logic [80:0] Main_setCFlag_out;
  logic [80:0] Main_setZFlag_out;
  logic [80:0] Main_setOutputs_out;
  logic [105:0] main_zzz_outR1;
  Main_inputs  inputs_i (s0, Main_inputs_out);
  wire [0:0] r_i = Main_inputs_out[1];
  wire [0:0] ie = s0[50];
  wire [0:0] i_i = Main_inputs_out[0];
  Main_setIEFlag  setIEFlag_i (s0, 1'h0, Main_setIEFlag_out);
  Main_pc  pc_i (Main_setIEFlag_out, Main_pc_out);
  Main_zFlag  zFlag_i (Main_setIEFlag_out, Main_zFlag_out);
  Main_cFlag  cFlag_i (Main_setIEFlag_out, Main_cFlag_out);
  wire [9:0] i = Main_setIEFlag_out[80:71];
  wire [17:0] o = Main_setIEFlag_out[70:53];
  wire [0:0] z = Main_setIEFlag_out[52];
  wire [0:0] c = Main_setIEFlag_out[51];
  wire [0:0] ieR1 = Main_setIEFlag_out[50];
  wire [7:0] pc = Main_setIEFlag_out[49:42];
  wire [0:0] zs = Main_setIEFlag_out[41];
  wire [0:0] cs = Main_setIEFlag_out[40];
  wire [7:0] r0 = Main_setIEFlag_out[31:24];
  wire [7:0] r1 = Main_setIEFlag_out[23:16];
  wire [7:0] r2 = Main_setIEFlag_out[15:8];
  wire [7:0] r3 = Main_setIEFlag_out[7:0];
  wire [80:0] Za = {i, o, z, c, ieR1, pc, zs, cs, Main_pc_out, r0, r1, r2, r3};
  wire [9:0] iR1 = Za[80:71];
  wire [17:0] oR1 = Za[70:53];
  wire [0:0] zR1 = Za[52];
  wire [0:0] cR1 = Za[51];
  wire [0:0] ieR2 = Za[50];
  wire [7:0] pcR1 = Za[49:42];
  wire [0:0] csR1 = Za[40];
  wire [7:0] pcs = Za[39:32];
  wire [7:0] r0R1 = Za[31:24];
  wire [7:0] r1R1 = Za[23:16];
  wire [7:0] r2R1 = Za[15:8];
  wire [7:0] r3R1 = Za[7:0];
  wire [80:0] ZaR1 = {iR1, oR1, zR1, cR1, ieR2, pcR1, Main_zFlag_out, csR1, pcs, r0R1, r1R1, r2R1, r3R1};
  wire [9:0] iR2 = ZaR1[80:71];
  wire [17:0] oR2 = ZaR1[70:53];
  wire [0:0] zR2 = ZaR1[52];
  wire [0:0] cR2 = ZaR1[51];
  wire [0:0] ieR3 = ZaR1[50];
  wire [7:0] pcR2 = ZaR1[49:42];
  wire [0:0] zsR1 = ZaR1[41];
  wire [7:0] pcsR1 = ZaR1[39:32];
  wire [7:0] r0R2 = ZaR1[31:24];
  wire [7:0] r1R2 = ZaR1[23:16];
  wire [7:0] r2R2 = ZaR1[15:8];
  wire [7:0] r3R2 = ZaR1[7:0];
  wire [80:0] ZaR2 = {iR2, oR2, zR2, cR2, ieR3, pcR2, zsR1, Main_cFlag_out, pcsR1, r0R2, r1R2, r2R2, r3R2};
  main_zzz  zzz_i (ZaR2, main_zzz_out);
  main_$fail  Zfail_i (Main_inputs_out, s0, main_$fail_out);
  Main_setCFlag  setCFlag_i (s0, 1'h0, Main_setCFlag_out);
  Main_setZFlag  setZFlag_i (Main_setCFlag_out, 1'h0, Main_setZFlag_out);
  Main_setOutputs  setOutputs_i (Main_setZFlag_out, 18'h0, Main_setOutputs_out);
  main_zzz  zzz_iR1 (Main_setOutputs_out, main_zzz_outR1);
  assign res = (~r_i) ? (ie ? (i_i ? main_zzz_out : main_$fail_out) : main_$fail_out) : main_zzz_outR1;
endmodule

// Main.notb
module Main_notb (input logic [0:0] b,
  output logic [0:0] res);
  assign res = ~b;
endmodule

// Main.mkReg
module Main_mkReg (input logic [0:0] Zds,
  input logic [0:0] Zds1,
  output logic [1:0] res);
  assign res = (~Zds) ? ((~Zds1) ? 2'h0 : 2'h1) :
    ((~Zds1) ? 2'h2 : 2'h3);
endmodule

// Main.outputs
module Main_outputs (input logic [80:0] Zds,
  output logic [17:0] res);
  wire [17:0] o = Zds[70:53];
  assign res = o;
endmodule

// Main.setInputs
module Main_setInputs (input logic [80:0] Zds,
  input logic [9:0] i,
  output logic [80:0] res);
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.pc
module Main_pc (input logic [80:0] Zds,
  output logic [7:0] res);
  wire [7:0] pc = Zds[49:42];
  assign res = pc;
endmodule

// Main.setPC
module Main_setPC (input logic [80:0] Zds,
  input logic [7:0] pc,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.r0
module Main_r0 (input logic [80:0] Zds,
  output logic [7:0] res);
  wire [7:0] r0 = Zds[31:24];
  assign res = r0;
endmodule

// Main.r1
module Main_r1 (input logic [80:0] Zds,
  output logic [7:0] res);
  wire [7:0] r1 = Zds[23:16];
  assign res = r1;
endmodule

// Main.r2
module Main_r2 (input logic [80:0] Zds,
  output logic [7:0] res);
  wire [7:0] r2 = Zds[15:8];
  assign res = r2;
endmodule

// Main.r3
module Main_r3 (input logic [80:0] Zds,
  output logic [7:0] res);
  wire [7:0] r3 = Zds[7:0];
  assign res = r3;
endmodule

// Main.setR0
module Main_setR0 (input logic [80:0] Zds,
  input logic [7:0] r0,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.setR1
module Main_setR1 (input logic [80:0] Zds,
  input logic [7:0] r1,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.setR2
module Main_setR2 (input logic [80:0] Zds,
  input logic [7:0] r2,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.setR3
module Main_setR3 (input logic [80:0] Zds,
  input logic [7:0] r3,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.zFlag
module Main_zFlag (input logic [80:0] Zds,
  output logic [0:0] res);
  wire [0:0] z = Zds[52];
  assign res = z;
endmodule

// Main.setZFlag
module Main_setZFlag (input logic [80:0] Zds,
  input logic [0:0] z,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.cFlag
module Main_cFlag (input logic [80:0] Zds,
  output logic [0:0] res);
  wire [0:0] c = Zds[51];
  assign res = c;
endmodule

// Main.setCFlag
module Main_setCFlag (input logic [80:0] Zds,
  input logic [0:0] c,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.setIEFlag
module Main_setIEFlag (input logic [80:0] Zds,
  input logic [0:0] ie,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [17:0] o = Zds[70:53];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.inputs
module Main_inputs (input logic [80:0] Zds,
  output logic [9:0] res);
  wire [9:0] i = Zds[80:71];
  assign res = i;
endmodule

// Main.setOutputs
module Main_setOutputs (input logic [80:0] Zds,
  input logic [17:0] o,
  output logic [80:0] res);
  wire [9:0] i = Zds[80:71];
  wire [0:0] z = Zds[52];
  wire [0:0] c = Zds[51];
  wire [0:0] ie = Zds[50];
  wire [7:0] pc = Zds[49:42];
  wire [0:0] zs = Zds[41];
  wire [0:0] cs = Zds[40];
  wire [7:0] pcs = Zds[39:32];
  wire [7:0] r0 = Zds[31:24];
  wire [7:0] r1 = Zds[23:16];
  wire [7:0] r2 = Zds[15:8];
  wire [7:0] r3 = Zds[7:0];
  assign res = {i, o, z, c, ie, pc, zs, cs, pcs, r0, r1, r2, r3};
endmodule

// Main.setAddrOut
module Main_setAddrOut (input logic [17:0] Zds,
  input logic [7:0] a_o,
  output logic [17:0] res);
  wire [7:0] d_o = Zds[9:2];
  wire [0:0] we_o = Zds[1];
  wire [0:0] iack_o = Zds[0];
  assign res = {a_o, d_o, we_o, iack_o};
endmodule

// Main.setDataOut
module Main_setDataOut (input logic [17:0] Zds,
  input logic [7:0] d_o,
  output logic [17:0] res);
  wire [7:0] a_o = Zds[17:10];
  wire [0:0] we_o = Zds[1];
  wire [0:0] iack_o = Zds[0];
  assign res = {a_o, d_o, we_o, iack_o};
endmodule

// Main.dataIn
module Main_dataIn (input logic [9:0] Zds,
  output logic [7:0] res);
  wire [7:0] d_i = Zds[9:2];
  assign res = d_i;
endmodule

// Main.setWeOut
module Main_setWeOut (input logic [17:0] Zds,
  input logic [0:0] we_o,
  output logic [17:0] res);
  wire [7:0] a_o = Zds[17:10];
  wire [7:0] d_o = Zds[9:2];
  wire [0:0] iack_o = Zds[0];
  assign res = {a_o, d_o, we_o, iack_o};
endmodule

// Main.lsbW8
module Main_lsbW8 (input logic [7:0] v,
  output logic [0:0] res);
  assign res = v[0];
endmodule

// Main.msbW8
module Main_msbW8 (input logic [7:0] v,
  output logic [0:0] res);
  assign res = v[7];
endmodule

// Main.plusCW8$sMain_oneW8_False_Bool
// partially applied from 'Main.plusCW8'
module Main_plusCW8$sMain__oneW8__False__Bool (input logic [7:0] a,
  output logic [8:0] res);
  wire [8:0] s = ({1'h0, a} + 9'h1) + 9'h0;
  assign res = {s[8], s[7:0]};
endmodule

// Main.minusCW8$sFalse_Bool
// partially applied from 'Main.minusCW8'
module Main_minusCW8$sFalse__Bool (input logic [7:0] a,
  input logic [7:0] b,
  output logic [8:0] res);
  wire [8:0] s = ({1'h0, a} - {1'h0, b}) - 9'h0;
  assign res = {s[8], s[7:0]};
endmodule