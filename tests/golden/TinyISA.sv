module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [16:0] __in0,
  output logic [14:0] __out0);
  logic [87:0] main_getIns2_out;
  logic [16:0] Main_inputs_out;
  logic [87:0] main_getIns2_outR1;
  logic [5:0] Main_pc_out;
  logic [5:0] Main_pc_outR1;
  logic [14:0] Main_outputs_out;
  logic [14:0] Main_outputs_outR1;
  logic [14:0] Main_outputs_outR2;
  logic [87:0] main_getPC2_out;
  logic [87:0] main_getPC_out;
  logic [87:0] Zres;
  // state registers
  // __resumption_tag: 3 bits, init 0x4
  //   states: 0=i 1=i3 2=i4 3=i5 4=i8
  // __st0: 70 bits, init 0x0
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [69:0] __st0;
  logic [69:0] __st0_next;
  // combinational logic
  wire [7:0] r0 = __st0[69:62];
  wire [7:0] r1 = __st0[61:54];
  wire [7:0] r2 = __st0[53:46];
  wire [7:0] r3 = __st0[45:38];
  wire [5:0] pc = __st0[37:32];
  wire [14:0] outputs = __st0[14:0];
  wire [69:0] Za = {r0, r1, r2, r3, pc, __in0, outputs};
  main_getIns2  getIns2_i (Za, main_getIns2_out);
  Main_inputs  inputs_i (Za, Main_inputs_out);
  wire [7:0] Zds2 = Main_inputs_out[7:0];
  wire [7:0] r1R1 = Za[61:54];
  wire [7:0] r2R1 = Za[53:46];
  wire [7:0] r3R1 = Za[45:38];
  wire [5:0] pcR1 = Za[37:32];
  wire [16:0] inputs = Za[31:15];
  wire [14:0] outputsR1 = Za[14:0];
  wire [69:0] ZaR1 = {Zds2, r1R1, r2R1, r3R1, pcR1, inputs, outputsR1};
  main_getIns2  getIns2_iR1 (ZaR1, main_getIns2_outR1);
  Main_pc  pc_i (Za, Main_pc_out);
  wire [5:0] a = Main_pc_out + 6'h1;
  wire [7:0] r0R1 = Za[69:62];
  wire [69:0] ZaR2 = {r0R1, r1R1, r2R1, r3R1, a, inputs, outputsR1};
  Main_pc  pc_iR1 (ZaR2, Main_pc_outR1);
  Main_outputs  outputs_i (ZaR2, Main_outputs_out);
  wire [0:0] weOut = Main_outputs_out[14];
  wire [7:0] dataOut = Main_outputs_out[7:0];
  wire [14:0] ZaR3 = {weOut, Main_pc_outR1, dataOut};
  wire [7:0] r0R2 = ZaR2[69:62];
  wire [7:0] r1R2 = ZaR2[61:54];
  wire [7:0] r2R2 = ZaR2[53:46];
  wire [7:0] r3R2 = ZaR2[45:38];
  wire [5:0] pcR2 = ZaR2[37:32];
  wire [16:0] inputsR1 = ZaR2[31:15];
  wire [69:0] ZaR4 = {r0R2, r1R2, r2R2, r3R2, pcR2, inputsR1, ZaR3};
  Main_outputs  outputs_iR1 (ZaR4, Main_outputs_outR1);
  wire [5:0] addrOut = Main_outputs_outR1[13:8];
  wire [7:0] dataOutR1 = Main_outputs_outR1[7:0];
  wire [14:0] ZaR5 = {1'h0, addrOut, dataOutR1};
  wire [7:0] r0R3 = ZaR4[69:62];
  wire [7:0] r1R3 = ZaR4[61:54];
  wire [7:0] r2R3 = ZaR4[53:46];
  wire [7:0] r3R3 = ZaR4[45:38];
  wire [5:0] pcR3 = ZaR4[37:32];
  wire [16:0] inputsR2 = ZaR4[31:15];
  wire [69:0] ZaR6 = {r0R3, r1R3, r2R3, r3R3, pcR3, inputsR2, ZaR5};
  Main_outputs  outputs_iR2 (ZaR6, Main_outputs_outR2);
  main_getPC2  getPC2_i (Za, main_getPC2_out);
  main_getPC  getPC_i (Za, main_getPC_out);
  localparam [2:0] ST_I = 3'h0;
  localparam [2:0] ST_I3 = 3'h1;
  localparam [2:0] ST_I4 = 3'h2;
  localparam [2:0] ST_I5 = 3'h3;
  localparam [2:0] ST_I8 = 3'h4;
  always_comb case (__resumption_tag)
    ST_I: Zres = main_getIns2_out;
    ST_I3: Zres = main_getIns2_outR1;
    ST_I4: Zres = {Main_outputs_outR2, 3'h1, ZaR6};
    ST_I5: Zres = main_getPC2_out;
    default: Zres = main_getPC_out;
  endcase
  assign __resumption_tag_next = Zres[72:70];
  assign __st0_next = Zres[69:0];
  // outputs
  assign __out0 = Zres[87:73];
  // state register update
  initial __resumption_tag = 3'h4;
  initial __st0 = {7'h46{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h4;
      __st0 <= {7'h46{1'h0}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule

// main.getPC
// block '$L.Main.getPC' of process main
module main_getPC (input logic [69:0] s0,
  output logic [87:0] res);
  logic [5:0] Main_pc_out;
  logic [14:0] Main_outputs_out;
  logic [14:0] Main_outputs_outR1;
  logic [14:0] Main_outputs_outR2;
  Main_pc  pc_i (s0, Main_pc_out);
  Main_outputs  outputs_i (s0, Main_outputs_out);
  wire [0:0] weOut = Main_outputs_out[14];
  wire [7:0] dataOut = Main_outputs_out[7:0];
  wire [14:0] Za = {weOut, Main_pc_out, dataOut};
  wire [7:0] r0 = s0[69:62];
  wire [7:0] r1 = s0[61:54];
  wire [7:0] r2 = s0[53:46];
  wire [7:0] r3 = s0[45:38];
  wire [5:0] pc = s0[37:32];
  wire [16:0] inputs = s0[31:15];
  wire [69:0] ZaR1 = {r0, r1, r2, r3, pc, inputs, Za};
  Main_outputs  outputs_iR1 (ZaR1, Main_outputs_outR1);
  wire [5:0] addrOut = Main_outputs_outR1[13:8];
  wire [7:0] dataOutR1 = Main_outputs_outR1[7:0];
  wire [14:0] ZaR2 = {1'h0, addrOut, dataOutR1};
  wire [7:0] r0R1 = ZaR1[69:62];
  wire [7:0] r1R1 = ZaR1[61:54];
  wire [7:0] r2R1 = ZaR1[53:46];
  wire [7:0] r3R1 = ZaR1[45:38];
  wire [5:0] pcR1 = ZaR1[37:32];
  wire [16:0] inputsR1 = ZaR1[31:15];
  wire [69:0] ZaR3 = {r0R1, r1R1, r2R1, r3R1, pcR1, inputsR1, ZaR2};
  Main_outputs  outputs_iR2 (ZaR3, Main_outputs_outR2);
  assign res = {Main_outputs_outR2, 3'h0, ZaR3};
endmodule

// main.putPC
// block '$L.Main.putPC' of process main
module main_putPC (input logic [5:0] a,
  input logic [69:0] s0,
  output logic [87:0] res);
  logic [87:0] main_getPC_out;
  wire [7:0] r0 = s0[69:62];
  wire [7:0] r1 = s0[61:54];
  wire [7:0] r2 = s0[53:46];
  wire [7:0] r3 = s0[45:38];
  wire [16:0] inputs = s0[31:15];
  wire [14:0] outputs = s0[14:0];
  wire [69:0] Za = {r0, r1, r2, r3, a, inputs, outputs};
  main_getPC  getPC_i (Za, main_getPC_out);
  assign res = main_getPC_out;
endmodule

// main.getPC2
// block '$L.Main.getPC2' of process main
module main_getPC2 (input logic [69:0] s0,
  output logic [87:0] res);
  logic [5:0] Main_pc_out;
  logic [87:0] main_putPC_out;
  Main_pc  pc_i (s0, Main_pc_out);
  main_putPC  putPC_i (Main_pc_out + 6'h1, s0, main_putPC_out);
  assign res = main_putPC_out;
endmodule

// main.putReg
// block '$L.Main.putReg' of process main
module main_putReg (input logic [1:0] Zds,
  input logic [7:0] b,
  input logic [69:0] s0,
  output logic [87:0] res);
  logic [87:0] main_getPC2_out;
  logic [87:0] main_getPC2_outR1;
  logic [87:0] main_getPC2_outR2;
  logic [87:0] main_getPC2_outR3;
  wire [7:0] r1 = s0[61:54];
  wire [7:0] r2 = s0[53:46];
  wire [7:0] r3 = s0[45:38];
  wire [5:0] pc = s0[37:32];
  wire [16:0] inputs = s0[31:15];
  wire [14:0] outputs = s0[14:0];
  wire [69:0] Za = {b, r1, r2, r3, pc, inputs, outputs};
  main_getPC2  getPC2_i (Za, main_getPC2_out);
  wire [7:0] r0 = s0[69:62];
  wire [69:0] ZaR1 = {r0, b, r2, r3, pc, inputs, outputs};
  main_getPC2  getPC2_iR1 (ZaR1, main_getPC2_outR1);
  wire [69:0] ZaR2 = {r0, r1, b, r3, pc, inputs, outputs};
  main_getPC2  getPC2_iR2 (ZaR2, main_getPC2_outR2);
  wire [69:0] ZaR3 = {r0, r1, r2, b, pc, inputs, outputs};
  main_getPC2  getPC2_iR3 (ZaR3, main_getPC2_outR3);
  always_comb case (Zds)
    2'h0: res = main_getPC2_out;
    2'h1: res = main_getPC2_outR1;
    2'h2: res = main_getPC2_outR2;
    default: res = main_getPC2_outR3;
  endcase
endmodule

// main.getReg
// block '$L.Main.getReg' of process main
module main_getReg (input logic [1:0] rd,
  input logic [7:0] a,
  input logic [1:0] Zds,
  input logic [69:0] s0,
  output logic [87:0] res);
  logic [7:0] Main_r0_out;
  logic [87:0] main_putReg_out;
  logic [7:0] Main_r1_out;
  logic [87:0] main_putReg_outR1;
  logic [7:0] Main_r2_out;
  logic [87:0] main_putReg_outR2;
  logic [7:0] Main_r3_out;
  logic [87:0] main_putReg_outR3;
  Main_r0  r0_i (s0, Main_r0_out);
  main_putReg  putReg_i (rd, ~(a & Main_r0_out), s0, main_putReg_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_putReg  putReg_iR1 (rd, ~(a & Main_r1_out), s0, main_putReg_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_putReg  putReg_iR2 (rd, ~(a & Main_r2_out), s0, main_putReg_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_putReg  putReg_iR3 (rd, ~(a & Main_r3_out), s0, main_putReg_outR3);
  always_comb case (Zds)
    2'h0: res = main_putReg_out;
    2'h1: res = main_putReg_outR1;
    2'h2: res = main_putReg_outR2;
    default: res = main_putReg_outR3;
  endcase
endmodule

// main.getIns2
// block '$L.Main.getIns2' of process main
module main_getIns2 (input logic [69:0] s0,
  output logic [87:0] res);
  logic [16:0] Main_inputs_out;
  logic [87:0] main_getPC2_out;
  logic [14:0] Main_outputs_out;
  logic [14:0] Main_outputs_outR1;
  logic [14:0] Main_outputs_outR2;
  logic [7:0] Main_r0_out;
  logic [14:0] Main_outputs_outR3;
  logic [14:0] Main_outputs_outR4;
  logic [87:0] main_getReg_out;
  logic [7:0] Main_r1_out;
  logic [87:0] main_getReg_outR1;
  logic [7:0] Main_r2_out;
  logic [87:0] main_getReg_outR2;
  logic [7:0] Main_r3_out;
  logic [87:0] main_getReg_outR3;
  logic [87:0] main_putPC_out;
  Main_inputs  inputs_i (s0, Main_inputs_out);
  wire [8:0] Zds1 = Main_inputs_out[16:8];
  main_getPC2  getPC2_i (s0, main_getPC2_out);
  wire [5:0] a = Zds1[5:0];
  Main_outputs  outputs_i (s0, Main_outputs_out);
  wire [0:0] weOut = Main_outputs_out[14];
  wire [7:0] dataOut = Main_outputs_out[7:0];
  wire [14:0] Za = {weOut, a, dataOut};
  wire [7:0] r0 = s0[69:62];
  wire [7:0] r1 = s0[61:54];
  wire [7:0] r2 = s0[53:46];
  wire [7:0] r3 = s0[45:38];
  wire [5:0] pc = s0[37:32];
  wire [16:0] inputs = s0[31:15];
  wire [69:0] ZaR1 = {r0, r1, r2, r3, pc, inputs, Za};
  Main_outputs  outputs_iR1 (ZaR1, Main_outputs_outR1);
  wire [5:0] addrOut = Main_outputs_outR1[13:8];
  wire [7:0] dataOutR1 = Main_outputs_outR1[7:0];
  wire [14:0] ZaR2 = {1'h0, addrOut, dataOutR1};
  wire [7:0] r0R1 = ZaR1[69:62];
  wire [7:0] r1R1 = ZaR1[61:54];
  wire [7:0] r2R1 = ZaR1[53:46];
  wire [7:0] r3R1 = ZaR1[45:38];
  wire [5:0] pcR1 = ZaR1[37:32];
  wire [16:0] inputsR1 = ZaR1[31:15];
  wire [69:0] ZaR3 = {r0R1, r1R1, r2R1, r3R1, pcR1, inputsR1, ZaR2};
  Main_outputs  outputs_iR2 (ZaR3, Main_outputs_outR2);
  Main_r0  r0_i (s0, Main_r0_out);
  wire [0:0] weOutR1 = Main_outputs_outR1[14];
  wire [14:0] ZaR4 = {weOutR1, addrOut, Main_r0_out};
  wire [69:0] ZaR5 = {r0R1, r1R1, r2R1, r3R1, pcR1, inputsR1, ZaR4};
  Main_outputs  outputs_iR3 (ZaR5, Main_outputs_outR3);
  wire [5:0] addrOutR1 = Main_outputs_outR3[13:8];
  wire [7:0] dataOutR2 = Main_outputs_outR3[7:0];
  wire [14:0] ZaR6 = {1'h1, addrOutR1, dataOutR2};
  wire [7:0] r0R2 = ZaR5[69:62];
  wire [7:0] r1R2 = ZaR5[61:54];
  wire [7:0] r2R2 = ZaR5[53:46];
  wire [7:0] r3R2 = ZaR5[45:38];
  wire [5:0] pcR2 = ZaR5[37:32];
  wire [16:0] inputsR2 = ZaR5[31:15];
  wire [69:0] ZaR7 = {r0R2, r1R2, r2R2, r3R2, pcR2, inputsR2, ZaR6};
  Main_outputs  outputs_iR4 (ZaR7, Main_outputs_outR4);
  wire [1:0] rd = Zds1[5:4];
  wire [1:0] r1R3 = Zds1[3:2];
  wire [1:0] r2R3 = Zds1[1:0];
  main_getReg  getReg_i (rd, Main_r0_out, r2R3, s0, main_getReg_out);
  Main_r1  r1_i (s0, Main_r1_out);
  main_getReg  getReg_iR1 (rd, Main_r1_out, r2R3, s0, main_getReg_outR1);
  Main_r2  r2_i (s0, Main_r2_out);
  main_getReg  getReg_iR2 (rd, Main_r2_out, r2R3, s0, main_getReg_outR2);
  Main_r3  r3_i (s0, Main_r3_out);
  main_getReg  getReg_iR3 (rd, Main_r3_out, r2R3, s0, main_getReg_outR3);
  wire [0:0] ZaR8 = Main_r0_out == 8'h0;
  main_putPC  putPC_i (a, s0, main_putPC_out);
  wire [2:0] scrut = Zds1[8:6];
  always_comb case (scrut)
    3'h0: res = main_getPC2_out;
    3'h1: res = {Main_outputs_outR2, 3'h2, ZaR3};
    3'h2: res = {Main_outputs_outR4, 3'h3, ZaR7};
    3'h3: res = (r1R3 == 2'h0) ? main_getReg_out :
      ((r1R3 == 2'h1) ? main_getReg_outR1 :
        ((r1R3 == 2'h2) ? main_getReg_outR2 : main_getReg_outR3));
    default: res = (~ZaR8) ? main_putPC_out : main_getPC2_out;
  endcase
endmodule

// Main.outputs
module Main_outputs (input logic [69:0] Zds,
  output logic [14:0] res);
  wire [14:0] Zds7 = Zds[14:0];
  assign res = Zds7;
endmodule

// Main.inputs
module Main_inputs (input logic [69:0] Zds,
  output logic [16:0] res);
  wire [16:0] Zds6 = Zds[31:15];
  assign res = Zds6;
endmodule

// Main.pc
module Main_pc (input logic [69:0] Zds,
  output logic [5:0] res);
  wire [5:0] Zds5 = Zds[37:32];
  assign res = Zds5;
endmodule

// Main.r3
module Main_r3 (input logic [69:0] Zds,
  output logic [7:0] res);
  wire [7:0] Zds4 = Zds[45:38];
  assign res = Zds4;
endmodule

// Main.r2
module Main_r2 (input logic [69:0] Zds,
  output logic [7:0] res);
  wire [7:0] Zds3 = Zds[53:46];
  assign res = Zds3;
endmodule

// Main.r1
module Main_r1 (input logic [69:0] Zds,
  output logic [7:0] res);
  wire [7:0] Zds2 = Zds[61:54];
  assign res = Zds2;
endmodule

// Main.r0
module Main_r0 (input logic [69:0] Zds,
  output logic [7:0] res);
  wire [7:0] Zds1 = Zds[69:62];
  assign res = Zds1;
endmodule