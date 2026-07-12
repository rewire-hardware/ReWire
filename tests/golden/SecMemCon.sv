module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [45:0] __in0,
  output logic [32:0] __out0);
  logic [9:0] Mainzuzlzazgzuout;
  logic [0:0] Main_ack_reg_out;
  logic [31:0] Main_data_out_reg_out;
  logic [0:0] Main_ack_reg_outR1;
  logic [31:0] Main_data_out_reg_outR1;
  logic [240:0] main__unused2_out;
  logic [9:0] Main_addr_reg_out;
  logic [127:0] Main_memTweak_out;
  logic [240:0] main_set_ack_reg$sMain_S_Main_Bit_out;
  logic [9:0] Main_addr_reg_outR1;
  logic [31:0] Main_memLookup_$fail1_out;
  logic [240:0] main_set_ack_reg$sMain_S_Main_Bit_outR1;
  logic [127:0] Main_memTweak_outR1;
  logic [240:0] main_get_addr_reg4_out;
  logic [240:0] main_get_addr_reg4_outR1;
  logic [240:0] Zres;
  // state registers
  // __resumption_tag: 3 bits, init 0x4
  //   states: 0=$x4 1=$x6 2=$x10 3=_unused22 4=$x
  // __st0: 77 bits, init 0x60000000000000000
  // __st1: 128 bits, init 0x0
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [76:0] __st0;
  logic [76:0] __st0_next;
  logic [127:0] __st1;
  logic [127:0] __st1_next;
  // combinational logic
  wire [9:0] Zds2 = __in0[13:4];
  wire [1:0] Zds5 = __in0[1:0];
  wire [0:0] Zds3 = __in0[3];
  wire [0:0] Zds4 = __in0[2];
  Mainzuzlzazg  zlzazgzui (Zds5, Zds2, Mainzuzlzazgzuout);
  wire [1:0] partition_reg = __st0[76:75];
  wire [31:0] data_reg = __st0[64:33];
  wire [0:0] ack_reg = __st0[32];
  wire [31:0] data_out_reg = __st0[31:0];
  wire [76:0] Za = {partition_reg, Mainzuzlzazgzuout, data_reg, ack_reg, data_out_reg};
  wire [204:0] Za1 = {Za, __st1};
  wire [31:0] Zds1 = __in0[45:14];
  wire [127:0] m = Za1[127:0];
  wire [1:0] partition_regR1 = Za1[204:203];
  wire [9:0] addr_reg = Za1[202:193];
  wire [0:0] ack_regR1 = Za1[160];
  wire [31:0] data_out_regR1 = Za1[159:128];
  wire [76:0] ZaR1 = {partition_regR1, addr_reg, Zds1, ack_regR1, data_out_regR1};
  wire [204:0] Za1R1 = {ZaR1, m};
  wire [127:0] mR1 = Za1R1[127:0];
  wire [9:0] addr_regR1 = Za1R1[202:193];
  wire [31:0] data_regR1 = Za1R1[192:161];
  wire [0:0] ack_regR2 = Za1R1[160];
  wire [31:0] data_out_regR2 = Za1R1[159:128];
  wire [76:0] ZaR2 = {Zds5, addr_regR1, data_regR1, ack_regR2, data_out_regR2};
  wire [204:0] Za1R2 = {ZaR2, mR1};
  wire [127:0] mR2 = Za1R2[127:0];
  wire [1:0] partition_regR2 = Za1R2[204:203];
  wire [9:0] addr_regR2 = Za1R2[202:193];
  wire [31:0] data_regR2 = Za1R2[192:161];
  wire [31:0] data_out_regR3 = Za1R2[159:128];
  wire [76:0] ZaR3 = {partition_regR2, addr_regR2, data_regR2, 1'h0, data_out_regR3};
  wire [204:0] Za1R3 = {ZaR3, mR2};
  wire [127:0] mR3 = Za1R3[127:0];
  wire [1:0] partition_regR3 = Za1R3[204:203];
  wire [9:0] addr_regR3 = Za1R3[202:193];
  wire [31:0] data_regR3 = Za1R3[192:161];
  wire [0:0] ack_regR3 = Za1R3[160];
  wire [76:0] ZaR4 = {partition_regR3, addr_regR3, data_regR3, ack_regR3, {6'h20{1'h0}}};
  wire [204:0] Za1R4 = {ZaR4, mR3};
  wire [76:0] x1 = Za1R4[204:128];
  Main_ack__reg  ack_reg_i (x1, Main_ack_reg_out);
  Main_data__out__reg  data_out_reg_i (x1, Main_data_out_reg_out);
  wire [32:0] Za2 = {Main_ack_reg_out, Main_data_out_reg_out};
  wire [31:0] data_regR4 = Za1[192:161];
  wire [76:0] ZaR5 = {Zds5, addr_reg, data_regR4, ack_regR1, data_out_regR1};
  wire [204:0] Za1R5 = {ZaR5, m};
  wire [127:0] mR4 = Za1R5[127:0];
  wire [1:0] partition_regR4 = Za1R5[204:203];
  wire [9:0] addr_regR4 = Za1R5[202:193];
  wire [31:0] data_regR5 = Za1R5[192:161];
  wire [31:0] data_out_regR4 = Za1R5[159:128];
  wire [76:0] ZaR6 = {partition_regR4, addr_regR4, data_regR5, 1'h0, data_out_regR4};
  wire [204:0] Za1R6 = {ZaR6, mR4};
  wire [127:0] mR5 = Za1R6[127:0];
  wire [1:0] partition_regR5 = Za1R6[204:203];
  wire [9:0] addr_regR5 = Za1R6[202:193];
  wire [31:0] data_regR6 = Za1R6[192:161];
  wire [0:0] ack_regR4 = Za1R6[160];
  wire [76:0] ZaR7 = {partition_regR5, addr_regR5, data_regR6, ack_regR4, {6'h20{1'h0}}};
  wire [204:0] Za1R7 = {ZaR7, mR5};
  wire [76:0] x1R1 = Za1R7[204:128];
  Main_ack__reg  ack_reg_iR1 (x1R1, Main_ack_reg_outR1);
  Main_data__out__reg  data_out_reg_iR1 (x1R1, Main_data_out_reg_outR1);
  wire [32:0] Za2R1 = {Main_ack_reg_outR1, Main_data_out_reg_outR1};
  wire [9:0] addr_regR6 = __st0[74:65];
  wire [76:0] ZaR8 = {partition_reg, addr_regR6, data_reg, 1'h0, data_out_reg};
  wire [204:0] Za1R8 = {ZaR8, __st1};
  wire [127:0] mR6 = Za1R8[127:0];
  wire [1:0] partition_regR6 = Za1R8[204:203];
  wire [9:0] addr_regR7 = Za1R8[202:193];
  wire [31:0] data_regR7 = Za1R8[192:161];
  wire [0:0] ack_regR5 = Za1R8[160];
  wire [76:0] ZaR9 = {partition_regR6, addr_regR7, data_regR7, ack_regR5, {6'h20{1'h0}}};
  wire [204:0] Za1R9 = {ZaR9, mR6};
  main___unused2  _unused2_i (Za1R9, main__unused2_out);
  wire [76:0] ZaR10 = {partition_reg, addr_regR6, data_reg, ack_reg, {6'h20{1'h0}}};
  wire [204:0] Za1R10 = {ZaR10, __st1};
  wire [76:0] s = Za1R10[204:128];
  Main_addr__reg  addr_reg_i (s, Main_addr_reg_out);
  wire [31:0] Zds3R1 = Za1R10[192:161];
  wire [127:0] mR7 = Za1R10[127:0];
  Main_memTweak  memTweak_i (Main_addr_reg_out, Zds3R1, mR7, Main_memTweak_out);
  wire [204:0] Za1R11 = {s, Main_memTweak_out};
  main_set__ack__reg$sMain__S__Main__Bit  set_ack_reg$sMain_S_Main_Bit_i (Za1R11, main_set_ack_reg$sMain_S_Main_Bit_out);
  Main_addr__reg  addr_reg_iR1 (__st0, Main_addr_reg_outR1);
  wire [0:0] Zds1R1 = Main_addr_reg_outR1[9];
  wire [0:0] Zds2R1 = Main_addr_reg_outR1[8];
  wire [0:0] Zds3R2 = Main_addr_reg_outR1[7];
  wire [0:0] Zds4R1 = Main_addr_reg_outR1[6];
  wire [0:0] Zds5R1 = Main_addr_reg_outR1[5];
  wire [0:0] Zds6 = Main_addr_reg_outR1[4];
  wire [0:0] Zds7 = Main_addr_reg_outR1[3];
  wire [0:0] Zds8 = Main_addr_reg_outR1[2];
  wire [0:0] Zds9 = Main_addr_reg_outR1[1];
  wire [0:0] Zds10 = Main_addr_reg_outR1[0];
  wire [31:0] Zds1R2 = __st1[127:96];
  wire [31:0] Zds2R2 = __st1[95:64];
  wire [31:0] Zds3R3 = __st1[63:32];
  wire [31:0] Zds4R2 = __st1[31:0];
  Main_memLookup_$fail1  Zfail1_i (__st1, Main_memLookup_$fail1_out);
  wire [31:0] ZaR11 = (~Zds1R1) ? ((~Zds2R1) ? ((~Zds3R2) ? ((~Zds4R1) ? ((~Zds5R1) ? ((~Zds6) ? ((~Zds7) ? ((~Zds8) ? ((~Zds9) ? ((~Zds10) ? Zds1R2 : Zds2R2) :
    ((~Zds10) ? Zds3R3 : Zds4R2)) : Main_memLookup_$fail1_out) : Main_memLookup_$fail1_out) : Main_memLookup_$fail1_out) : Main_memLookup_$fail1_out) : Main_memLookup_$fail1_out) : Main_memLookup_$fail1_out) : Main_memLookup_$fail1_out) : Main_memLookup_$fail1_out;
  wire [76:0] ZaR12 = {partition_reg, addr_regR6, data_reg, ack_reg, ZaR11};
  wire [204:0] Za1R12 = {ZaR12, __st1};
  main_set__ack__reg$sMain__S__Main__Bit  set_ack_reg$sMain_S_Main_Bit_iR1 (Za1R12, main_set_ack_reg$sMain_S_Main_Bit_outR1);
  Main_memTweak  memTweak_iR1 (Main_addr_reg_outR1, {6'h20{1'h0}}, __st1, Main_memTweak_outR1);
  wire [204:0] Za1R13 = {__st0, Main_memTweak_outR1};
  wire [9:0] ZaR13 = (~Zds1R1) ? ((~Zds2R1) ? ((~Zds3R2) ? ((~Zds4R1) ? ((~Zds5R1) ? ((~Zds6) ? ((~Zds7) ? ((~Zds8) ? ((~Zds9) ? (Zds10 ? 10'h0 : 10'h3ff) :
    ((~Zds10) ? 10'h1 : 10'h2)) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff) : 10'h3ff;
  wire [127:0] mR8 = Za1R13[127:0];
  wire [1:0] partition_regR7 = Za1R13[204:203];
  wire [31:0] data_regR8 = Za1R13[192:161];
  wire [0:0] ack_regR6 = Za1R13[160];
  wire [31:0] data_out_regR5 = Za1R13[159:128];
  wire [76:0] ZaR14 = {partition_regR7, ZaR13, data_regR8, ack_regR6, data_out_regR5};
  wire [204:0] Za1R14 = {ZaR14, mR8};
  main_get__addr__reg4  get_addr_reg4_i (Za1R14, Za1R14, main_get_addr_reg4_out);
  wire [204:0] s0 = {__st0, __st1};
  main_get__addr__reg4  get_addr_reg4_iR1 (s0, s0, main_get_addr_reg4_outR1);
  localparam [2:0] ST_ZX4 = 3'h0;
  localparam [2:0] ST_ZX6 = 3'h1;
  localparam [2:0] ST_ZX10 = 3'h2;
  localparam [2:0] ST__UNUSED22 = 3'h3;
  localparam [2:0] ST_ZX = 3'h4;
  always_comb case (__resumption_tag)
    ST_ZX4: Zres = Zds3 ? ((~Zds4) ? {Za2, 3'h1, Za1R4} : {Za2R1, 3'h2, Za1R7}) : main__unused2_out;
    ST_ZX6: Zres = main_set_ack_reg$sMain_S_Main_Bit_out;
    ST_ZX10: Zres = main_set_ack_reg$sMain_S_Main_Bit_outR1;
    ST__UNUSED22: Zres = main_get_addr_reg4_out;
    default: Zres = main_get_addr_reg4_outR1;
  endcase
  assign __resumption_tag_next = Zres[207:205];
  assign __st0_next = Zres[204:128];
  assign __st1_next = Zres[127:0];
  // outputs
  assign __out0 = Zres[240:208];
  // state register update
  initial __resumption_tag = 3'h4;
  initial __st0 = {12'h3, {7'h41{1'h0}}};
  initial __st1 = {8'h80{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h4;
      __st0 <= {12'h3, {7'h41{1'h0}}};
      __st1 <= {8'h80{1'h0}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
      __st1 <= __st1_next;
    end
  end
endmodule

// main._unused2
// block '$L._unused2' of process main
module main___unused2 (input logic [204:0] s0,
  output logic [240:0] res);
  logic [0:0] Main_ack_reg_out;
  logic [31:0] Main_data_out_reg_out;
  wire [76:0] x1 = s0[204:128];
  Main_ack__reg  ack_reg_i (x1, Main_ack_reg_out);
  Main_data__out__reg  data_out_reg_i (x1, Main_data_out_reg_out);
  wire [32:0] Za2 = {Main_ack_reg_out, Main_data_out_reg_out};
  assign res = {Za2, 3'h0, s0};
endmodule

// main.set_ack_reg$sMain_S_Main_Bit
// block '$L.Main.set_ack_reg$sMain_S_Main_Bit' of process main
module main_set__ack__reg$sMain__S__Main__Bit (input logic [204:0] s0,
  output logic [240:0] res);
  logic [240:0] main__unused2_out;
  wire [127:0] m = s0[127:0];
  wire [1:0] partition_reg = s0[204:203];
  wire [9:0] addr_reg = s0[202:193];
  wire [31:0] data_reg = s0[192:161];
  wire [31:0] data_out_reg = s0[159:128];
  wire [76:0] Za = {partition_reg, addr_reg, data_reg, 1'h1, data_out_reg};
  wire [204:0] Za1 = {Za, m};
  main___unused2  _unused2_i (Za1, main__unused2_out);
  assign res = main__unused2_out;
endmodule

// main.get_addr_reg4
// block '$L.Main.get_addr_reg4' of process main
module main_get__addr__reg4 (input logic [204:0] x,
  input logic [204:0] s0,
  output logic [240:0] res);
  logic [9:0] Main_addr_reg_out;
  logic [0:0] Main_ack_reg_out;
  logic [31:0] Main_data_out_reg_out;
  logic [0:0] Main_ack_reg_outR1;
  logic [31:0] Main_data_out_reg_outR1;
  wire [76:0] s = s0[204:128];
  Main_addr__reg  addr_reg_i (s, Main_addr_reg_out);
  wire [0:0] Zds1 = Main_addr_reg_out[9];
  wire [0:0] Zds2 = Main_addr_reg_out[8];
  wire [0:0] Zds3 = Main_addr_reg_out[7];
  wire [0:0] Zds4 = Main_addr_reg_out[6];
  wire [0:0] Zds5 = Main_addr_reg_out[5];
  wire [0:0] Zds6 = Main_addr_reg_out[4];
  wire [0:0] Zds7 = Main_addr_reg_out[3];
  wire [0:0] Zds8 = Main_addr_reg_out[2];
  wire [0:0] Zds9 = Main_addr_reg_out[1];
  wire [0:0] Zds10 = Main_addr_reg_out[0];
  wire [0:0] Za = Zds1 ? (Zds2 ? (Zds3 ? (Zds4 ? (Zds5 ? (Zds6 ? (Zds7 ? (Zds8 ? (Zds9 ? (~Zds10) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1) : 1'h1;
  Main_ack__reg  ack_reg_i (s, Main_ack_reg_out);
  Main_data__out__reg  data_out_reg_i (s, Main_data_out_reg_out);
  wire [32:0] Za2 = {Main_ack_reg_out, Main_data_out_reg_out};
  wire [76:0] x1 = x[204:128];
  Main_ack__reg  ack_reg_iR1 (x1, Main_ack_reg_outR1);
  Main_data__out__reg  data_out_reg_iR1 (x1, Main_data_out_reg_outR1);
  wire [32:0] Za2R1 = {Main_ack_reg_outR1, Main_data_out_reg_outR1};
  assign res = (~Za) ? {Za2, 3'h0, s0} : {Za2R1, 3'h3, x};
endmodule

// Main.data_out_reg
module Main_data__out__reg (input logic [76:0] Zds,
  output logic [31:0] res);
  wire [31:0] Zds5 = Zds[31:0];
  assign res = Zds5;
endmodule

// Main.ack_reg
module Main_ack__reg (input logic [76:0] Zds,
  output logic [0:0] res);
  wire [0:0] Zds4 = Zds[32];
  assign res = Zds4;
endmodule

// Main.addr_reg
module Main_addr__reg (input logic [76:0] Zds,
  output logic [9:0] res);
  wire [9:0] Zds2 = Zds[74:65];
  assign res = Zds2;
endmodule

// Main.memLookup.$fail1
module Main_memLookup_$fail1 (input logic [127:0] Zeta0,
  output logic [31:0] res);
  assign res = {6'h20{1'h0}};
endmodule

// Main.memTweak
module Main_memTweak (input logic [9:0] Zds,
  input logic [31:0] v,
  input logic [127:0] m,
  output logic [127:0] res);
  wire [0:0] Zds1 = Zds[9];
  wire [0:0] Zds2 = Zds[8];
  wire [0:0] Zds3 = Zds[7];
  wire [0:0] Zds4 = Zds[6];
  wire [0:0] Zds5 = Zds[5];
  wire [0:0] Zds6 = Zds[4];
  wire [0:0] Zds7 = Zds[3];
  wire [0:0] Zds8 = Zds[2];
  wire [0:0] Zds9 = Zds[1];
  wire [0:0] Zds10 = Zds[0];
  wire [31:0] mem1 = m[95:64];
  wire [31:0] mem2 = m[63:32];
  wire [31:0] mem3 = m[31:0];
  wire [31:0] mem0 = m[127:96];
  assign res = (~Zds1) ? ((~Zds2) ? ((~Zds3) ? ((~Zds4) ? ((~Zds5) ? ((~Zds6) ? ((~Zds7) ? ((~Zds8) ? ((~Zds9) ? ((~Zds10) ? {v, mem1, mem2, mem3} : {mem0, v, mem2, mem3}) :
    ((~Zds10) ? {mem0, mem1, v, mem3} : {mem0, mem1, mem2, v})) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}}) : {8'h80{1'h0}};
endmodule

// Main.<&>
module Mainzuzlzazg (input logic [1:0] Zds,
  input logic [9:0] Zds1,
  output logic [9:0] res);
  wire [0:0] b0 = Zds[1];
  wire [0:0] b1 = Zds[0];
  wire [0:0] b2 = Zds1[7];
  wire [0:0] b3 = Zds1[6];
  wire [0:0] b4 = Zds1[5];
  wire [0:0] b5 = Zds1[4];
  wire [0:0] b6 = Zds1[3];
  wire [0:0] b7 = Zds1[2];
  wire [0:0] b8 = Zds1[1];
  wire [0:0] b9 = Zds1[0];
  assign res = {b0, b1, b2, b3, b4, b5, b6, b7, b8, b9};
endmodule