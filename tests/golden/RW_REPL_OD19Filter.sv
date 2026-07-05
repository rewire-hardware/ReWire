module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  input logic [31:0] __in1,
  output logic [0:0] __out0,
  output logic [0:0] __out1);
  logic [31:0] __st0;
  logic [31:0] __st0_next;
  logic [6:0] __st1;
  logic [6:0] __st1_next;
  logic [38:0] zi0;
  logic [38:0] main_putreg_out;
  logic [38:0] zi4;
  logic [72:0] zi5;
  logic [38:0] zi6;
  logic [72:0] zll_main_repl4_out;
  logic [70:0] main_getreg_out;
  logic [70:0] zi7;
  logic [31:0] zi8;
  logic [38:0] zi9;
  logic [72:0] zi10;
  logic [31:0] zi11;
  logic [38:0] zi12;
  logic [72:0] zll_main_repl1_out;
  logic [38:0] main_putreg_outR1;
  logic [38:0] zi13;
  logic [72:0] zi14;
  logic [38:0] zi15;
  logic [72:0] zll_main_repl4_outR1;
  logic [70:0] main_getreg_outR1;
  logic [70:0] zi16;
  logic [31:0] zi17;
  logic [38:0] zi18;
  logic [72:0] zi19;
  logic [31:0] zi20;
  logic [38:0] zi21;
  logic [72:0] zll_main_repl1_outR1;
  logic [70:0] main_getreg_outR2;
  logic [70:0] zi22;
  logic [31:0] zi23;
  logic [38:0] zi24;
  logic [72:0] zi25;
  logic [31:0] zi26;
  logic [38:0] zi27;
  logic [0:0] extres;
  logic [70:0] main_getreg_outR3;
  logic [70:0] zi28;
  logic [31:0] zi29;
  logic [38:0] zi30;
  logic [72:0] zi31;
  logic [31:0] zi32;
  logic [38:0] zi33;
  logic [0:0] extresR1;
  logic [70:0] main_getreg_outR4;
  logic [70:0] zi34;
  logic [31:0] zi35;
  logic [38:0] zi36;
  logic [72:0] zi37;
  logic [31:0] zi38;
  logic [38:0] zi39;
  logic [0:0] extresR2;
  logic [38:0] main_nextpc_out;
  logic [38:0] zi40;
  logic [72:0] zi41;
  logic [38:0] zi42;
  logic [72:0] zll_main_repl6_out;
  logic [72:0] zres;
  assign zi0 = {__st0, __st1};
  Main_putReg  inst (__in1, zi0, main_putreg_out);
  assign zi4 = main_putreg_out;
  assign zi5 = {{2'h1, {6'h20{1'h0}}}, zi4};
  assign zi6 = zi5[38:0];
  ZLL_Main_repl4  instR1 (zi6, zll_main_repl4_out);
  Main_getReg  instR2 (zi0, main_getreg_out);
  assign zi7 = main_getreg_out;
  assign zi8 = zi7[70:39];
  assign zi9 = zi7[38:0];
  assign zi10 = {2'h0, zi8, zi9};
  assign zi11 = zi10[70:39];
  assign zi12 = zi10[38:0];
  ZLL_Main_repl1  instR3 (__in1, zi11, zi12, zll_main_repl1_out);
  Main_putReg  instR4 (__in1, zi0, main_putreg_outR1);
  assign zi13 = main_putreg_outR1;
  assign zi14 = {{2'h1, {6'h20{1'h0}}}, zi13};
  assign zi15 = zi14[38:0];
  ZLL_Main_repl4  instR5 (zi15, zll_main_repl4_outR1);
  Main_getReg  instR6 (zi0, main_getreg_outR1);
  assign zi16 = main_getreg_outR1;
  assign zi17 = zi16[70:39];
  assign zi18 = zi16[38:0];
  assign zi19 = {2'h0, zi17, zi18};
  assign zi20 = zi19[70:39];
  assign zi21 = zi19[38:0];
  ZLL_Main_repl1  instR7 (__in1, zi20, zi21, zll_main_repl1_outR1);
  Main_getReg  instR8 (zi0, main_getreg_outR2);
  assign zi22 = main_getreg_outR2;
  assign zi23 = zi22[70:39];
  assign zi24 = zi22[38:0];
  assign zi25 = {2'h0, zi23, zi24};
  assign zi26 = zi25[70:39];
  assign zi27 = zi25[38:0];
  test2  instR9 (__in1, zi26, extres[0]);
  Main_getReg  instR10 (zi0, main_getreg_outR3);
  assign zi28 = main_getreg_outR3;
  assign zi29 = zi28[70:39];
  assign zi30 = zi28[38:0];
  assign zi31 = {2'h0, zi29, zi30};
  assign zi32 = zi31[70:39];
  assign zi33 = zi31[38:0];
  test3  instR11 (__in1, zi32, extresR1[0]);
  Main_getReg  instR12 (zi0, main_getreg_outR4);
  assign zi34 = main_getreg_outR4;
  assign zi35 = zi34[70:39];
  assign zi36 = zi34[38:0];
  assign zi37 = {2'h0, zi35, zi36};
  assign zi38 = zi37[70:39];
  assign zi39 = zi37[38:0];
  test4  instR13 (__in1, zi38, extresR2[0]);
  Main_nextPC  instR14 (zi0, main_nextpc_out);
  assign zi40 = main_nextpc_out;
  assign zi41 = {{2'h1, {6'h20{1'h0}}}, zi40};
  assign zi42 = zi41[38:0];
  ZLL_Main_repl6  instR15 (zi42, zll_main_repl6_out);
  assign zres = (__in0 == 1'h0) ? ((__st1 == 7'h0) ? zll_main_repl4_out : ((__st1 == 7'h1) ? zll_main_repl1_out : ((__st1 == 7'h2) ? zll_main_repl4_outR1 : ((__st1 == 7'h3) ? zll_main_repl1_outR1 : ((__st1 == 7'h4) ? {{1'h1, {6'h20{1'h0}}}, extres, zi27} : ((__st1 == 7'h5) ? {{1'h1, {6'h20{1'h0}}}, extresR1, zi33} : ((__st1 == 7'h6) ? {{1'h1, {6'h20{1'h0}}}, extresR2, zi39} : zll_main_repl6_out))))))) : {34'h200000002, zi0};
  assign __st0_next = zres[38:7];
  assign __st1_next = zres[6:0];
  assign __out0 = zres[40];
  assign __out1 = zres[39];
  initial {__st0, __st1} = {6'h27{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= {6'h27{1'h0}};
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_repl6 (input logic [38:0] arg0,
  output logic [72:0] res);
  assign res = {34'h200000002, arg0};
endmodule

module Main_getReg (input logic [38:0] arg0,
  output logic [70:0] res);
  logic [31:0] zi0;
  assign zi0 = arg0[38:7];
  assign res = {zi0, arg0};
endmodule

module ZLL_Main_repl4 (input logic [38:0] arg0,
  output logic [72:0] res);
  logic [38:0] main_nextpc_out;
  logic [38:0] zt0;
  logic [72:0] zt1;
  logic [38:0] s0;
  logic [72:0] zll_main_repl6_out;
  Main_nextPC  inst (arg0, main_nextpc_out);
  assign zt0 = main_nextpc_out;
  assign zt1 = {{2'h1, {6'h20{1'h0}}}, zt0};
  assign s0 = zt1[38:0];
  ZLL_Main_repl6  instR1 (s0, zll_main_repl6_out);
  assign res = zll_main_repl6_out;
endmodule

module Main_putReg (input logic [31:0] arg0,
  input logic [38:0] arg1,
  output logic [38:0] res);
  logic [6:0] zi0;
  assign zi0 = arg1[6:0];
  assign res = {arg0, zi0};
endmodule

module ZLL_Main_repl1 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  input logic [38:0] arg2,
  output logic [72:0] res);
  logic [0:0] extres;
  test1  inst (arg0, arg1, extres[0]);
  assign res = {{1'h1, {6'h20{1'h0}}}, extres, arg2};
endmodule

module Main_nextPC (input logic [38:0] arg0,
  output logic [38:0] res);
  logic [31:0] zi0;
  logic [6:0] zi1;
  assign zi0 = arg0[38:7];
  assign zi1 = arg0[6:0];
  assign res = {zi0, (zi1 == 7'h0) ? 7'h1 : ((zi1 == 7'h1) ? 7'h2 : ((zi1 == 7'h2) ? 7'h3 : ((zi1 == 7'h3) ? 7'h4 : ((zi1 == 7'h4) ? 7'h5 : ((zi1 == 7'h5) ? 7'h6 : ((zi1 == 7'h6) ? 7'h7 : ((zi1 == 7'h7) ? 7'h8 : ((zi1 == 7'h8) ? 7'h9 : ((zi1 == 7'h9) ? 7'ha : ((zi1 == 7'ha) ? 7'hb : ((zi1 == 7'hb) ? 7'hc : ((zi1 == 7'hc) ? 7'hd : ((zi1 == 7'hd) ? 7'he : ((zi1 == 7'he) ? 7'hf : ((zi1 == 7'hf) ? 7'h10 : ((zi1 == 7'h10) ? 7'h11 : ((zi1 == 7'h11) ? 7'h12 : ((zi1 == 7'h12) ? 7'h13 : ((zi1 == 7'h13) ? 7'h14 : ((zi1 == 7'h14) ? 7'h15 : ((zi1 == 7'h15) ? 7'h16 : ((zi1 == 7'h16) ? 7'h17 : ((zi1 == 7'h17) ? 7'h18 : ((zi1 == 7'h18) ? 7'h19 : ((zi1 == 7'h19) ? 7'h1a : ((zi1 == 7'h1a) ? 7'h1b : ((zi1 == 7'h1b) ? 7'h1c : ((zi1 == 7'h1c) ? 7'h1d : ((zi1 == 7'h1d) ? 7'h1e : ((zi1 == 7'h1e) ? 7'h1f : ((zi1 == 7'h1f) ? 7'h20 : ((zi1 == 7'h20) ? 7'h21 : ((zi1 == 7'h21) ? 7'h22 : ((zi1 == 7'h22) ? 7'h23 : ((zi1 == 7'h23) ? 7'h24 : ((zi1 == 7'h24) ? 7'h25 : ((zi1 == 7'h25) ? 7'h26 : ((zi1 == 7'h26) ? 7'h27 : ((zi1 == 7'h27) ? 7'h28 : ((zi1 == 7'h28) ? 7'h29 : ((zi1 == 7'h29) ? 7'h2a : ((zi1 == 7'h2a) ? 7'h2b : ((zi1 == 7'h2b) ? 7'h2c : ((zi1 == 7'h2c) ? 7'h2d : ((zi1 == 7'h2d) ? 7'h2e : ((zi1 == 7'h2e) ? 7'h2f : ((zi1 == 7'h2f) ? 7'h30 : ((zi1 == 7'h30) ? 7'h31 : ((zi1 == 7'h31) ? 7'h32 : ((zi1 == 7'h32) ? 7'h33 : ((zi1 == 7'h33) ? 7'h34 : ((zi1 == 7'h34) ? 7'h35 : ((zi1 == 7'h35) ? 7'h36 : ((zi1 == 7'h36) ? 7'h37 : ((zi1 == 7'h37) ? 7'h38 : ((zi1 == 7'h38) ? 7'h39 : ((zi1 == 7'h39) ? 7'h3a : ((zi1 == 7'h3a) ? 7'h3b : ((zi1 == 7'h3b) ? 7'h3c : ((zi1 == 7'h3c) ? 7'h3d : ((zi1 == 7'h3d) ? 7'h3e : ((zi1 == 7'h3e) ? 7'h3f : ((zi1 == 7'h3f) ? 7'h40 : ((zi1 == 7'h40) ? 7'h41 : ((zi1 == 7'h41) ? 7'h42 : ((zi1 == 7'h42) ? 7'h43 : ((zi1 == 7'h43) ? 7'h44 : ((zi1 == 7'h44) ? 7'h45 : ((zi1 == 7'h45) ? 7'h46 : ((zi1 == 7'h46) ? 7'h47 : ((zi1 == 7'h47) ? 7'h48 : ((zi1 == 7'h48) ? 7'h49 : ((zi1 == 7'h49) ? 7'h4a : ((zi1 == 7'h4a) ? 7'h4b : ((zi1 == 7'h4b) ? 7'h4c : ((zi1 == 7'h4c) ? 7'h4d : ((zi1 == 7'h4d) ? 7'h4e : ((zi1 == 7'h4e) ? 7'h4f : ((zi1 == 7'h4f) ? 7'h50 : ((zi1 == 7'h50) ? 7'h51 : ((zi1 == 7'h51) ? 7'h52 : ((zi1 == 7'h52) ? 7'h53 : ((zi1 == 7'h53) ? 7'h54 : ((zi1 == 7'h54) ? 7'h55 : ((zi1 == 7'h55) ? 7'h56 : ((zi1 == 7'h56) ? 7'h57 : ((zi1 == 7'h57) ? 7'h58 : ((zi1 == 7'h58) ? 7'h59 : ((zi1 == 7'h59) ? 7'h5a : ((zi1 == 7'h5a) ? 7'h5b : ((zi1 == 7'h5b) ? 7'h5c : ((zi1 == 7'h5c) ? 7'h5d : ((zi1 == 7'h5d) ? 7'h5e : ((zi1 == 7'h5e) ? 7'h5f : ((zi1 == 7'h5f) ? 7'h60 : ((zi1 == 7'h60) ? 7'h61 : ((zi1 == 7'h61) ? 7'h62 : ((zi1 == 7'h62) ? 7'h63 : ((zi1 == 7'h63) ? 7'h64 : ((zi1 == 7'h64) ? 7'h65 : ((zi1 == 7'h65) ? 7'h66 : ((zi1 == 7'h66) ? 7'h67 : ((zi1 == 7'h67) ? 7'h68 : ((zi1 == 7'h68) ? 7'h69 : ((zi1 == 7'h69) ? 7'h6a : ((zi1 == 7'h6a) ? 7'h6b : ((zi1 == 7'h6b) ? 7'h6c : ((zi1 == 7'h6c) ? 7'h6d : ((zi1 == 7'h6d) ? 7'h6e : ((zi1 == 7'h6e) ? 7'h6f : ((zi1 == 7'h6f) ? 7'h70 : ((zi1 == 7'h70) ? 7'h71 : ((zi1 == 7'h71) ? 7'h72 : ((zi1 == 7'h72) ? 7'h73 : ((zi1 == 7'h73) ? 7'h74 : ((zi1 == 7'h74) ? 7'h75 : ((zi1 == 7'h75) ? 7'h76 : ((zi1 == 7'h76) ? 7'h77 : ((zi1 == 7'h77) ? 7'h78 : ((zi1 == 7'h78) ? 7'h79 : ((zi1 == 7'h79) ? 7'h7a : ((zi1 == 7'h7a) ? 7'h7b : ((zi1 == 7'h7b) ? 7'h7c : ((zi1 == 7'h7c) ? 7'h7d : ((zi1 == 7'h7d) ? 7'h7e : 7'h0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))};
endmodule