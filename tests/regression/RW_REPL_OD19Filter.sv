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
  logic [71:0] zin;
  logic [38:0] zi0;
  logic [32:0] zi1;
  logic [71:0] zi2;
  logic [32:0] zi3;
  logic [38:0] zi4;
  logic [71:0] zi5;
  logic [38:0] zi6;
  logic [31:0] zi7;
  logic [77:0] zi8;
  logic [38:0] zi9;
  logic [38:0] zi10;
  logic [80:0] zi11;
  logic [112:0] zi12;
  logic [31:0] zi13;
  logic [38:0] zi14;
  logic [38:0] zi15;
  logic [109:0] zi16;
  logic [38:0] zi17;
  logic [31:0] zi18;
  logic [31:0] zi19;
  logic [6:0] zi20;
  logic [77:0] zi21;
  logic [80:0] zll_main_repl77_out;
  logic [77:0] zi22;
  logic [80:0] zll_main_repl112_out;
  logic [77:0] zi23;
  logic [80:0] zll_main_repl77_outR1;
  logic [77:0] zi24;
  logic [80:0] zll_main_repl112_outR1;
  logic [77:0] zi25;
  logic [38:0] zi26;
  logic [31:0] zi27;
  logic [70:0] main_getreg_out;
  logic [80:0] zll_main_repl92_out;
  logic [80:0] zi28;
  logic [112:0] zi29;
  logic [31:0] zi30;
  logic [31:0] zi31;
  logic [38:0] zi32;
  logic [0:0] extres;
  logic [77:0] zi33;
  logic [38:0] zi34;
  logic [31:0] zi35;
  logic [70:0] main_getreg_outR1;
  logic [80:0] zll_main_repl92_outR1;
  logic [80:0] zi36;
  logic [112:0] zi37;
  logic [31:0] zi38;
  logic [31:0] zi39;
  logic [38:0] zi40;
  logic [0:0] extresR1;
  logic [77:0] zi41;
  logic [38:0] zi42;
  logic [31:0] zi43;
  logic [70:0] main_getreg_outR2;
  logic [80:0] zll_main_repl92_outR2;
  logic [80:0] zi44;
  logic [112:0] zi45;
  logic [31:0] zi46;
  logic [31:0] zi47;
  logic [38:0] zi48;
  logic [0:0] extresR2;
  logic [38:0] main_nextpc_out;
  logic [80:0] zll_main_repl101_out;
  logic [80:0] zll_main_repl43_out;
  logic [71:0] zi49;
  logic [38:0] zi50;
  logic [80:0] zres;
  assign zin = {__st0, __st1, __in0, __in1};
  assign zi0 = zin[71:33];
  assign zi1 = zin[32:0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[71:39];
  assign zi4 = zi2[38:0];
  assign zi5 = {zi4, zi3};
  assign zi6 = zi5[71:33];
  assign zi7 = zi5[31:0];
  assign zi8 = {zi6, zi6};
  assign zi9 = zi8[77:39];
  assign zi10 = zi8[38:0];
  assign zi11 = {3'h2, zi9, zi10};
  assign zi12 = {zi7, zi11};
  assign zi13 = zi12[112:81];
  assign zi14 = zi12[77:39];
  assign zi15 = zi12[38:0];
  assign zi16 = {zi15, zi13, zi14};
  assign zi17 = zi16[109:71];
  assign zi18 = zi16[70:39];
  assign zi19 = zi16[38:7];
  assign zi20 = zi16[6:0];
  assign zi21 = {zi17, zi18, zi20};
  ZLL_Main_repl77  inst (zi21[77:39], zi21[38:7], zll_main_repl77_out);
  assign zi22 = {zi17, zi18, zi20};
  ZLL_Main_repl112  instR1 (zi22[77:39], zi22[38:7], zll_main_repl112_out);
  assign zi23 = {zi17, zi18, zi20};
  ZLL_Main_repl77  instR2 (zi23[77:39], zi23[38:7], zll_main_repl77_outR1);
  assign zi24 = {zi17, zi18, zi20};
  ZLL_Main_repl112  instR3 (zi24[77:39], zi24[38:7], zll_main_repl112_outR1);
  assign zi25 = {zi17, zi18, zi20};
  assign zi26 = zi25[77:39];
  assign zi27 = zi25[38:7];
  Main_getReg  instR4 (zi26, main_getreg_out);
  ZLL_Main_repl92  instR5 (main_getreg_out, zll_main_repl92_out);
  assign zi28 = zll_main_repl92_out;
  assign zi29 = {zi27, zi28};
  assign zi30 = zi29[112:81];
  assign zi31 = zi29[70:39];
  assign zi32 = zi29[38:0];
  test2  instR6 (zi30, zi31, extres[0]);
  assign zi33 = {zi17, zi18, zi20};
  assign zi34 = zi33[77:39];
  assign zi35 = zi33[38:7];
  Main_getReg  instR7 (zi34, main_getreg_outR1);
  ZLL_Main_repl92  instR8 (main_getreg_outR1, zll_main_repl92_outR1);
  assign zi36 = zll_main_repl92_outR1;
  assign zi37 = {zi35, zi36};
  assign zi38 = zi37[112:81];
  assign zi39 = zi37[70:39];
  assign zi40 = zi37[38:0];
  test3  instR9 (zi38, zi39, extresR1[0]);
  assign zi41 = {zi17, zi18, zi20};
  assign zi42 = zi41[77:39];
  assign zi43 = zi41[38:7];
  Main_getReg  instR10 (zi42, main_getreg_outR2);
  ZLL_Main_repl92  instR11 (main_getreg_outR2, zll_main_repl92_outR2);
  assign zi44 = zll_main_repl92_outR2;
  assign zi45 = {zi43, zi44};
  assign zi46 = zi45[112:81];
  assign zi47 = zi45[70:39];
  assign zi48 = zi45[38:0];
  test4  instR12 (zi46, zi47, extresR2[0]);
  Main_nextPC  instR13 (zi17, main_nextpc_out);
  ZLL_Main_repl101  instR14 (main_nextpc_out, zll_main_repl101_out);
  ZLL_Main_repl43  instR15 (zll_main_repl101_out, zll_main_repl43_out);
  assign zi49 = {zi4, zi3};
  assign zi50 = zi49[71:33];
  assign zres = (zi5[32] == 1'h0) ? ((zi21[6:0] == 7'h0) ? zll_main_repl77_out : ((zi22[6:0] == 7'h1) ? zll_main_repl112_out : ((zi23[6:0] == 7'h2) ? zll_main_repl77_outR1 : ((zi24[6:0] == 7'h3) ? zll_main_repl112_outR1 : ((zi25[6:0] == 7'h4) ? {{1'h1, {6'h28{1'h0}}}, extres, zi32} : ((zi33[6:0] == 7'h5) ? {{1'h1, {6'h28{1'h0}}}, extresR1, zi40} : ((zi41[6:0] == 7'h6) ? {{1'h1, {6'h28{1'h0}}}, extresR2, zi48} : zll_main_repl43_out))))))) : {42'h20000000002, zi50};
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

module ZLL_Main_repl112 (input logic [38:0] arg0,
  input logic [31:0] arg1,
  output logic [80:0] res);
  logic [70:0] main_getreg_out;
  logic [80:0] zll_main_repl92_out;
  logic [80:0] zi0;
  logic [112:0] zi1;
  logic [31:0] zi2;
  logic [31:0] zi3;
  logic [38:0] zi4;
  logic [0:0] extres;
  Main_getReg  inst (arg0, main_getreg_out);
  ZLL_Main_repl92  instR1 (main_getreg_out, zll_main_repl92_out);
  assign zi0 = zll_main_repl92_out;
  assign zi1 = {arg1, zi0};
  assign zi2 = zi1[112:81];
  assign zi3 = zi1[70:39];
  assign zi4 = zi1[38:0];
  test1  instR2 (zi2, zi3, extres[0]);
  assign res = {{1'h1, {6'h28{1'h0}}}, extres, zi4};
endmodule

module ZLL_Main_repl101 (input logic [38:0] arg0,
  output logic [80:0] res);
  assign res = {{3'h1, {6'h27{1'h0}}}, arg0};
endmodule

module ZLL_Main_repl92 (input logic [70:0] arg0,
  output logic [80:0] res);
  logic [31:0] zi0;
  logic [38:0] zi1;
  assign zi0 = arg0[70:39];
  assign zi1 = arg0[38:0];
  assign res = {10'h0, zi0, zi1};
endmodule

module Main_getReg (input logic [38:0] arg0,
  output logic [70:0] res);
  logic [77:0] zi0;
  logic [38:0] zi1;
  logic [38:0] zi2;
  logic [77:0] zi3;
  logic [38:0] zi4;
  logic [31:0] zi5;
  logic [6:0] zi6;
  assign zi0 = {arg0, arg0};
  assign zi1 = zi0[77:39];
  assign zi2 = zi0[38:0];
  assign zi3 = {zi2, zi1};
  assign zi4 = zi3[77:39];
  assign zi5 = zi3[38:7];
  assign zi6 = zi3[6:0];
  assign res = {zi5, zi4};
endmodule

module ZLL_Main_repl77 (input logic [38:0] arg0,
  input logic [31:0] arg1,
  output logic [80:0] res);
  logic [77:0] zi0;
  logic [109:0] zi1;
  logic [31:0] zi2;
  logic [38:0] zi3;
  logic [38:0] zi4;
  logic [109:0] zi5;
  logic [38:0] zi6;
  logic [31:0] zi7;
  logic [31:0] zi8;
  logic [6:0] zi9;
  logic [80:0] zll_main_repl101_out;
  logic [80:0] zi10;
  logic [38:0] zi11;
  logic [38:0] main_nextpc_out;
  logic [80:0] zll_main_repl101_outR1;
  logic [80:0] zll_main_repl43_out;
  assign zi0 = {arg0, arg0};
  assign zi1 = {arg1, zi0};
  assign zi2 = zi1[109:78];
  assign zi3 = zi1[77:39];
  assign zi4 = zi1[38:0];
  assign zi5 = {zi4, zi2, zi3};
  assign zi6 = zi5[109:71];
  assign zi7 = zi5[70:39];
  assign zi8 = zi5[38:7];
  assign zi9 = zi5[6:0];
  ZLL_Main_repl101  inst ({zi7, zi9}, zll_main_repl101_out);
  assign zi10 = zll_main_repl101_out;
  assign zi11 = zi10[38:0];
  Main_nextPC  instR1 (zi11, main_nextpc_out);
  ZLL_Main_repl101  instR2 (main_nextpc_out, zll_main_repl101_outR1);
  ZLL_Main_repl43  instR3 (zll_main_repl101_outR1, zll_main_repl43_out);
  assign res = zll_main_repl43_out;
endmodule

module ZLL_Main_repl43 (input logic [80:0] arg0,
  output logic [80:0] res);
  logic [38:0] zi0;
  assign zi0 = arg0[38:0];
  assign res = {42'h20000000002, zi0};
endmodule

module Main_nextPC (input logic [38:0] arg0,
  output logic [38:0] res);
  logic [77:0] zi0;
  logic [38:0] zi1;
  logic [38:0] zi2;
  logic [77:0] zi3;
  logic [38:0] zi4;
  logic [31:0] zi5;
  logic [6:0] zi6;
  assign zi0 = {arg0, arg0};
  assign zi1 = zi0[77:39];
  assign zi2 = zi0[38:0];
  assign zi3 = {zi2, zi1};
  assign zi4 = zi3[77:39];
  assign zi5 = zi3[38:7];
  assign zi6 = zi3[6:0];
  assign res = {zi5, (zi6 == 7'h0) ? 7'h1 : ((zi6 == 7'h1) ? 7'h2 : ((zi6 == 7'h2) ? 7'h3 : ((zi6 == 7'h3) ? 7'h4 : ((zi6 == 7'h4) ? 7'h5 : ((zi6 == 7'h5) ? 7'h6 : ((zi6 == 7'h6) ? 7'h7 : ((zi6 == 7'h7) ? 7'h8 : ((zi6 == 7'h8) ? 7'h9 : ((zi6 == 7'h9) ? 7'ha : ((zi6 == 7'ha) ? 7'hb : ((zi6 == 7'hb) ? 7'hc : ((zi6 == 7'hc) ? 7'hd : ((zi6 == 7'hd) ? 7'he : ((zi6 == 7'he) ? 7'hf : ((zi6 == 7'hf) ? 7'h10 : ((zi6 == 7'h10) ? 7'h11 : ((zi6 == 7'h11) ? 7'h12 : ((zi6 == 7'h12) ? 7'h13 : ((zi6 == 7'h13) ? 7'h14 : ((zi6 == 7'h14) ? 7'h15 : ((zi6 == 7'h15) ? 7'h16 : ((zi6 == 7'h16) ? 7'h17 : ((zi6 == 7'h17) ? 7'h18 : ((zi6 == 7'h18) ? 7'h19 : ((zi6 == 7'h19) ? 7'h1a : ((zi6 == 7'h1a) ? 7'h1b : ((zi6 == 7'h1b) ? 7'h1c : ((zi6 == 7'h1c) ? 7'h1d : ((zi6 == 7'h1d) ? 7'h1e : ((zi6 == 7'h1e) ? 7'h1f : ((zi6 == 7'h1f) ? 7'h20 : ((zi6 == 7'h20) ? 7'h21 : ((zi6 == 7'h21) ? 7'h22 : ((zi6 == 7'h22) ? 7'h23 : ((zi6 == 7'h23) ? 7'h24 : ((zi6 == 7'h24) ? 7'h25 : ((zi6 == 7'h25) ? 7'h26 : ((zi6 == 7'h26) ? 7'h27 : ((zi6 == 7'h27) ? 7'h28 : ((zi6 == 7'h28) ? 7'h29 : ((zi6 == 7'h29) ? 7'h2a : ((zi6 == 7'h2a) ? 7'h2b : ((zi6 == 7'h2b) ? 7'h2c : ((zi6 == 7'h2c) ? 7'h2d : ((zi6 == 7'h2d) ? 7'h2e : ((zi6 == 7'h2e) ? 7'h2f : ((zi6 == 7'h2f) ? 7'h30 : ((zi6 == 7'h30) ? 7'h31 : ((zi6 == 7'h31) ? 7'h32 : ((zi6 == 7'h32) ? 7'h33 : ((zi6 == 7'h33) ? 7'h34 : ((zi6 == 7'h34) ? 7'h35 : ((zi6 == 7'h35) ? 7'h36 : ((zi6 == 7'h36) ? 7'h37 : ((zi6 == 7'h37) ? 7'h38 : ((zi6 == 7'h38) ? 7'h39 : ((zi6 == 7'h39) ? 7'h3a : ((zi6 == 7'h3a) ? 7'h3b : ((zi6 == 7'h3b) ? 7'h3c : ((zi6 == 7'h3c) ? 7'h3d : ((zi6 == 7'h3d) ? 7'h3e : ((zi6 == 7'h3e) ? 7'h3f : ((zi6 == 7'h3f) ? 7'h40 : ((zi6 == 7'h40) ? 7'h41 : ((zi6 == 7'h41) ? 7'h42 : ((zi6 == 7'h42) ? 7'h43 : ((zi6 == 7'h43) ? 7'h44 : ((zi6 == 7'h44) ? 7'h45 : ((zi6 == 7'h45) ? 7'h46 : ((zi6 == 7'h46) ? 7'h47 : ((zi6 == 7'h47) ? 7'h48 : ((zi6 == 7'h48) ? 7'h49 : ((zi6 == 7'h49) ? 7'h4a : ((zi6 == 7'h4a) ? 7'h4b : ((zi6 == 7'h4b) ? 7'h4c : ((zi6 == 7'h4c) ? 7'h4d : ((zi6 == 7'h4d) ? 7'h4e : ((zi6 == 7'h4e) ? 7'h4f : ((zi6 == 7'h4f) ? 7'h50 : ((zi6 == 7'h50) ? 7'h51 : ((zi6 == 7'h51) ? 7'h52 : ((zi6 == 7'h52) ? 7'h53 : ((zi6 == 7'h53) ? 7'h54 : ((zi6 == 7'h54) ? 7'h55 : ((zi6 == 7'h55) ? 7'h56 : ((zi6 == 7'h56) ? 7'h57 : ((zi6 == 7'h57) ? 7'h58 : ((zi6 == 7'h58) ? 7'h59 : ((zi6 == 7'h59) ? 7'h5a : ((zi6 == 7'h5a) ? 7'h5b : ((zi6 == 7'h5b) ? 7'h5c : ((zi6 == 7'h5c) ? 7'h5d : ((zi6 == 7'h5d) ? 7'h5e : ((zi6 == 7'h5e) ? 7'h5f : ((zi6 == 7'h5f) ? 7'h60 : ((zi6 == 7'h60) ? 7'h61 : ((zi6 == 7'h61) ? 7'h62 : ((zi6 == 7'h62) ? 7'h63 : ((zi6 == 7'h63) ? 7'h64 : ((zi6 == 7'h64) ? 7'h65 : ((zi6 == 7'h65) ? 7'h66 : ((zi6 == 7'h66) ? 7'h67 : ((zi6 == 7'h67) ? 7'h68 : ((zi6 == 7'h68) ? 7'h69 : ((zi6 == 7'h69) ? 7'h6a : ((zi6 == 7'h6a) ? 7'h6b : ((zi6 == 7'h6b) ? 7'h6c : ((zi6 == 7'h6c) ? 7'h6d : ((zi6 == 7'h6d) ? 7'h6e : ((zi6 == 7'h6e) ? 7'h6f : ((zi6 == 7'h6f) ? 7'h70 : ((zi6 == 7'h70) ? 7'h71 : ((zi6 == 7'h71) ? 7'h72 : ((zi6 == 7'h72) ? 7'h73 : ((zi6 == 7'h73) ? 7'h74 : ((zi6 == 7'h74) ? 7'h75 : ((zi6 == 7'h75) ? 7'h76 : ((zi6 == 7'h76) ? 7'h77 : ((zi6 == 7'h77) ? 7'h78 : ((zi6 == 7'h78) ? 7'h79 : ((zi6 == 7'h79) ? 7'h7a : ((zi6 == 7'h7a) ? 7'h7b : ((zi6 == 7'h7b) ? 7'h7c : ((zi6 == 7'h7c) ? 7'h7d : ((zi6 == 7'h7d) ? 7'h7e : 7'h0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))};
endmodule