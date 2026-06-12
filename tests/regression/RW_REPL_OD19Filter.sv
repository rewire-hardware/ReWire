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
  logic [80:0] zll_main_repl92_out;
  logic [80:0] zll_main_repl78_out;
  logic [80:0] zll_main_repl92_outR1;
  logic [80:0] zll_main_repl78_outR1;
  logic [70:0] main_getreg_out;
  logic [80:0] zll_main_repl58_out;
  logic [80:0] zi11;
  logic [31:0] zi12;
  logic [38:0] zi13;
  logic [0:0] extres;
  logic [70:0] main_getreg_outR1;
  logic [80:0] zll_main_repl58_outR1;
  logic [80:0] zi14;
  logic [31:0] zi15;
  logic [38:0] zi16;
  logic [0:0] extresR1;
  logic [70:0] main_getreg_outR2;
  logic [80:0] zll_main_repl58_outR2;
  logic [80:0] zi17;
  logic [31:0] zi18;
  logic [38:0] zi19;
  logic [0:0] extresR2;
  logic [38:0] main_nextpc_out;
  logic [80:0] zll_main_repl110_out;
  logic [80:0] zll_main_repl53_out;
  logic [80:0] zres;
  assign zi0 = {__st0, __st1};
  ZLL_Main_repl92  inst (zi0, __in1, zll_main_repl92_out);
  ZLL_Main_repl78  instR1 (zi0, __in1, zll_main_repl78_out);
  ZLL_Main_repl92  instR2 (zi0, __in1, zll_main_repl92_outR1);
  ZLL_Main_repl78  instR3 (zi0, __in1, zll_main_repl78_outR1);
  Main_getReg  instR4 (zi0, main_getreg_out);
  ZLL_Main_repl58  instR5 (main_getreg_out, zll_main_repl58_out);
  assign zi11 = zll_main_repl58_out;
  assign zi12 = zi11[70:39];
  assign zi13 = zi11[38:0];
  test2  instR6 (__in1, zi12, extres[0]);
  Main_getReg  instR7 (zi0, main_getreg_outR1);
  ZLL_Main_repl58  instR8 (main_getreg_outR1, zll_main_repl58_outR1);
  assign zi14 = zll_main_repl58_outR1;
  assign zi15 = zi14[70:39];
  assign zi16 = zi14[38:0];
  test3  instR9 (__in1, zi15, extresR1[0]);
  Main_getReg  instR10 (zi0, main_getreg_outR2);
  ZLL_Main_repl58  instR11 (main_getreg_outR2, zll_main_repl58_outR2);
  assign zi17 = zll_main_repl58_outR2;
  assign zi18 = zi17[70:39];
  assign zi19 = zi17[38:0];
  test4  instR12 (__in1, zi18, extresR2[0]);
  Main_nextPC  instR13 (zi0, main_nextpc_out);
  ZLL_Main_repl110  instR14 (main_nextpc_out, zll_main_repl110_out);
  ZLL_Main_repl53  instR15 (zll_main_repl110_out, zll_main_repl53_out);
  assign zres = (__in0 == 1'h0) ? ((__st1 == 7'h0) ? zll_main_repl92_out : ((__st1 == 7'h1) ? zll_main_repl78_out : ((__st1 == 7'h2) ? zll_main_repl92_outR1 : ((__st1 == 7'h3) ? zll_main_repl78_outR1 : ((__st1 == 7'h4) ? {{1'h1, {6'h28{1'h0}}}, extres, zi13} : ((__st1 == 7'h5) ? {{1'h1, {6'h28{1'h0}}}, extresR1, zi16} : ((__st1 == 7'h6) ? {{1'h1, {6'h28{1'h0}}}, extresR2, zi19} : zll_main_repl53_out))))))) : {42'h20000000002, zi0};
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

module ZLL_Main_repl110 (input logic [38:0] arg0,
  output logic [80:0] res);
  assign res = {{3'h1, {6'h27{1'h0}}}, arg0};
endmodule

module ZLL_Main_repl92 (input logic [38:0] arg0,
  input logic [31:0] arg1,
  output logic [80:0] res);
  logic [6:0] zi4;
  logic [80:0] zll_main_repl110_out;
  logic [80:0] zi5;
  logic [38:0] zi6;
  logic [38:0] main_nextpc_out;
  logic [80:0] zll_main_repl110_outR1;
  logic [80:0] zll_main_repl53_out;
  assign zi4 = arg0[6:0];
  ZLL_Main_repl110  inst ({arg1, zi4}, zll_main_repl110_out);
  assign zi5 = zll_main_repl110_out;
  assign zi6 = zi5[38:0];
  Main_nextPC  instR1 (zi6, main_nextpc_out);
  ZLL_Main_repl110  instR2 (main_nextpc_out, zll_main_repl110_outR1);
  ZLL_Main_repl53  instR3 (zll_main_repl110_outR1, zll_main_repl53_out);
  assign res = zll_main_repl53_out;
endmodule

module Main_getReg (input logic [38:0] arg0,
  output logic [70:0] res);
  logic [31:0] zi3;
  assign zi3 = arg0[38:7];
  assign res = {zi3, arg0};
endmodule

module ZLL_Main_repl78 (input logic [38:0] arg0,
  input logic [31:0] arg1,
  output logic [80:0] res);
  logic [70:0] main_getreg_out;
  logic [80:0] zll_main_repl58_out;
  logic [80:0] zi0;
  logic [31:0] zi1;
  logic [38:0] zi2;
  logic [0:0] extres;
  Main_getReg  inst (arg0, main_getreg_out);
  ZLL_Main_repl58  instR1 (main_getreg_out, zll_main_repl58_out);
  assign zi0 = zll_main_repl58_out;
  assign zi1 = zi0[70:39];
  assign zi2 = zi0[38:0];
  test1  instR2 (arg1, zi1, extres[0]);
  assign res = {{1'h1, {6'h28{1'h0}}}, extres, zi2};
endmodule

module ZLL_Main_repl58 (input logic [70:0] arg0,
  output logic [80:0] res);
  logic [31:0] zi0;
  logic [38:0] zi1;
  assign zi0 = arg0[70:39];
  assign zi1 = arg0[38:0];
  assign res = {10'h0, zi0, zi1};
endmodule

module ZLL_Main_repl53 (input logic [80:0] arg0,
  output logic [80:0] res);
  logic [38:0] zi0;
  assign zi0 = arg0[38:0];
  assign res = {42'h20000000002, zi0};
endmodule

module Main_nextPC (input logic [38:0] arg0,
  output logic [38:0] res);
  logic [31:0] zi3;
  logic [6:0] zi4;
  assign zi3 = arg0[38:7];
  assign zi4 = arg0[6:0];
  assign res = {zi3, (zi4 == 7'h0) ? 7'h1 : ((zi4 == 7'h1) ? 7'h2 : ((zi4 == 7'h2) ? 7'h3 : ((zi4 == 7'h3) ? 7'h4 : ((zi4 == 7'h4) ? 7'h5 : ((zi4 == 7'h5) ? 7'h6 : ((zi4 == 7'h6) ? 7'h7 : ((zi4 == 7'h7) ? 7'h8 : ((zi4 == 7'h8) ? 7'h9 : ((zi4 == 7'h9) ? 7'ha : ((zi4 == 7'ha) ? 7'hb : ((zi4 == 7'hb) ? 7'hc : ((zi4 == 7'hc) ? 7'hd : ((zi4 == 7'hd) ? 7'he : ((zi4 == 7'he) ? 7'hf : ((zi4 == 7'hf) ? 7'h10 : ((zi4 == 7'h10) ? 7'h11 : ((zi4 == 7'h11) ? 7'h12 : ((zi4 == 7'h12) ? 7'h13 : ((zi4 == 7'h13) ? 7'h14 : ((zi4 == 7'h14) ? 7'h15 : ((zi4 == 7'h15) ? 7'h16 : ((zi4 == 7'h16) ? 7'h17 : ((zi4 == 7'h17) ? 7'h18 : ((zi4 == 7'h18) ? 7'h19 : ((zi4 == 7'h19) ? 7'h1a : ((zi4 == 7'h1a) ? 7'h1b : ((zi4 == 7'h1b) ? 7'h1c : ((zi4 == 7'h1c) ? 7'h1d : ((zi4 == 7'h1d) ? 7'h1e : ((zi4 == 7'h1e) ? 7'h1f : ((zi4 == 7'h1f) ? 7'h20 : ((zi4 == 7'h20) ? 7'h21 : ((zi4 == 7'h21) ? 7'h22 : ((zi4 == 7'h22) ? 7'h23 : ((zi4 == 7'h23) ? 7'h24 : ((zi4 == 7'h24) ? 7'h25 : ((zi4 == 7'h25) ? 7'h26 : ((zi4 == 7'h26) ? 7'h27 : ((zi4 == 7'h27) ? 7'h28 : ((zi4 == 7'h28) ? 7'h29 : ((zi4 == 7'h29) ? 7'h2a : ((zi4 == 7'h2a) ? 7'h2b : ((zi4 == 7'h2b) ? 7'h2c : ((zi4 == 7'h2c) ? 7'h2d : ((zi4 == 7'h2d) ? 7'h2e : ((zi4 == 7'h2e) ? 7'h2f : ((zi4 == 7'h2f) ? 7'h30 : ((zi4 == 7'h30) ? 7'h31 : ((zi4 == 7'h31) ? 7'h32 : ((zi4 == 7'h32) ? 7'h33 : ((zi4 == 7'h33) ? 7'h34 : ((zi4 == 7'h34) ? 7'h35 : ((zi4 == 7'h35) ? 7'h36 : ((zi4 == 7'h36) ? 7'h37 : ((zi4 == 7'h37) ? 7'h38 : ((zi4 == 7'h38) ? 7'h39 : ((zi4 == 7'h39) ? 7'h3a : ((zi4 == 7'h3a) ? 7'h3b : ((zi4 == 7'h3b) ? 7'h3c : ((zi4 == 7'h3c) ? 7'h3d : ((zi4 == 7'h3d) ? 7'h3e : ((zi4 == 7'h3e) ? 7'h3f : ((zi4 == 7'h3f) ? 7'h40 : ((zi4 == 7'h40) ? 7'h41 : ((zi4 == 7'h41) ? 7'h42 : ((zi4 == 7'h42) ? 7'h43 : ((zi4 == 7'h43) ? 7'h44 : ((zi4 == 7'h44) ? 7'h45 : ((zi4 == 7'h45) ? 7'h46 : ((zi4 == 7'h46) ? 7'h47 : ((zi4 == 7'h47) ? 7'h48 : ((zi4 == 7'h48) ? 7'h49 : ((zi4 == 7'h49) ? 7'h4a : ((zi4 == 7'h4a) ? 7'h4b : ((zi4 == 7'h4b) ? 7'h4c : ((zi4 == 7'h4c) ? 7'h4d : ((zi4 == 7'h4d) ? 7'h4e : ((zi4 == 7'h4e) ? 7'h4f : ((zi4 == 7'h4f) ? 7'h50 : ((zi4 == 7'h50) ? 7'h51 : ((zi4 == 7'h51) ? 7'h52 : ((zi4 == 7'h52) ? 7'h53 : ((zi4 == 7'h53) ? 7'h54 : ((zi4 == 7'h54) ? 7'h55 : ((zi4 == 7'h55) ? 7'h56 : ((zi4 == 7'h56) ? 7'h57 : ((zi4 == 7'h57) ? 7'h58 : ((zi4 == 7'h58) ? 7'h59 : ((zi4 == 7'h59) ? 7'h5a : ((zi4 == 7'h5a) ? 7'h5b : ((zi4 == 7'h5b) ? 7'h5c : ((zi4 == 7'h5c) ? 7'h5d : ((zi4 == 7'h5d) ? 7'h5e : ((zi4 == 7'h5e) ? 7'h5f : ((zi4 == 7'h5f) ? 7'h60 : ((zi4 == 7'h60) ? 7'h61 : ((zi4 == 7'h61) ? 7'h62 : ((zi4 == 7'h62) ? 7'h63 : ((zi4 == 7'h63) ? 7'h64 : ((zi4 == 7'h64) ? 7'h65 : ((zi4 == 7'h65) ? 7'h66 : ((zi4 == 7'h66) ? 7'h67 : ((zi4 == 7'h67) ? 7'h68 : ((zi4 == 7'h68) ? 7'h69 : ((zi4 == 7'h69) ? 7'h6a : ((zi4 == 7'h6a) ? 7'h6b : ((zi4 == 7'h6b) ? 7'h6c : ((zi4 == 7'h6c) ? 7'h6d : ((zi4 == 7'h6d) ? 7'h6e : ((zi4 == 7'h6e) ? 7'h6f : ((zi4 == 7'h6f) ? 7'h70 : ((zi4 == 7'h70) ? 7'h71 : ((zi4 == 7'h71) ? 7'h72 : ((zi4 == 7'h72) ? 7'h73 : ((zi4 == 7'h73) ? 7'h74 : ((zi4 == 7'h74) ? 7'h75 : ((zi4 == 7'h75) ? 7'h76 : ((zi4 == 7'h76) ? 7'h77 : ((zi4 == 7'h77) ? 7'h78 : ((zi4 == 7'h78) ? 7'h79 : ((zi4 == 7'h79) ? 7'h7a : ((zi4 == 7'h7a) ? 7'h7b : ((zi4 == 7'h7b) ? 7'h7c : ((zi4 == 7'h7c) ? 7'h7d : ((zi4 == 7'h7d) ? 7'h7e : 7'h0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))};
endmodule