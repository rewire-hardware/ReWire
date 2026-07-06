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
  logic [40:0] zl_main_putreg19_out;
  logic [40:0] zl_main_getreg24_out;
  logic [40:0] zl_main_putreg19_outR1;
  logic [40:0] zl_main_getreg24_outR1;
  logic [0:0] extres;
  logic [40:0] zll_l_r45_out;
  logic [0:0] extresR1;
  logic [40:0] zll_l_r45_outR1;
  logic [0:0] extresR2;
  logic [40:0] zll_l_r45_outR2;
  logic [40:0] zl_main_nextpc9_out;
  logic [40:0] zres;
  assign zi0 = {__st0, __st1};
  ZL_Main_putReg19  inst (__in1, zi0, zl_main_putreg19_out);
  ZL_Main_getReg24  instR1 (__in1, zi0, zl_main_getreg24_out);
  ZL_Main_putReg19  instR2 (__in1, zi0, zl_main_putreg19_outR1);
  ZL_Main_getReg24  instR3 (__in1, zi0, zl_main_getreg24_outR1);
  test2  instR4 (__in1, __st0, extres[0]);
  ZLL_L_r45  instR5 (zi0, extres, zll_l_r45_out);
  test3  instR6 (__in1, __st0, extresR1[0]);
  ZLL_L_r45  instR7 (zi0, extresR1, zll_l_r45_outR1);
  test4  instR8 (__in1, __st0, extresR2[0]);
  ZLL_L_r45  instR9 (zi0, extresR2, zll_l_r45_outR2);
  ZL_Main_nextPC9  instR10 (zi0, zl_main_nextpc9_out);
  assign zres = (__in0 == 1'h0) ? ((__st1 == 7'h0) ? zl_main_putreg19_out : ((__st1 == 7'h1) ? zl_main_getreg24_out : ((__st1 == 7'h2) ? zl_main_putreg19_outR1 : ((__st1 == 7'h3) ? zl_main_getreg24_outR1 : ((__st1 == 7'h4) ? zll_l_r45_out : ((__st1 == 7'h5) ? zll_l_r45_outR1 : ((__st1 == 7'h6) ? zll_l_r45_outR2 : zl_main_nextpc9_out))))))) : {2'h2, zi0};
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

module ZLL_L_r45 (input logic [38:0] arg0,
  input logic [0:0] arg1,
  output logic [40:0] res);
  logic [1:0] zi0;
  assign zi0 = {1'h0, arg1};
  assign res = {zi0, arg0};
endmodule

module ZL_Main_getReg24 (input logic [31:0] arg0,
  input logic [38:0] arg1,
  output logic [40:0] res);
  logic [31:0] zi0;
  logic [0:0] extres;
  logic [40:0] zll_l_r45_out;
  assign zi0 = arg1[38:7];
  test1  inst (arg0, zi0, extres[0]);
  ZLL_L_r45  instR1 (arg1, extres, zll_l_r45_out);
  assign res = zll_l_r45_out;
endmodule

module ZL_Main_putReg19 (input logic [31:0] arg0,
  input logic [38:0] arg1,
  output logic [40:0] res);
  logic [6:0] zi1;
  logic [40:0] zl_main_nextpc9_out;
  assign zi1 = arg1[6:0];
  ZL_Main_nextPC9  inst ({arg0, zi1}, zl_main_nextpc9_out);
  assign res = zl_main_nextpc9_out;
endmodule

module ZL_Main_nextPC9 (input logic [38:0] arg0,
  output logic [40:0] res);
  logic [31:0] zi0;
  logic [6:0] zi1;
  logic [6:0] zi2;
  logic [38:0] zi3;
  assign zi0 = arg0[38:7];
  assign zi1 = arg0[6:0];
  assign zi2 = (zi1 == 7'h0) ? 7'h1 : ((zi1 == 7'h1) ? 7'h2 : ((zi1 == 7'h2) ? 7'h3 : ((zi1 == 7'h3) ? 7'h4 : ((zi1 == 7'h4) ? 7'h5 : ((zi1 == 7'h5) ? 7'h6 : ((zi1 == 7'h6) ? 7'h7 : ((zi1 == 7'h7) ? 7'h8 : ((zi1 == 7'h8) ? 7'h9 : ((zi1 == 7'h9) ? 7'ha : ((zi1 == 7'ha) ? 7'hb : ((zi1 == 7'hb) ? 7'hc : ((zi1 == 7'hc) ? 7'hd : ((zi1 == 7'hd) ? 7'he : ((zi1 == 7'he) ? 7'hf : ((zi1 == 7'hf) ? 7'h10 : ((zi1 == 7'h10) ? 7'h11 : ((zi1 == 7'h11) ? 7'h12 : ((zi1 == 7'h12) ? 7'h13 : ((zi1 == 7'h13) ? 7'h14 : ((zi1 == 7'h14) ? 7'h15 : ((zi1 == 7'h15) ? 7'h16 : ((zi1 == 7'h16) ? 7'h17 : ((zi1 == 7'h17) ? 7'h18 : ((zi1 == 7'h18) ? 7'h19 : ((zi1 == 7'h19) ? 7'h1a : ((zi1 == 7'h1a) ? 7'h1b : ((zi1 == 7'h1b) ? 7'h1c : ((zi1 == 7'h1c) ? 7'h1d : ((zi1 == 7'h1d) ? 7'h1e : ((zi1 == 7'h1e) ? 7'h1f : ((zi1 == 7'h1f) ? 7'h20 : ((zi1 == 7'h20) ? 7'h21 : ((zi1 == 7'h21) ? 7'h22 : ((zi1 == 7'h22) ? 7'h23 : ((zi1 == 7'h23) ? 7'h24 : ((zi1 == 7'h24) ? 7'h25 : ((zi1 == 7'h25) ? 7'h26 : ((zi1 == 7'h26) ? 7'h27 : ((zi1 == 7'h27) ? 7'h28 : ((zi1 == 7'h28) ? 7'h29 : ((zi1 == 7'h29) ? 7'h2a : ((zi1 == 7'h2a) ? 7'h2b : ((zi1 == 7'h2b) ? 7'h2c : ((zi1 == 7'h2c) ? 7'h2d : ((zi1 == 7'h2d) ? 7'h2e : ((zi1 == 7'h2e) ? 7'h2f : ((zi1 == 7'h2f) ? 7'h30 : ((zi1 == 7'h30) ? 7'h31 : ((zi1 == 7'h31) ? 7'h32 : ((zi1 == 7'h32) ? 7'h33 : ((zi1 == 7'h33) ? 7'h34 : ((zi1 == 7'h34) ? 7'h35 : ((zi1 == 7'h35) ? 7'h36 : ((zi1 == 7'h36) ? 7'h37 : ((zi1 == 7'h37) ? 7'h38 : ((zi1 == 7'h38) ? 7'h39 : ((zi1 == 7'h39) ? 7'h3a : ((zi1 == 7'h3a) ? 7'h3b : ((zi1 == 7'h3b) ? 7'h3c : ((zi1 == 7'h3c) ? 7'h3d : ((zi1 == 7'h3d) ? 7'h3e : ((zi1 == 7'h3e) ? 7'h3f : ((zi1 == 7'h3f) ? 7'h40 : ((zi1 == 7'h40) ? 7'h41 : ((zi1 == 7'h41) ? 7'h42 : ((zi1 == 7'h42) ? 7'h43 : ((zi1 == 7'h43) ? 7'h44 : ((zi1 == 7'h44) ? 7'h45 : ((zi1 == 7'h45) ? 7'h46 : ((zi1 == 7'h46) ? 7'h47 : ((zi1 == 7'h47) ? 7'h48 : ((zi1 == 7'h48) ? 7'h49 : ((zi1 == 7'h49) ? 7'h4a : ((zi1 == 7'h4a) ? 7'h4b : ((zi1 == 7'h4b) ? 7'h4c : ((zi1 == 7'h4c) ? 7'h4d : ((zi1 == 7'h4d) ? 7'h4e : ((zi1 == 7'h4e) ? 7'h4f : ((zi1 == 7'h4f) ? 7'h50 : ((zi1 == 7'h50) ? 7'h51 : ((zi1 == 7'h51) ? 7'h52 : ((zi1 == 7'h52) ? 7'h53 : ((zi1 == 7'h53) ? 7'h54 : ((zi1 == 7'h54) ? 7'h55 : ((zi1 == 7'h55) ? 7'h56 : ((zi1 == 7'h56) ? 7'h57 : ((zi1 == 7'h57) ? 7'h58 : ((zi1 == 7'h58) ? 7'h59 : ((zi1 == 7'h59) ? 7'h5a : ((zi1 == 7'h5a) ? 7'h5b : ((zi1 == 7'h5b) ? 7'h5c : ((zi1 == 7'h5c) ? 7'h5d : ((zi1 == 7'h5d) ? 7'h5e : ((zi1 == 7'h5e) ? 7'h5f : ((zi1 == 7'h5f) ? 7'h60 : ((zi1 == 7'h60) ? 7'h61 : ((zi1 == 7'h61) ? 7'h62 : ((zi1 == 7'h62) ? 7'h63 : ((zi1 == 7'h63) ? 7'h64 : ((zi1 == 7'h64) ? 7'h65 : ((zi1 == 7'h65) ? 7'h66 : ((zi1 == 7'h66) ? 7'h67 : ((zi1 == 7'h67) ? 7'h68 : ((zi1 == 7'h68) ? 7'h69 : ((zi1 == 7'h69) ? 7'h6a : ((zi1 == 7'h6a) ? 7'h6b : ((zi1 == 7'h6b) ? 7'h6c : ((zi1 == 7'h6c) ? 7'h6d : ((zi1 == 7'h6d) ? 7'h6e : ((zi1 == 7'h6e) ? 7'h6f : ((zi1 == 7'h6f) ? 7'h70 : ((zi1 == 7'h70) ? 7'h71 : ((zi1 == 7'h71) ? 7'h72 : ((zi1 == 7'h72) ? 7'h73 : ((zi1 == 7'h73) ? 7'h74 : ((zi1 == 7'h74) ? 7'h75 : ((zi1 == 7'h75) ? 7'h76 : ((zi1 == 7'h76) ? 7'h77 : ((zi1 == 7'h77) ? 7'h78 : ((zi1 == 7'h78) ? 7'h79 : ((zi1 == 7'h79) ? 7'h7a : ((zi1 == 7'h7a) ? 7'h7b : ((zi1 == 7'h7b) ? 7'h7c : ((zi1 == 7'h7c) ? 7'h7d : ((zi1 == 7'h7d) ? 7'h7e : 7'h0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
  assign zi3 = {zi0, zi2};
  assign res = {2'h2, zi3};
endmodule