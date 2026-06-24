module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [513:0] __in0,
  output logic [257:0] __out0);
  logic [7:0] __resumption_tag;
  logic [7:0] __resumption_tag_next;
  logic [223:0] __st0;
  logic [223:0] __st0_next;
  logic [31:0] __st1;
  logic [31:0] __st1_next;
  logic [511:0] __st2;
  logic [511:0] __st2_next;
  logic [261:0] __st3;
  logic [261:0] __st3_next;
  logic [255:0] zi1;
  logic [255:0] zi3;
  logic [5:0] zi4;
  logic [1296:0] main_dev_out;
  logic [1296:0] zll_pure_dispatch8_out;
  logic [1296:0] zll_pure_dispatch8_outR1;
  logic [5:0] zi5;
  logic [255:0] zi6;
  logic [255:0] zi8;
  logic [5:0] zi9;
  logic [31:0] zi16;
  logic [31:0] zi17;
  logic [31:0] zi18;
  logic [31:0] zi19;
  logic [31:0] zi20;
  logic [31:0] zi21;
  logic [31:0] zi22;
  logic [31:0] zi23;
  logic [31:0] zi30;
  logic [31:0] zi31;
  logic [31:0] zi32;
  logic [31:0] zi33;
  logic [31:0] zi34;
  logic [31:0] zi35;
  logic [31:0] zi36;
  logic [1029:0] zi38;
  logic [255:0] zi39;
  logic [511:0] zi40;
  logic [255:0] zi41;
  logic [5:0] zi42;
  logic [1296:0] zi43;
  logic [255:0] zi44;
  logic [511:0] zi45;
  logic [255:0] zi46;
  logic [5:0] zi47;
  logic [1296:0] main_dev_outR1;
  logic [1296:0] main_loop_out;
  logic [1296:0] zres;
  assign zi1 = {__st0, __st1};
  assign zi3 = __st3[261:6];
  assign zi4 = __st3[5:0];
  Main_dev  inst (__in0, zi1, __st2, zi3, zi4, main_dev_out);
  ZLL_Pure_dispatch8  instR1 (__in0, {__st0, __st1}, __st2, __st3[261:6], __st3[5:0], zll_pure_dispatch8_out);
  ZLL_Pure_dispatch8  instR2 (__in0, {__st0, __st1}, __st2, __st3[261:6], __st3[5:0], zll_pure_dispatch8_outR1);
  assign zi5 = __resumption_tag[5:0];
  assign zi6 = {__st0, __st1};
  assign zi8 = __st3[261:6];
  assign zi9 = __st3[5:0];
  assign zi16 = __st3[261:230];
  assign zi17 = __st3[229:198];
  assign zi18 = __st3[197:166];
  assign zi19 = __st3[165:134];
  assign zi20 = __st3[133:102];
  assign zi21 = __st3[101:70];
  assign zi22 = __st3[69:38];
  assign zi23 = __st3[37:6];
  assign zi30 = __st0[223:192];
  assign zi31 = __st0[191:160];
  assign zi32 = __st0[159:128];
  assign zi33 = __st0[127:96];
  assign zi34 = __st0[95:64];
  assign zi35 = __st0[63:32];
  assign zi36 = __st0[31:0];
  assign zi38 = {zi6, __st2, zi30 + zi16, zi31 + zi17, zi32 + zi18, zi33 + zi19, zi34 + zi20, zi35 + zi21, zi36 + zi22, __st1 + zi23, zi9};
  assign zi39 = zi38[1029:774];
  assign zi40 = zi38[773:262];
  assign zi41 = zi38[261:6];
  assign zi42 = zi38[5:0];
  assign zi43 = {{10'h1, {9'h101{1'h0}}}, zi39, zi40, zi41, zi42};
  assign zi44 = zi43[1029:774];
  assign zi45 = zi43[773:262];
  assign zi46 = zi43[261:6];
  assign zi47 = zi43[5:0];
  Main_dev  instR3 (__in0, zi44, zi45, zi46, zi47, main_dev_outR1);
  Main_loop  instR4 (zi6, __st2, zi8, zi9, main_loop_out);
  assign zres = (__resumption_tag[7:6] == 2'h1) ? main_dev_out : ((__resumption_tag[7:6] == 2'h2) ? zll_pure_dispatch8_out : ((__resumption_tag[7:6] == 2'h3) ? zll_pure_dispatch8_outR1 : ((zi5 == 6'h3f) ? main_dev_outR1 : main_loop_out)));
  assign __resumption_tag_next = zres[1037:1030];
  assign __st0_next = zres[1029:806];
  assign __st1_next = zres[805:774];
  assign __st2_next = zres[773:262];
  assign __st3_next = zres[261:0];
  assign __out0 = zres[1295:1038];
  initial {__resumption_tag, __st0, __st1, __st2, __st3} = {2'h1, {11'h40c{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1, __st2, __st3} <= {2'h1, {11'h40c{1'h0}}};
    end else begin
      {__resumption_tag, __st0, __st1, __st2, __st3} <= {__resumption_tag_next, __st0_next, __st1_next, __st2_next, __st3_next};
    end
  end
endmodule

module ZLL_Main_sigma115 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  output logic [31:0] res);
  assign res = (arg1 >> arg0) | (arg1 << (32'h20 - arg0));
endmodule

module ZLL_Main_dev104 (input logic [1029:0] arg0,
  output logic [1296:0] res);
  logic [255:0] zi0;
  logic [511:0] zi1;
  logic [255:0] zi2;
  logic [5:0] zi3;
  assign zi0 = arg0[1029:774];
  assign zi1 = arg0[773:262];
  assign zi2 = arg0[261:6];
  assign zi3 = arg0[5:0];
  assign res = {{10'h1, {9'h101{1'h0}}}, zi0, zi1, zi2, zi3};
endmodule

module Main_dev (input logic [513:0] arg0,
  input logic [255:0] arg1,
  input logic [511:0] arg2,
  input logic [255:0] arg3,
  input logic [5:0] arg4,
  output logic [1296:0] res);
  logic [511:0] zi0;
  logic [1029:0] zll_main_dev50_out;
  logic [1296:0] zll_main_dev104_out;
  logic [1296:0] zi22;
  logic [255:0] zi23;
  logic [511:0] zi24;
  logic [255:0] zi25;
  logic [5:0] zi26;
  logic [511:0] zi27;
  logic [1029:0] zll_main_dev50_outR1;
  logic [1296:0] zll_main_dev104_outR1;
  logic [1296:0] zi44;
  logic [255:0] zi45;
  logic [511:0] zi46;
  logic [255:0] zi47;
  logic [5:0] zi48;
  assign zi0 = arg0[511:0];
  ZLL_Main_dev50  inst (zi0, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, arg2, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, 6'h0, zll_main_dev50_out);
  ZLL_Main_dev104  instR1 (zll_main_dev50_out, zll_main_dev104_out);
  assign zi22 = zll_main_dev104_out;
  assign zi23 = zi22[1029:774];
  assign zi24 = zi22[773:262];
  assign zi25 = zi22[261:6];
  assign zi26 = zi22[5:0];
  assign zi27 = arg0[511:0];
  ZLL_Main_dev50  instR2 (zi27, arg3, arg2, arg3, 6'h0, zll_main_dev50_outR1);
  ZLL_Main_dev104  instR3 (zll_main_dev50_outR1, zll_main_dev104_outR1);
  assign zi44 = zll_main_dev104_outR1;
  assign zi45 = zi44[1029:774];
  assign zi46 = zi44[773:262];
  assign zi47 = zi44[261:6];
  assign zi48 = zi44[5:0];
  assign res = (arg0[513:512] == 2'h0) ? {267'h5000000000000000000000000000000000000000000000000000000000000000080, zi23, zi24, zi25, zi26} : ((arg0[513:512] == 2'h1) ? {267'h50000000000000000000000000000000000000000000000000000000000000000c0, zi45, zi46, zi47, zi48} : {3'h4, arg3, 8'h40, arg1, arg2, arg3, arg4});
endmodule

module Main_loop (input logic [255:0] arg0,
  input logic [511:0] arg1,
  input logic [255:0] arg2,
  input logic [5:0] arg3,
  output logic [1296:0] res);
  logic [31:0] zi12;
  logic [31:0] zi28;
  logic [31:0] zi29;
  logic [31:0] zi30;
  logic [31:0] zi31;
  logic [31:0] zi32;
  logic [31:0] zi33;
  logic [31:0] zi34;
  logic [31:0] zi35;
  logic [31:0] zi36;
  logic [31:0] zi37;
  logic [31:0] zi38;
  logic [31:0] zi39;
  logic [31:0] zi40;
  logic [31:0] zi41;
  logic [31:0] zi42;
  logic [31:0] zi43;
  logic [31:0] zll_main_sigma115_out;
  logic [31:0] zll_main_sigma115_outR1;
  logic [31:0] zll_main_sigma115_outR2;
  logic [31:0] zll_main_sigma115_outR3;
  logic [31:0] zi44;
  logic [1029:0] zi45;
  logic [255:0] zi46;
  logic [511:0] zi47;
  logic [255:0] zi48;
  logic [5:0] zi49;
  logic [1061:0] zi50;
  logic [31:0] zi51;
  logic [511:0] zi53;
  logic [255:0] zi54;
  logic [5:0] zi55;
  logic [31:0] zi56;
  logic [31:0] zi69;
  logic [31:0] zi70;
  logic [31:0] zi71;
  logic [31:0] zi72;
  logic [31:0] zi73;
  logic [31:0] zi74;
  logic [31:0] zi75;
  logic [31:0] zi76;
  logic [31:0] zll_main_sigma115_outR4;
  logic [31:0] zll_main_sigma115_outR5;
  logic [31:0] zll_main_sigma115_outR6;
  logic [31:0] zi81;
  logic [31:0] zll_main_sigma115_outR7;
  logic [31:0] zll_main_sigma115_outR8;
  logic [31:0] zll_main_sigma115_outR9;
  logic [31:0] zi86;
  logic [31:0] zi87;
  logic [31:0] zi88;
  logic [1029:0] zi89;
  logic [255:0] zi90;
  logic [511:0] zi91;
  logic [255:0] zi92;
  logic [1029:0] zi94;
  logic [255:0] zi95;
  logic [511:0] zi96;
  logic [255:0] zi97;
  logic [5:0] zi98;
  logic [1035:0] zi99;
  logic [5:0] zi100;
  logic [255:0] zi101;
  logic [511:0] zi102;
  logic [255:0] zi103;
  logic [5:0] zi104;
  logic [1296:0] zi105;
  logic [5:0] zi106;
  logic [255:0] zi107;
  logic [511:0] zi108;
  logic [255:0] zi109;
  logic [5:0] zi110;
  assign zi12 = arg1[511:480];
  assign zi28 = arg1[511:480];
  assign zi29 = arg1[479:448];
  assign zi30 = arg1[447:416];
  assign zi31 = arg1[415:384];
  assign zi32 = arg1[383:352];
  assign zi33 = arg1[351:320];
  assign zi34 = arg1[319:288];
  assign zi35 = arg1[287:256];
  assign zi36 = arg1[255:224];
  assign zi37 = arg1[223:192];
  assign zi38 = arg1[191:160];
  assign zi39 = arg1[159:128];
  assign zi40 = arg1[127:96];
  assign zi41 = arg1[95:64];
  assign zi42 = arg1[63:32];
  assign zi43 = arg1[31:0];
  ZLL_Main_sigma115  inst (32'h11, zi42, zll_main_sigma115_out);
  ZLL_Main_sigma115  instR1 (32'h13, zi42, zll_main_sigma115_outR1);
  ZLL_Main_sigma115  instR2 (32'h7, zi29, zll_main_sigma115_outR2);
  ZLL_Main_sigma115  instR3 (32'h12, zi29, zll_main_sigma115_outR3);
  assign zi44 = (((zll_main_sigma115_out ^ zll_main_sigma115_outR1) ^ (zi42 >> 32'ha)) + zi37) + (((zll_main_sigma115_outR2 ^ zll_main_sigma115_outR3) ^ (zi29 >> 32'h3)) + zi28);
  assign zi45 = {arg0, {zi29, zi30, zi31, zi32, zi33, zi34, zi35, zi36, zi37, zi38, zi39, zi40, zi41, zi42, zi43, zi44}, arg2, arg3};
  assign zi46 = zi45[1029:774];
  assign zi47 = zi45[773:262];
  assign zi48 = zi45[261:6];
  assign zi49 = zi45[5:0];
  assign zi50 = {zi12, zi46, zi47, zi48, zi49};
  assign zi51 = zi50[1061:1030];
  assign zi53 = zi50[773:262];
  assign zi54 = zi50[261:6];
  assign zi55 = zi50[5:0];
  assign zi56 = (arg3 == 6'h0) ? 32'h428a2f98 : ((arg3 == 6'h1) ? 32'h71374491 : ((arg3 == 6'h2) ? 32'hb5c0fbcf : ((arg3 == 6'h3) ? 32'he9b5dba5 : ((arg3 == 6'h4) ? 32'h3956c25b : ((arg3 == 6'h5) ? 32'h59f111f1 : ((arg3 == 6'h6) ? 32'h923f82a4 : ((arg3 == 6'h7) ? 32'hab1c5ed5 : ((arg3 == 6'h8) ? 32'hd807aa98 : ((arg3 == 6'h9) ? 32'h12835b01 : ((arg3 == 6'ha) ? 32'h243185be : ((arg3 == 6'hb) ? 32'h550c7dc3 : ((arg3 == 6'hc) ? 32'h72be5d74 : ((arg3 == 6'hd) ? 32'h80deb1fe : ((arg3 == 6'he) ? 32'h9bdc06a7 : ((arg3 == 6'hf) ? 32'hc19bf174 : ((arg3 == 6'h10) ? 32'he49b69c1 : ((arg3 == 6'h11) ? 32'hefbe4786 : ((arg3 == 6'h12) ? 32'hfc19dc6 : ((arg3 == 6'h13) ? 32'h240ca1cc : ((arg3 == 6'h14) ? 32'h2de92c6f : ((arg3 == 6'h15) ? 32'h4a7484aa : ((arg3 == 6'h16) ? 32'h5cb0a9dc : ((arg3 == 6'h17) ? 32'h76f988da : ((arg3 == 6'h18) ? 32'h983e5152 : ((arg3 == 6'h19) ? 32'ha831c66d : ((arg3 == 6'h1a) ? 32'hb00327c8 : ((arg3 == 6'h1b) ? 32'hbf597fc7 : ((arg3 == 6'h1c) ? 32'hc6e00bf3 : ((arg3 == 6'h1d) ? 32'hd5a79147 : ((arg3 == 6'h1e) ? 32'h6ca6351 : ((arg3 == 6'h1f) ? 32'h14292967 : ((arg3 == 6'h20) ? 32'h27b70a85 : ((arg3 == 6'h21) ? 32'h2e1b2138 : ((arg3 == 6'h22) ? 32'h4d2c6dfc : ((arg3 == 6'h23) ? 32'h53380d13 : ((arg3 == 6'h24) ? 32'h650a7354 : ((arg3 == 6'h25) ? 32'h766a0abb : ((arg3 == 6'h26) ? 32'h81c2c92e : ((arg3 == 6'h27) ? 32'h92722c85 : ((arg3 == 6'h28) ? 32'ha2bfe8a1 : ((arg3 == 6'h29) ? 32'ha81a664b : ((arg3 == 6'h2a) ? 32'hc24b8b70 : ((arg3 == 6'h2b) ? 32'hc76c51a3 : ((arg3 == 6'h2c) ? 32'hd192e819 : ((arg3 == 6'h2d) ? 32'hd6990624 : ((arg3 == 6'h2e) ? 32'hf40e3585 : ((arg3 == 6'h2f) ? 32'h106aa070 : ((arg3 == 6'h30) ? 32'h19a4c116 : ((arg3 == 6'h31) ? 32'h1e376c08 : ((arg3 == 6'h32) ? 32'h2748774c : ((arg3 == 6'h33) ? 32'h34b0bcb5 : ((arg3 == 6'h34) ? 32'h391c0cb3 : ((arg3 == 6'h35) ? 32'h4ed8aa4a : ((arg3 == 6'h36) ? 32'h5b9cca4f : ((arg3 == 6'h37) ? 32'h682e6ff3 : ((arg3 == 6'h38) ? 32'h748f82ee : ((arg3 == 6'h39) ? 32'h78a5636f : ((arg3 == 6'h3a) ? 32'h84c87814 : ((arg3 == 6'h3b) ? 32'h8cc70208 : ((arg3 == 6'h3c) ? 32'h90befffa : ((arg3 == 6'h3d) ? 32'ha4506ceb : ((arg3 == 6'h3e) ? 32'hbef9a3f7 : 32'hc67178f2))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
  assign zi69 = zi50[1029:998];
  assign zi70 = zi50[997:966];
  assign zi71 = zi50[965:934];
  assign zi72 = zi50[933:902];
  assign zi73 = zi50[901:870];
  assign zi74 = zi50[869:838];
  assign zi75 = zi50[837:806];
  assign zi76 = zi50[805:774];
  ZLL_Main_sigma115  instR4 (32'h6, zi73, zll_main_sigma115_outR4);
  ZLL_Main_sigma115  instR5 (32'hb, zi73, zll_main_sigma115_outR5);
  ZLL_Main_sigma115  instR6 (32'h19, zi73, zll_main_sigma115_outR6);
  assign zi81 = zi76 + ((((zll_main_sigma115_outR4 ^ zll_main_sigma115_outR5) ^ zll_main_sigma115_outR6) + ((zi73 & zi74) ^ ((~zi73) & zi75))) + (zi56 + zi51));
  ZLL_Main_sigma115  instR7 (32'h2, zi69, zll_main_sigma115_outR7);
  ZLL_Main_sigma115  instR8 (32'hd, zi69, zll_main_sigma115_outR8);
  ZLL_Main_sigma115  instR9 (32'h16, zi69, zll_main_sigma115_outR9);
  assign zi86 = ((zll_main_sigma115_outR7 ^ zll_main_sigma115_outR8) ^ zll_main_sigma115_outR9) + (((zi69 & zi70) ^ (zi69 & zi71)) ^ (zi70 & zi71));
  assign zi87 = zi72 + zi81;
  assign zi88 = zi81 + zi86;
  assign zi89 = {{zi88, zi69, zi70, zi71, zi87, zi73, zi74, zi75}, zi53, zi54, zi55};
  assign zi90 = zi89[1029:774];
  assign zi91 = zi89[773:262];
  assign zi92 = zi89[261:6];
  assign zi94 = {zi90, zi91, zi92, (arg3 == 6'h0) ? 6'h1 : ((arg3 == 6'h1) ? 6'h2 : ((arg3 == 6'h2) ? 6'h3 : ((arg3 == 6'h3) ? 6'h4 : ((arg3 == 6'h4) ? 6'h5 : ((arg3 == 6'h5) ? 6'h6 : ((arg3 == 6'h6) ? 6'h7 : ((arg3 == 6'h7) ? 6'h8 : ((arg3 == 6'h8) ? 6'h9 : ((arg3 == 6'h9) ? 6'ha : ((arg3 == 6'ha) ? 6'hb : ((arg3 == 6'hb) ? 6'hc : ((arg3 == 6'hc) ? 6'hd : ((arg3 == 6'hd) ? 6'he : ((arg3 == 6'he) ? 6'hf : ((arg3 == 6'hf) ? 6'h10 : ((arg3 == 6'h10) ? 6'h11 : ((arg3 == 6'h11) ? 6'h12 : ((arg3 == 6'h12) ? 6'h13 : ((arg3 == 6'h13) ? 6'h14 : ((arg3 == 6'h14) ? 6'h15 : ((arg3 == 6'h15) ? 6'h16 : ((arg3 == 6'h16) ? 6'h17 : ((arg3 == 6'h17) ? 6'h18 : ((arg3 == 6'h18) ? 6'h19 : ((arg3 == 6'h19) ? 6'h1a : ((arg3 == 6'h1a) ? 6'h1b : ((arg3 == 6'h1b) ? 6'h1c : ((arg3 == 6'h1c) ? 6'h1d : ((arg3 == 6'h1d) ? 6'h1e : ((arg3 == 6'h1e) ? 6'h1f : ((arg3 == 6'h1f) ? 6'h20 : ((arg3 == 6'h20) ? 6'h21 : ((arg3 == 6'h21) ? 6'h22 : ((arg3 == 6'h22) ? 6'h23 : ((arg3 == 6'h23) ? 6'h24 : ((arg3 == 6'h24) ? 6'h25 : ((arg3 == 6'h25) ? 6'h26 : ((arg3 == 6'h26) ? 6'h27 : ((arg3 == 6'h27) ? 6'h28 : ((arg3 == 6'h28) ? 6'h29 : ((arg3 == 6'h29) ? 6'h2a : ((arg3 == 6'h2a) ? 6'h2b : ((arg3 == 6'h2b) ? 6'h2c : ((arg3 == 6'h2c) ? 6'h2d : ((arg3 == 6'h2d) ? 6'h2e : ((arg3 == 6'h2e) ? 6'h2f : ((arg3 == 6'h2f) ? 6'h30 : ((arg3 == 6'h30) ? 6'h31 : ((arg3 == 6'h31) ? 6'h32 : ((arg3 == 6'h32) ? 6'h33 : ((arg3 == 6'h33) ? 6'h34 : ((arg3 == 6'h34) ? 6'h35 : ((arg3 == 6'h35) ? 6'h36 : ((arg3 == 6'h36) ? 6'h37 : ((arg3 == 6'h37) ? 6'h38 : ((arg3 == 6'h38) ? 6'h39 : ((arg3 == 6'h39) ? 6'h3a : ((arg3 == 6'h3a) ? 6'h3b : ((arg3 == 6'h3b) ? 6'h3c : ((arg3 == 6'h3c) ? 6'h3d : ((arg3 == 6'h3d) ? 6'h3e : ((arg3 == 6'h3e) ? 6'h3f : 6'h0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))};
  assign zi95 = zi94[1029:774];
  assign zi96 = zi94[773:262];
  assign zi97 = zi94[261:6];
  assign zi98 = zi94[5:0];
  assign zi99 = {arg3, zi95, zi96, zi97, zi98};
  assign zi100 = zi99[1035:1030];
  assign zi101 = zi99[1029:774];
  assign zi102 = zi99[773:262];
  assign zi103 = zi99[261:6];
  assign zi104 = zi99[5:0];
  assign zi105 = {{11'h1, {8'hfa{1'h0}}}, zi100, zi101, zi102, zi103, zi104};
  assign zi106 = zi105[1035:1030];
  assign zi107 = zi105[1029:774];
  assign zi108 = zi105[773:262];
  assign zi109 = zi105[261:6];
  assign zi110 = zi105[5:0];
  assign res = {{2'h3, {9'h103{1'h0}}}, zi106, zi107, zi108, zi109, zi110};
endmodule

module ZLL_Main_dev50 (input logic [511:0] arg0,
  input logic [255:0] arg1,
  input logic [511:0] arg2,
  input logic [255:0] arg3,
  input logic [5:0] arg4,
  output logic [1029:0] res);
  assign res = {arg1, arg0, arg3, arg4};
endmodule

module ZLL_Pure_dispatch8 (input logic [513:0] arg0,
  input logic [255:0] arg1,
  input logic [511:0] arg2,
  input logic [255:0] arg3,
  input logic [5:0] arg4,
  output logic [1296:0] res);
  logic [1296:0] main_loop_out;
  Main_loop  inst (arg1, arg2, arg3, arg4, main_loop_out);
  assign res = main_loop_out;
endmodule