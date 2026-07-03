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
  logic [5:0] zi5;
  logic [255:0] zi6;
  logic [255:0] zi8;
  logic [5:0] zi9;
  logic [0:0] zi10;
  logic [1296:0] main_loop_out;
  logic [31:0] zi11;
  logic [31:0] zi12;
  logic [31:0] zi13;
  logic [31:0] zi14;
  logic [31:0] zi15;
  logic [31:0] zi16;
  logic [31:0] zi17;
  logic [31:0] zi18;
  logic [31:0] zi19;
  logic [31:0] zi20;
  logic [31:0] zi21;
  logic [31:0] zi22;
  logic [31:0] zi23;
  logic [31:0] zi24;
  logic [31:0] zi25;
  logic [1029:0] zi27;
  logic [255:0] zi28;
  logic [511:0] zi29;
  logic [255:0] zi30;
  logic [5:0] zi31;
  logic [1296:0] zi32;
  logic [255:0] zi33;
  logic [511:0] zi34;
  logic [255:0] zi35;
  logic [5:0] zi36;
  logic [1296:0] main_dev_outR1;
  logic [255:0] zi37;
  logic [255:0] zi39;
  logic [5:0] zi40;
  logic [1296:0] main_loop_outR1;
  logic [1296:0] zres;
  assign zi1 = {__st0, __st1};
  assign zi3 = __st3[261:6];
  assign zi4 = __st3[5:0];
  Main_dev  inst (__in0, zi1, __st2, zi3, zi4, main_dev_out);
  assign zi5 = __resumption_tag[5:0];
  assign zi6 = {__st0, __st1};
  assign zi8 = __st3[261:6];
  assign zi9 = __st3[5:0];
  assign zi10 = zi5 == 6'h3f;
  Main_loop  instR1 (zi6, __st2, zi8, zi9, main_loop_out);
  assign zi11 = __st3[261:230];
  assign zi12 = __st3[229:198];
  assign zi13 = __st3[197:166];
  assign zi14 = __st3[165:134];
  assign zi15 = __st3[133:102];
  assign zi16 = __st3[101:70];
  assign zi17 = __st3[69:38];
  assign zi18 = __st3[37:6];
  assign zi19 = __st0[223:192];
  assign zi20 = __st0[191:160];
  assign zi21 = __st0[159:128];
  assign zi22 = __st0[127:96];
  assign zi23 = __st0[95:64];
  assign zi24 = __st0[63:32];
  assign zi25 = __st0[31:0];
  assign zi27 = {zi6, __st2, zi19 + zi11, zi20 + zi12, zi21 + zi13, zi22 + zi14, zi23 + zi15, zi24 + zi16, zi25 + zi17, __st1 + zi18, zi9};
  assign zi28 = zi27[1029:774];
  assign zi29 = zi27[773:262];
  assign zi30 = zi27[261:6];
  assign zi31 = zi27[5:0];
  assign zi32 = {267'h40, zi28, zi29, zi30, zi31};
  assign zi33 = zi32[1029:774];
  assign zi34 = zi32[773:262];
  assign zi35 = zi32[261:6];
  assign zi36 = zi32[5:0];
  Main_dev  instR2 (__in0, zi33, zi34, zi35, zi36, main_dev_outR1);
  assign zi37 = {__st0, __st1};
  assign zi39 = __st3[261:6];
  assign zi40 = __st3[5:0];
  Main_loop  instR3 (zi37, __st2, zi39, zi40, main_loop_outR1);
  assign zres = (__resumption_tag[7:6] == 2'h1) ? main_dev_out : ((__resumption_tag[7:6] == 2'h2) ? ((zi10 == 1'h0) ? main_loop_out : main_dev_outR1) : main_loop_outR1);
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

module ZLL_Main_bigsigma04 (input logic [31:0] arg0,
  input logic [31:0] arg1,
  output logic [31:0] res);
  assign res = (arg1 >> arg0) | (arg1 << (32'h20 - arg0));
endmodule

module ZLL_Main_dev10 (input logic [255:0] arg0,
  input logic [511:0] arg1,
  input logic [255:0] arg2,
  input logic [5:0] arg3,
  output logic [1296:0] res);
  assign res = {{3'h5, {9'h108{1'h0}}}, arg0, arg1, arg2, arg3};
endmodule

module Main_dev (input logic [513:0] arg0,
  input logic [255:0] arg1,
  input logic [511:0] arg2,
  input logic [255:0] arg3,
  input logic [5:0] arg4,
  output logic [1296:0] res);
  logic [511:0] zi0;
  logic [1029:0] zll_main_dev2_out;
  logic [1029:0] zi1;
  logic [255:0] zi2;
  logic [511:0] zi3;
  logic [255:0] zi4;
  logic [5:0] zi5;
  logic [1296:0] zi6;
  logic [255:0] zi7;
  logic [511:0] zi8;
  logic [255:0] zi9;
  logic [5:0] zi10;
  logic [1296:0] zll_main_dev10_out;
  logic [511:0] zi11;
  logic [1029:0] zll_main_dev2_outR1;
  logic [1029:0] zi12;
  logic [255:0] zi13;
  logic [511:0] zi14;
  logic [255:0] zi15;
  logic [5:0] zi16;
  logic [1296:0] zi17;
  logic [255:0] zi18;
  logic [511:0] zi19;
  logic [255:0] zi20;
  logic [5:0] zi21;
  logic [1296:0] zll_main_dev10_outR1;
  assign zi0 = arg0[511:0];
  ZLL_Main_dev2  inst (zi0, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, arg2, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, 6'h0, zll_main_dev2_out);
  assign zi1 = zll_main_dev2_out;
  assign zi2 = zi1[1029:774];
  assign zi3 = zi1[773:262];
  assign zi4 = zi1[261:6];
  assign zi5 = zi1[5:0];
  assign zi6 = {267'h40, zi2, zi3, zi4, zi5};
  assign zi7 = zi6[1029:774];
  assign zi8 = zi6[773:262];
  assign zi9 = zi6[261:6];
  assign zi10 = zi6[5:0];
  ZLL_Main_dev10  instR1 (zi7, zi8, zi9, zi10, zll_main_dev10_out);
  assign zi11 = arg0[511:0];
  ZLL_Main_dev2  instR2 (zi11, arg3, arg2, arg3, 6'h0, zll_main_dev2_outR1);
  assign zi12 = zll_main_dev2_outR1;
  assign zi13 = zi12[1029:774];
  assign zi14 = zi12[773:262];
  assign zi15 = zi12[261:6];
  assign zi16 = zi12[5:0];
  assign zi17 = {267'h40, zi13, zi14, zi15, zi16};
  assign zi18 = zi17[1029:774];
  assign zi19 = zi17[773:262];
  assign zi20 = zi17[261:6];
  assign zi21 = zi17[5:0];
  ZLL_Main_dev10  instR3 (zi18, zi19, zi20, zi21, zll_main_dev10_outR1);
  assign res = (arg0[513:512] == 2'h0) ? zll_main_dev10_out : ((arg0[513:512] == 2'h1) ? zll_main_dev10_outR1 : {3'h4, arg3, 8'h40, arg1, arg2, arg3, arg4});
endmodule

module Main_loop (input logic [255:0] arg0,
  input logic [511:0] arg1,
  input logic [255:0] arg2,
  input logic [5:0] arg3,
  output logic [1296:0] res);
  logic [31:0] zi0;
  logic [31:0] zi1;
  logic [31:0] zi2;
  logic [31:0] zi3;
  logic [31:0] zi4;
  logic [31:0] zi5;
  logic [31:0] zi6;
  logic [31:0] zi7;
  logic [31:0] zi8;
  logic [31:0] zi9;
  logic [31:0] zi10;
  logic [31:0] zi11;
  logic [31:0] zi12;
  logic [31:0] zi13;
  logic [31:0] zi14;
  logic [31:0] zi15;
  logic [31:0] zi16;
  logic [31:0] zll_main_bigsigma04_out;
  logic [31:0] zll_main_bigsigma04_outR1;
  logic [31:0] zll_main_bigsigma04_outR2;
  logic [31:0] zll_main_bigsigma04_outR3;
  logic [511:0] zi17;
  logic [1061:0] zi18;
  logic [31:0] zi19;
  logic [511:0] zi21;
  logic [255:0] zi22;
  logic [5:0] zi23;
  logic [2047:0] slice_in;
  logic [31:0] zi24;
  logic [31:0] zi25;
  logic [31:0] zi26;
  logic [31:0] zi27;
  logic [31:0] zi28;
  logic [31:0] zi29;
  logic [31:0] zi30;
  logic [31:0] zi31;
  logic [31:0] zi32;
  logic [31:0] zll_main_bigsigma04_outR4;
  logic [31:0] zll_main_bigsigma04_outR5;
  logic [31:0] zll_main_bigsigma04_outR6;
  logic [31:0] zi33;
  logic [31:0] zll_main_bigsigma04_outR7;
  logic [31:0] zll_main_bigsigma04_outR8;
  logic [31:0] zll_main_bigsigma04_outR9;
  logic [1029:0] zi34;
  logic [255:0] zi35;
  logic [511:0] zi36;
  logic [255:0] zi37;
  logic [5:0] zi39;
  logic [1035:0] zt0;
  logic [5:0] v;
  logic [255:0] s0;
  logic [511:0] s1;
  logic [255:0] s2;
  logic [5:0] s3;
  logic [1296:0] zt1;
  logic [5:0] vR1;
  logic [255:0] s0R1;
  logic [511:0] s1R1;
  logic [255:0] s2R1;
  logic [5:0] s3R1;
  assign zi0 = arg1[511:480];
  assign zi1 = arg1[511:480];
  assign zi2 = arg1[479:448];
  assign zi3 = arg1[447:416];
  assign zi4 = arg1[415:384];
  assign zi5 = arg1[383:352];
  assign zi6 = arg1[351:320];
  assign zi7 = arg1[319:288];
  assign zi8 = arg1[287:256];
  assign zi9 = arg1[255:224];
  assign zi10 = arg1[223:192];
  assign zi11 = arg1[191:160];
  assign zi12 = arg1[159:128];
  assign zi13 = arg1[127:96];
  assign zi14 = arg1[95:64];
  assign zi15 = arg1[63:32];
  assign zi16 = arg1[31:0];
  ZLL_Main_bigsigma04  inst (32'h11, zi15, zll_main_bigsigma04_out);
  ZLL_Main_bigsigma04  instR1 (32'h13, zi15, zll_main_bigsigma04_outR1);
  ZLL_Main_bigsigma04  instR2 (32'h7, zi2, zll_main_bigsigma04_outR2);
  ZLL_Main_bigsigma04  instR3 (32'h12, zi2, zll_main_bigsigma04_outR3);
  assign zi17 = {zi2, zi3, zi4, zi5, zi6, zi7, zi8, zi9, zi10, zi11, zi12, zi13, zi14, zi15, zi16, (((zll_main_bigsigma04_out ^ zll_main_bigsigma04_outR1) ^ (zi15 >> 32'ha)) + zi10) + (((zll_main_bigsigma04_outR2 ^ zll_main_bigsigma04_outR3) ^ (zi2 >> 32'h3)) + zi1)};
  assign zi18 = {zi0, arg0, zi17, arg2, arg3};
  assign zi19 = zi18[1061:1030];
  assign zi21 = zi18[773:262];
  assign zi22 = zi18[261:6];
  assign zi23 = zi18[5:0];
  assign slice_in = 2048'h428a2f9871374491b5c0fbcfe9b5dba53956c25b59f111f1923f82a4ab1c5ed5d807aa9812835b01243185be550c7dc372be5d7480deb1fe9bdc06a7c19bf174e49b69c1efbe47860fc19dc6240ca1cc2de92c6f4a7484aa5cb0a9dc76f988da983e5152a831c66db00327c8bf597fc7c6e00bf3d5a7914706ca63511429296727b70a852e1b21384d2c6dfc53380d13650a7354766a0abb81c2c92e92722c85a2bfe8a1a81a664bc24b8b70c76c51a3d192e819d6990624f40e3585106aa07019a4c1161e376c082748774c34b0bcb5391c0cb34ed8aa4a5b9cca4f682e6ff3748f82ee78a5636f84c878148cc7020890befffaa4506cebbef9a3f7c67178f2 >> (((128'h40 - {{7'h7a{1'h0}}, arg3}) - 128'h1) * 128'h20);
  assign zi24 = slice_in[31:0];
  assign zi25 = zi18[1029:998];
  assign zi26 = zi18[997:966];
  assign zi27 = zi18[965:934];
  assign zi28 = zi18[933:902];
  assign zi29 = zi18[901:870];
  assign zi30 = zi18[869:838];
  assign zi31 = zi18[837:806];
  assign zi32 = zi18[805:774];
  ZLL_Main_bigsigma04  instR4 (32'h6, zi29, zll_main_bigsigma04_outR4);
  ZLL_Main_bigsigma04  instR5 (32'hb, zi29, zll_main_bigsigma04_outR5);
  ZLL_Main_bigsigma04  instR6 (32'h19, zi29, zll_main_bigsigma04_outR6);
  assign zi33 = zi32 + ((((zll_main_bigsigma04_outR4 ^ zll_main_bigsigma04_outR5) ^ zll_main_bigsigma04_outR6) + ((zi29 & zi30) ^ ((~zi29) & zi31))) + (zi24 + zi19));
  ZLL_Main_bigsigma04  instR7 (32'h2, zi25, zll_main_bigsigma04_outR7);
  ZLL_Main_bigsigma04  instR8 (32'hd, zi25, zll_main_bigsigma04_outR8);
  ZLL_Main_bigsigma04  instR9 (32'h16, zi25, zll_main_bigsigma04_outR9);
  assign zi34 = {{zi33 + (((zll_main_bigsigma04_outR7 ^ zll_main_bigsigma04_outR8) ^ zll_main_bigsigma04_outR9) + (((zi25 & zi26) ^ (zi25 & zi27)) ^ (zi26 & zi27))), zi25, zi26, zi27, zi28 + zi33, zi29, zi30, zi31}, zi21, zi22, zi23};
  assign zi35 = zi34[1029:774];
  assign zi36 = zi34[773:262];
  assign zi37 = zi34[261:6];
  assign zi39 = arg3 + 6'h1;
  assign zt0 = {arg3, zi35, zi36, zi37, zi39};
  assign v = zt0[1035:1030];
  assign s0 = zt0[1029:774];
  assign s1 = zt0[773:262];
  assign s2 = zt0[261:6];
  assign s3 = zt0[5:0];
  assign zt1 = {{9'h105{1'h0}}, v, s0, s1, s2, s3};
  assign vR1 = zt1[1035:1030];
  assign s0R1 = zt1[1029:774];
  assign s1R1 = zt1[773:262];
  assign s2R1 = zt1[261:6];
  assign s3R1 = zt1[5:0];
  assign res = {261'h180000000000000000000000000000000000000000000000000000000000000002, vR1, s0R1, s1R1, s2R1, s3R1};
endmodule

module ZLL_Main_dev2 (input logic [511:0] arg0,
  input logic [255:0] arg1,
  input logic [511:0] arg2,
  input logic [255:0] arg3,
  input logic [5:0] arg4,
  output logic [1029:0] res);
  assign res = {arg1, arg0, arg3, arg4};
endmodule