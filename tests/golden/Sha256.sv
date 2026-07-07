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
  logic [479:0] __st2;
  logic [479:0] __st2_next;
  logic [31:0] __st3;
  logic [31:0] __st3_next;
  logic [223:0] __st4;
  logic [223:0] __st4_next;
  logic [31:0] __st5;
  logic [31:0] __st5_next;
  logic [5:0] __st6;
  logic [5:0] __st6_next;
  logic [5:0] zi1;
  logic [255:0] zi2;
  logic [511:0] zi3;
  logic [255:0] zi4;
  logic [0:0] zi6;
  logic [1295:0] main_$l_main_loop6$705_out;
  logic [31:0] zi7;
  logic [31:0] zi8;
  logic [31:0] zi9;
  logic [31:0] zi10;
  logic [31:0] zi11;
  logic [31:0] zi12;
  logic [31:0] zi13;
  logic [31:0] zi15;
  logic [31:0] zi16;
  logic [31:0] zi17;
  logic [31:0] zi18;
  logic [31:0] zi19;
  logic [31:0] zi20;
  logic [31:0] zi21;
  logic [255:0] zi23;
  logic [1295:0] main_$l_main_dev3$702_out;
  logic [255:0] zi24;
  logic [511:0] zi25;
  logic [255:0] zi26;
  logic [1295:0] main_$l_main_loop6$705_outR1;
  logic [255:0] zi28;
  logic [511:0] zi29;
  logic [255:0] zi30;
  logic [1295:0] main_$l_main_dev3$702_outR1;
  logic [1295:0] zres;
  assign zi1 = __resumption_tag[5:0];
  assign zi2 = {__st0, __st1};
  assign zi3 = {__st2, __st3};
  assign zi4 = {__st4, __st5};
  assign zi6 = zi1 == 6'h3f;
  main_$L_Main_loop6$705  inst (zi2, zi3, zi4, __st6, main_$l_main_loop6$705_out);
  assign zi7 = __st4[223:192];
  assign zi8 = __st4[191:160];
  assign zi9 = __st4[159:128];
  assign zi10 = __st4[127:96];
  assign zi11 = __st4[95:64];
  assign zi12 = __st4[63:32];
  assign zi13 = __st4[31:0];
  assign zi15 = __st0[223:192];
  assign zi16 = __st0[191:160];
  assign zi17 = __st0[159:128];
  assign zi18 = __st0[127:96];
  assign zi19 = __st0[95:64];
  assign zi20 = __st0[63:32];
  assign zi21 = __st0[31:0];
  assign zi23 = {zi15 + zi7, zi16 + zi8, zi17 + zi9, zi18 + zi10, zi19 + zi11, zi20 + zi12, zi21 + zi13, __st1 + __st5};
  main_$L_Main_dev3$702  instR1 (__in0, zi2, zi3, zi23, __st6, main_$l_main_dev3$702_out);
  assign zi24 = {__st0, __st1};
  assign zi25 = {__st2, __st3};
  assign zi26 = {__st4, __st5};
  main_$L_Main_loop6$705  instR2 (zi24, zi25, zi26, __st6, main_$l_main_loop6$705_outR1);
  assign zi28 = {__st0, __st1};
  assign zi29 = {__st2, __st3};
  assign zi30 = {__st4, __st5};
  main_$L_Main_dev3$702  instR3 (__in0, zi28, zi29, zi30, __st6, main_$l_main_dev3$702_outR1);
  assign zres = (__resumption_tag[7:6] == 2'h0) ? ((zi6 == 1'h0) ? main_$l_main_loop6$705_out : main_$l_main_dev3$702_out) : ((__resumption_tag[7:6] == 2'h1) ? main_$l_main_loop6$705_outR1 : main_$l_main_dev3$702_outR1);
  assign __resumption_tag_next = zres[1037:1030];
  assign __st0_next = zres[1029:806];
  assign __st1_next = zres[805:774];
  assign __st2_next = zres[773:294];
  assign __st3_next = zres[293:262];
  assign __st4_next = zres[261:38];
  assign __st5_next = zres[37:6];
  assign __st6_next = zres[5:0];
  assign __out0 = zres[1295:1038];
  initial {__resumption_tag, __st0, __st1, __st2, __st3, __st4, __st5, __st6} = {1'h1, {11'h40d{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1, __st2, __st3, __st4, __st5, __st6} <= {1'h1, {11'h40d{1'h0}}};
    end else begin
      {__resumption_tag, __st0, __st1, __st2, __st3, __st4, __st5, __st6} <= {__resumption_tag_next, __st0_next, __st1_next, __st2_next, __st3_next, __st4_next, __st5_next, __st6_next};
    end
  end
endmodule

module main_$L_Main_loop6$705 (input logic [255:0] arg0,
  input logic [511:0] arg1,
  input logic [255:0] arg2,
  input logic [5:0] arg3,
  output logic [1295:0] res);
  logic [31:0] zi0;
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
  logic [31:0] zi26;
  logic [31:0] zi27;
  logic [31:0] zi28;
  logic [31:0] zi29;
  logic [31:0] zi30;
  logic [31:0] zi31;
  logic [511:0] zi32;
  logic [2047:0] slice_in;
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
  logic [255:0] zi43;
  logic [5:0] zi44;
  assign zi0 = arg1[511:480];
  assign zi16 = arg1[511:480];
  assign zi17 = arg1[479:448];
  assign zi18 = arg1[447:416];
  assign zi19 = arg1[415:384];
  assign zi20 = arg1[383:352];
  assign zi21 = arg1[351:320];
  assign zi22 = arg1[319:288];
  assign zi23 = arg1[287:256];
  assign zi24 = arg1[255:224];
  assign zi25 = arg1[223:192];
  assign zi26 = arg1[191:160];
  assign zi27 = arg1[159:128];
  assign zi28 = arg1[127:96];
  assign zi29 = arg1[95:64];
  assign zi30 = arg1[63:32];
  assign zi31 = arg1[31:0];
  assign zi32 = {zi17, zi18, zi19, zi20, zi21, zi22, zi23, zi24, zi25, zi26, zi27, zi28, zi29, zi30, zi31, (((((zi30 >> 32'h11) | (zi30 << 32'hf)) ^ ((zi30 >> 32'h13) | (zi30 << 32'hd))) ^ (zi30 >> 32'ha)) + zi25) + (((((zi17 >> 32'h7) | (zi17 << 32'h19)) ^ ((zi17 >> 32'h12) | (zi17 << 32'he))) ^ (zi17 >> 32'h3)) + zi16)};
  assign slice_in = 2048'h428a2f9871374491b5c0fbcfe9b5dba53956c25b59f111f1923f82a4ab1c5ed5d807aa9812835b01243185be550c7dc372be5d7480deb1fe9bdc06a7c19bf174e49b69c1efbe47860fc19dc6240ca1cc2de92c6f4a7484aa5cb0a9dc76f988da983e5152a831c66db00327c8bf597fc7c6e00bf3d5a7914706ca63511429296727b70a852e1b21384d2c6dfc53380d13650a7354766a0abb81c2c92e92722c85a2bfe8a1a81a664bc24b8b70c76c51a3d192e819d6990624f40e3585106aa07019a4c1161e376c082748774c34b0bcb5391c0cb34ed8aa4a5b9cca4f682e6ff3748f82ee78a5636f84c878148cc7020890befffaa4506cebbef9a3f7c67178f2 >> (((128'h40 - {{7'h7a{1'h0}}, arg3}) - 128'h1) * 128'h20);
  assign zi33 = slice_in[31:0];
  assign zi34 = arg0[255:224];
  assign zi35 = arg0[223:192];
  assign zi36 = arg0[191:160];
  assign zi37 = arg0[159:128];
  assign zi38 = arg0[127:96];
  assign zi39 = arg0[95:64];
  assign zi40 = arg0[63:32];
  assign zi41 = arg0[31:0];
  assign zi42 = zi41 + ((((((zi38 >> 32'h6) | (zi38 << 32'h1a)) ^ ((zi38 >> 32'hb) | (zi38 << 32'h15))) ^ ((zi38 >> 32'h19) | (zi38 << 32'h7))) + ((zi38 & zi39) ^ ((~zi38) & zi40))) + (zi33 + zi0));
  assign zi43 = {zi42 + (((((zi34 >> 32'h2) | (zi34 << 32'h1e)) ^ ((zi34 >> 32'hd) | (zi34 << 32'h13))) ^ ((zi34 >> 32'h16) | (zi34 << 32'ha))) + (((zi34 & zi35) ^ (zi34 & zi36)) ^ (zi35 & zi36))), zi34, zi35, zi36, zi37 + zi42, zi38, zi39, zi40};
  assign zi44 = arg3 + 6'h1;
  assign res = {{1'h1, {9'h103{1'h0}}}, arg3, zi43, zi32, arg2, zi44};
endmodule

module main_$L___unused26$730 (input logic [511:0] arg0,
  input logic [255:0] arg1,
  input logic [511:0] arg2,
  input logic [255:0] arg3,
  input logic [5:0] arg4,
  output logic [1295:0] res);
  assign res = {266'h1000000000000000000000000000000000000000000000000000000000000000040, arg1, arg0, arg3, arg4};
endmodule

module main_$L_Main_dev3$702 (input logic [513:0] arg0,
  input logic [255:0] arg1,
  input logic [511:0] arg2,
  input logic [255:0] arg3,
  input logic [5:0] arg4,
  output logic [1295:0] res);
  logic [511:0] hw32;
  logic [1295:0] main_$l__unused26$730_out;
  logic [511:0] hw32R1;
  logic [1295:0] main_$l__unused26$730_outR1;
  logic [257:0] zi0;
  assign hw32 = arg0[511:0];
  main_$L___unused26$730  inst (hw32, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, arg2, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, 6'h0, main_$l__unused26$730_out);
  assign hw32R1 = arg0[511:0];
  main_$L___unused26$730  instR1 (hw32R1, arg3, arg2, arg3, 6'h0, main_$l__unused26$730_outR1);
  assign zi0 = {2'h0, arg3};
  assign res = (arg0[513:512] == 2'h0) ? main_$l__unused26$730_out : ((arg0[513:512] == 2'h1) ? main_$l__unused26$730_outR1 : {zi0, 8'h80, arg1, arg2, arg3, arg4});
endmodule