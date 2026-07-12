module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [513:0] __in0,
  output logic [257:0] __out0);
  logic [1295:0] main_loop_out;
  logic [1295:0] main_dev_out;
  logic [1295:0] main_dev_outR1;
  logic [1295:0] Zres;
  // state registers
  // __resumption_tag: 8 bits, init 0x80
  //   states: 0=i 1=_unused2 2=$x2
  // __st0: 224 bits, init 0x0
  // __st1: 32 bits, init 0x0
  // __st2: 480 bits, init 0x0
  // __st3: 32 bits, init 0x0
  // __st4: 224 bits, init 0x0
  // __st5: 32 bits, init 0x0
  // __st6: 6 bits, init 0x0
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
  // combinational logic
  wire [5:0] ctr = __resumption_tag[5:0];
  wire [255:0] s0 = {__st0, __st1};
  wire [511:0] s1 = {__st2, __st3};
  wire [255:0] s2 = {__st4, __st5};
  wire [0:0] Za = ctr == 6'h3f;
  main_loop  loop_i (s0, s1, s2, __st6, main_loop_out);
  wire [31:0] h1 = __st4[223:192];
  wire [31:0] h2 = __st4[191:160];
  wire [31:0] h3 = __st4[159:128];
  wire [31:0] h4 = __st4[127:96];
  wire [31:0] h5 = __st4[95:64];
  wire [31:0] h6 = __st4[63:32];
  wire [31:0] h7 = __st4[31:0];
  wire [31:0] a = __st0[223:192];
  wire [31:0] b = __st0[191:160];
  wire [31:0] c = __st0[159:128];
  wire [31:0] d = __st0[127:96];
  wire [31:0] e = __st0[95:64];
  wire [31:0] f = __st0[63:32];
  wire [31:0] g = __st0[31:0];
  wire [255:0] ZaR1 = {a + h1, b + h2, c + h3, d + h4, e + h5, f + h6, g + h7, __st1 + __st5};
  main_dev  dev_i (__in0, s0, s1, ZaR1, __st6, main_dev_out);
  main_dev  dev_iR1 (__in0, s0, s1, s2, __st6, main_dev_outR1);
  wire [1:0] scrut = __resumption_tag[7:6];
  always_comb case (scrut)
    2'h0: Zres = (~Za) ? main_loop_out : main_dev_out;
    2'h1: Zres = main_loop_out;
    default: Zres = main_dev_outR1;
  endcase
  assign __resumption_tag_next = Zres[1037:1030];
  assign __st0_next = Zres[1029:806];
  assign __st1_next = Zres[805:774];
  assign __st2_next = Zres[773:294];
  assign __st3_next = Zres[293:262];
  assign __st4_next = Zres[261:38];
  assign __st5_next = Zres[37:6];
  assign __st6_next = Zres[5:0];
  // outputs
  assign __out0 = Zres[1295:1038];
  // state register update
  initial __resumption_tag = 8'h80;
  initial __st0 = {8'he0{1'h0}};
  initial __st1 = {6'h20{1'h0}};
  initial __st2 = {9'h1e0{1'h0}};
  initial __st3 = {6'h20{1'h0}};
  initial __st4 = {8'he0{1'h0}};
  initial __st5 = {6'h20{1'h0}};
  initial __st6 = 6'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 8'h80;
      __st0 <= {8'he0{1'h0}};
      __st1 <= {6'h20{1'h0}};
      __st2 <= {9'h1e0{1'h0}};
      __st3 <= {6'h20{1'h0}};
      __st4 <= {8'he0{1'h0}};
      __st5 <= {6'h20{1'h0}};
      __st6 <= 6'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
      __st1 <= __st1_next;
      __st2 <= __st2_next;
      __st3 <= __st3_next;
      __st4 <= __st4_next;
      __st5 <= __st5_next;
      __st6 <= __st6_next;
    end
  end
endmodule

// main.loop
// block '$L.Main.loop' of process main
module main_loop (input logic [255:0] s0,
  input logic [511:0] s1,
  input logic [255:0] s2,
  input logic [5:0] s3,
  output logic [1295:0] res);
  wire [31:0] w00 = s1[511:480];
  wire [31:0] w01 = s1[479:448];
  wire [31:0] w02 = s1[447:416];
  wire [31:0] w03 = s1[415:384];
  wire [31:0] w04 = s1[383:352];
  wire [31:0] w05 = s1[351:320];
  wire [31:0] w06 = s1[319:288];
  wire [31:0] w07 = s1[287:256];
  wire [31:0] w08 = s1[255:224];
  wire [31:0] w09 = s1[223:192];
  wire [31:0] w10 = s1[191:160];
  wire [31:0] w11 = s1[159:128];
  wire [31:0] w12 = s1[127:96];
  wire [31:0] w13 = s1[95:64];
  wire [31:0] w14 = s1[63:32];
  wire [31:0] w15 = s1[31:0];
  wire [511:0] Za = {w01, w02, w03, w04, w05, w06, w07, w08, w09, w10, w11, w12, w13, w14, w15,
    (((((w14 >> 32'h11) | (w14 << 32'hf)) ^ ((w14 >> 32'h13) | (w14 << 32'hd))) ^ (w14 >> 32'ha)) + w09) + (((((w01 >> 32'h7) | (w01 << 32'h19)) ^ ((w01 >> 32'h12) | (w01 << 32'he))) ^ (w01 >> 32'h3)) + w00)};
  wire [2047:0] slice_in = 2048'h428a2f9871374491b5c0fbcfe9b5dba53956c25b59f111f1923f82a4ab1c5ed5d807aa9812835b01243185be550c7dc372be5d7480deb1fe9bdc06a7c19bf174e49b69c1efbe47860fc19dc6240ca1cc2de92c6f4a7484aa5cb0a9dc76f988da983e5152a831c66db00327c8bf597fc7c6e00bf3d5a7914706ca63511429296727b70a852e1b21384d2c6dfc53380d13650a7354766a0abb81c2c92e92722c85a2bfe8a1a81a664bc24b8b70c76c51a3d192e819d6990624f40e3585106aa07019a4c1161e376c082748774c34b0bcb5391c0cb34ed8aa4a5b9cca4f682e6ff3748f82ee78a5636f84c878148cc7020890befffaa4506cebbef9a3f7c67178f2 >> (((128'h40 - {{7'h7a{1'h0}}, s3}) - 128'h1) * 128'h20);
  wire [31:0] ZaR1 = slice_in[31:0];
  wire [31:0] a = s0[255:224];
  wire [31:0] b = s0[223:192];
  wire [31:0] c = s0[191:160];
  wire [31:0] d = s0[159:128];
  wire [31:0] e = s0[127:96];
  wire [31:0] f = s0[95:64];
  wire [31:0] g = s0[63:32];
  wire [31:0] h = s0[31:0];
  wire [31:0] t1 = h + ((((((e >> 32'h6) | (e << 32'h1a)) ^ ((e >> 32'hb) | (e << 32'h15))) ^ ((e >> 32'h19) | (e << 32'h7))) + ((e & f) ^ ((~e) & g))) + (ZaR1 + w00));
  wire [255:0] Za1 = {t1 + (((((a >> 32'h2) | (a << 32'h1e)) ^ ((a >> 32'hd) | (a << 32'h13))) ^ ((a >> 32'h16) | (a << 32'ha))) + (((a & b) ^ (a & c)) ^ (b & c))),
    a, b, c, d + t1, e, f, g};
  wire [5:0] ZaR2 = s3 + 6'h1;
  assign res = {{1'h1, {9'h103{1'h0}}}, s3, Za1, Za, s2, ZaR2};
endmodule

// main._unused7
// block '$L._unused7' of process main
module main___unused7 (input logic [511:0] hw32,
  input logic [255:0] s0,
  input logic [511:0] s1,
  input logic [255:0] s2,
  input logic [5:0] s3,
  output logic [1295:0] res);
  assign res = {266'h1000000000000000000000000000000000000000000000000000000000000000040, s0, hw32, s2, s3};
endmodule

// main.dev
// block '$L.Main.dev' of process main
module main_dev (input logic [513:0] Zds,
  input logic [255:0] s0,
  input logic [511:0] s1,
  input logic [255:0] s2,
  input logic [5:0] s3,
  output logic [1295:0] res);
  logic [1295:0] main__unused7_out;
  logic [1295:0] main__unused7_outR1;
  wire [511:0] hw32 = Zds[511:0];
  main___unused7  _unused7_i (hw32, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, s1, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, 6'h0, main__unused7_out);
  main___unused7  _unused7_iR1 (hw32, s2, s1, s2, 6'h0, main__unused7_outR1);
  wire [257:0] Za = {2'h0, s2};
  wire [1:0] scrut = Zds[513:512];
  always_comb case (scrut)
    2'h0: res = main__unused7_out;
    2'h1: res = main__unused7_outR1;
    default: res = {Za, 8'h80, s0, s1, s2, s3};
  endcase
endmodule