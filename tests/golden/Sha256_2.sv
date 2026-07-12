module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [67:0] __in0,
  output logic [64:0] __out0);
  logic [1102:0] main_dev_out;
  logic [31:0] Main_plusW32_out;
  logic [31:0] Main_plusW32_outR1;
  logic [31:0] Main_plusW32_outR2;
  logic [31:0] Main_plusW32_outR3;
  logic [31:0] Main_plusW32_outR4;
  logic [31:0] Main_plusW32_outR5;
  logic [31:0] Main_plusW32_outR6;
  logic [31:0] Main_plusW32_outR7;
  logic [1102:0] main_dev_outR1;
  logic [1102:0] main_genhash$_out;
  logic [1102:0] Zres;
  // state registers
  // __resumption_tag: 8 bits, init 0x0
  //   states: 0=$x2 1=i 2=_unused11
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
  wire [255:0] s0 = {__st0, __st1};
  wire [511:0] s1 = {__st2, __st3};
  wire [255:0] s2 = {__st4, __st5};
  main_dev  dev_i (__in0, s0, s1, s2, __st6, main_dev_out);
  wire [5:0] ctr = __resumption_tag[5:0];
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
  Main_plusW32  plusW32_i (a, h1, Main_plusW32_out);
  Main_plusW32  plusW32_iR1 (b, h2, Main_plusW32_outR1);
  Main_plusW32  plusW32_iR2 (c, h3, Main_plusW32_outR2);
  Main_plusW32  plusW32_iR3 (d, h4, Main_plusW32_outR3);
  Main_plusW32  plusW32_iR4 (e, h5, Main_plusW32_outR4);
  Main_plusW32  plusW32_iR5 (f, h6, Main_plusW32_outR5);
  Main_plusW32  plusW32_iR6 (g, h7, Main_plusW32_outR6);
  Main_plusW32  plusW32_iR7 (__st1, __st5, Main_plusW32_outR7);
  wire [255:0] Za8 = {Main_plusW32_out, Main_plusW32_outR1, Main_plusW32_outR2, Main_plusW32_outR3, Main_plusW32_outR4,
    Main_plusW32_outR5, Main_plusW32_outR6, Main_plusW32_outR7};
  main_dev  dev_iR1 (__in0, s0, s1, Za8, __st6, main_dev_outR1);
  main_genhash$  genhash$_i (s0, s1, s2, __st6, main_genhash$_out);
  wire [1:0] scrut = __resumption_tag[7:6];
  always_comb case (scrut)
    2'h0: Zres = main_dev_out;
    2'h1: Zres = (ctr == 6'h3f) ? main_dev_outR1 : main_genhash$_out;
    default: Zres = main_genhash$_out;
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
  assign __out0 = Zres[1102:1038];
  // state register update
  initial __resumption_tag = 8'h0;
  initial __st0 = {8'he0{1'h0}};
  initial __st1 = {6'h20{1'h0}};
  initial __st2 = {9'h1e0{1'h0}};
  initial __st3 = {6'h20{1'h0}};
  initial __st4 = {8'he0{1'h0}};
  initial __st5 = {6'h20{1'h0}};
  initial __st6 = 6'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 8'h0;
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

// main._unused
// block '$L._unused' of process main
// also: main.devsha256'
module main___unused (input logic [255:0] s0,
  input logic [511:0] s1,
  input logic [255:0] s2,
  input logic [5:0] s3,
  output logic [1102:0] res);
  assign res = {{1'h1, {7'h48{1'h0}}}, s0, s1, s2, s3};
endmodule

// main.arm2
// block '$L.arm2' of process main
// also: main._unused2
module main_arm2 (input logic [31:0] w1,
  input logic [31:0] w2,
  input logic [255:0] s0,
  input logic [511:0] s1,
  input logic [255:0] s2,
  input logic [5:0] s3,
  output logic [1102:0] res);
  logic [1102:0] main__unused_out;
  wire [31:0] x2 = s1[447:416];
  wire [31:0] x3 = s1[415:384];
  wire [31:0] x4 = s1[383:352];
  wire [31:0] x5 = s1[351:320];
  wire [31:0] x6 = s1[319:288];
  wire [31:0] x7 = s1[287:256];
  wire [31:0] x8 = s1[255:224];
  wire [31:0] x9 = s1[223:192];
  wire [31:0] xa = s1[191:160];
  wire [31:0] xb = s1[159:128];
  wire [31:0] xc = s1[127:96];
  wire [31:0] xd = s1[95:64];
  wire [31:0] xe = s1[63:32];
  wire [31:0] xf = s1[31:0];
  wire [511:0] Za = {w1, w2, x2, x3, x4, x5, x6, x7, x8, x9, xa, xb, xc, xd, xe, xf};
  main___unused  _unused_i (s0, Za, s2, s3, main__unused_out);
  assign res = main__unused_out;
endmodule

// main.genhash'
// block '$L.Main.genhash'' of process main
module main_genhash$ (input logic [255:0] s0,
  input logic [511:0] s1,
  input logic [255:0] s2,
  input logic [5:0] s3,
  output logic [1102:0] res);
  logic [31:0] Main_xorW32_out;
  logic [31:0] Main_xorW32_outR1;
  logic [31:0] Main_plusW32_out;
  logic [31:0] Main_xorW32_outR2;
  logic [31:0] Main_xorW32_outR3;
  logic [31:0] Main_plusW32_outR1;
  logic [31:0] Main_plusW32_outR2;
  logic [31:0] ZaR1;
  logic [31:0] Main_xorW32_outR4;
  logic [31:0] Main_xorW32_outR5;
  logic [31:0] Main_andW32_out;
  logic [31:0] extres;
  logic [31:0] Main_andW32_outR1;
  logic [31:0] Main_xorW32_outR6;
  logic [31:0] Main_plusW32_outR3;
  logic [31:0] Main_plusW32_outR4;
  logic [31:0] Main_plusW32_outR5;
  logic [31:0] Main_plusW32_outR6;
  logic [31:0] Main_xorW32_outR7;
  logic [31:0] Main_xorW32_outR8;
  logic [31:0] Main_andW32_outR2;
  logic [31:0] Main_andW32_outR3;
  logic [31:0] Main_xorW32_outR9;
  logic [31:0] Main_andW32_outR4;
  logic [31:0] Main_xorW32_outR10;
  logic [31:0] Main_plusW32_outR7;
  logic [31:0] Main_plusW32_outR8;
  logic [31:0] Main_plusW32_outR9;
  logic [5:0] ZaR2;
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
  wire [0:0] b0 = s1[63];
  wire [0:0] b1 = s1[62];
  wire [0:0] b2 = s1[61];
  wire [0:0] b3 = s1[60];
  wire [0:0] b4 = s1[59];
  wire [0:0] b5 = s1[58];
  wire [0:0] b6 = s1[57];
  wire [0:0] b7 = s1[56];
  wire [0:0] b8 = s1[55];
  wire [0:0] b9 = s1[54];
  wire [0:0] b10 = s1[53];
  wire [0:0] b11 = s1[52];
  wire [0:0] b12 = s1[51];
  wire [0:0] b13 = s1[50];
  wire [0:0] b14 = s1[49];
  wire [0:0] b15 = s1[48];
  wire [0:0] b16 = s1[47];
  wire [0:0] b17 = s1[46];
  wire [0:0] b18 = s1[45];
  wire [0:0] b19 = s1[44];
  wire [0:0] b20 = s1[43];
  wire [0:0] b21 = s1[42];
  wire [0:0] b22 = s1[41];
  wire [0:0] b23 = s1[40];
  wire [0:0] b24 = s1[39];
  wire [0:0] b25 = s1[38];
  wire [0:0] b26 = s1[37];
  wire [0:0] b27 = s1[36];
  wire [0:0] b28 = s1[35];
  wire [0:0] b29 = s1[34];
  wire [0:0] b30 = s1[33];
  wire [0:0] b31 = s1[32];
  Main_xorW32  xorW32_i ({b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b0, b1,
    b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14}, {b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23,
    b24, b25, b26, b27, b28, b29, b30, b31, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12}, Main_xorW32_out);
  Main_xorW32  xorW32_iR1 (Main_xorW32_out, {10'h0, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14,
    b15, b16, b17, b18, b19, b20, b21}, Main_xorW32_outR1);
  Main_plusW32  plusW32_i (Main_xorW32_outR1, w09, Main_plusW32_out);
  wire [0:0] b0R1 = s1[479];
  wire [0:0] b1R1 = s1[478];
  wire [0:0] b2R1 = s1[477];
  wire [0:0] b3R1 = s1[476];
  wire [0:0] b4R1 = s1[475];
  wire [0:0] b5R1 = s1[474];
  wire [0:0] b6R1 = s1[473];
  wire [0:0] b7R1 = s1[472];
  wire [0:0] b8R1 = s1[471];
  wire [0:0] b9R1 = s1[470];
  wire [0:0] b10R1 = s1[469];
  wire [0:0] b11R1 = s1[468];
  wire [0:0] b12R1 = s1[467];
  wire [0:0] b13R1 = s1[466];
  wire [0:0] b14R1 = s1[465];
  wire [0:0] b15R1 = s1[464];
  wire [0:0] b16R1 = s1[463];
  wire [0:0] b17R1 = s1[462];
  wire [0:0] b18R1 = s1[461];
  wire [0:0] b19R1 = s1[460];
  wire [0:0] b20R1 = s1[459];
  wire [0:0] b21R1 = s1[458];
  wire [0:0] b22R1 = s1[457];
  wire [0:0] b23R1 = s1[456];
  wire [0:0] b24R1 = s1[455];
  wire [0:0] b25R1 = s1[454];
  wire [0:0] b26R1 = s1[453];
  wire [0:0] b27R1 = s1[452];
  wire [0:0] b28R1 = s1[451];
  wire [0:0] b29R1 = s1[450];
  wire [0:0] b30R1 = s1[449];
  wire [0:0] b31R1 = s1[448];
  Main_xorW32  xorW32_iR2 ({b25R1, b26R1, b27R1, b28R1, b29R1, b30R1, b31R1, b0R1, b1R1, b2R1, b3R1, b4R1, b5R1, b6R1,
    b7R1, b8R1, b9R1, b10R1, b11R1, b12R1, b13R1, b14R1, b15R1, b16R1, b17R1, b18R1, b19R1, b20R1, b21R1, b22R1, b23R1,
    b24R1}, {b14R1, b15R1, b16R1, b17R1, b18R1, b19R1, b20R1, b21R1, b22R1, b23R1, b24R1, b25R1, b26R1, b27R1, b28R1,
    b29R1, b30R1, b31R1, b0R1, b1R1, b2R1, b3R1, b4R1, b5R1, b6R1, b7R1, b8R1, b9R1, b10R1, b11R1, b12R1,
    b13R1}, Main_xorW32_outR2);
  Main_xorW32  xorW32_iR3 (Main_xorW32_outR2, {3'h0, b0R1, b1R1, b2R1, b3R1, b4R1, b5R1, b6R1, b7R1, b8R1, b9R1, b10R1,
    b11R1, b12R1, b13R1, b14R1, b15R1, b16R1, b17R1, b18R1, b19R1, b20R1, b21R1, b22R1, b23R1, b24R1, b25R1, b26R1,
    b27R1, b28R1}, Main_xorW32_outR3);
  Main_plusW32  plusW32_iR1 (Main_xorW32_outR3, w00, Main_plusW32_outR1);
  Main_plusW32  plusW32_iR2 (Main_plusW32_out, Main_plusW32_outR1, Main_plusW32_outR2);
  wire [511:0] Za = {w01, w02, w03, w04, w05, w06, w07, w08, w09, w10, w11, w12, w13, w14, w15, Main_plusW32_outR2};
  always_comb case (s3)
    6'h0: ZaR1 = 32'h428a2f98;
    6'h1: ZaR1 = 32'h71374491;
    6'h2: ZaR1 = 32'hb5c0fbcf;
    6'h3: ZaR1 = 32'he9b5dba5;
    6'h4: ZaR1 = 32'h3956c25b;
    6'h5: ZaR1 = 32'h59f111f1;
    6'h6: ZaR1 = 32'h923f82a4;
    6'h7: ZaR1 = 32'hab1c5ed5;
    6'h8: ZaR1 = 32'hd807aa98;
    6'h9: ZaR1 = 32'h12835b01;
    6'ha: ZaR1 = 32'h243185be;
    6'hb: ZaR1 = 32'h550c7dc3;
    6'hc: ZaR1 = 32'h72be5d74;
    6'hd: ZaR1 = 32'h80deb1fe;
    6'he: ZaR1 = 32'h9bdc06a7;
    6'hf: ZaR1 = 32'hc19bf174;
    6'h10: ZaR1 = 32'he49b69c1;
    6'h11: ZaR1 = 32'hefbe4786;
    6'h12: ZaR1 = 32'hfc19dc6;
    6'h13: ZaR1 = 32'h240ca1cc;
    6'h14: ZaR1 = 32'h2de92c6f;
    6'h15: ZaR1 = 32'h4a7484aa;
    6'h16: ZaR1 = 32'h5cb0a9dc;
    6'h17: ZaR1 = 32'h76f988da;
    6'h18: ZaR1 = 32'h983e5152;
    6'h19: ZaR1 = 32'ha831c66d;
    6'h1a: ZaR1 = 32'hb00327c8;
    6'h1b: ZaR1 = 32'hbf597fc7;
    6'h1c: ZaR1 = 32'hc6e00bf3;
    6'h1d: ZaR1 = 32'hd5a79147;
    6'h1e: ZaR1 = 32'h6ca6351;
    6'h1f: ZaR1 = 32'h14292967;
    6'h20: ZaR1 = 32'h27b70a85;
    6'h21: ZaR1 = 32'h2e1b2138;
    6'h22: ZaR1 = 32'h4d2c6dfc;
    6'h23: ZaR1 = 32'h53380d13;
    6'h24: ZaR1 = 32'h650a7354;
    6'h25: ZaR1 = 32'h766a0abb;
    6'h26: ZaR1 = 32'h81c2c92e;
    6'h27: ZaR1 = 32'h92722c85;
    6'h28: ZaR1 = 32'ha2bfe8a1;
    6'h29: ZaR1 = 32'ha81a664b;
    6'h2a: ZaR1 = 32'hc24b8b70;
    6'h2b: ZaR1 = 32'hc76c51a3;
    6'h2c: ZaR1 = 32'hd192e819;
    6'h2d: ZaR1 = 32'hd6990624;
    6'h2e: ZaR1 = 32'hf40e3585;
    6'h2f: ZaR1 = 32'h106aa070;
    6'h30: ZaR1 = 32'h19a4c116;
    6'h31: ZaR1 = 32'h1e376c08;
    6'h32: ZaR1 = 32'h2748774c;
    6'h33: ZaR1 = 32'h34b0bcb5;
    6'h34: ZaR1 = 32'h391c0cb3;
    6'h35: ZaR1 = 32'h4ed8aa4a;
    6'h36: ZaR1 = 32'h5b9cca4f;
    6'h37: ZaR1 = 32'h682e6ff3;
    6'h38: ZaR1 = 32'h748f82ee;
    6'h39: ZaR1 = 32'h78a5636f;
    6'h3a: ZaR1 = 32'h84c87814;
    6'h3b: ZaR1 = 32'h8cc70208;
    6'h3c: ZaR1 = 32'h90befffa;
    6'h3d: ZaR1 = 32'ha4506ceb;
    6'h3e: ZaR1 = 32'hbef9a3f7;
    default: ZaR1 = 32'hc67178f2;
  endcase
  wire [31:0] a = s0[255:224];
  wire [31:0] b = s0[223:192];
  wire [31:0] c = s0[191:160];
  wire [31:0] d = s0[159:128];
  wire [31:0] e = s0[127:96];
  wire [31:0] f = s0[95:64];
  wire [31:0] g = s0[63:32];
  wire [31:0] h = s0[31:0];
  wire [0:0] b0R2 = s0[127];
  wire [0:0] b1R2 = s0[126];
  wire [0:0] b2R2 = s0[125];
  wire [0:0] b3R2 = s0[124];
  wire [0:0] b4R2 = s0[123];
  wire [0:0] b5R2 = s0[122];
  wire [0:0] b6R2 = s0[121];
  wire [0:0] b7R2 = s0[120];
  wire [0:0] b8R2 = s0[119];
  wire [0:0] b9R2 = s0[118];
  wire [0:0] b10R2 = s0[117];
  wire [0:0] b11R2 = s0[116];
  wire [0:0] b12R2 = s0[115];
  wire [0:0] b13R2 = s0[114];
  wire [0:0] b14R2 = s0[113];
  wire [0:0] b15R2 = s0[112];
  wire [0:0] b16R2 = s0[111];
  wire [0:0] b17R2 = s0[110];
  wire [0:0] b18R2 = s0[109];
  wire [0:0] b19R2 = s0[108];
  wire [0:0] b20R2 = s0[107];
  wire [0:0] b21R2 = s0[106];
  wire [0:0] b22R2 = s0[105];
  wire [0:0] b23R2 = s0[104];
  wire [0:0] b24R2 = s0[103];
  wire [0:0] b25R2 = s0[102];
  wire [0:0] b26R2 = s0[101];
  wire [0:0] b27R2 = s0[100];
  wire [0:0] b28R2 = s0[99];
  wire [0:0] b29R2 = s0[98];
  wire [0:0] b30R2 = s0[97];
  wire [0:0] b31R2 = s0[96];
  Main_xorW32  xorW32_iR4 ({b26R2, b27R2, b28R2, b29R2, b30R2, b31R2, b0R2, b1R2, b2R2, b3R2, b4R2, b5R2, b6R2, b7R2,
    b8R2, b9R2, b10R2, b11R2, b12R2, b13R2, b14R2, b15R2, b16R2, b17R2, b18R2, b19R2, b20R2, b21R2, b22R2, b23R2, b24R2,
    b25R2}, {b21R2, b22R2, b23R2, b24R2, b25R2, b26R2, b27R2, b28R2, b29R2, b30R2, b31R2, b0R2, b1R2, b2R2, b3R2, b4R2,
    b5R2, b6R2, b7R2, b8R2, b9R2, b10R2, b11R2, b12R2, b13R2, b14R2, b15R2, b16R2, b17R2, b18R2, b19R2,
    b20R2}, Main_xorW32_outR4);
  Main_xorW32  xorW32_iR5 (Main_xorW32_outR4, {b7R2, b8R2, b9R2, b10R2, b11R2, b12R2, b13R2, b14R2, b15R2, b16R2, b17R2,
    b18R2, b19R2, b20R2, b21R2, b22R2, b23R2, b24R2, b25R2, b26R2, b27R2, b28R2, b29R2, b30R2, b31R2, b0R2, b1R2, b2R2,
    b3R2, b4R2, b5R2, b6R2}, Main_xorW32_outR5);
  Main_andW32  andW32_i (e, f, Main_andW32_out);
  notW32  notW32_i (e, extres[31:0]);
  Main_andW32  andW32_iR1 (extres, g, Main_andW32_outR1);
  Main_xorW32  xorW32_iR6 (Main_andW32_out, Main_andW32_outR1, Main_xorW32_outR6);
  Main_plusW32  plusW32_iR3 (Main_xorW32_outR5, Main_xorW32_outR6, Main_plusW32_outR3);
  Main_plusW32  plusW32_iR4 (ZaR1, w00, Main_plusW32_outR4);
  Main_plusW32  plusW32_iR5 (Main_plusW32_outR3, Main_plusW32_outR4, Main_plusW32_outR5);
  Main_plusW32  plusW32_iR6 (h, Main_plusW32_outR5, Main_plusW32_outR6);
  wire [0:0] b0R3 = s0[255];
  wire [0:0] b1R3 = s0[254];
  wire [0:0] b2R3 = s0[253];
  wire [0:0] b3R3 = s0[252];
  wire [0:0] b4R3 = s0[251];
  wire [0:0] b5R3 = s0[250];
  wire [0:0] b6R3 = s0[249];
  wire [0:0] b7R3 = s0[248];
  wire [0:0] b8R3 = s0[247];
  wire [0:0] b9R3 = s0[246];
  wire [0:0] b10R3 = s0[245];
  wire [0:0] b11R3 = s0[244];
  wire [0:0] b12R3 = s0[243];
  wire [0:0] b13R3 = s0[242];
  wire [0:0] b14R3 = s0[241];
  wire [0:0] b15R3 = s0[240];
  wire [0:0] b16R3 = s0[239];
  wire [0:0] b17R3 = s0[238];
  wire [0:0] b18R3 = s0[237];
  wire [0:0] b19R3 = s0[236];
  wire [0:0] b20R3 = s0[235];
  wire [0:0] b21R3 = s0[234];
  wire [0:0] b22R3 = s0[233];
  wire [0:0] b23R3 = s0[232];
  wire [0:0] b24R3 = s0[231];
  wire [0:0] b25R3 = s0[230];
  wire [0:0] b26R3 = s0[229];
  wire [0:0] b27R3 = s0[228];
  wire [0:0] b28R3 = s0[227];
  wire [0:0] b29R3 = s0[226];
  wire [0:0] b30R3 = s0[225];
  wire [0:0] b31R3 = s0[224];
  Main_xorW32  xorW32_iR7 ({b30R3, b31R3, b0R3, b1R3, b2R3, b3R3, b4R3, b5R3, b6R3, b7R3, b8R3, b9R3, b10R3, b11R3,
    b12R3, b13R3, b14R3, b15R3, b16R3, b17R3, b18R3, b19R3, b20R3, b21R3, b22R3, b23R3, b24R3, b25R3, b26R3, b27R3,
    b28R3, b29R3}, {b19R3, b20R3, b21R3, b22R3, b23R3, b24R3, b25R3, b26R3, b27R3, b28R3, b29R3, b30R3, b31R3, b0R3,
    b1R3, b2R3, b3R3, b4R3, b5R3, b6R3, b7R3, b8R3, b9R3, b10R3, b11R3, b12R3, b13R3, b14R3, b15R3, b16R3, b17R3,
    b18R3}, Main_xorW32_outR7);
  Main_xorW32  xorW32_iR8 (Main_xorW32_outR7, {b10R3, b11R3, b12R3, b13R3, b14R3, b15R3, b16R3, b17R3, b18R3, b19R3,
    b20R3, b21R3, b22R3, b23R3, b24R3, b25R3, b26R3, b27R3, b28R3, b29R3, b30R3, b31R3, b0R3, b1R3, b2R3, b3R3, b4R3,
    b5R3, b6R3, b7R3, b8R3, b9R3}, Main_xorW32_outR8);
  Main_andW32  andW32_iR2 (a, b, Main_andW32_outR2);
  Main_andW32  andW32_iR3 (a, c, Main_andW32_outR3);
  Main_xorW32  xorW32_iR9 (Main_andW32_outR2, Main_andW32_outR3, Main_xorW32_outR9);
  Main_andW32  andW32_iR4 (b, c, Main_andW32_outR4);
  Main_xorW32  xorW32_iR10 (Main_xorW32_outR9, Main_andW32_outR4, Main_xorW32_outR10);
  Main_plusW32  plusW32_iR7 (Main_xorW32_outR8, Main_xorW32_outR10, Main_plusW32_outR7);
  Main_plusW32  plusW32_iR8 (Main_plusW32_outR6, Main_plusW32_outR7, Main_plusW32_outR8);
  Main_plusW32  plusW32_iR9 (d, Main_plusW32_outR6, Main_plusW32_outR9);
  wire [255:0] Za1 = {Main_plusW32_outR8, a, b, c, Main_plusW32_outR9, e, f, g};
  always_comb case (s3)
    6'h0: ZaR2 = 6'h1;
    6'h1: ZaR2 = 6'h2;
    6'h2: ZaR2 = 6'h3;
    6'h3: ZaR2 = 6'h4;
    6'h4: ZaR2 = 6'h5;
    6'h5: ZaR2 = 6'h6;
    6'h6: ZaR2 = 6'h7;
    6'h7: ZaR2 = 6'h8;
    6'h8: ZaR2 = 6'h9;
    6'h9: ZaR2 = 6'ha;
    6'ha: ZaR2 = 6'hb;
    6'hb: ZaR2 = 6'hc;
    6'hc: ZaR2 = 6'hd;
    6'hd: ZaR2 = 6'he;
    6'he: ZaR2 = 6'hf;
    6'hf: ZaR2 = 6'h10;
    6'h10: ZaR2 = 6'h11;
    6'h11: ZaR2 = 6'h12;
    6'h12: ZaR2 = 6'h13;
    6'h13: ZaR2 = 6'h14;
    6'h14: ZaR2 = 6'h15;
    6'h15: ZaR2 = 6'h16;
    6'h16: ZaR2 = 6'h17;
    6'h17: ZaR2 = 6'h18;
    6'h18: ZaR2 = 6'h19;
    6'h19: ZaR2 = 6'h1a;
    6'h1a: ZaR2 = 6'h1b;
    6'h1b: ZaR2 = 6'h1c;
    6'h1c: ZaR2 = 6'h1d;
    6'h1d: ZaR2 = 6'h1e;
    6'h1e: ZaR2 = 6'h1f;
    6'h1f: ZaR2 = 6'h20;
    6'h20: ZaR2 = 6'h21;
    6'h21: ZaR2 = 6'h22;
    6'h22: ZaR2 = 6'h23;
    6'h23: ZaR2 = 6'h24;
    6'h24: ZaR2 = 6'h25;
    6'h25: ZaR2 = 6'h26;
    6'h26: ZaR2 = 6'h27;
    6'h27: ZaR2 = 6'h28;
    6'h28: ZaR2 = 6'h29;
    6'h29: ZaR2 = 6'h2a;
    6'h2a: ZaR2 = 6'h2b;
    6'h2b: ZaR2 = 6'h2c;
    6'h2c: ZaR2 = 6'h2d;
    6'h2d: ZaR2 = 6'h2e;
    6'h2e: ZaR2 = 6'h2f;
    6'h2f: ZaR2 = 6'h30;
    6'h30: ZaR2 = 6'h31;
    6'h31: ZaR2 = 6'h32;
    6'h32: ZaR2 = 6'h33;
    6'h33: ZaR2 = 6'h34;
    6'h34: ZaR2 = 6'h35;
    6'h35: ZaR2 = 6'h36;
    6'h36: ZaR2 = 6'h37;
    6'h37: ZaR2 = 6'h38;
    6'h38: ZaR2 = 6'h39;
    6'h39: ZaR2 = 6'h3a;
    6'h3a: ZaR2 = 6'h3b;
    6'h3b: ZaR2 = 6'h3c;
    6'h3c: ZaR2 = 6'h3d;
    6'h3d: ZaR2 = 6'h3e;
    6'h3e: ZaR2 = 6'h3f;
    default: ZaR2 = 6'h0;
  endcase
  assign res = {67'h40000000000000001, s3, Za1, Za, s2, ZaR2};
endmodule

// main.dev
// block '$L.Main.dev' of process main
module main_dev (input logic [67:0] Zds,
  input logic [255:0] s0,
  input logic [511:0] s1,
  input logic [255:0] s2,
  input logic [5:0] s3,
  output logic [1102:0] res);
  logic [1102:0] main_arm2_out;
  logic [1102:0] main_arm2_outR1;
  logic [1102:0] main__unused_out;
  logic [1102:0] main__unused_outR1;
  logic [1102:0] main__unused_outR2;
  logic [1102:0] main__unused_outR3;
  logic [1102:0] main__unused_outR4;
  logic [1102:0] main__unused_outR5;
  wire [31:0] w1 = Zds[63:32];
  wire [31:0] w2 = Zds[31:0];
  main_arm2  arm2_i (w1, w2, s0, s1, 256'h6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19, s3, main_arm2_out);
  main_arm2  arm2_iR1 (w1, w2, s0, s1, s2, s3, main_arm2_outR1);
  wire [31:0] x0 = s1[511:480];
  wire [31:0] x1 = s1[479:448];
  wire [31:0] x4 = s1[383:352];
  wire [31:0] x5 = s1[351:320];
  wire [31:0] x6 = s1[319:288];
  wire [31:0] x7 = s1[287:256];
  wire [31:0] x8 = s1[255:224];
  wire [31:0] x9 = s1[223:192];
  wire [31:0] xa = s1[191:160];
  wire [31:0] xb = s1[159:128];
  wire [31:0] xc = s1[127:96];
  wire [31:0] xd = s1[95:64];
  wire [31:0] xe = s1[63:32];
  wire [31:0] xf = s1[31:0];
  wire [511:0] Za = {x0, x1, w1, w2, x4, x5, x6, x7, x8, x9, xa, xb, xc, xd, xe, xf};
  main___unused  _unused_i (s0, Za, s2, s3, main__unused_out);
  wire [31:0] x2 = s1[447:416];
  wire [31:0] x3 = s1[415:384];
  wire [511:0] ZaR1 = {x0, x1, x2, x3, w1, w2, x6, x7, x8, x9, xa, xb, xc, xd, xe, xf};
  main___unused  _unused_iR1 (s0, ZaR1, s2, s3, main__unused_outR1);
  wire [511:0] ZaR2 = {x0, x1, x2, x3, x4, x5, w1, w2, x8, x9, xa, xb, xc, xd, xe, xf};
  main___unused  _unused_iR2 (s0, ZaR2, s2, s3, main__unused_outR2);
  wire [511:0] ZaR3 = {x0, x1, x2, x3, x4, x5, x6, x7, w1, w2, xa, xb, xc, xd, xe, xf};
  main___unused  _unused_iR3 (s0, ZaR3, s2, s3, main__unused_outR3);
  wire [511:0] ZaR4 = {x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, w1, w2, xc, xd, xe, xf};
  main___unused  _unused_iR4 (s0, ZaR4, s2, s3, main__unused_outR4);
  wire [511:0] ZaR5 = {x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xa, xb, w1, w2, xe, xf};
  main___unused  _unused_iR5 (s0, ZaR5, s2, s3, main__unused_outR5);
  wire [511:0] ZaR6 = {x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xa, xb, xc, xd, w1, w2};
  wire [31:0] x0R1 = s2[255:224];
  wire [31:0] x1R1 = s2[223:192];
  wire [64:0] ZaR7 = {1'h0, x0R1, x1R1};
  wire [31:0] x2R1 = s2[191:160];
  wire [31:0] x3R1 = s2[159:128];
  wire [64:0] ZaR8 = {1'h0, x2R1, x3R1};
  wire [31:0] x4R1 = s2[127:96];
  wire [31:0] x5R1 = s2[95:64];
  wire [64:0] ZaR9 = {1'h0, x4R1, x5R1};
  wire [31:0] x6R1 = s2[63:32];
  wire [31:0] x7R1 = s2[31:0];
  wire [64:0] ZaR10 = {1'h0, x6R1, x7R1};
  wire [3:0] scrut = Zds[67:64];
  always_comb case (scrut)
    4'h0: res = main_arm2_out;
    4'h1: res = main_arm2_outR1;
    4'h2: res = main__unused_out;
    4'h3: res = main__unused_outR1;
    4'h4: res = main__unused_outR2;
    4'h5: res = main__unused_outR3;
    4'h6: res = main__unused_outR4;
    4'h7: res = main__unused_outR5;
    4'h8: res = {73'h1000000000000000080, s2, ZaR6, s2, 6'h0};
    4'h9: res = {ZaR7, 8'h0, s0, s1, s2, s3};
    4'ha: res = {ZaR8, 8'h0, s0, s1, s2, s3};
    4'hb: res = {ZaR9, 8'h0, s0, s1, s2, s3};
    default: res = {ZaR10, 8'h0, s0, s1, s2, s3};
  endcase
endmodule

// Main.plusW32
module Main_plusW32 (input logic [31:0] Zeta0,
  input logic [31:0] Zeta1,
  output logic [31:0] res);
  logic [31:0] extres;
  plusW32  plusW32_i (Zeta0, Zeta1, extres[31:0]);
  assign res = extres;
endmodule

// Main.andW32
module Main_andW32 (input logic [31:0] Zeta0,
  input logic [31:0] Zeta1,
  output logic [31:0] res);
  logic [31:0] extres;
  andW32  andW32_i (Zeta0, Zeta1, extres[31:0]);
  assign res = extres;
endmodule

// Main.xorW32
module Main_xorW32 (input logic [31:0] Zeta0,
  input logic [31:0] Zeta1,
  output logic [31:0] res);
  logic [31:0] extres;
  xorW32  xorW32_i (Zeta0, Zeta1, extres[31:0]);
  assign res = extres;
endmodule