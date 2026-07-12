module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [1023:0] __out0);
  logic [1023:0] Main_add_out;
  logic [31:0] Main_rot_out;
  logic [31:0] Main_rot_outR1;
  logic [31:0] Main_rot_outR2;
  logic [31:0] Main_rot_outR3;
  logic [31:0] Main_rot_outR4;
  logic [31:0] Main_rot_outR5;
  logic [31:0] Main_rot_outR6;
  logic [31:0] Main_rot_outR7;
  logic [31:0] Main_rot_outR8;
  logic [31:0] Main_rot_outR9;
  logic [31:0] Main_rot_outR10;
  logic [31:0] Main_rot_outR11;
  logic [31:0] Main_rot_outR12;
  logic [31:0] Main_rot_outR13;
  logic [31:0] Main_rot_outR14;
  logic [31:0] Main_rot_outR15;
  logic [31:0] Main_rot_outR16;
  logic [31:0] Main_rot_outR17;
  logic [31:0] Main_rot_outR18;
  logic [31:0] Main_rot_outR19;
  logic [31:0] Main_rot_outR20;
  logic [31:0] Main_rot_outR21;
  logic [31:0] Main_rot_outR22;
  logic [31:0] Main_rot_outR23;
  logic [31:0] Main_rot_outR24;
  logic [31:0] Main_rot_outR25;
  logic [31:0] Main_rot_outR26;
  logic [31:0] Main_rot_outR27;
  logic [31:0] Main_rot_outR28;
  logic [31:0] Main_rot_outR29;
  logic [31:0] Main_rot_outR30;
  logic [31:0] Main_rot_outR31;
  logic [31:0] Main_swapix1_out;
  logic [31:0] Main_swapix1_outR1;
  logic [31:0] Main_swapix1_outR2;
  logic [31:0] Main_swapix1_outR3;
  logic [31:0] Main_swapix1_outR4;
  logic [31:0] Main_swapix1_outR5;
  logic [31:0] Main_swapix1_outR6;
  logic [31:0] Main_swapix1_outR7;
  logic [31:0] Main_swapix1_outR8;
  logic [31:0] Main_swapix1_outR9;
  logic [31:0] Main_swapix1_outR10;
  logic [31:0] Main_swapix1_outR11;
  logic [31:0] Main_swapix1_outR12;
  logic [31:0] Main_swapix1_outR13;
  logic [31:0] Main_swapix1_outR14;
  logic [31:0] Main_swapix1_outR15;
  logic [31:0] Main_swapix1_outR16;
  logic [31:0] Main_swapix1_outR17;
  logic [31:0] Main_swapix1_outR18;
  logic [31:0] Main_swapix1_outR19;
  logic [31:0] Main_swapix1_outR20;
  logic [31:0] Main_swapix1_outR21;
  logic [31:0] Main_swapix1_outR22;
  logic [31:0] Main_swapix1_outR23;
  logic [31:0] Main_swapix1_outR24;
  logic [31:0] Main_swapix1_outR25;
  logic [31:0] Main_swapix1_outR26;
  logic [31:0] Main_swapix1_outR27;
  logic [31:0] Main_swapix1_outR28;
  logic [31:0] Main_swapix1_outR29;
  logic [31:0] Main_swapix1_outR30;
  logic [31:0] Main_swapix1_outR31;
  logic [1023:0] Main_xor_out;
  logic [31:0] Main_swapix2_out;
  logic [31:0] Main_swapix2_outR1;
  logic [31:0] Main_swapix2_outR2;
  logic [31:0] Main_swapix2_outR3;
  logic [31:0] Main_swapix2_outR4;
  logic [31:0] Main_swapix2_outR5;
  logic [31:0] Main_swapix2_outR6;
  logic [31:0] Main_swapix2_outR7;
  logic [31:0] Main_swapix2_outR8;
  logic [31:0] Main_swapix2_outR9;
  logic [31:0] Main_swapix2_outR10;
  logic [31:0] Main_swapix2_outR11;
  logic [31:0] Main_swapix2_outR12;
  logic [31:0] Main_swapix2_outR13;
  logic [31:0] Main_swapix2_outR14;
  logic [31:0] Main_swapix2_outR15;
  logic [31:0] Main_swapix2_outR16;
  logic [31:0] Main_swapix2_outR17;
  logic [31:0] Main_swapix2_outR18;
  logic [31:0] Main_swapix2_outR19;
  logic [31:0] Main_swapix2_outR20;
  logic [31:0] Main_swapix2_outR21;
  logic [31:0] Main_swapix2_outR22;
  logic [31:0] Main_swapix2_outR23;
  logic [31:0] Main_swapix2_outR24;
  logic [31:0] Main_swapix2_outR25;
  logic [31:0] Main_swapix2_outR26;
  logic [31:0] Main_swapix2_outR27;
  logic [31:0] Main_swapix2_outR28;
  logic [31:0] Main_swapix2_outR29;
  logic [31:0] Main_swapix2_outR30;
  logic [31:0] Main_swapix2_outR31;
  logic [1023:0] Main_add_outR1;
  logic [31:0] Main_rot_outR32;
  logic [31:0] Main_rot_outR33;
  logic [31:0] Main_rot_outR34;
  logic [31:0] Main_rot_outR35;
  logic [31:0] Main_rot_outR36;
  logic [31:0] Main_rot_outR37;
  logic [31:0] Main_rot_outR38;
  logic [31:0] Main_rot_outR39;
  logic [31:0] Main_rot_outR40;
  logic [31:0] Main_rot_outR41;
  logic [31:0] Main_rot_outR42;
  logic [31:0] Main_rot_outR43;
  logic [31:0] Main_rot_outR44;
  logic [31:0] Main_rot_outR45;
  logic [31:0] Main_rot_outR46;
  logic [31:0] Main_rot_outR47;
  logic [31:0] Main_rot_outR48;
  logic [31:0] Main_rot_outR49;
  logic [31:0] Main_rot_outR50;
  logic [31:0] Main_rot_outR51;
  logic [31:0] Main_rot_outR52;
  logic [31:0] Main_rot_outR53;
  logic [31:0] Main_rot_outR54;
  logic [31:0] Main_rot_outR55;
  logic [31:0] Main_rot_outR56;
  logic [31:0] Main_rot_outR57;
  logic [31:0] Main_rot_outR58;
  logic [31:0] Main_rot_outR59;
  logic [31:0] Main_rot_outR60;
  logic [31:0] Main_rot_outR61;
  logic [31:0] Main_rot_outR62;
  logic [31:0] Main_rot_outR63;
  logic [31:0] Main_swapix3_out;
  logic [31:0] Main_swapix3_outR1;
  logic [31:0] Main_swapix3_outR2;
  logic [31:0] Main_swapix3_outR3;
  logic [31:0] Main_swapix3_outR4;
  logic [31:0] Main_swapix3_outR5;
  logic [31:0] Main_swapix3_outR6;
  logic [31:0] Main_swapix3_outR7;
  logic [31:0] Main_swapix3_outR8;
  logic [31:0] Main_swapix3_outR9;
  logic [31:0] Main_swapix3_outR10;
  logic [31:0] Main_swapix3_outR11;
  logic [31:0] Main_swapix3_outR12;
  logic [31:0] Main_swapix3_outR13;
  logic [31:0] Main_swapix3_outR14;
  logic [31:0] Main_swapix3_outR15;
  logic [31:0] Main_swapix3_outR16;
  logic [31:0] Main_swapix3_outR17;
  logic [31:0] Main_swapix3_outR18;
  logic [31:0] Main_swapix3_outR19;
  logic [31:0] Main_swapix3_outR20;
  logic [31:0] Main_swapix3_outR21;
  logic [31:0] Main_swapix3_outR22;
  logic [31:0] Main_swapix3_outR23;
  logic [31:0] Main_swapix3_outR24;
  logic [31:0] Main_swapix3_outR25;
  logic [31:0] Main_swapix3_outR26;
  logic [31:0] Main_swapix3_outR27;
  logic [31:0] Main_swapix3_outR28;
  logic [31:0] Main_swapix3_outR29;
  logic [31:0] Main_swapix3_outR30;
  logic [31:0] Main_swapix3_outR31;
  logic [1023:0] Main_xor_outR1;
  logic [31:0] Main_swapix4_out;
  logic [31:0] Main_swapix4_outR1;
  logic [31:0] Main_swapix4_outR2;
  logic [31:0] Main_swapix4_outR3;
  logic [31:0] Main_swapix4_outR4;
  logic [31:0] Main_swapix4_outR5;
  logic [31:0] Main_swapix4_outR6;
  logic [31:0] Main_swapix4_outR7;
  logic [31:0] Main_swapix4_outR8;
  logic [31:0] Main_swapix4_outR9;
  logic [31:0] Main_swapix4_outR10;
  logic [31:0] Main_swapix4_outR11;
  logic [31:0] Main_swapix4_outR12;
  logic [31:0] Main_swapix4_outR13;
  logic [31:0] Main_swapix4_outR14;
  logic [31:0] Main_swapix4_outR15;
  logic [31:0] Main_swapix4_outR16;
  logic [31:0] Main_swapix4_outR17;
  logic [31:0] Main_swapix4_outR18;
  logic [31:0] Main_swapix4_outR19;
  logic [31:0] Main_swapix4_outR20;
  logic [31:0] Main_swapix4_outR21;
  logic [31:0] Main_swapix4_outR22;
  logic [31:0] Main_swapix4_outR23;
  logic [31:0] Main_swapix4_outR24;
  logic [31:0] Main_swapix4_outR25;
  logic [31:0] Main_swapix4_outR26;
  logic [31:0] Main_swapix4_outR27;
  logic [31:0] Main_swapix4_outR28;
  logic [31:0] Main_swapix4_outR29;
  logic [31:0] Main_swapix4_outR30;
  logic [31:0] Main_swapix4_outR31;
  logic [2047:0] main_arm2_out;
  logic [2047:0] main_arm2_outR1;
  // state registers
  // __st0: 1024 bits, init 0x2000000001000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
  logic [1023:0] __st0;
  logic [1023:0] __st0_next;
  // combinational logic
  Main_add  add_i (__st0, Main_add_out);
  Main_rot  rot_i (32'h7, Main_add_out, 5'h0, Main_rot_out);
  Main_rot  rot_iR1 (32'h7, Main_add_out, 5'h1, Main_rot_outR1);
  Main_rot  rot_iR2 (32'h7, Main_add_out, 5'h2, Main_rot_outR2);
  Main_rot  rot_iR3 (32'h7, Main_add_out, 5'h3, Main_rot_outR3);
  Main_rot  rot_iR4 (32'h7, Main_add_out, 5'h4, Main_rot_outR4);
  Main_rot  rot_iR5 (32'h7, Main_add_out, 5'h5, Main_rot_outR5);
  Main_rot  rot_iR6 (32'h7, Main_add_out, 5'h6, Main_rot_outR6);
  Main_rot  rot_iR7 (32'h7, Main_add_out, 5'h7, Main_rot_outR7);
  Main_rot  rot_iR8 (32'h7, Main_add_out, 5'h8, Main_rot_outR8);
  Main_rot  rot_iR9 (32'h7, Main_add_out, 5'h9, Main_rot_outR9);
  Main_rot  rot_iR10 (32'h7, Main_add_out, 5'ha, Main_rot_outR10);
  Main_rot  rot_iR11 (32'h7, Main_add_out, 5'hb, Main_rot_outR11);
  Main_rot  rot_iR12 (32'h7, Main_add_out, 5'hc, Main_rot_outR12);
  Main_rot  rot_iR13 (32'h7, Main_add_out, 5'hd, Main_rot_outR13);
  Main_rot  rot_iR14 (32'h7, Main_add_out, 5'he, Main_rot_outR14);
  Main_rot  rot_iR15 (32'h7, Main_add_out, 5'hf, Main_rot_outR15);
  Main_rot  rot_iR16 (32'h7, Main_add_out, 5'h10, Main_rot_outR16);
  Main_rot  rot_iR17 (32'h7, Main_add_out, 5'h11, Main_rot_outR17);
  Main_rot  rot_iR18 (32'h7, Main_add_out, 5'h12, Main_rot_outR18);
  Main_rot  rot_iR19 (32'h7, Main_add_out, 5'h13, Main_rot_outR19);
  Main_rot  rot_iR20 (32'h7, Main_add_out, 5'h14, Main_rot_outR20);
  Main_rot  rot_iR21 (32'h7, Main_add_out, 5'h15, Main_rot_outR21);
  Main_rot  rot_iR22 (32'h7, Main_add_out, 5'h16, Main_rot_outR22);
  Main_rot  rot_iR23 (32'h7, Main_add_out, 5'h17, Main_rot_outR23);
  Main_rot  rot_iR24 (32'h7, Main_add_out, 5'h18, Main_rot_outR24);
  Main_rot  rot_iR25 (32'h7, Main_add_out, 5'h19, Main_rot_outR25);
  Main_rot  rot_iR26 (32'h7, Main_add_out, 5'h1a, Main_rot_outR26);
  Main_rot  rot_iR27 (32'h7, Main_add_out, 5'h1b, Main_rot_outR27);
  Main_rot  rot_iR28 (32'h7, Main_add_out, 5'h1c, Main_rot_outR28);
  Main_rot  rot_iR29 (32'h7, Main_add_out, 5'h1d, Main_rot_outR29);
  Main_rot  rot_iR30 (32'h7, Main_add_out, 5'h1e, Main_rot_outR30);
  Main_rot  rot_iR31 (32'h7, Main_add_out, 5'h1f, Main_rot_outR31);
  wire [1023:0] s = {Main_rot_out, Main_rot_outR1, Main_rot_outR2, Main_rot_outR3, Main_rot_outR4, Main_rot_outR5,
    Main_rot_outR6, Main_rot_outR7, Main_rot_outR8, Main_rot_outR9, Main_rot_outR10, Main_rot_outR11, Main_rot_outR12,
    Main_rot_outR13, Main_rot_outR14, Main_rot_outR15, Main_rot_outR16, Main_rot_outR17, Main_rot_outR18,
    Main_rot_outR19, Main_rot_outR20, Main_rot_outR21, Main_rot_outR22, Main_rot_outR23, Main_rot_outR24,
    Main_rot_outR25, Main_rot_outR26, Main_rot_outR27, Main_rot_outR28, Main_rot_outR29, Main_rot_outR30,
    Main_rot_outR31};
  Main_swapix1  swapix1_i (s, 5'h0, Main_swapix1_out);
  Main_swapix1  swapix1_iR1 (s, 5'h1, Main_swapix1_outR1);
  Main_swapix1  swapix1_iR2 (s, 5'h2, Main_swapix1_outR2);
  Main_swapix1  swapix1_iR3 (s, 5'h3, Main_swapix1_outR3);
  Main_swapix1  swapix1_iR4 (s, 5'h4, Main_swapix1_outR4);
  Main_swapix1  swapix1_iR5 (s, 5'h5, Main_swapix1_outR5);
  Main_swapix1  swapix1_iR6 (s, 5'h6, Main_swapix1_outR6);
  Main_swapix1  swapix1_iR7 (s, 5'h7, Main_swapix1_outR7);
  Main_swapix1  swapix1_iR8 (s, 5'h8, Main_swapix1_outR8);
  Main_swapix1  swapix1_iR9 (s, 5'h9, Main_swapix1_outR9);
  Main_swapix1  swapix1_iR10 (s, 5'ha, Main_swapix1_outR10);
  Main_swapix1  swapix1_iR11 (s, 5'hb, Main_swapix1_outR11);
  Main_swapix1  swapix1_iR12 (s, 5'hc, Main_swapix1_outR12);
  Main_swapix1  swapix1_iR13 (s, 5'hd, Main_swapix1_outR13);
  Main_swapix1  swapix1_iR14 (s, 5'he, Main_swapix1_outR14);
  Main_swapix1  swapix1_iR15 (s, 5'hf, Main_swapix1_outR15);
  Main_swapix1  swapix1_iR16 (s, 5'h10, Main_swapix1_outR16);
  Main_swapix1  swapix1_iR17 (s, 5'h11, Main_swapix1_outR17);
  Main_swapix1  swapix1_iR18 (s, 5'h12, Main_swapix1_outR18);
  Main_swapix1  swapix1_iR19 (s, 5'h13, Main_swapix1_outR19);
  Main_swapix1  swapix1_iR20 (s, 5'h14, Main_swapix1_outR20);
  Main_swapix1  swapix1_iR21 (s, 5'h15, Main_swapix1_outR21);
  Main_swapix1  swapix1_iR22 (s, 5'h16, Main_swapix1_outR22);
  Main_swapix1  swapix1_iR23 (s, 5'h17, Main_swapix1_outR23);
  Main_swapix1  swapix1_iR24 (s, 5'h18, Main_swapix1_outR24);
  Main_swapix1  swapix1_iR25 (s, 5'h19, Main_swapix1_outR25);
  Main_swapix1  swapix1_iR26 (s, 5'h1a, Main_swapix1_outR26);
  Main_swapix1  swapix1_iR27 (s, 5'h1b, Main_swapix1_outR27);
  Main_swapix1  swapix1_iR28 (s, 5'h1c, Main_swapix1_outR28);
  Main_swapix1  swapix1_iR29 (s, 5'h1d, Main_swapix1_outR29);
  Main_swapix1  swapix1_iR30 (s, 5'h1e, Main_swapix1_outR30);
  Main_swapix1  swapix1_iR31 (s, 5'h1f, Main_swapix1_outR31);
  Main_xor  xor_i ({Main_swapix1_out, Main_swapix1_outR1, Main_swapix1_outR2, Main_swapix1_outR3, Main_swapix1_outR4,
    Main_swapix1_outR5, Main_swapix1_outR6, Main_swapix1_outR7, Main_swapix1_outR8, Main_swapix1_outR9,
    Main_swapix1_outR10, Main_swapix1_outR11, Main_swapix1_outR12, Main_swapix1_outR13, Main_swapix1_outR14,
    Main_swapix1_outR15, Main_swapix1_outR16, Main_swapix1_outR17, Main_swapix1_outR18, Main_swapix1_outR19,
    Main_swapix1_outR20, Main_swapix1_outR21, Main_swapix1_outR22, Main_swapix1_outR23, Main_swapix1_outR24,
    Main_swapix1_outR25, Main_swapix1_outR26, Main_swapix1_outR27, Main_swapix1_outR28, Main_swapix1_outR29,
    Main_swapix1_outR30, Main_swapix1_outR31}, Main_xor_out);
  Main_swapix2  swapix2_i (Main_xor_out, 5'h0, Main_swapix2_out);
  Main_swapix2  swapix2_iR1 (Main_xor_out, 5'h1, Main_swapix2_outR1);
  Main_swapix2  swapix2_iR2 (Main_xor_out, 5'h2, Main_swapix2_outR2);
  Main_swapix2  swapix2_iR3 (Main_xor_out, 5'h3, Main_swapix2_outR3);
  Main_swapix2  swapix2_iR4 (Main_xor_out, 5'h4, Main_swapix2_outR4);
  Main_swapix2  swapix2_iR5 (Main_xor_out, 5'h5, Main_swapix2_outR5);
  Main_swapix2  swapix2_iR6 (Main_xor_out, 5'h6, Main_swapix2_outR6);
  Main_swapix2  swapix2_iR7 (Main_xor_out, 5'h7, Main_swapix2_outR7);
  Main_swapix2  swapix2_iR8 (Main_xor_out, 5'h8, Main_swapix2_outR8);
  Main_swapix2  swapix2_iR9 (Main_xor_out, 5'h9, Main_swapix2_outR9);
  Main_swapix2  swapix2_iR10 (Main_xor_out, 5'ha, Main_swapix2_outR10);
  Main_swapix2  swapix2_iR11 (Main_xor_out, 5'hb, Main_swapix2_outR11);
  Main_swapix2  swapix2_iR12 (Main_xor_out, 5'hc, Main_swapix2_outR12);
  Main_swapix2  swapix2_iR13 (Main_xor_out, 5'hd, Main_swapix2_outR13);
  Main_swapix2  swapix2_iR14 (Main_xor_out, 5'he, Main_swapix2_outR14);
  Main_swapix2  swapix2_iR15 (Main_xor_out, 5'hf, Main_swapix2_outR15);
  Main_swapix2  swapix2_iR16 (Main_xor_out, 5'h10, Main_swapix2_outR16);
  Main_swapix2  swapix2_iR17 (Main_xor_out, 5'h11, Main_swapix2_outR17);
  Main_swapix2  swapix2_iR18 (Main_xor_out, 5'h12, Main_swapix2_outR18);
  Main_swapix2  swapix2_iR19 (Main_xor_out, 5'h13, Main_swapix2_outR19);
  Main_swapix2  swapix2_iR20 (Main_xor_out, 5'h14, Main_swapix2_outR20);
  Main_swapix2  swapix2_iR21 (Main_xor_out, 5'h15, Main_swapix2_outR21);
  Main_swapix2  swapix2_iR22 (Main_xor_out, 5'h16, Main_swapix2_outR22);
  Main_swapix2  swapix2_iR23 (Main_xor_out, 5'h17, Main_swapix2_outR23);
  Main_swapix2  swapix2_iR24 (Main_xor_out, 5'h18, Main_swapix2_outR24);
  Main_swapix2  swapix2_iR25 (Main_xor_out, 5'h19, Main_swapix2_outR25);
  Main_swapix2  swapix2_iR26 (Main_xor_out, 5'h1a, Main_swapix2_outR26);
  Main_swapix2  swapix2_iR27 (Main_xor_out, 5'h1b, Main_swapix2_outR27);
  Main_swapix2  swapix2_iR28 (Main_xor_out, 5'h1c, Main_swapix2_outR28);
  Main_swapix2  swapix2_iR29 (Main_xor_out, 5'h1d, Main_swapix2_outR29);
  Main_swapix2  swapix2_iR30 (Main_xor_out, 5'h1e, Main_swapix2_outR30);
  Main_swapix2  swapix2_iR31 (Main_xor_out, 5'h1f, Main_swapix2_outR31);
  Main_add  add_iR1 ({Main_swapix2_out, Main_swapix2_outR1, Main_swapix2_outR2, Main_swapix2_outR3, Main_swapix2_outR4,
    Main_swapix2_outR5, Main_swapix2_outR6, Main_swapix2_outR7, Main_swapix2_outR8, Main_swapix2_outR9,
    Main_swapix2_outR10, Main_swapix2_outR11, Main_swapix2_outR12, Main_swapix2_outR13, Main_swapix2_outR14,
    Main_swapix2_outR15, Main_swapix2_outR16, Main_swapix2_outR17, Main_swapix2_outR18, Main_swapix2_outR19,
    Main_swapix2_outR20, Main_swapix2_outR21, Main_swapix2_outR22, Main_swapix2_outR23, Main_swapix2_outR24,
    Main_swapix2_outR25, Main_swapix2_outR26, Main_swapix2_outR27, Main_swapix2_outR28, Main_swapix2_outR29,
    Main_swapix2_outR30, Main_swapix2_outR31}, Main_add_outR1);
  Main_rot  rot_iR32 (32'hb, Main_add_outR1, 5'h0, Main_rot_outR32);
  Main_rot  rot_iR33 (32'hb, Main_add_outR1, 5'h1, Main_rot_outR33);
  Main_rot  rot_iR34 (32'hb, Main_add_outR1, 5'h2, Main_rot_outR34);
  Main_rot  rot_iR35 (32'hb, Main_add_outR1, 5'h3, Main_rot_outR35);
  Main_rot  rot_iR36 (32'hb, Main_add_outR1, 5'h4, Main_rot_outR36);
  Main_rot  rot_iR37 (32'hb, Main_add_outR1, 5'h5, Main_rot_outR37);
  Main_rot  rot_iR38 (32'hb, Main_add_outR1, 5'h6, Main_rot_outR38);
  Main_rot  rot_iR39 (32'hb, Main_add_outR1, 5'h7, Main_rot_outR39);
  Main_rot  rot_iR40 (32'hb, Main_add_outR1, 5'h8, Main_rot_outR40);
  Main_rot  rot_iR41 (32'hb, Main_add_outR1, 5'h9, Main_rot_outR41);
  Main_rot  rot_iR42 (32'hb, Main_add_outR1, 5'ha, Main_rot_outR42);
  Main_rot  rot_iR43 (32'hb, Main_add_outR1, 5'hb, Main_rot_outR43);
  Main_rot  rot_iR44 (32'hb, Main_add_outR1, 5'hc, Main_rot_outR44);
  Main_rot  rot_iR45 (32'hb, Main_add_outR1, 5'hd, Main_rot_outR45);
  Main_rot  rot_iR46 (32'hb, Main_add_outR1, 5'he, Main_rot_outR46);
  Main_rot  rot_iR47 (32'hb, Main_add_outR1, 5'hf, Main_rot_outR47);
  Main_rot  rot_iR48 (32'hb, Main_add_outR1, 5'h10, Main_rot_outR48);
  Main_rot  rot_iR49 (32'hb, Main_add_outR1, 5'h11, Main_rot_outR49);
  Main_rot  rot_iR50 (32'hb, Main_add_outR1, 5'h12, Main_rot_outR50);
  Main_rot  rot_iR51 (32'hb, Main_add_outR1, 5'h13, Main_rot_outR51);
  Main_rot  rot_iR52 (32'hb, Main_add_outR1, 5'h14, Main_rot_outR52);
  Main_rot  rot_iR53 (32'hb, Main_add_outR1, 5'h15, Main_rot_outR53);
  Main_rot  rot_iR54 (32'hb, Main_add_outR1, 5'h16, Main_rot_outR54);
  Main_rot  rot_iR55 (32'hb, Main_add_outR1, 5'h17, Main_rot_outR55);
  Main_rot  rot_iR56 (32'hb, Main_add_outR1, 5'h18, Main_rot_outR56);
  Main_rot  rot_iR57 (32'hb, Main_add_outR1, 5'h19, Main_rot_outR57);
  Main_rot  rot_iR58 (32'hb, Main_add_outR1, 5'h1a, Main_rot_outR58);
  Main_rot  rot_iR59 (32'hb, Main_add_outR1, 5'h1b, Main_rot_outR59);
  Main_rot  rot_iR60 (32'hb, Main_add_outR1, 5'h1c, Main_rot_outR60);
  Main_rot  rot_iR61 (32'hb, Main_add_outR1, 5'h1d, Main_rot_outR61);
  Main_rot  rot_iR62 (32'hb, Main_add_outR1, 5'h1e, Main_rot_outR62);
  Main_rot  rot_iR63 (32'hb, Main_add_outR1, 5'h1f, Main_rot_outR63);
  wire [1023:0] sR1 = {Main_rot_outR32, Main_rot_outR33, Main_rot_outR34, Main_rot_outR35, Main_rot_outR36,
    Main_rot_outR37, Main_rot_outR38, Main_rot_outR39, Main_rot_outR40, Main_rot_outR41, Main_rot_outR42,
    Main_rot_outR43, Main_rot_outR44, Main_rot_outR45, Main_rot_outR46, Main_rot_outR47, Main_rot_outR48,
    Main_rot_outR49, Main_rot_outR50, Main_rot_outR51, Main_rot_outR52, Main_rot_outR53, Main_rot_outR54,
    Main_rot_outR55, Main_rot_outR56, Main_rot_outR57, Main_rot_outR58, Main_rot_outR59, Main_rot_outR60,
    Main_rot_outR61, Main_rot_outR62, Main_rot_outR63};
  Main_swapix3  swapix3_i (sR1, 5'h0, Main_swapix3_out);
  Main_swapix3  swapix3_iR1 (sR1, 5'h1, Main_swapix3_outR1);
  Main_swapix3  swapix3_iR2 (sR1, 5'h2, Main_swapix3_outR2);
  Main_swapix3  swapix3_iR3 (sR1, 5'h3, Main_swapix3_outR3);
  Main_swapix3  swapix3_iR4 (sR1, 5'h4, Main_swapix3_outR4);
  Main_swapix3  swapix3_iR5 (sR1, 5'h5, Main_swapix3_outR5);
  Main_swapix3  swapix3_iR6 (sR1, 5'h6, Main_swapix3_outR6);
  Main_swapix3  swapix3_iR7 (sR1, 5'h7, Main_swapix3_outR7);
  Main_swapix3  swapix3_iR8 (sR1, 5'h8, Main_swapix3_outR8);
  Main_swapix3  swapix3_iR9 (sR1, 5'h9, Main_swapix3_outR9);
  Main_swapix3  swapix3_iR10 (sR1, 5'ha, Main_swapix3_outR10);
  Main_swapix3  swapix3_iR11 (sR1, 5'hb, Main_swapix3_outR11);
  Main_swapix3  swapix3_iR12 (sR1, 5'hc, Main_swapix3_outR12);
  Main_swapix3  swapix3_iR13 (sR1, 5'hd, Main_swapix3_outR13);
  Main_swapix3  swapix3_iR14 (sR1, 5'he, Main_swapix3_outR14);
  Main_swapix3  swapix3_iR15 (sR1, 5'hf, Main_swapix3_outR15);
  Main_swapix3  swapix3_iR16 (sR1, 5'h10, Main_swapix3_outR16);
  Main_swapix3  swapix3_iR17 (sR1, 5'h11, Main_swapix3_outR17);
  Main_swapix3  swapix3_iR18 (sR1, 5'h12, Main_swapix3_outR18);
  Main_swapix3  swapix3_iR19 (sR1, 5'h13, Main_swapix3_outR19);
  Main_swapix3  swapix3_iR20 (sR1, 5'h14, Main_swapix3_outR20);
  Main_swapix3  swapix3_iR21 (sR1, 5'h15, Main_swapix3_outR21);
  Main_swapix3  swapix3_iR22 (sR1, 5'h16, Main_swapix3_outR22);
  Main_swapix3  swapix3_iR23 (sR1, 5'h17, Main_swapix3_outR23);
  Main_swapix3  swapix3_iR24 (sR1, 5'h18, Main_swapix3_outR24);
  Main_swapix3  swapix3_iR25 (sR1, 5'h19, Main_swapix3_outR25);
  Main_swapix3  swapix3_iR26 (sR1, 5'h1a, Main_swapix3_outR26);
  Main_swapix3  swapix3_iR27 (sR1, 5'h1b, Main_swapix3_outR27);
  Main_swapix3  swapix3_iR28 (sR1, 5'h1c, Main_swapix3_outR28);
  Main_swapix3  swapix3_iR29 (sR1, 5'h1d, Main_swapix3_outR29);
  Main_swapix3  swapix3_iR30 (sR1, 5'h1e, Main_swapix3_outR30);
  Main_swapix3  swapix3_iR31 (sR1, 5'h1f, Main_swapix3_outR31);
  Main_xor  xor_iR1 ({Main_swapix3_out, Main_swapix3_outR1, Main_swapix3_outR2, Main_swapix3_outR3, Main_swapix3_outR4,
    Main_swapix3_outR5, Main_swapix3_outR6, Main_swapix3_outR7, Main_swapix3_outR8, Main_swapix3_outR9,
    Main_swapix3_outR10, Main_swapix3_outR11, Main_swapix3_outR12, Main_swapix3_outR13, Main_swapix3_outR14,
    Main_swapix3_outR15, Main_swapix3_outR16, Main_swapix3_outR17, Main_swapix3_outR18, Main_swapix3_outR19,
    Main_swapix3_outR20, Main_swapix3_outR21, Main_swapix3_outR22, Main_swapix3_outR23, Main_swapix3_outR24,
    Main_swapix3_outR25, Main_swapix3_outR26, Main_swapix3_outR27, Main_swapix3_outR28, Main_swapix3_outR29,
    Main_swapix3_outR30, Main_swapix3_outR31}, Main_xor_outR1);
  Main_swapix4  swapix4_i (Main_xor_outR1, 5'h0, Main_swapix4_out);
  Main_swapix4  swapix4_iR1 (Main_xor_outR1, 5'h1, Main_swapix4_outR1);
  Main_swapix4  swapix4_iR2 (Main_xor_outR1, 5'h2, Main_swapix4_outR2);
  Main_swapix4  swapix4_iR3 (Main_xor_outR1, 5'h3, Main_swapix4_outR3);
  Main_swapix4  swapix4_iR4 (Main_xor_outR1, 5'h4, Main_swapix4_outR4);
  Main_swapix4  swapix4_iR5 (Main_xor_outR1, 5'h5, Main_swapix4_outR5);
  Main_swapix4  swapix4_iR6 (Main_xor_outR1, 5'h6, Main_swapix4_outR6);
  Main_swapix4  swapix4_iR7 (Main_xor_outR1, 5'h7, Main_swapix4_outR7);
  Main_swapix4  swapix4_iR8 (Main_xor_outR1, 5'h8, Main_swapix4_outR8);
  Main_swapix4  swapix4_iR9 (Main_xor_outR1, 5'h9, Main_swapix4_outR9);
  Main_swapix4  swapix4_iR10 (Main_xor_outR1, 5'ha, Main_swapix4_outR10);
  Main_swapix4  swapix4_iR11 (Main_xor_outR1, 5'hb, Main_swapix4_outR11);
  Main_swapix4  swapix4_iR12 (Main_xor_outR1, 5'hc, Main_swapix4_outR12);
  Main_swapix4  swapix4_iR13 (Main_xor_outR1, 5'hd, Main_swapix4_outR13);
  Main_swapix4  swapix4_iR14 (Main_xor_outR1, 5'he, Main_swapix4_outR14);
  Main_swapix4  swapix4_iR15 (Main_xor_outR1, 5'hf, Main_swapix4_outR15);
  Main_swapix4  swapix4_iR16 (Main_xor_outR1, 5'h10, Main_swapix4_outR16);
  Main_swapix4  swapix4_iR17 (Main_xor_outR1, 5'h11, Main_swapix4_outR17);
  Main_swapix4  swapix4_iR18 (Main_xor_outR1, 5'h12, Main_swapix4_outR18);
  Main_swapix4  swapix4_iR19 (Main_xor_outR1, 5'h13, Main_swapix4_outR19);
  Main_swapix4  swapix4_iR20 (Main_xor_outR1, 5'h14, Main_swapix4_outR20);
  Main_swapix4  swapix4_iR21 (Main_xor_outR1, 5'h15, Main_swapix4_outR21);
  Main_swapix4  swapix4_iR22 (Main_xor_outR1, 5'h16, Main_swapix4_outR22);
  Main_swapix4  swapix4_iR23 (Main_xor_outR1, 5'h17, Main_swapix4_outR23);
  Main_swapix4  swapix4_iR24 (Main_xor_outR1, 5'h18, Main_swapix4_outR24);
  Main_swapix4  swapix4_iR25 (Main_xor_outR1, 5'h19, Main_swapix4_outR25);
  Main_swapix4  swapix4_iR26 (Main_xor_outR1, 5'h1a, Main_swapix4_outR26);
  Main_swapix4  swapix4_iR27 (Main_xor_outR1, 5'h1b, Main_swapix4_outR27);
  Main_swapix4  swapix4_iR28 (Main_xor_outR1, 5'h1c, Main_swapix4_outR28);
  Main_swapix4  swapix4_iR29 (Main_xor_outR1, 5'h1d, Main_swapix4_outR29);
  Main_swapix4  swapix4_iR30 (Main_xor_outR1, 5'h1e, Main_swapix4_outR30);
  Main_swapix4  swapix4_iR31 (Main_xor_outR1, 5'h1f, Main_swapix4_outR31);
  wire [1023:0] Za = {Main_swapix4_out, Main_swapix4_outR1, Main_swapix4_outR2, Main_swapix4_outR3, Main_swapix4_outR4,
    Main_swapix4_outR5, Main_swapix4_outR6, Main_swapix4_outR7, Main_swapix4_outR8, Main_swapix4_outR9,
    Main_swapix4_outR10, Main_swapix4_outR11, Main_swapix4_outR12, Main_swapix4_outR13, Main_swapix4_outR14,
    Main_swapix4_outR15, Main_swapix4_outR16, Main_swapix4_outR17, Main_swapix4_outR18, Main_swapix4_outR19,
    Main_swapix4_outR20, Main_swapix4_outR21, Main_swapix4_outR22, Main_swapix4_outR23, Main_swapix4_outR24,
    Main_swapix4_outR25, Main_swapix4_outR26, Main_swapix4_outR27, Main_swapix4_outR28, Main_swapix4_outR29,
    Main_swapix4_outR30, Main_swapix4_outR31};
  main_arm2  arm2_i (Za, main_arm2_out);
  main_arm2  arm2_iR1 (__st0, main_arm2_outR1);
  wire [2047:0] Zres = (~__in0) ? main_arm2_out : main_arm2_outR1;
  assign __st0_next = Zres[1023:0];
  // outputs
  assign __out0 = Zres[2047:1024];
  // state register update
  initial __st0 = {93'h40000000020000001, {10'h3a3{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= {93'h40000000020000001, {10'h3a3{1'h0}}};
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

// main.arm2
// block '$L.arm2' of process main
// also: main._unused
module main_arm2 (input logic [1023:0] s0,
  output logic [2047:0] res);
  assign res = {s0, s0};
endmodule

// Main.explode5
module Main_explode5 (input logic [4:0] w5,
  output logic [4:0] res);
  wire [4:0] slice_in = w5 >> 128'h4;
  wire [4:0] slice_inR1 = w5 >> 128'h3;
  wire [4:0] slice_inR2 = w5 >> 128'h2;
  wire [4:0] slice_inR3 = w5 >> 128'h1;
  wire [4:0] slice_inR4 = w5 >> {8'h80{1'h0}};
  assign res = {slice_in[0], slice_inR1[0], slice_inR2[0], slice_inR3[0], slice_inR4[0]};
endmodule

// Main.rot
module Main_rot (input logic [31:0] rc,
  input logic [1023:0] s,
  input logic [4:0] f32,
  output logic [31:0] res);
  logic [4:0] Main_explode5_out;
  Main_explode5  explode5_i (f32, Main_explode5_out);
  wire [0:0] h = Main_explode5_out[4];
  wire [1023:0] slice_in = s >> (((128'h20 - {{7'h7b{1'h0}}, f32}) - 128'h1) * 128'h20);
  wire [32:0] Zds = {h, slice_in[31:0]};
  wire [31:0] w0 = Zds[31:0];
  wire [0:0] hR1 = Zds[32];
  assign res = (~hR1) ? ((w0 << rc) | (w0 >> (32'h20 - rc))) : w0;
endmodule

// Main.addix
module Main_addix (input logic [1023:0] s,
  input logic [4:0] f32,
  output logic [31:0] res);
  logic [4:0] Main_explode5_out;
  Main_explode5  explode5_i (f32, Main_explode5_out);
  wire [0:0] j = Main_explode5_out[3];
  wire [0:0] k = Main_explode5_out[2];
  wire [0:0] l = Main_explode5_out[1];
  wire [0:0] m = Main_explode5_out[0];
  wire [0:0] h = Main_explode5_out[4];
  wire [1023:0] slice_in = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h0, j, k, l, m}}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR1 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h1, j, k, l, m}}) - 128'h1) * 128'h20);
  wire [64:0] Zds = {h, slice_in[31:0], slice_inR1[31:0]};
  wire [31:0] v0 = Zds[63:32];
  wire [0:0] hR1 = Zds[64];
  wire [31:0] v1 = Zds[31:0];
  assign res = (~hR1) ? v0 : (v0 + v1);
endmodule

// Main.add
module Main_add (input logic [1023:0] s,
  output logic [1023:0] res);
  logic [31:0] Main_addix_out;
  logic [31:0] Main_addix_outR1;
  logic [31:0] Main_addix_outR2;
  logic [31:0] Main_addix_outR3;
  logic [31:0] Main_addix_outR4;
  logic [31:0] Main_addix_outR5;
  logic [31:0] Main_addix_outR6;
  logic [31:0] Main_addix_outR7;
  logic [31:0] Main_addix_outR8;
  logic [31:0] Main_addix_outR9;
  logic [31:0] Main_addix_outR10;
  logic [31:0] Main_addix_outR11;
  logic [31:0] Main_addix_outR12;
  logic [31:0] Main_addix_outR13;
  logic [31:0] Main_addix_outR14;
  logic [31:0] Main_addix_outR15;
  logic [31:0] Main_addix_outR16;
  logic [31:0] Main_addix_outR17;
  logic [31:0] Main_addix_outR18;
  logic [31:0] Main_addix_outR19;
  logic [31:0] Main_addix_outR20;
  logic [31:0] Main_addix_outR21;
  logic [31:0] Main_addix_outR22;
  logic [31:0] Main_addix_outR23;
  logic [31:0] Main_addix_outR24;
  logic [31:0] Main_addix_outR25;
  logic [31:0] Main_addix_outR26;
  logic [31:0] Main_addix_outR27;
  logic [31:0] Main_addix_outR28;
  logic [31:0] Main_addix_outR29;
  logic [31:0] Main_addix_outR30;
  logic [31:0] Main_addix_outR31;
  Main_addix  addix_i (s, 5'h0, Main_addix_out);
  Main_addix  addix_iR1 (s, 5'h1, Main_addix_outR1);
  Main_addix  addix_iR2 (s, 5'h2, Main_addix_outR2);
  Main_addix  addix_iR3 (s, 5'h3, Main_addix_outR3);
  Main_addix  addix_iR4 (s, 5'h4, Main_addix_outR4);
  Main_addix  addix_iR5 (s, 5'h5, Main_addix_outR5);
  Main_addix  addix_iR6 (s, 5'h6, Main_addix_outR6);
  Main_addix  addix_iR7 (s, 5'h7, Main_addix_outR7);
  Main_addix  addix_iR8 (s, 5'h8, Main_addix_outR8);
  Main_addix  addix_iR9 (s, 5'h9, Main_addix_outR9);
  Main_addix  addix_iR10 (s, 5'ha, Main_addix_outR10);
  Main_addix  addix_iR11 (s, 5'hb, Main_addix_outR11);
  Main_addix  addix_iR12 (s, 5'hc, Main_addix_outR12);
  Main_addix  addix_iR13 (s, 5'hd, Main_addix_outR13);
  Main_addix  addix_iR14 (s, 5'he, Main_addix_outR14);
  Main_addix  addix_iR15 (s, 5'hf, Main_addix_outR15);
  Main_addix  addix_iR16 (s, 5'h10, Main_addix_outR16);
  Main_addix  addix_iR17 (s, 5'h11, Main_addix_outR17);
  Main_addix  addix_iR18 (s, 5'h12, Main_addix_outR18);
  Main_addix  addix_iR19 (s, 5'h13, Main_addix_outR19);
  Main_addix  addix_iR20 (s, 5'h14, Main_addix_outR20);
  Main_addix  addix_iR21 (s, 5'h15, Main_addix_outR21);
  Main_addix  addix_iR22 (s, 5'h16, Main_addix_outR22);
  Main_addix  addix_iR23 (s, 5'h17, Main_addix_outR23);
  Main_addix  addix_iR24 (s, 5'h18, Main_addix_outR24);
  Main_addix  addix_iR25 (s, 5'h19, Main_addix_outR25);
  Main_addix  addix_iR26 (s, 5'h1a, Main_addix_outR26);
  Main_addix  addix_iR27 (s, 5'h1b, Main_addix_outR27);
  Main_addix  addix_iR28 (s, 5'h1c, Main_addix_outR28);
  Main_addix  addix_iR29 (s, 5'h1d, Main_addix_outR29);
  Main_addix  addix_iR30 (s, 5'h1e, Main_addix_outR30);
  Main_addix  addix_iR31 (s, 5'h1f, Main_addix_outR31);
  assign res = {Main_addix_out, Main_addix_outR1, Main_addix_outR2, Main_addix_outR3, Main_addix_outR4,
    Main_addix_outR5, Main_addix_outR6, Main_addix_outR7, Main_addix_outR8, Main_addix_outR9, Main_addix_outR10,
    Main_addix_outR11, Main_addix_outR12, Main_addix_outR13, Main_addix_outR14, Main_addix_outR15, Main_addix_outR16,
    Main_addix_outR17, Main_addix_outR18, Main_addix_outR19, Main_addix_outR20, Main_addix_outR21, Main_addix_outR22,
    Main_addix_outR23, Main_addix_outR24, Main_addix_outR25, Main_addix_outR26, Main_addix_outR27, Main_addix_outR28,
    Main_addix_outR29, Main_addix_outR30, Main_addix_outR31};
endmodule

// Main.swapix1
module Main_swapix1 (input logic [1023:0] s,
  input logic [4:0] f32,
  output logic [31:0] res);
  logic [4:0] Main_explode5_out;
  Main_explode5  explode5_i (f32, Main_explode5_out);
  wire [0:0] b1 = Main_explode5_out[4];
  wire [0:0] b2 = Main_explode5_out[3];
  wire [0:0] k = Main_explode5_out[2];
  wire [0:0] l = Main_explode5_out[1];
  wire [0:0] m = Main_explode5_out[0];
  wire [0:0] Zt0 = ~b1;
  wire [0:0] Zt1 = Zt0 ? b2 : 1'h0;
  wire [0:0] Zt3 = Zt0 ? (~b2) : 1'h0;
  wire [1023:0] slice_in = s >> (((128'h20 - {{7'h7b{1'h0}}, f32}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR1 = s >> (((128'h20 - {{7'h7b{1'h0}}, {2'h1, k, l, m}}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR2 = s >> (((128'h20 - {{7'h7b{1'h0}}, {2'h0, k, l, m}}) - 128'h1) * 128'h20);
  assign res = (~Zt1) ? ((~Zt3) ? slice_in[31:0] : slice_inR1[31:0]) : slice_inR2[31:0];
endmodule

// Main.swapix2
module Main_swapix2 (input logic [1023:0] s,
  input logic [4:0] f32,
  output logic [31:0] res);
  logic [4:0] Main_explode5_out;
  Main_explode5  explode5_i (f32, Main_explode5_out);
  wire [0:0] b1 = Main_explode5_out[4];
  wire [0:0] j = Main_explode5_out[3];
  wire [0:0] k = Main_explode5_out[2];
  wire [0:0] b2 = Main_explode5_out[1];
  wire [0:0] m = Main_explode5_out[0];
  wire [0:0] Zt0 = b1 ? (~b2) : 1'h0;
  wire [0:0] Zt1 = b1 ? b2 : 1'h0;
  wire [1023:0] slice_in = s >> (((128'h20 - {{7'h7b{1'h0}}, f32}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR1 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h1, j, k, 1'h0, m}}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR2 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h1, j, k, 1'h1, m}}) - 128'h1) * 128'h20);
  assign res = (~Zt0) ? ((~Zt1) ? slice_in[31:0] : slice_inR1[31:0]) : slice_inR2[31:0];
endmodule

// Main.swapix3
module Main_swapix3 (input logic [1023:0] s,
  input logic [4:0] f32,
  output logic [31:0] res);
  logic [4:0] Main_explode5_out;
  Main_explode5  explode5_i (f32, Main_explode5_out);
  wire [0:0] b1 = Main_explode5_out[4];
  wire [0:0] j = Main_explode5_out[3];
  wire [0:0] b2 = Main_explode5_out[2];
  wire [0:0] l = Main_explode5_out[1];
  wire [0:0] m = Main_explode5_out[0];
  wire [0:0] Zt0 = ~b1;
  wire [0:0] Zt1 = Zt0 ? (~b2) : 1'h0;
  wire [0:0] Zt3 = Zt0 ? b2 : 1'h0;
  wire [1023:0] slice_in = s >> (((128'h20 - {{7'h7b{1'h0}}, f32}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR1 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h0, j, 1'h0, l, m}}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR2 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h0, j, 1'h1, l, m}}) - 128'h1) * 128'h20);
  assign res = (~Zt1) ? ((~Zt3) ? slice_in[31:0] : slice_inR1[31:0]) : slice_inR2[31:0];
endmodule

// Main.swapix4
module Main_swapix4 (input logic [1023:0] s,
  input logic [4:0] f32,
  output logic [31:0] res);
  logic [4:0] Main_explode5_out;
  Main_explode5  explode5_i (f32, Main_explode5_out);
  wire [0:0] b1 = Main_explode5_out[4];
  wire [0:0] j = Main_explode5_out[3];
  wire [0:0] k = Main_explode5_out[2];
  wire [0:0] l = Main_explode5_out[1];
  wire [0:0] b2 = Main_explode5_out[0];
  wire [0:0] Zt0 = b1 ? (~b2) : 1'h0;
  wire [0:0] Zt1 = b1 ? b2 : 1'h0;
  wire [1023:0] slice_in = s >> (((128'h20 - {{7'h7b{1'h0}}, f32}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR1 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h1, j, k, l, 1'h0}}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR2 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h1, j, k, l, 1'h1}}) - 128'h1) * 128'h20);
  assign res = (~Zt0) ? ((~Zt1) ? slice_in[31:0] : slice_inR1[31:0]) : slice_inR2[31:0];
endmodule

// Main.xorix
module Main_xorix (input logic [1023:0] s,
  input logic [4:0] f32,
  output logic [31:0] res);
  logic [4:0] Main_explode5_out;
  Main_explode5  explode5_i (f32, Main_explode5_out);
  wire [0:0] j = Main_explode5_out[3];
  wire [0:0] k = Main_explode5_out[2];
  wire [0:0] l = Main_explode5_out[1];
  wire [0:0] m = Main_explode5_out[0];
  wire [4:0] i1 = {1'h1, j, k, l, m};
  wire [0:0] h = Main_explode5_out[4];
  wire [0:0] Zt1 = ~h;
  wire [1023:0] slice_in = s >> (((128'h20 - {{7'h7b{1'h0}}, i1}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR1 = s >> (((128'h20 - {{7'h7b{1'h0}}, {1'h0, j, k, l, m}}) - 128'h1) * 128'h20);
  wire [1023:0] slice_inR2 = s >> (((128'h20 - {{7'h7b{1'h0}}, i1}) - 128'h1) * 128'h20);
  assign res = (~Zt1) ? slice_in[31:0] : (slice_inR1[31:0] ^ slice_inR2[31:0]);
endmodule

// Main.xor
module Main_xor (input logic [1023:0] s,
  output logic [1023:0] res);
  logic [31:0] Main_xorix_out;
  logic [31:0] Main_xorix_outR1;
  logic [31:0] Main_xorix_outR2;
  logic [31:0] Main_xorix_outR3;
  logic [31:0] Main_xorix_outR4;
  logic [31:0] Main_xorix_outR5;
  logic [31:0] Main_xorix_outR6;
  logic [31:0] Main_xorix_outR7;
  logic [31:0] Main_xorix_outR8;
  logic [31:0] Main_xorix_outR9;
  logic [31:0] Main_xorix_outR10;
  logic [31:0] Main_xorix_outR11;
  logic [31:0] Main_xorix_outR12;
  logic [31:0] Main_xorix_outR13;
  logic [31:0] Main_xorix_outR14;
  logic [31:0] Main_xorix_outR15;
  logic [31:0] Main_xorix_outR16;
  logic [31:0] Main_xorix_outR17;
  logic [31:0] Main_xorix_outR18;
  logic [31:0] Main_xorix_outR19;
  logic [31:0] Main_xorix_outR20;
  logic [31:0] Main_xorix_outR21;
  logic [31:0] Main_xorix_outR22;
  logic [31:0] Main_xorix_outR23;
  logic [31:0] Main_xorix_outR24;
  logic [31:0] Main_xorix_outR25;
  logic [31:0] Main_xorix_outR26;
  logic [31:0] Main_xorix_outR27;
  logic [31:0] Main_xorix_outR28;
  logic [31:0] Main_xorix_outR29;
  logic [31:0] Main_xorix_outR30;
  logic [31:0] Main_xorix_outR31;
  Main_xorix  xorix_i (s, 5'h0, Main_xorix_out);
  Main_xorix  xorix_iR1 (s, 5'h1, Main_xorix_outR1);
  Main_xorix  xorix_iR2 (s, 5'h2, Main_xorix_outR2);
  Main_xorix  xorix_iR3 (s, 5'h3, Main_xorix_outR3);
  Main_xorix  xorix_iR4 (s, 5'h4, Main_xorix_outR4);
  Main_xorix  xorix_iR5 (s, 5'h5, Main_xorix_outR5);
  Main_xorix  xorix_iR6 (s, 5'h6, Main_xorix_outR6);
  Main_xorix  xorix_iR7 (s, 5'h7, Main_xorix_outR7);
  Main_xorix  xorix_iR8 (s, 5'h8, Main_xorix_outR8);
  Main_xorix  xorix_iR9 (s, 5'h9, Main_xorix_outR9);
  Main_xorix  xorix_iR10 (s, 5'ha, Main_xorix_outR10);
  Main_xorix  xorix_iR11 (s, 5'hb, Main_xorix_outR11);
  Main_xorix  xorix_iR12 (s, 5'hc, Main_xorix_outR12);
  Main_xorix  xorix_iR13 (s, 5'hd, Main_xorix_outR13);
  Main_xorix  xorix_iR14 (s, 5'he, Main_xorix_outR14);
  Main_xorix  xorix_iR15 (s, 5'hf, Main_xorix_outR15);
  Main_xorix  xorix_iR16 (s, 5'h10, Main_xorix_outR16);
  Main_xorix  xorix_iR17 (s, 5'h11, Main_xorix_outR17);
  Main_xorix  xorix_iR18 (s, 5'h12, Main_xorix_outR18);
  Main_xorix  xorix_iR19 (s, 5'h13, Main_xorix_outR19);
  Main_xorix  xorix_iR20 (s, 5'h14, Main_xorix_outR20);
  Main_xorix  xorix_iR21 (s, 5'h15, Main_xorix_outR21);
  Main_xorix  xorix_iR22 (s, 5'h16, Main_xorix_outR22);
  Main_xorix  xorix_iR23 (s, 5'h17, Main_xorix_outR23);
  Main_xorix  xorix_iR24 (s, 5'h18, Main_xorix_outR24);
  Main_xorix  xorix_iR25 (s, 5'h19, Main_xorix_outR25);
  Main_xorix  xorix_iR26 (s, 5'h1a, Main_xorix_outR26);
  Main_xorix  xorix_iR27 (s, 5'h1b, Main_xorix_outR27);
  Main_xorix  xorix_iR28 (s, 5'h1c, Main_xorix_outR28);
  Main_xorix  xorix_iR29 (s, 5'h1d, Main_xorix_outR29);
  Main_xorix  xorix_iR30 (s, 5'h1e, Main_xorix_outR30);
  Main_xorix  xorix_iR31 (s, 5'h1f, Main_xorix_outR31);
  assign res = {Main_xorix_out, Main_xorix_outR1, Main_xorix_outR2, Main_xorix_outR3, Main_xorix_outR4,
    Main_xorix_outR5, Main_xorix_outR6, Main_xorix_outR7, Main_xorix_outR8, Main_xorix_outR9, Main_xorix_outR10,
    Main_xorix_outR11, Main_xorix_outR12, Main_xorix_outR13, Main_xorix_outR14, Main_xorix_outR15, Main_xorix_outR16,
    Main_xorix_outR17, Main_xorix_outR18, Main_xorix_outR19, Main_xorix_outR20, Main_xorix_outR21, Main_xorix_outR22,
    Main_xorix_outR23, Main_xorix_outR24, Main_xorix_outR25, Main_xorix_outR26, Main_xorix_outR27, Main_xorix_outR28,
    Main_xorix_outR29, Main_xorix_outR30, Main_xorix_outR31};
endmodule