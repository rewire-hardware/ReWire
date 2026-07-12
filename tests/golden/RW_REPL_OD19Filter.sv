module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  input logic [31:0] __in1,
  output logic [0:0] __out0,
  output logic [0:0] __out1);
  logic [40:0] main_putReg_out;
  logic [40:0] main_getReg_out;
  logic [0:0] extres;
  logic [0:0] extresR1;
  logic [0:0] extresR2;
  logic [40:0] main_nextPC_out;
  // state registers
  // __st0: 32 bits, init 0x0
  // __st1: 7 bits, init 0x0
  logic [31:0] __st0;
  logic [31:0] __st0_next;
  logic [6:0] __st1;
  logic [6:0] __st1_next;
  // combinational logic
  wire [38:0] disc = {__st0, __st1};
  main_putReg  putReg_i (__in1, disc, main_putReg_out);
  main_getReg  getReg_i (__in1, disc, main_getReg_out);
  test2  test2_i (__in1, __st0, extres[0]);
  wire [1:0] Za1 = {1'h0, extres};
  test3  test3_i (__in1, __st0, extresR1[0]);
  wire [1:0] Za1R1 = {1'h0, extresR1};
  test4  test4_i (__in1, __st0, extresR2[0]);
  wire [1:0] Za1R2 = {1'h0, extresR2};
  main_nextPC  nextPC_i (disc, main_nextPC_out);
  wire [40:0] Zres = (~__in0) ? ((__st1 == 7'h0) ? main_putReg_out :
    ((__st1 == 7'h1) ? main_getReg_out :
      ((__st1 == 7'h2) ? main_putReg_out :
        ((__st1 == 7'h3) ? main_getReg_out :
          ((__st1 == 7'h4) ? {Za1, disc} :
            ((__st1 == 7'h5) ? {Za1R1, disc} :
              ((__st1 == 7'h6) ? {Za1R2, disc} : main_nextPC_out))))))) : {2'h2, disc};
  assign __st0_next = Zres[38:7];
  assign __st1_next = Zres[6:0];
  // outputs
  assign __out0 = Zres[40];
  assign __out1 = Zres[39];
  // state register update
  initial __st0 = {6'h20{1'h0}};
  initial __st1 = 7'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= {6'h20{1'h0}};
      __st1 <= 7'h0;
    end else begin
      __st0 <= __st0_next;
      __st1 <= __st1_next;
    end
  end
endmodule

// main.nextPC
// block '$L.Main.nextPC' of process main
module main_nextPC (input logic [38:0] s0,
  output logic [40:0] res);
  logic [6:0] Za;
  wire [31:0] r = s0[38:7];
  wire [6:0] pc = s0[6:0];
  always_comb case (pc)
    7'h0: Za = 7'h1;
    7'h1: Za = 7'h2;
    7'h2: Za = 7'h3;
    7'h3: Za = 7'h4;
    7'h4: Za = 7'h5;
    7'h5: Za = 7'h6;
    7'h6: Za = 7'h7;
    7'h7: Za = 7'h8;
    7'h8: Za = 7'h9;
    7'h9: Za = 7'ha;
    7'ha: Za = 7'hb;
    7'hb: Za = 7'hc;
    7'hc: Za = 7'hd;
    7'hd: Za = 7'he;
    7'he: Za = 7'hf;
    7'hf: Za = 7'h10;
    7'h10: Za = 7'h11;
    7'h11: Za = 7'h12;
    7'h12: Za = 7'h13;
    7'h13: Za = 7'h14;
    7'h14: Za = 7'h15;
    7'h15: Za = 7'h16;
    7'h16: Za = 7'h17;
    7'h17: Za = 7'h18;
    7'h18: Za = 7'h19;
    7'h19: Za = 7'h1a;
    7'h1a: Za = 7'h1b;
    7'h1b: Za = 7'h1c;
    7'h1c: Za = 7'h1d;
    7'h1d: Za = 7'h1e;
    7'h1e: Za = 7'h1f;
    7'h1f: Za = 7'h20;
    7'h20: Za = 7'h21;
    7'h21: Za = 7'h22;
    7'h22: Za = 7'h23;
    7'h23: Za = 7'h24;
    7'h24: Za = 7'h25;
    7'h25: Za = 7'h26;
    7'h26: Za = 7'h27;
    7'h27: Za = 7'h28;
    7'h28: Za = 7'h29;
    7'h29: Za = 7'h2a;
    7'h2a: Za = 7'h2b;
    7'h2b: Za = 7'h2c;
    7'h2c: Za = 7'h2d;
    7'h2d: Za = 7'h2e;
    7'h2e: Za = 7'h2f;
    7'h2f: Za = 7'h30;
    7'h30: Za = 7'h31;
    7'h31: Za = 7'h32;
    7'h32: Za = 7'h33;
    7'h33: Za = 7'h34;
    7'h34: Za = 7'h35;
    7'h35: Za = 7'h36;
    7'h36: Za = 7'h37;
    7'h37: Za = 7'h38;
    7'h38: Za = 7'h39;
    7'h39: Za = 7'h3a;
    7'h3a: Za = 7'h3b;
    7'h3b: Za = 7'h3c;
    7'h3c: Za = 7'h3d;
    7'h3d: Za = 7'h3e;
    7'h3e: Za = 7'h3f;
    7'h3f: Za = 7'h40;
    7'h40: Za = 7'h41;
    7'h41: Za = 7'h42;
    7'h42: Za = 7'h43;
    7'h43: Za = 7'h44;
    7'h44: Za = 7'h45;
    7'h45: Za = 7'h46;
    7'h46: Za = 7'h47;
    7'h47: Za = 7'h48;
    7'h48: Za = 7'h49;
    7'h49: Za = 7'h4a;
    7'h4a: Za = 7'h4b;
    7'h4b: Za = 7'h4c;
    7'h4c: Za = 7'h4d;
    7'h4d: Za = 7'h4e;
    7'h4e: Za = 7'h4f;
    7'h4f: Za = 7'h50;
    7'h50: Za = 7'h51;
    7'h51: Za = 7'h52;
    7'h52: Za = 7'h53;
    7'h53: Za = 7'h54;
    7'h54: Za = 7'h55;
    7'h55: Za = 7'h56;
    7'h56: Za = 7'h57;
    7'h57: Za = 7'h58;
    7'h58: Za = 7'h59;
    7'h59: Za = 7'h5a;
    7'h5a: Za = 7'h5b;
    7'h5b: Za = 7'h5c;
    7'h5c: Za = 7'h5d;
    7'h5d: Za = 7'h5e;
    7'h5e: Za = 7'h5f;
    7'h5f: Za = 7'h60;
    7'h60: Za = 7'h61;
    7'h61: Za = 7'h62;
    7'h62: Za = 7'h63;
    7'h63: Za = 7'h64;
    7'h64: Za = 7'h65;
    7'h65: Za = 7'h66;
    7'h66: Za = 7'h67;
    7'h67: Za = 7'h68;
    7'h68: Za = 7'h69;
    7'h69: Za = 7'h6a;
    7'h6a: Za = 7'h6b;
    7'h6b: Za = 7'h6c;
    7'h6c: Za = 7'h6d;
    7'h6d: Za = 7'h6e;
    7'h6e: Za = 7'h6f;
    7'h6f: Za = 7'h70;
    7'h70: Za = 7'h71;
    7'h71: Za = 7'h72;
    7'h72: Za = 7'h73;
    7'h73: Za = 7'h74;
    7'h74: Za = 7'h75;
    7'h75: Za = 7'h76;
    7'h76: Za = 7'h77;
    7'h77: Za = 7'h78;
    7'h78: Za = 7'h79;
    7'h79: Za = 7'h7a;
    7'h7a: Za = 7'h7b;
    7'h7b: Za = 7'h7c;
    7'h7c: Za = 7'h7d;
    7'h7d: Za = 7'h7e;
    default: Za = 7'h0;
  endcase
  wire [38:0] Za1 = {r, Za};
  assign res = {2'h2, Za1};
endmodule

// main.putReg
// block '$L.Main.putReg' of process main
module main_putReg (input logic [31:0] w32,
  input logic [38:0] s0,
  output logic [40:0] res);
  logic [40:0] main_nextPC_out;
  wire [6:0] pc = s0[6:0];
  wire [38:0] Za = {w32, pc};
  main_nextPC  nextPC_i (Za, main_nextPC_out);
  assign res = main_nextPC_out;
endmodule

// main.getReg
// block '$L.Main.getReg' of process main
module main_getReg (input logic [31:0] w32,
  input logic [38:0] s0,
  output logic [40:0] res);
  logic [0:0] extres;
  wire [31:0] r = s0[38:7];
  test1  test1_i (w32, r, extres[0]);
  wire [1:0] Za1 = {1'h0, extres};
  assign res = {Za1, s0};
endmodule