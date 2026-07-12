module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [99:0] __in0,
  output logic [99:0] __out0);
  logic [99:0] Main_x2_out;
  logic [99:0] Main_x2_outR1;
  logic [99:0] Main_x2_outR2;
  logic [99:0] Main_x2_outR3;
  logic [99:0] Main_x2_outR4;
  logic [99:0] Main_x2_outR5;
  logic [99:0] Main_x2_outR6;
  logic [99:0] Main_x2_outR7;
  logic [99:0] Main_x2_outR8;
  logic [99:0] Main_x2_outR9;
  // state registers
  // __resumption_tag: 1000 bits, init 0x0
  //   states: 0=i
  // __st0: 1000 bits, init 0x0
  logic [999:0] __resumption_tag;
  logic [999:0] __resumption_tag_next;
  logic [999:0] __st0;
  logic [999:0] __st0_next;
  // combinational logic
  Main_x2  x2_i (__resumption_tag[899:800], Main_x2_out);
  Main_x2  x2_iR1 (__resumption_tag[799:700], Main_x2_outR1);
  Main_x2  x2_iR2 (__resumption_tag[699:600], Main_x2_outR2);
  Main_x2  x2_iR3 (__resumption_tag[599:500], Main_x2_outR3);
  Main_x2  x2_iR4 (__resumption_tag[499:400], Main_x2_outR4);
  Main_x2  x2_iR5 (__resumption_tag[399:300], Main_x2_outR5);
  Main_x2  x2_iR6 (__resumption_tag[299:200], Main_x2_outR6);
  Main_x2  x2_iR7 (__resumption_tag[199:100], Main_x2_outR7);
  Main_x2  x2_iR8 (__resumption_tag[99:0], Main_x2_outR8);
  Main_x2  x2_iR9 (__in0, Main_x2_outR9);
  wire [999:0] Za = {Main_x2_out, Main_x2_outR1, Main_x2_outR2, Main_x2_outR3, Main_x2_outR4, Main_x2_outR5,
    Main_x2_outR6, Main_x2_outR7, Main_x2_outR8, Main_x2_outR9};
  wire [2099:0] Zres = {Za[999:900], Za, Za};
  assign __resumption_tag_next = Zres[1999:1000];
  assign __st0_next = Zres[999:0];
  // outputs
  assign __out0 = Zres[2099:2000];
  // state register update
  initial __resumption_tag = {10'h3e8{1'h0}};
  initial __st0 = {10'h3e8{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {10'h3e8{1'h0}};
      __st0 <= {10'h3e8{1'h0}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule

// Main.x2
module Main_x2 (input logic [99:0] x,
  output logic [99:0] res);
  assign res = x * 100'h2;
endmodule