module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [5:0] main_y_out;
  logic [5:0] main_y_outR1;
  logic [5:0] Zres;
  // state registers
  // __resumption_tag: 3 bits, init 0x4
  //   states: 0=z 1=y 2=x
  // __st0: 1 bits, init 0x0
  // __st1: 1 bits, init 0x1
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  // combinational logic
  wire [0:0] y = __resumption_tag[0];
  main_y  y_i (__resumption_tag[0], __in0, __st0, __st1, main_y_out);
  main_y  y_iR1 (__in0, __st0, __st0, __st1, main_y_outR1);
  wire [1:0] scrut = __resumption_tag[2:1];
  always_comb case (scrut)
    2'h0: Zres = {4'h4, __st0, y};
    2'h1: Zres = main_y_out;
    default: Zres = (~__in0) ? main_y_outR1 : {3'h1, __in0, __st0, __st1};
  endcase
  assign __resumption_tag_next = Zres[4:2];
  assign __st0_next = Zres[1];
  assign __st1_next = Zres[0];
  // outputs
  assign __out0 = Zres[5];
  // state register update
  initial __resumption_tag = 3'h4;
  initial __st0 = 1'h0;
  initial __st1 = 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h4;
      __st0 <= 1'h0;
      __st1 <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
      __st1 <= __st1_next;
    end
  end
endmodule

// main.y
// block '$L.y' of process main (state 1)
module main_y (input logic [0:0] x,
  input logic [0:0] y,
  input logic [0:0] s0,
  input logic [0:0] s1,
  output logic [5:0] res);
  wire [0:0] Za = x ? x : 1'h0;
  wire [0:0] ZaR1 = (~y) ? Za : x;
  wire [0:0] Za1 = ZaR1 ? y : 1'h0;
  assign res = {Za1, 2'h0, y, s0, s1};
endmodule