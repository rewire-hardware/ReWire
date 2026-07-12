module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [23:0] main_sig_out;
  logic [23:0] main_sig_outR1;
  // state registers
  // __st0: 8 bits, init 0x0
  // __st1: 8 bits, init 0x1
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  // combinational logic
  main_sig  sig_i (__st0, __st1, main_sig_out);
  wire [7:0] Za = __st0 + __st1;
  main_sig  sig_iR1 (__st1, Za, main_sig_outR1);
  wire [23:0] Zres = (~__in0) ? main_sig_out : main_sig_outR1;
  assign __st0_next = Zres[15:8];
  assign __st1_next = Zres[7:0];
  // outputs
  assign __out0 = Zres[23:16];
  // state register update
  initial __st0 = 8'h0;
  initial __st1 = 8'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
      __st1 <= 8'h1;
    end else begin
      __st0 <= __st0_next;
      __st1 <= __st1_next;
    end
  end
endmodule

// main.sig
// block '$L.Main.sig' of process main
module main_sig (input logic [7:0] s0,
  input logic [7:0] s1,
  output logic [23:0] res);
  assign res = {s0, s0, s1};
endmodule