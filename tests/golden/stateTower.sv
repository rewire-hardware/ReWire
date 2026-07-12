module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] main__unused_out;
  logic [2:0] main__unused_outR1;
  // state registers
  // __st0: 1 bits, init 0x1
  // __st1: 1 bits, init 0x1
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  // combinational logic
  wire [0:0] Za = ~__in0;
  main___unused  _unused_i (__st0, __st1, main__unused_out);
  wire [0:0] Zt1 = __st0 ^ __st1;
  main___unused  _unused_iR1 (__st1, Zt1, main__unused_outR1);
  wire [2:0] Zres = (~Za) ? main__unused_out : main__unused_outR1;
  assign __st0_next = Zres[1];
  assign __st1_next = Zres[0];
  // outputs
  assign __out0 = Zres[2];
  // state register update
  initial __st0 = 1'h1;
  initial __st1 = 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
      __st1 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
      __st1 <= __st1_next;
    end
  end
endmodule

// main._unused
// block '$L._unused' of process main
module main___unused (input logic [0:0] s0,
  input logic [0:0] s1,
  output logic [2:0] res);
  assign res = {s0, s0, s1};
endmodule