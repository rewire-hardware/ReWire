module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] main__unused_out;
  logic [1:0] main__unused_outR1;
  // state registers
  // __st0: 1 bits, init 0x1
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  // combinational logic
  main___unused  _unused_i (__st0, __st0, main__unused_out);
  main___unused  _unused_iR1 (__st0, 1'h0, main__unused_outR1);
  wire [1:0] Zres = (~__in0) ? ((~__st0) ? main__unused_out : main__unused_outR1) : 2'h2;
  assign __st0_next = Zres[0];
  // outputs
  assign __out0 = Zres[1];
  // state register update
  initial __st0 = 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

// main._unused2
// block '$L._unused2' of process main
module main___unused2 (input logic [0:0] s,
  input logic [0:0] s0,
  output logic [1:0] res);
  assign res = {s, s0};
endmodule

// main._unused
// block '$L._unused' of process main
module main___unused (input logic [0:0] s,
  input logic [0:0] s0,
  output logic [1:0] res);
  logic [1:0] main__unused2_out;
  logic [1:0] main__unused2_outR1;
  main___unused2  _unused2_i (s, 1'h1, main__unused2_out);
  main___unused2  _unused2_iR1 (s, s0, main__unused2_outR1);
  assign res = (~s) ? main__unused2_out : main__unused2_outR1;
endmodule