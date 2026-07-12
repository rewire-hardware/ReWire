module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] main_arm_out;
  logic [1:0] main_arm_outR1;
  // state registers
  // __st0: 1 bits, init 0x0
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  // combinational logic
  main_arm  arm_i (__st0, main_arm_out);
  wire [0:0] Za = ~__st0;
  main_arm  arm_iR1 (Za, main_arm_outR1);
  wire [1:0] Zres = (~__in0) ? main_arm_out : main_arm_outR1;
  assign __st0_next = Zres[0];
  // outputs
  assign __out0 = Zres[1];
  // state register update
  initial __st0 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

// main.arm
// block '$L.arm' of process main
// also: main._unused3
module main_arm (input logic [0:0] s0,
  output logic [1:0] res);
  assign res = {s0, s0};
endmodule