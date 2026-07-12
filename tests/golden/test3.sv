module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] main_tick_out;
  // state registers
  // __st0: 8 bits, init 0x0
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  // combinational logic
  main_tick  tick_i (__st0, main_tick_out);
  wire [15:0] Zres = (~__in0) ? main_tick_out : main_tick_out;
  assign __st0_next = Zres[7:0];
  // outputs
  assign __out0 = Zres[15:8];
  // state register update
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

// main.tick
// block '$L.Main.tick' of process main
module main_tick (input logic [7:0] s0,
  output logic [15:0] res);
  assign res = {s0, s0};
endmodule