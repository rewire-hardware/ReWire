module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] main_go_out;
  logic [15:0] main_go_outR1;
  // state registers
  // __st0: 8 bits, init 0x0
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  // combinational logic
  wire [7:0] Za = __st0 + 8'h1;
  main_go  go_i (Za, main_go_out);
  wire [7:0] ZaR1 = (__st0 << 8'h1) | {7'h0, __st0[7]};
  main_go  go_iR1 (ZaR1, main_go_outR1);
  wire [15:0] Zres = (~__in0) ? main_go_out : main_go_outR1;
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

// main.go
// block '$L.Main.go' of process main
module main_go (input logic [7:0] s0,
  output logic [15:0] res);
  assign res = {s0, s0};
endmodule