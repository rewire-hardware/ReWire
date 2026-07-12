module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [23:0] main_first2_out;
  logic [23:0] main_first2_outR1;
  // state registers
  // __st0: 16 bits, init 0x1
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  // combinational logic
  wire [7:0] r0 = __st0[15:8];
  wire [15:0] slice_in = __st0 >> {8'h80{1'h0}};
  wire [7:0] r1 = slice_in[7:0];
  wire [7:0] Za = r0 + r1;
  wire [15:0] s01 = {r1, Za};
  main_first2  first2_i (s01, main_first2_out);
  main_first2  first2_iR1 (__st0, main_first2_outR1);
  wire [23:0] Zres = (~__in0) ? main_first2_out : main_first2_outR1;
  assign __st0_next = Zres[15:0];
  // outputs
  assign __out0 = Zres[23:16];
  // state register update
  initial __st0 = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

// main.first2
// block '$L.Main.first2' of process main
module main_first2 (input logic [15:0] s0,
  output logic [23:0] res);
  wire [7:0] r0 = s0[15:8];
  assign res = {r0, s0};
endmodule