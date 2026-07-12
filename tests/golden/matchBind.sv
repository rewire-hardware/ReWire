module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] main_loop_out;
  logic [16:0] main_loop_outR1;
  // state registers
  // __resumption_tag: 1 bits, init 0x1
  //   states: 0=$ds 1=i2
  // __st0: 8 bits, init 0x0
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  // combinational logic
  main_loop  loop_i (__st0, main_loop_out);
  main_loop  loop_iR1 (__in0, main_loop_outR1);
  wire [16:0] Zres = (~__resumption_tag) ? main_loop_out : main_loop_outR1;
  assign __resumption_tag_next = Zres[8];
  assign __st0_next = Zres[7:0];
  // outputs
  assign __out0 = Zres[16:9];
  // state register update
  initial __resumption_tag = 1'h1;
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
      __st0 <= 8'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule

// main.incr
// block '$L.Main.incr' of process main
module main_incr (input logic [7:0] s0,
  output logic [16:0] res);
  assign res = {9'h4, s0};
endmodule

// main.loop
// block '$L.Main.loop' of process main
module main_loop (input logic [7:0] s0,
  output logic [16:0] res);
  logic [16:0] main_incr_out;
  wire [0:0] Zt0 = s0[0];
  wire [0:0] Za1 = ~Zt0;
  main_incr  incr_i (s0, main_incr_out);
  assign res = (~Za1) ? main_incr_out : main_incr_out;
endmodule