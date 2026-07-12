module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [24:0] main_loop_out;
  logic [24:0] main_loop_outR1;
  logic [24:0] main_loop_outR2;
  logic [24:0] main_loop_outR3;
  // state registers
  // __resumption_tag: 17 bits, init 0x10001
  //   states: 0=b2 1=b
  logic [16:0] __resumption_tag;
  logic [16:0] __resumption_tag_next;
  // combinational logic
  wire [7:0] n = __resumption_tag[15:8];
  wire [7:0] m = __resumption_tag[7:0];
  main_loop  loop_i (m, n + m, main_loop_out);
  main_loop  loop_iR1 (n, m, main_loop_outR1);
  main_loop  loop_iR2 (m, 8'h0 + m, main_loop_outR2);
  main_loop  loop_iR3 (8'h0, m, main_loop_outR3);
  wire [24:0] Zres = (~__resumption_tag[16]) ? ((~__in0) ? main_loop_out : main_loop_outR1) :
    ((~__in0) ? main_loop_outR2 : main_loop_outR3);
  assign __resumption_tag_next = Zres[16:0];
  // outputs
  assign __out0 = Zres[24:17];
  // state register update
  initial __resumption_tag = 17'h10001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 17'h10001;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

// main.loop
// block '$L.Main.loop' of process main
module main_loop (input logic [7:0] n,
  input logic [7:0] m,
  output logic [24:0] res);
  assign res = {n, 1'h0, n, m};
endmodule