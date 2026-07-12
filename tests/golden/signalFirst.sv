module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [24:0] main_dev_out;
  logic [24:0] main_dev_outR1;
  // state registers
  // __resumption_tag: 9 bits, init 0x100
  //   states: 0=i2 1=i
  // __st0: 8 bits, init 0x0
  logic [8:0] __resumption_tag;
  logic [8:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  // combinational logic
  wire [7:0] k = __resumption_tag[7:0];
  main_dev  dev_i (k, __in0, main_dev_out);
  main_dev  dev_iR1 (__in0, 8'h0, main_dev_outR1);
  wire [24:0] Zres = (~__resumption_tag[8]) ? main_dev_out : main_dev_outR1;
  assign __resumption_tag_next = Zres[16:8];
  assign __st0_next = Zres[7:0];
  // outputs
  assign __out0 = Zres[24:17];
  // state register update
  initial __resumption_tag = 9'h100;
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 9'h100;
      __st0 <= 8'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule

// main.dev
// block '$L.Main.dev' of process main
module main_dev (input logic [7:0] k,
  input logic [7:0] s0,
  output logic [24:0] res);
  assign res = {s0 + k, 1'h0, k, s0};
endmodule