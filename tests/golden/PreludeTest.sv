module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] Main_zookus_out;
  logic [0:0] Main_zookus_outR1;
  // state registers
  // __resumption_tag: 2 bits, init 0x2
  //   states: 0=b2 1=b
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  // combinational logic
  wire [0:0] a = __resumption_tag[0];
  Main_zookus  zookus_i (a, Main_zookus_out);
  Main_zookus  zookus_iR1 (Main_zookus_out, Main_zookus_outR1);
  wire [2:0] Zres = (~__resumption_tag[1]) ? {Main_zookus_outR1, 1'h0, a} : 3'h0;
  assign __resumption_tag_next = Zres[1:0];
  // outputs
  assign __out0 = Zres[2];
  // state register update
  initial __resumption_tag = 2'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

// Main.zookus
module Main_zookus (input logic [0:0] x,
  output logic [0:0] res);
  assign res = x;
endmodule