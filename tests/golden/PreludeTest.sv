module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [0:0] zi3;
  logic [0:0] main_zookus_out;
  logic [0:0] main_zookus_outR1;
  logic [1:0] zres;
  assign zi3 = (__resumption_tag == 1'h1) ? 1'h1 : 1'h0;
  Main_zookus  inst (zi3, main_zookus_out);
  Main_zookus  instR1 (main_zookus_out, main_zookus_outR1);
  assign zres = {main_zookus_outR1, zi3};
  assign __resumption_tag_next = zres[0];
  assign __out0 = zres[1];
  initial __resumption_tag = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_zookus (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule