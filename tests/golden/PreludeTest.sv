module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  logic [0:0] zi0;
  logic [0:0] main_zookus_out;
  logic [0:0] zi1;
  logic [0:0] main_zookus_outR1;
  logic [0:0] zi2;
  logic [2:0] zres;
  assign zi0 = __resumption_tag[0];
  Main_zookus  inst (zi0, main_zookus_out);
  assign zi1 = main_zookus_out;
  Main_zookus  instR1 (zi1, main_zookus_outR1);
  assign zi2 = main_zookus_outR1;
  assign zres = (~__resumption_tag[1]) ? {zi2, 1'h0, zi0} : 3'h0;
  assign __resumption_tag_next = zres[1:0];
  assign __out0 = zres[2];
  initial __resumption_tag = 2'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_zookus (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule