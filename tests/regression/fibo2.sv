module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [23:0] main_loop_out;
  logic [23:0] main_loop_outR1;
  logic [23:0] zres;
  assign zi0 = __resumption_tag[15:8];
  assign zi1 = __resumption_tag[7:0];
  Main_loop  inst (zi0, zi1, main_loop_out);
  Main_loop  instR1 (zi1, zi0 + zi1, main_loop_outR1);
  assign zres = (__in0 == 1'h1) ? main_loop_out : main_loop_outR1;
  assign __resumption_tag_next = zres[15:0];
  assign __out0 = zres[23:16];
  initial __resumption_tag = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_loop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [23:0] res);
  assign res = {arg0, arg0, arg1};
endmodule