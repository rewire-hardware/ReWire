module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] __resumption_tag;
  logic [16:0] __resumption_tag_next;
  logic [7:0] zi0;
  logic [7:0] zi1;
  logic [24:0] main_loop_out;
  logic [24:0] main_loop_outR1;
  logic [7:0] zi2;
  logic [24:0] main_loop_outR2;
  logic [24:0] main_loop_outR3;
  logic [24:0] zres;
  assign zi0 = __resumption_tag[15:8];
  assign zi1 = __resumption_tag[7:0];
  Main_loop  inst (zi0, zi1 + zi0, main_loop_out);
  Main_loop  instR1 (zi1, zi0, main_loop_outR1);
  assign zi2 = __resumption_tag[7:0];
  Main_loop  instR2 (zi2, 8'h0 + zi2, main_loop_outR2);
  Main_loop  instR3 (8'h0, zi2, main_loop_outR3);
  assign zres = (__resumption_tag[16] == 1'h1) ? ((__in0 == 1'h0) ? main_loop_out : main_loop_outR1) : ((__in0 == 1'h0) ? main_loop_outR2 : main_loop_outR3);
  assign __resumption_tag_next = zres[16:0];
  assign __out0 = zres[24:17];
  initial __resumption_tag = 17'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 17'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_loop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [24:0] res);
  assign res = {arg0, 1'h1, arg1, arg0};
endmodule