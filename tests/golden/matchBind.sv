module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [16:0] main_loop_out;
  logic [16:0] main_loop_outR1;
  logic [16:0] zres;
  main_loop  inst (__st0, main_loop_out);
  main_loop  instR1 (__in0, main_loop_outR1);
  assign zres = (~__resumption_tag) ? main_loop_out : main_loop_outR1;
  assign __resumption_tag_next = zres[8];
  assign __st0_next = zres[7:0];
  assign __out0 = zres[16:9];
  initial {__resumption_tag, __st0} = 9'h100;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 9'h100;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module main_incr (input logic [7:0] arg0,
  output logic [16:0] res);
  assign res = {9'h4, arg0};
endmodule

module main_loop (input logic [7:0] arg0,
  output logic [16:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [16:0] main_incr_out;
  logic [16:0] main_incr_outR1;
  assign zi0 = arg0[0];
  assign zi1 = zi0;
  assign zi2 = ~zi1;
  main_incr  inst (arg0, main_incr_out);
  main_incr  instR1 (arg0, main_incr_outR1);
  assign res = (~zi2) ? main_incr_out : main_incr_outR1;
endmodule