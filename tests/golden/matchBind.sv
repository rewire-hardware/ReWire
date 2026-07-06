module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [16:0] zl_main_loop4_out;
  logic [16:0] zl_main_loop4_outR1;
  logic [16:0] zres;
  ZL_Main_loop4  inst (__st0, zl_main_loop4_out);
  ZL_Main_loop4  instR1 (__in0, zl_main_loop4_outR1);
  assign zres = (__resumption_tag == 1'h0) ? zl_main_loop4_out : zl_main_loop4_outR1;
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

module ZL_Main_loop4 (input logic [7:0] arg0,
  output logic [16:0] res);
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [16:0] zl_main_incr7_out;
  logic [16:0] zl_main_incr7_outR1;
  assign zi0 = arg0[0];
  assign zi1 = zi0;
  assign zi2 = (zi1 == 1'h1) ? 1'h0 : 1'h1;
  ZL_Main_incr7  inst (arg0, zl_main_incr7_out);
  ZL_Main_incr7  instR1 (arg0, zl_main_incr7_outR1);
  assign res = (zi2 == 1'h0) ? zl_main_incr7_out : zl_main_incr7_outR1;
endmodule

module ZL_Main_incr7 (input logic [7:0] arg0,
  output logic [16:0] res);
  assign res = {9'h4, arg0};
endmodule