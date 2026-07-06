module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] __resumption_tag;
  logic [8:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] zi1;
  logic [24:0] main_$l_main_dev2$244_out;
  logic [24:0] main_$l_main_dev2$244_outR1;
  logic [24:0] zres;
  assign zi1 = __resumption_tag[7:0];
  main_$L_Main_dev2$244  inst (zi1, __in0, main_$l_main_dev2$244_out);
  main_$L_Main_dev2$244  instR1 (__in0, 8'h0, main_$l_main_dev2$244_outR1);
  assign zres = (__resumption_tag[8] == 1'h0) ? main_$l_main_dev2$244_out : main_$l_main_dev2$244_outR1;
  assign __resumption_tag_next = zres[16:8];
  assign __st0_next = zres[7:0];
  assign __out0 = zres[24:17];
  initial {__resumption_tag, __st0} = 17'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 17'h10000;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module main_$L_Main_dev2$244 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [24:0] res);
  assign res = {arg1 + arg0, 1'h0, arg0, arg1};
endmodule