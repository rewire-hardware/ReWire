module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  logic [0:0] zi1;
  logic [5:0] main_y_out;
  logic [5:0] main_y_outR1;
  logic [5:0] zres;
  assign zi1 = __resumption_tag[0];
  main_y  inst (__resumption_tag[0], __in0, __st0, __st1, main_y_out);
  main_y  instR1 (__in0, __st0, __st0, __st1, main_y_outR1);
  assign zres = (__resumption_tag[2:1] == 2'h0) ? {4'h4, __st0, zi1} : ((__resumption_tag[2:1] == 2'h1) ? main_y_out : ((~__in0) ? main_y_outR1 : {3'h1, __in0, __st0, __st1}));
  assign __resumption_tag_next = zres[4:2];
  assign __st0_next = zres[1];
  assign __st1_next = zres[0];
  assign __out0 = zres[5];
  initial {__resumption_tag, __st0, __st1} = 5'h11;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h11;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module main_y (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [5:0] res);
  logic [0:0] za;
  logic [0:0] zaR1;
  logic [0:0] za1;
  assign za = arg0 ? arg0 : 1'h0;
  assign zaR1 = (~arg1) ? za : arg0;
  assign za1 = zaR1 ? arg1 : 1'h0;
  assign res = {za1, 2'h0, arg1, arg2, arg3};
endmodule