module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  logic [0:0] zi3;
  logic [2:0] zl__unused7_out;
  logic [0:0] zi4;
  logic [2:0] zl__unused7_outR1;
  logic [2:0] zres;
  assign zi3 = (__in0 == 1'h1) ? 1'h0 : 1'h1;
  ZL___unused7  inst (__st0, __st1, zl__unused7_out);
  assign zi4 = __st0 ^ __st1;
  ZL___unused7  instR1 (__st1, zi4, zl__unused7_outR1);
  assign zres = (zi3 == 1'h0) ? zl__unused7_out : zl__unused7_outR1;
  assign __st0_next = zres[1];
  assign __st1_next = zres[0];
  assign __out0 = zres[2];
  initial {__st0, __st1} = 2'h3;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 2'h3;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZL___unused7 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [2:0] res);
  assign res = {arg0, arg0, arg1};
endmodule