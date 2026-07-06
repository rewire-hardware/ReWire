module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [1:0] zl__unused3_out;
  logic [1:0] zl__unused3_outR1;
  logic [1:0] zres;
  ZL___unused3  inst (__st0, __st0, zl__unused3_out);
  ZL___unused3  instR1 (__st0, 1'h0, zl__unused3_outR1);
  assign zres = (__in0 == 1'h0) ? ((__st0 == 1'h0) ? zl__unused3_out : zl__unused3_outR1) : 2'h2;
  assign __st0_next = zres[0];
  assign __out0 = zres[1];
  initial __st0 = 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZL___unused3 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  logic [1:0] zl__unused4_out;
  logic [1:0] zl__unused4_outR1;
  ZL___unused4  inst (arg0, 1'h1, zl__unused4_out);
  ZL___unused4  instR1 (arg0, arg1, zl__unused4_outR1);
  assign res = (arg0 == 1'h0) ? zl__unused4_out : zl__unused4_outR1;
endmodule

module ZL___unused4 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  assign res = {arg0, arg1};
endmodule