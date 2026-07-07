module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [1:0] main_$l__unused3$295_out;
  logic [1:0] main_$l__unused3$295_outR1;
  logic [1:0] zres;
  main_$L___unused3$295  inst (__st0, __st0, main_$l__unused3$295_out);
  main_$L___unused3$295  instR1 (__st0, 1'h0, main_$l__unused3$295_outR1);
  assign zres = (__in0 == 1'h0) ? ((__st0 == 1'h0) ? main_$l__unused3$295_out : main_$l__unused3$295_outR1) : 2'h2;
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

module main_$L___unused4$296 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  assign res = {arg0, arg1};
endmodule

module main_$L___unused3$295 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  logic [1:0] main_$l__unused4$296_out;
  logic [1:0] main_$l__unused4$296_outR1;
  main_$L___unused4$296  inst (arg0, 1'h1, main_$l__unused4$296_out);
  main_$L___unused4$296  instR1 (arg0, arg1, main_$l__unused4$296_outR1);
  assign res = (arg0 == 1'h0) ? main_$l__unused4$296_out : main_$l__unused4$296_outR1;
endmodule