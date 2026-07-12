module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [1:0] main_arm_out;
  logic [0:0] zi0;
  logic [1:0] main_arm_outR1;
  logic [1:0] zres;
  main_arm  inst (__st0, main_arm_out);
  assign zi0 = ~__st0;
  main_arm  instR1 (zi0, main_arm_outR1);
  assign zres = (~__in0) ? main_arm_out : main_arm_outR1;
  assign __st0_next = zres[0];
  assign __out0 = zres[1];
  initial __st0 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module main_arm (input logic [0:0] arg0,
  output logic [1:0] res);
  assign res = {arg0, arg0};
endmodule