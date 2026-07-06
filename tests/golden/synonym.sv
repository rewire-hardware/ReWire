module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  logic [23:0] main_$l_main_sig4$259_out;
  logic [7:0] zi3;
  logic [23:0] main_$l_main_sig4$259_outR1;
  logic [23:0] zres;
  main_$L_Main_sig4$259  inst (__st0, __st1, main_$l_main_sig4$259_out);
  assign zi3 = __st0 + __st1;
  main_$L_Main_sig4$259  instR1 (__st1, zi3, main_$l_main_sig4$259_outR1);
  assign zres = (__in0 == 1'h0) ? main_$l_main_sig4$259_out : main_$l_main_sig4$259_outR1;
  assign __st0_next = zres[15:8];
  assign __st1_next = zres[7:0];
  assign __out0 = zres[23:16];
  initial {__st0, __st1} = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h1;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module main_$L_Main_sig4$259 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [23:0] res);
  assign res = {arg0, arg0, arg1};
endmodule