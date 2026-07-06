module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  logic [7:0] zi0;
  logic [15:0] slice_in;
  logic [7:0] zi1;
  logic [7:0] zi2;
  logic [15:0] zi3;
  logic [23:0] main_$l_main_first14$292_out;
  logic [23:0] main_$l_main_first14$292_outR1;
  logic [23:0] zres;
  assign zi0 = __st0[15:8];
  assign slice_in = __st0 >> {8'h80{1'h0}};
  assign zi1 = slice_in[7:0];
  assign zi2 = zi0 + zi1;
  assign zi3 = {zi1, zi2};
  main_$L_Main_first14$292  inst (zi3, main_$l_main_first14$292_out);
  main_$L_Main_first14$292  instR1 (__st0, main_$l_main_first14$292_outR1);
  assign zres = (__in0 == 1'h0) ? main_$l_main_first14$292_out : main_$l_main_first14$292_outR1;
  assign __st0_next = zres[15:0];
  assign __out0 = zres[23:16];
  initial __st0 = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module main_$L_Main_first14$292 (input logic [15:0] arg0,
  output logic [23:0] res);
  logic [7:0] zi0;
  assign zi0 = arg0[15:8];
  assign res = {zi0, arg0};
endmodule