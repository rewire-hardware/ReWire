module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] zll_main_go6_in;
  logic [8:0] zll_main_go1_in;
  logic [7:0] zll_main_go_in;
  logic [15:0] zll_main_go28_in;
  logic [17:0] zll_main_go28_out;
  logic [17:0] zll_main_go29_in;
  logic [17:0] zll_main_go16_in;
  logic [15:0] zll_main_go26_in;
  logic [7:0] zll_main_incw81_in;
  logic [15:0] binop_in;
  logic [7:0] zll_main_go31_in;
  logic [17:0] zll_main_go31_out;
  logic [17:0] zll_main_go33_in;
  logic [17:0] zll_main_go33_out;
  logic [8:0] zll_main_go15_in;
  logic [7:0] zll_main_go12_in;
  logic [15:0] zll_main_go28_inR1;
  logic [17:0] zll_main_go28_outR1;
  logic [17:0] zll_main_go10_in;
  logic [17:0] zll_main_go25_in;
  logic [15:0] zll_main_go23_in;
  logic [7:0] zll_main_rolw81_in;
  logic [15:0] binop_inR1;
  logic [7:0] zll_main_msbitw83_in;
  logic [7:0] msbit_in;
  logic [0:0] resize_in;
  logic [15:0] binop_inR2;
  logic [7:0] zll_main_go31_inR1;
  logic [17:0] zll_main_go31_outR1;
  logic [17:0] zll_main_go33_inR1;
  logic [17:0] zll_main_go33_outR1;
  logic [1:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign zll_main_go6_in = {__in0, __st0};
  assign zll_main_go1_in = {zll_main_go6_in[7:0], zll_main_go6_in[8]};
  assign zll_main_go_in = zll_main_go1_in[8:1];
  assign zll_main_go28_in = {zll_main_go_in[7:0], zll_main_go_in[7:0]};
  ZLL_Main_go28  inst (zll_main_go28_in[15:0], zll_main_go28_out);
  assign zll_main_go29_in = zll_main_go28_out;
  assign zll_main_go16_in = zll_main_go29_in[17:0];
  assign zll_main_go26_in = {zll_main_go16_in[15:8], zll_main_go16_in[7:0]};
  assign zll_main_incw81_in = zll_main_go26_in[15:8];
  assign binop_in = {zll_main_incw81_in[7:0], 8'h1};
  assign zll_main_go31_in = binop_in[15:8] + binop_in[7:0];
  ZLL_Main_go31  instR1 (zll_main_go31_in[7:0], zll_main_go31_out);
  assign zll_main_go33_in = zll_main_go31_out;
  ZLL_Main_go33  instR2 (zll_main_go33_in[17:0], zll_main_go33_out);
  assign zll_main_go15_in = {zll_main_go6_in[7:0], zll_main_go6_in[8]};
  assign zll_main_go12_in = zll_main_go15_in[8:1];
  assign zll_main_go28_inR1 = {zll_main_go12_in[7:0], zll_main_go12_in[7:0]};
  ZLL_Main_go28  instR3 (zll_main_go28_inR1[15:0], zll_main_go28_outR1);
  assign zll_main_go10_in = zll_main_go28_outR1;
  assign zll_main_go25_in = zll_main_go10_in[17:0];
  assign zll_main_go23_in = {zll_main_go25_in[15:8], zll_main_go25_in[7:0]};
  assign zll_main_rolw81_in = zll_main_go23_in[15:8];
  assign binop_inR1 = {zll_main_rolw81_in[7:0], 8'h1};
  assign zll_main_msbitw83_in = zll_main_rolw81_in[7:0];
  assign msbit_in = zll_main_msbitw83_in[7:0];
  assign resize_in = msbit_in[7];
  assign binop_inR2 = {binop_inR1[15:8] << binop_inR1[7:0], 8'(resize_in[0])};
  assign zll_main_go31_inR1 = binop_inR2[15:8] | binop_inR2[7:0];
  ZLL_Main_go31  instR4 (zll_main_go31_inR1[7:0], zll_main_go31_outR1);
  assign zll_main_go33_inR1 = zll_main_go31_outR1;
  ZLL_Main_go33  instR5 (zll_main_go33_inR1[17:0], zll_main_go33_outR1);
  assign {__padding, __out0, __st0_next} = (zll_main_go15_in[0] == 1'h1) ? zll_main_go33_outR1 : zll_main_go33_out;
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_go33 (input logic [17:0] arg0,
  output logic [17:0] res);
  logic [17:0] zll_main_go22_in;
  logic [7:0] main_go_in;
  logic [15:0] zll_main_go28_in;
  logic [17:0] zll_main_go28_out;
  logic [17:0] zll_main_go21_in;
  logic [17:0] zll_main_go18_in;
  logic [15:0] zll_main_go8_in;
  assign zll_main_go22_in = arg0;
  assign main_go_in = zll_main_go22_in[7:0];
  assign zll_main_go28_in = {main_go_in[7:0], main_go_in[7:0]};
  ZLL_Main_go28  inst (zll_main_go28_in[15:0], zll_main_go28_out);
  assign zll_main_go21_in = zll_main_go28_out;
  assign zll_main_go18_in = zll_main_go21_in[17:0];
  assign zll_main_go8_in = {zll_main_go18_in[15:8], zll_main_go18_in[7:0]};
  assign res = {2'h2, zll_main_go8_in[15:8], zll_main_go8_in[7:0]};
endmodule

module ZLL_Main_go31 (input logic [7:0] arg0,
  output logic [17:0] res);
  assign res = {10'h100, arg0};
endmodule

module ZLL_Main_go28 (input logic [15:0] arg0,
  output logic [17:0] res);
  logic [15:0] zll_main_go17_in;
  assign zll_main_go17_in = arg0;
  assign res = {2'h0, zll_main_go17_in[15:8], zll_main_go17_in[7:0]};
endmodule