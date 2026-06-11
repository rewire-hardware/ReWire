module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [3:0] zin;
  logic [2:0] zi0;
  logic [0:0] zi1;
  logic [3:0] zi2;
  logic [0:0] zi3;
  logic [3:0] zi4;
  logic [0:0] zi5;
  logic [1:0] zi6;
  logic [0:0] zi7;
  logic [2:0] zi8;
  logic [0:0] zi9;
  logic [0:0] zi10;
  logic [3:0] zi11;
  logic [3:0] zll_pure_dispatch5_out;
  logic [3:0] zi12;
  logic [3:0] zll_pure_dispatch5_outR1;
  logic [3:0] zres;
  assign zin = {__resumption_tag, __in0};
  assign zi0 = zin[3:1];
  assign zi1 = zin[0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[3];
  assign zi4 = {zi1, zi0};
  assign zi5 = zi4[3];
  assign zi6 = {zi5, zi5};
  assign zi7 = zi6[1];
  assign zi8 = {zi5, zi5, zi5};
  assign zi9 = zi8[2];
  assign zi10 = zi8[1];
  assign zi11 = {zi1, zi0};
  ZLL_Pure_dispatch5  inst (zi11[3], zi11[0], zll_pure_dispatch5_out);
  assign zi12 = {zi1, zi0};
  ZLL_Pure_dispatch5  instR1 (zi12[3], zi12[0], zll_pure_dispatch5_outR1);
  assign zres = (zi2[2:1] == 2'h1) ? 4'h4 : ((zi4[2:1] == 2'h2) ? ((zi6[0] == 1'h1) ? {3'h3, zi7} : {zi10, 2'h0, zi9}) : ((zi11[2:1] == 2'h3) ? zll_pure_dispatch5_out : zll_pure_dispatch5_outR1));
  assign __resumption_tag_next = zres[2:0];
  assign __out0 = zres[3];
  initial __resumption_tag = 3'h4;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h4;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_go11 (input logic [0:0] arg0,
  output logic [0:0] res);
  assign res = arg0;
endmodule

module ZLL_Pure_dispatch5 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [1:0] zi0;
  logic [0:0] zll_main_go11_out;
  logic [1:0] zi1;
  logic [0:0] zi2;
  logic [0:0] rewirezupreludezuzazazuout;
  logic [0:0] rewirezupreludezuzazazuoutR1;
  assign zi0 = {arg1, arg0};
  ZLL_Main_go11  inst (zi0[1], zll_main_go11_out);
  assign zi1 = {arg1, arg0};
  assign zi2 = zi1[1];
  ReWirezuPreludezuzaza  instR1 (zi2, zi2, rewirezupreludezuzazazuout);
  ReWirezuPreludezuzaza  instR2 ((zi0[0] == 1'h1) ? zll_main_go11_out : rewirezupreludezuzazazuout, arg0, rewirezupreludezuzazazuoutR1);
  assign res = {rewirezupreludezuzazazuoutR1, 3'h2};
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [1:0] zi0;
  logic [0:0] zll_main_go11_out;
  logic [1:0] zi1;
  logic [0:0] zi2;
  assign zi0 = {arg0, arg1};
  ZLL_Main_go11  inst (zi0[0], zll_main_go11_out);
  assign zi1 = {arg0, arg1};
  assign zi2 = zi1[0];
  assign res = (zi0[1] == 1'h1) ? zll_main_go11_out : 1'h0;
endmodule