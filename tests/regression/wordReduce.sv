module top_level (input logic [7:0] __in0,
  output logic [5:0] __out0);
  logic [7:0] zll_main_compute1_in;
  logic [7:0] unop_in;
  logic [7:0] unop_inR1;
  logic [7:0] unop_inR2;
  logic [7:0] unop_inR3;
  logic [7:0] unop_inR4;
  logic [7:0] unop_inR5;
  assign zll_main_compute1_in = __in0;
  assign unop_in = zll_main_compute1_in[7:0];
  assign unop_inR1 = zll_main_compute1_in[7:0];
  assign unop_inR2 = zll_main_compute1_in[7:0];
  assign unop_inR3 = zll_main_compute1_in[7:0];
  assign unop_inR4 = zll_main_compute1_in[7:0];
  assign unop_inR5 = zll_main_compute1_in[7:0];
  assign __out0 = {&unop_in[7:0], ~&unop_inR1[7:0], |unop_inR2[7:0], ~|unop_inR3[7:0], ^unop_inR4[7:0], ~^unop_inR5[7:0]};
endmodule