module top_level (input logic [6:0] __in0,
  output logic [4:0] __out0);
  logic [6:0] zin;
  logic [6:0] slice_in;
  logic [4:0] zi0;
  logic [9:0] zi1;
  logic [4:0] zi2;
  logic [4:0] zi3;
  logic [127:0] slice_inR1;
  logic [5:0] zi4;
  logic [4:0] zi5;
  logic [5:0] zres;
  assign zin = __in0;
  assign slice_in = (7'h14 == 7'h0) ? zin : (zin % 7'h14);
  assign zi0 = slice_in[4:0];
  assign zi1 = {zi0, 5'h6};
  assign zi2 = zi1[9:5];
  assign zi3 = zi1[4:0];
  assign slice_inR1 = (128'h14 == {8'h80{1'h0}}) ? ({{7'h7b{1'h0}}, zi2} + {{7'h7b{1'h0}}, zi3}) : (({{7'h7b{1'h0}}, zi2} + {{7'h7b{1'h0}}, zi3}) % 128'h14);
  assign zi4 = {1'h0, slice_inR1[4:0]};
  assign zi5 = zi4[4:0];
  assign zres = {1'h1, zi5};
  assign __out0 = zres[4:0];
endmodule