module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  output logic [7:0] __out0);
  // combinational logic
  wire [7:0] zr$b$0 = (8'hfb == 8'h0) ? __in0 : (__in0 % 8'hfb);
  wire [15:0] slice_in = (16'hfb == 16'h0) ? ({8'h0, zr$b$0} * {8'h0, zr$b$0}) : (({8'h0, zr$b$0} * {8'h0, zr$b$0}) % 16'hfb);
  wire [7:0] zr$b$1 = slice_in[7:0];
  wire [15:0] slice_inR1 = (16'hfb == 16'h0) ? ({8'h0, zr$b$1} * {8'h0, zr$b$1}) : (({8'h0, zr$b$1} * {8'h0, zr$b$1}) % 16'hfb);
  wire [7:0] zr$b$2 = slice_inR1[7:0];
  wire [15:0] slice_inR2 = (16'hfb == 16'h0) ? ({8'h0, zr$b$2} * {8'h0, zr$b$2}) : (({8'h0, zr$b$2} * {8'h0, zr$b$2}) % 16'hfb);
  wire [7:0] zr$b$3 = slice_inR2[7:0];
  wire [15:0] slice_inR3 = (16'hfb == 16'h0) ? ({8'h0, zr$b$3} * {8'h0, zr$b$3}) : (({8'h0, zr$b$3} * {8'h0, zr$b$3}) % 16'hfb);
  wire [7:0] zr$b$4 = slice_inR3[7:0];
  wire [15:0] slice_inR4 = (16'hfb == 16'h0) ? ({8'h0, zr$b$4} * {8'h0, zr$b$4}) : (({8'h0, zr$b$4} * {8'h0, zr$b$4}) % 16'hfb);
  wire [7:0] zr$b$5 = slice_inR4[7:0];
  wire [15:0] slice_inR5 = (16'hfb == 16'h0) ? ({8'h0, zr$b$5} * {8'h0, zr$b$5}) : (({8'h0, zr$b$5} * {8'h0, zr$b$5}) % 16'hfb);
  wire [7:0] zr$b$6 = slice_inR5[7:0];
  wire [15:0] slice_inR6 = (16'hfb == 16'h0) ? ({8'h0, zr$b$6} * {8'h0, zr$b$6}) : (({8'h0, zr$b$6} * {8'h0, zr$b$6}) % 16'hfb);
  wire [7:0] zr$b$7 = slice_inR6[7:0];
  wire [15:0] slice_inR7 = (16'hfb == 16'h0) ? (16'h1 * {8'h0, zr$b$0}) : ((16'h1 * {8'h0, zr$b$0}) % 16'hfb);
  wire [15:0] slice_inR8 = (16'hfb == 16'h0) ? ({8'h0, slice_inR7[7:0]} * {8'h0, zr$b$3}) : (({8'h0, slice_inR7[7:0]} * {8'h0, zr$b$3}) % 16'hfb);
  wire [15:0] slice_inR9 = (16'hfb == 16'h0) ? ({8'h0, slice_inR8[7:0]} * {8'h0, zr$b$4}) : (({8'h0, slice_inR8[7:0]} * {8'h0, zr$b$4}) % 16'hfb);
  wire [15:0] slice_inR10 = (16'hfb == 16'h0) ? ({8'h0, slice_inR9[7:0]} * {8'h0, zr$b$5}) : (({8'h0, slice_inR9[7:0]} * {8'h0, zr$b$5}) % 16'hfb);
  wire [15:0] slice_inR11 = (16'hfb == 16'h0) ? ({8'h0, slice_inR10[7:0]} * {8'h0, zr$b$6}) : (({8'h0, slice_inR10[7:0]} * {8'h0, zr$b$6}) % 16'hfb);
  wire [15:0] slice_inR12 = (16'hfb == 16'h0) ? ({8'h0, slice_inR11[7:0]} * {8'h0, zr$b$7}) : (({8'h0, slice_inR11[7:0]} * {8'h0, zr$b$7}) % 16'hfb);
  wire [7:0] zr$b$0R1 = (8'hfb == 8'h0) ? __in1 : (__in1 % 8'hfb);
  wire [15:0] slice_inR13 = (16'hfb == 16'h0) ? ({8'h0, zr$b$0R1} * {8'h0, zr$b$0R1}) : (({8'h0, zr$b$0R1} * {8'h0, zr$b$0R1}) % 16'hfb);
  wire [7:0] zr$b$1R1 = slice_inR13[7:0];
  wire [15:0] slice_inR14 = (16'hfb == 16'h0) ? ({8'h0, zr$b$1R1} * {8'h0, zr$b$1R1}) : (({8'h0, zr$b$1R1} * {8'h0, zr$b$1R1}) % 16'hfb);
  wire [7:0] zr$b$2R1 = slice_inR14[7:0];
  wire [15:0] slice_inR15 = (16'hfb == 16'h0) ? ({8'h0, zr$b$2R1} * {8'h0, zr$b$2R1}) : (({8'h0, zr$b$2R1} * {8'h0, zr$b$2R1}) % 16'hfb);
  wire [7:0] zr$b$3R1 = slice_inR15[7:0];
  wire [15:0] slice_inR16 = (16'hfb == 16'h0) ? ({8'h0, zr$b$3R1} * {8'h0, zr$b$3R1}) : (({8'h0, zr$b$3R1} * {8'h0, zr$b$3R1}) % 16'hfb);
  wire [7:0] zr$b$4R1 = slice_inR16[7:0];
  wire [15:0] slice_inR17 = (16'hfb == 16'h0) ? ({8'h0, zr$b$4R1} * {8'h0, zr$b$4R1}) : (({8'h0, zr$b$4R1} * {8'h0, zr$b$4R1}) % 16'hfb);
  wire [7:0] zr$b$5R1 = slice_inR17[7:0];
  wire [15:0] slice_inR18 = (16'hfb == 16'h0) ? ({8'h0, zr$b$5R1} * {8'h0, zr$b$5R1}) : (({8'h0, zr$b$5R1} * {8'h0, zr$b$5R1}) % 16'hfb);
  wire [7:0] zr$b$6R1 = slice_inR18[7:0];
  wire [15:0] slice_inR19 = (16'hfb == 16'h0) ? ({8'h0, zr$b$6R1} * {8'h0, zr$b$6R1}) : (({8'h0, zr$b$6R1} * {8'h0, zr$b$6R1}) % 16'hfb);
  wire [7:0] zr$b$7R1 = slice_inR19[7:0];
  wire [15:0] slice_inR20 = (16'hfb == 16'h0) ? (16'h1 * {8'h0, zr$b$0R1}) : ((16'h1 * {8'h0, zr$b$0R1}) % 16'hfb);
  wire [15:0] slice_inR21 = (16'hfb == 16'h0) ? ({8'h0, slice_inR20[7:0]} * {8'h0, zr$b$3R1}) : (({8'h0, slice_inR20[7:0]} * {8'h0, zr$b$3R1}) % 16'hfb);
  wire [15:0] slice_inR22 = (16'hfb == 16'h0) ? ({8'h0, slice_inR21[7:0]} * {8'h0, zr$b$4R1}) : (({8'h0, slice_inR21[7:0]} * {8'h0, zr$b$4R1}) % 16'hfb);
  wire [15:0] slice_inR23 = (16'hfb == 16'h0) ? ({8'h0, slice_inR22[7:0]} * {8'h0, zr$b$5R1}) : (({8'h0, slice_inR22[7:0]} * {8'h0, zr$b$5R1}) % 16'hfb);
  wire [15:0] slice_inR24 = (16'hfb == 16'h0) ? ({8'h0, slice_inR23[7:0]} * {8'h0, zr$b$6R1}) : (({8'h0, slice_inR23[7:0]} * {8'h0, zr$b$6R1}) % 16'hfb);
  wire [15:0] slice_inR25 = (16'hfb == 16'h0) ? ({8'h0, slice_inR24[7:0]} * {8'h0, zr$b$7R1}) : (({8'h0, slice_inR24[7:0]} * {8'h0, zr$b$7R1}) % 16'hfb);
  wire [15:0] slice_inR26 = (16'hfb == 16'h0) ? ({8'h0, (8'hfb == 8'h0) ? __in0 : (__in0 % 8'hfb)} * {8'h0, slice_inR25[7:0]}) : (({8'h0, (8'hfb == 8'h0) ? __in0 : (__in0 % 8'hfb)} * {8'h0, slice_inR25[7:0]}) % 16'hfb);
  wire [7:0] Za = slice_inR12[7:0] + slice_inR26[7:0];
  // outputs
  assign __out0 = Za;
endmodule