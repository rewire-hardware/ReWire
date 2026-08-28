module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] Main_$cscramble_out;
  logic [7:0] Main_$cscramble_outR1;
  logic [7:0] Main_$cscramble_outR2;
  // combinational logic
  wire [7:0] w = __in0 ^ 8'ha5;
  Main_$cscramble  Zcscramble_i (w, Main_$cscramble_out);
  Main_$cscramble  Zcscramble_iR1 (__in0, Main_$cscramble_outR1);
  Main_$cscramble  Zcscramble_iR2 (Main_$cscramble_outR1, Main_$cscramble_outR2);
  wire [7:0] Za = (Main_$cscramble_out | 8'hf) ^ (Main_$cscramble_outR2 & 8'hf0);
  // outputs
  assign __out0 = Za;
endmodule

// Main.$cscramble
module Main_$cscramble (input logic [7:0] w,
  output logic [7:0] res);
  assign res = w ^ 8'h5a;
endmodule