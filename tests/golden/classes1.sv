module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] Main_$ckeep_out;
  logic [7:0] Main_$cinvert_out;
  logic [7:0] Main_$ckeep_outR1;
  // combinational logic
  wire [0:0] x = __in0[0];
  wire [0:0] Zt0 = __in0[7];
  wire [0:0] y = ~Zt0;
  wire [0:0] Zt0R1 = ~x;
  wire [0:0] Zt0R2 = ~y;
  wire [0:0] Zt1 = Zt0R1 ? (x ? 1'h1 : (~Zt0R2)) : 1'h0;
  Main_$ckeep  Zckeep_i (__in0, Main_$ckeep_out);
  Main_$cinvert  Zcinvert_i (__in0, Main_$cinvert_out);
  Main_$ckeep  Zckeep_iR1 (Main_$cinvert_out, Main_$ckeep_outR1);
  wire [7:0] Za = (~Zt1) ? Main_$ckeep_out : (Main_$cinvert_out & (__in0 | Main_$ckeep_outR1));
  // outputs
  assign __out0 = Za;
endmodule

// Main.$ckeep
module Main_$ckeep (input logic [7:0] w,
  output logic [7:0] res);
  assign res = w;
endmodule

// Main.$cinvert
module Main_$cinvert (input logic [7:0] w,
  output logic [7:0] res);
  assign res = w ^ 8'hff;
endmodule