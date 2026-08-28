module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] Main_$cmask_out;
  logic [0:0] Main_$cbackfit_out;
  logic [0:0] Main_$cbackfit_outR1;
  // combinational logic
  Main_$cmask  Zcmask_i (__in0, Main_$cmask_out);
  Main_$cbackfit  Zcbackfit_i (Main_$cmask_out, Main_$cbackfit_out);
  Main_$cbackfit  Zcbackfit_iR1 (__in0, Main_$cbackfit_outR1);
  wire [0:0] Zeta0 = ~Main_$cbackfit_outR1;
  wire [7:0] Za = ((~Main_$cbackfit_out) ? 8'h0 : 8'h1) ^ ((~Zeta0) ? Main_$cmask_out : __in0);
  // outputs
  assign __out0 = Za;
endmodule

// Main.$cmask
module Main_$cmask (input logic [7:0] w,
  output logic [7:0] res);
  assign res = w & 8'h3c;
endmodule

// Main.$cbackfit
module Main_$cbackfit (input logic [7:0] w,
  output logic [0:0] res);
  assign res = w == 8'h1;
endmodule