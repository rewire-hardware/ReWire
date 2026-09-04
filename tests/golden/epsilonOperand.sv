module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  // combinational logic
  wire [0:0] p = __in0 == 8'h3;
  wire [0:0] Zf0 = __in0[7];
  wire [0:0] Za = (~p) ? Zf0 : 1'h0;
  wire [0:0] Zf1 = __in0[6];
  wire [0:0] ZaR1 = (~p) ? Zf1 : 1'h0;
  wire [0:0] Zf2 = __in0[5];
  wire [0:0] ZaR2 = (~p) ? Zf2 : 1'h0;
  wire [0:0] Zf3 = __in0[4];
  wire [0:0] ZaR3 = (~p) ? Zf3 : 1'h0;
  wire [0:0] Zf4 = __in0[3];
  wire [0:0] ZaR4 = (~p) ? Zf4 : 1'h0;
  wire [0:0] Zf5 = __in0[2];
  wire [0:0] ZaR5 = (~p) ? Zf5 : 1'h0;
  wire [0:0] Zf6 = __in0[1];
  wire [0:0] ZaR6 = (~p) ? Zf6 : 1'h0;
  wire [0:0] Zf7 = __in0[0];
  wire [0:0] ZaR7 = (~p) ? Zf7 : 1'h0;
  wire [7:0] v = {Za, ZaR1, ZaR2, ZaR3, ZaR4, ZaR5, ZaR6, ZaR7};
  // outputs
  assign __out0 = v;
endmodule