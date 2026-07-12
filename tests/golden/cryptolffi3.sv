module top_level (input logic [1:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [9:0] o$0;
  logic [7:0] Za;
  // combinational logic
  wire [7:0] v$1 = __in1 + __in2;
  wire [15:0] q$4740 = {v$1, __in1};
  wire [7:0] c$$4747 = {__in1[3:0] + ((__in1[7:4] == 4'hf) ? 4'h1 : 4'h0), __in1[7:4] + 4'h1};
  wire [7:0] Zeta1 = (q$4740[15:8] ^ q$4740[7:0]) ^ {c$$4747[3:0], c$$4747[7:4]};
  wire [7:0] Zeta0 = {__in1[0], __in1[1], __in1[2], __in1[3], __in1[4], __in1[5], __in1[6], __in1[7]};
  wire [7:0] Zeta2 = __in2 - Zeta0;
  wire [7:0] v$1R1 = Zeta1 ^ Zeta2;
  always_comb case (__in0)
    2'h0: o$0 = 10'h0;
    2'h1: o$0 = 10'h100;
    default: o$0 = {2'h2, v$1R1};
  endcase
  wire [1:0] scrut = o$0[9:8];
  always_comb case (scrut)
    2'h0: Za = Zeta1 + Zeta2;
    2'h1: Za = Zeta1 - Zeta2;
    2'h2: Za = o$0[7:0];
    default: Za = 8'h0;
  endcase
  // outputs
  assign __out0 = Za;
endmodule