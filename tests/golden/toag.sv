module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [3:0] Zres;
  // state registers
  // __resumption_tag: 3 bits, init 0x4
  //   states: 0=z 1=y 2=x
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  // combinational logic
  wire [0:0] x = __resumption_tag[0];
  wire [0:0] Za = x ? x : 1'h0;
  wire [0:0] ZaR1 = (~__in0) ? Za : x;
  wire [0:0] Za1 = ZaR1 ? __in0 : 1'h0;
  wire [1:0] scrut = __resumption_tag[2:1];
  always_comb case (scrut)
    2'h0: Zres = 4'h4;
    2'h1: Zres = {Za1, 3'h0};
    default: Zres = (~__in0) ? {__in0, 2'h1, __in0} : {3'h1, __in0};
  endcase
  assign __resumption_tag_next = Zres[2:0];
  // outputs
  assign __out0 = Zres[3];
  // state register update
  initial __resumption_tag = 3'h4;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h4;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule