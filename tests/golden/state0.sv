module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [9:0] Zres;
  // state registers
  // __resumption_tag: 6 bits, init 0x20
  //   states: 0=$x2 1=_unused 2=_unused2 3=_unused3 4=_unused4
  logic [5:0] __resumption_tag;
  logic [5:0] __resumption_tag_next;
  // combinational logic
  wire [2:0] i = __resumption_tag[2:0];
  wire [3:0] Za = {1'h1, i};
  wire [2:0] scrut = __resumption_tag[5:3];
  always_comb case (scrut)
    3'h0: Zres = {7'h2, __in0};
    3'h1: Zres = {Za, 6'h0};
    3'h2: Zres = {7'h1, i};
    3'h3: Zres = 10'h200;
    default: Zres = 10'h18;
  endcase
  assign __resumption_tag_next = Zres[5:0];
  // outputs
  assign __out0 = Zres[9];
  assign __out1 = Zres[8:6];
  // state register update
  initial __resumption_tag = 6'h20;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 6'h20;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule