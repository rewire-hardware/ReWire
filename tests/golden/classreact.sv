module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [18:0] Zres;
  // state registers
  // __resumption_tag: 11 bits, init 0x600
  //   states: 0=i 1=i2 2=i3 3=i4 4=i5 5=i6 6=i7 7=i8
  logic [10:0] __resumption_tag;
  logic [10:0] __resumption_tag_next;
  // combinational logic
  wire [7:0] n = __resumption_tag[7:0];
  wire [0:0] Za = __in0 == 8'h0;
  wire [7:0] nR1 = n + __in0;
  wire [7:0] nR2 = n - __in0;
  wire [2:0] scrut = __resumption_tag[10:8];
  always_comb case (scrut)
    3'h0: Zres = (~Za) ? {nR1, 3'h0, nR1} : 19'h200;
    3'h1: Zres = (~Za) ? {nR2, 3'h1, nR2} : 19'h200;
    3'h2: Zres = (~Za) ? {nR1, 3'h2, nR1} : 19'h7f9ff;
    3'h3: Zres = (~Za) ? {nR2, 3'h3, nR2} : 19'h7f9ff;
    3'h4: Zres = (~Za) ? {nR1, 3'h4, nR1} : 19'h200;
    3'h5: Zres = (~Za) ? {nR2, 3'h5, nR2} : 19'h200;
    3'h6: Zres = (~Za) ? {nR1, 3'h6, nR1} : 19'h7fdff;
    default: Zres = (~Za) ? {nR2, 3'h7, nR2} : 19'h7fdff;
  endcase
  assign __resumption_tag_next = Zres[10:0];
  // outputs
  assign __out0 = Zres[18:11];
  // state register update
  initial __resumption_tag = 11'h600;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 11'h600;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule