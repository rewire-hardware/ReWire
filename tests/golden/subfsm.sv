module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  // state registers
  // __resumption_tag: 9 bits, init 0x100
  //   states: 0=$ds 1=i
  // __st0: 8 bits, init 0x0
  logic [8:0] __resumption_tag;
  logic [8:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  // combinational logic
  wire [7:0] s = __resumption_tag[7:0];
  wire [7:0] s01 = s + 8'h1;
  wire [24:0] Zres = (~__resumption_tag[8]) ? 25'h10000 :
    ((~__in0) ? {s + s, 9'h0, s01} : {s01, 1'h1, s01, s01});
  assign __resumption_tag_next = Zres[16:8];
  assign __st0_next = Zres[7:0];
  // outputs
  assign __out0 = Zres[24:17];
  // state register update
  initial __resumption_tag = 9'h100;
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 9'h100;
      __st0 <= 8'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule