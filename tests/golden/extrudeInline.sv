module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [3:0] Zres;
  // state registers
  // __resumption_tag: 2 bits, init 0x2
  //   states: 0=$ds2 1=j 2=i
  // __st0: 1 bits, init 0x0
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  // combinational logic
  wire [0:0] Zt1 = __st0 & __in0;
  wire [0:0] Za = ~__in0;
  localparam [1:0] ST_ZDS2 = 2'h0;
  localparam [1:0] ST_J = 2'h1;
  localparam [1:0] ST_I = 2'h2;
  always_comb case (__resumption_tag)
    ST_ZDS2: Zres = 4'hc;
    ST_J: Zres = {Zt1, 2'h0, __st0};
    default: Zres = {Za, 2'h1, __in0};
  endcase
  assign __resumption_tag_next = Zres[2:1];
  assign __st0_next = Zres[0];
  // outputs
  assign __out0 = Zres[3];
  // state register update
  initial __resumption_tag = 2'h2;
  initial __st0 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h2;
      __st0 <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule