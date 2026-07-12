module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  // state registers
  // __st0: 1 bits, init 0x0
  // __st1: 1 bits, init 0x0
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] __st1;
  logic [0:0] __st1_next;
  // combinational logic
  wire [1:0] disc = {__st0, __st1};
  wire [2:0] Zres = {1'h0, disc};
  assign __st0_next = Zres[1];
  assign __st1_next = Zres[0];
  // outputs
  assign __out0 = Zres[2];
  // state register update
  initial __st0 = 1'h0;
  initial __st1 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
      __st1 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
      __st1 <= __st1_next;
    end
  end
endmodule