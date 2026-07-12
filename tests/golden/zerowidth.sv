module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  // state registers
  // __st0: 8 bits, init 0x1
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  // combinational logic
  wire [7:0] s01 = __st0 + 8'h1;
  wire [15:0] Zres = {__st0, s01};
  assign __st0_next = Zres[7:0];
  // outputs
  assign __out0 = Zres[15:8];
  // state register update
  initial __st0 = 8'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule