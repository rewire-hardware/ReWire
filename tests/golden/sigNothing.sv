module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [0:0] __out0,
  output logic [7:0] __out1);
  // state registers
  // __st0: 8 bits, init 0x0
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  // combinational logic
  assign __st0_next = __st0;
  // outputs
  assign __out0 = 1'h0;
  assign __out1 = 8'h0;
  // state register update
  initial __st0 = 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule