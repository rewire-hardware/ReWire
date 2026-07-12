module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [3:0] __in0,
  output logic [15:0] __out0);
  // state registers
  // __st0: 4 bits, init 0x0
  // __st1: 8 bits, init 0x3
  // __st2: 16 bits, init 0x5
  logic [3:0] __st0;
  logic [3:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  logic [15:0] __st2;
  logic [15:0] __st2_next;
  // combinational logic
  wire [7:0] s11 = {4'h0, __st0} + __st1;
  wire [15:0] s21 = {8'h0, __st1} + __st2;
  wire [43:0] Zres = {{12'h0, __st0} + __st2, __in0, s11, s21};
  assign __st0_next = Zres[27:24];
  assign __st1_next = Zres[23:16];
  assign __st2_next = Zres[15:0];
  // outputs
  assign __out0 = Zres[43:28];
  // state register update
  initial __st0 = 4'h0;
  initial __st1 = 8'h3;
  initial __st2 = 16'h5;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 4'h0;
      __st1 <= 8'h3;
      __st2 <= 16'h5;
    end else begin
      __st0 <= __st0_next;
      __st1 <= __st1_next;
      __st2 <= __st2_next;
    end
  end
endmodule