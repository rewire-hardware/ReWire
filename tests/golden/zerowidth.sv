module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] zi0;
  logic [15:0] zres;
  assign zi0 = __st0 + 8'h1;
  assign zres = {__st0, zi0};
  assign __st0_next = zres[7:0];
  assign __out0 = zres[15:8];
  initial __st0 = 8'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule