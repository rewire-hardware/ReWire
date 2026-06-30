module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [7:0] __resumption_tag;
  logic [7:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] zi2;
  logic [23:0] zres;
  assign zi2 = __resumption_tag + 8'h1;
  assign zres = {zi2, zi2, zi2};
  assign __resumption_tag_next = zres[15:8];
  assign __st0_next = zres[7:0];
  assign __out0 = zres[23:16];
  initial {__resumption_tag, __st0} = 16'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 16'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule