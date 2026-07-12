module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] __resumption_tag;
  logic [8:0] __resumption_tag_next;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  logic [7:0] zi2;
  logic [7:0] zi4;
  logic [24:0] zres;
  assign zi2 = __resumption_tag[7:0];
  assign zi4 = zi2 + 8'h1;
  assign zres = (~__resumption_tag[8]) ? 25'h10000 : ((~__in0) ? {zi2 + zi2, 9'h0, zi4} : {zi4, 1'h1, zi4, zi4});
  assign __resumption_tag_next = zres[16:8];
  assign __st0_next = zres[7:0];
  assign __out0 = zres[24:17];
  initial {__resumption_tag, __st0} = 17'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 17'h10000;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule