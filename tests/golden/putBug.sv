module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  logic [0:0] zi1;
  logic [3:0] zres;
  assign zi1 = __resumption_tag[0];
  assign zres = (__resumption_tag[1] == 1'h0) ? {zi1, 1'h0, zi1, zi1} : 4'h0;
  assign __resumption_tag_next = zres[2:1];
  assign __st0_next = zres[0];
  assign __out0 = zres[3];
  initial {__resumption_tag, __st0} = 3'h4;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 3'h4;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule