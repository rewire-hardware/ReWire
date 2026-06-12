module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign __resumption_tag_next = __resumption_tag;
  assign __st0_next = __resumption_tag;
  assign __out0 = __resumption_tag;
  initial {__resumption_tag, __st0} = 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule