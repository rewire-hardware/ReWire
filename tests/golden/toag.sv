module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] __resumption_tag;
  logic [2:0] __resumption_tag_next;
  logic [0:0] zi0;
  logic [0:0] zi1;
  logic [0:0] zi2;
  logic [0:0] zi3;
  logic [3:0] zres;
  assign zi0 = __resumption_tag[0];
  assign zi1 = (zi0 == 1'h1) ? zi0 : 1'h0;
  assign zi2 = (__in0 == 1'h0) ? zi1 : zi0;
  assign zi3 = (zi2 == 1'h1) ? __in0 : 1'h0;
  assign zres = (__resumption_tag[2:1] == 2'h0) ? 4'h4 : ((__resumption_tag[2:1] == 2'h1) ? {zi3, 3'h0} : ((__in0 == 1'h0) ? {__in0, 2'h1, __in0} : {3'h1, __in0}));
  assign __resumption_tag_next = zres[2:0];
  assign __out0 = zres[3];
  initial __resumption_tag = 3'h4;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 3'h4;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule