module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [5:0] __resumption_tag;
  logic [5:0] __resumption_tag_next;
  logic [2:0] zi0;
  logic [2:0] zi1;
  logic [9:0] zres;
  assign zi0 = __resumption_tag[2:0];
  assign zi1 = __resumption_tag[2:0];
  assign zres = (__resumption_tag[5:3] == 3'h1) ? {7'h3, __in0} : ((__resumption_tag[5:3] == 3'h2) ? 10'h208 : ((__resumption_tag[5:3] == 3'h3) ? {7'h4, zi0} : ((__resumption_tag[5:3] == 3'h4) ? {1'h1, zi1, 6'h8} : 10'h10)));
  assign __resumption_tag_next = zres[5:0];
  assign __out0 = zres[9];
  assign __out1 = zres[8:6];
  initial __resumption_tag = 6'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 6'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule