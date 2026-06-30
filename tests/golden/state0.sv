module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [4:0] __resumption_tag;
  logic [4:0] __resumption_tag_next;
  logic [2:0] zi0;
  logic [2:0] zi1;
  logic [8:0] zres;
  assign zi0 = __resumption_tag[2:0];
  assign zi1 = __resumption_tag[2:0];
  assign zres = (__resumption_tag[4:3] == 2'h1) ? {1'h1, zi0, 5'h0} : ((__resumption_tag[4:3] == 2'h2) ? {6'h1, zi1} : {6'h2, __in0});
  assign __resumption_tag_next = zres[4:0];
  assign __out0 = zres[8];
  assign __out1 = zres[7:5];
  initial __resumption_tag = 5'h10;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 5'h10;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule