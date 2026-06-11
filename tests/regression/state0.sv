module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [4:0] __resumption_tag;
  logic [4:0] __resumption_tag_next;
  logic [7:0] zin;
  logic [4:0] zi0;
  logic [2:0] zi1;
  logic [7:0] zi2;
  logic [2:0] zi3;
  logic [2:0] zi4;
  logic [7:0] zi5;
  logic [2:0] zi6;
  logic [2:0] zi7;
  logic [7:0] zi8;
  logic [2:0] zi9;
  logic [8:0] zres;
  assign zin = {__resumption_tag, __in0};
  assign zi0 = zin[7:3];
  assign zi1 = zin[2:0];
  assign zi2 = {zi1, zi0};
  assign zi3 = zi2[7:5];
  assign zi4 = zi2[2:0];
  assign zi5 = {zi1, zi0};
  assign zi6 = zi5[7:5];
  assign zi7 = zi5[2:0];
  assign zi8 = {zi1, zi0};
  assign zi9 = zi8[7:5];
  assign zres = (zi2[4:3] == 2'h1) ? {1'h1, zi4, 5'h0} : ((zi5[4:3] == 2'h2) ? {6'h1, zi7} : {6'h2, zi9});
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