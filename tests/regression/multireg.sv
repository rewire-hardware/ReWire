module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [3:0] __in0,
  output logic [15:0] __out0);
  logic [3:0] __st0;
  logic [3:0] __st0_next;
  logic [7:0] __st1;
  logic [7:0] __st1_next;
  logic [15:0] __st2;
  logic [15:0] __st2_next;
  logic [27:0] zi42;
  logic [3:0] zi43;
  logic [7:0] zi44;
  logic [15:0] zi45;
  logic [46:0] zi46;
  logic [3:0] zi47;
  logic [7:0] zi48;
  logic [27:0] zi50;
  logic [3:0] zi51;
  logic [7:0] zi52;
  logic [15:0] zi53;
  logic [46:0] zi54;
  logic [3:0] zi55;
  logic [7:0] zi56;
  logic [15:0] zi57;
  logic [46:0] zres;
  assign zi42 = {__in0, {4'h0, __st0} + __st1, __st2};
  assign zi43 = zi42[27:24];
  assign zi44 = zi42[23:16];
  assign zi45 = zi42[15:0];
  assign zi46 = {19'h30000, zi43, zi44, zi45};
  assign zi47 = zi46[27:24];
  assign zi48 = zi46[23:16];
  assign zi50 = {zi47, zi48, {8'h0, __st1} + __st2};
  assign zi51 = zi50[27:24];
  assign zi52 = zi50[23:16];
  assign zi53 = zi50[15:0];
  assign zi54 = {19'h30000, zi51, zi52, zi53};
  assign zi55 = zi54[27:24];
  assign zi56 = zi54[23:16];
  assign zi57 = zi54[15:0];
  assign zres = {3'h4, {12'h0, __st0} + __st2, zi55, zi56, zi57};
  assign __st0_next = zres[27:24];
  assign __st1_next = zres[23:16];
  assign __st2_next = zres[15:0];
  assign __out0 = zres[43:28];
  initial {__st0, __st1, __st2} = 28'h30005;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1, __st2} <= 28'h30005;
    end else begin
      {__st0, __st1, __st2} <= {__st0_next, __st1_next, __st2_next};
    end
  end
endmodule