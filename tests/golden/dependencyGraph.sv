module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [9:0] __in0,
  output logic [15:0] __out0);
  logic [29:0] __st0;
  logic [29:0] __st0_next;
  logic [3:0] zi5;
  logic [0:0] zi6;
  logic [7:0] zi8;
  logic [0:0] zi9;
  logic [4:0] zi10;
  logic [3:0] zi12;
  logic [8:0] zi14;
  logic [8:0] zi16;
  logic [44:0] zi17;
  logic [14:0] zi18;
  logic [29:0] zi19;
  logic [14:0] zi20;
  logic [29:0] slice_in;
  logic [44:0] zi24;
  logic [29:0] zi25;
  logic [4:0] zi27;
  logic [3:0] zi28;
  logic [4:0] zi29;
  logic [7:0] zi30;
  logic [8:0] zi31;
  logic [75:0] zi32;
  logic [45:0] zi33;
  logic [29:0] zi34;
  logic [76:0] zi35;
  logic [15:0] zi38;
  logic [29:0] zi39;
  logic [76:0] zres;
  assign zi5 = __in0[3:0];
  assign zi6 = __st0[29];
  assign zi8 = __in0[7:0];
  assign zi9 = __st0[29];
  assign zi10 = __st0[28:24];
  assign zi12 = __st0[27:24];
  assign zi14 = __st0[23:15];
  assign zi16 = __st0[23:15];
  assign zi17 = {(__in0[9:8] == 2'h0) ? {zi6, 1'h1, zi5, 5'h0, zi5} : ((__in0[9:8] == 2'h1) ? {zi9, zi10, 1'h1, zi8} : (((__in0[9:8] == 2'h2) & ((__in0[0] == 1'h1) & (__st0[28] == 1'h1))) ? {11'h400, zi12} : (((__in0[9:8] == 2'h2) & ((__in0[0] == 1'h1) & (__st0[28] == 1'h0))) ? {6'h20, zi14} : {6'h0, zi16}))), __st0};
  assign zi18 = zi17[44:30];
  assign zi19 = zi17[29:0];
  assign zi20 = zi17[29:15];
  assign slice_in = zi19 >> {8'h80{1'h0}};
  assign zi24 = {{zi18, zi20}, slice_in[14:0]};
  assign zi25 = zi24[44:15];
  assign zi27 = zi24[13:9];
  assign zi28 = zi24[3:0];
  assign zi29 = zi24[13:9];
  assign zi30 = zi24[7:0];
  assign zi31 = zi24[8:0];
  assign zi32 = {{((zi24[14] == 1'h1) & (zi24[8] == 1'h0)) ? {2'h3, zi27, 5'h0, zi28} : (((zi24[14] == 1'h1) & (zi24[8] == 1'h1)) ? {2'h1, zi29, 1'h1, zi30} : {7'h0, zi31}), zi25}, __st0};
  assign zi33 = zi32[75:30];
  assign zi34 = zi32[29:0];
  assign zi35 = {1'h0, zi33, zi34};
  assign zi38 = zi35[75:60];
  assign zi39 = zi35[59:30];
  assign zres = {31'h40000000, zi38, zi39};
  assign __st0_next = zres[29:0];
  assign __out0 = zres[45:30];
  initial __st0 = 30'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 30'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule