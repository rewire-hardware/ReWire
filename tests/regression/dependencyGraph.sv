module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [9:0] __in0,
  output logic [15:0] __out0);
  logic [29:0] __st0;
  logic [29:0] __st0_next;
  logic [3:0] zi8;
  logic [0:0] zi9;
  logic [7:0] zi13;
  logic [0:0] zi14;
  logic [4:0] zi15;
  logic [3:0] zi19;
  logic [8:0] zi23;
  logic [8:0] zi27;
  logic [44:0] zi28;
  logic [14:0] zi29;
  logic [29:0] zi30;
  logic [14:0] zi31;
  logic [29:0] slice_in;
  logic [44:0] zi35;
  logic [29:0] zi36;
  logic [4:0] zi38;
  logic [3:0] zi39;
  logic [4:0] zi40;
  logic [7:0] zi41;
  logic [8:0] zi43;
  logic [75:0] zi44;
  logic [45:0] zi45;
  logic [29:0] zi46;
  logic [77:0] zi47;
  logic [15:0] zi50;
  logic [29:0] zi51;
  logic [77:0] zres;
  assign zi8 = __in0[3:0];
  assign zi9 = __st0[29];
  assign zi13 = __in0[7:0];
  assign zi14 = __st0[29];
  assign zi15 = __st0[28:24];
  assign zi19 = __st0[27:24];
  assign zi23 = __st0[23:15];
  assign zi27 = __st0[23:15];
  assign zi28 = {(__in0[9:8] == 2'h0) ? {zi9, 1'h1, zi8, 5'h0, zi8} : ((__in0[9:8] == 2'h1) ? {zi14, zi15, 1'h1, zi13} : (((__in0[9:8] == 2'h2) & ((__in0[0] == 1'h1) & (__st0[28] == 1'h1))) ? {11'h400, zi19} : (((__in0[9:8] == 2'h2) & ((__in0[0] == 1'h1) & (__st0[28] == 1'h0))) ? {6'h20, zi23} : {6'h0, zi27}))), __st0};
  assign zi29 = zi28[44:30];
  assign zi30 = zi28[29:0];
  assign zi31 = zi28[29:15];
  assign slice_in = zi30 >> {8'h80{1'h0}};
  assign zi35 = {{zi29, zi31}, slice_in[14:0]};
  assign zi36 = zi35[44:15];
  assign zi38 = zi35[13:9];
  assign zi39 = zi35[3:0];
  assign zi40 = zi35[13:9];
  assign zi41 = zi35[7:0];
  assign zi43 = zi35[8:0];
  assign zi44 = {{((zi35[14] == 1'h1) & (zi35[8] == 1'h0)) ? {2'h3, zi38, 5'h0, zi39} : (((zi35[14] == 1'h1) & (zi35[8] == 1'h1)) ? {2'h1, zi40, 1'h1, zi41} : {7'h0, zi43}), zi36}, __st0};
  assign zi45 = zi44[75:30];
  assign zi46 = zi44[29:0];
  assign zi47 = {2'h0, zi45, zi46};
  assign zi50 = zi47[75:60];
  assign zi51 = zi47[59:30];
  assign zres = {{1'h1, {5'h1f{1'h0}}}, zi50, zi51};
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