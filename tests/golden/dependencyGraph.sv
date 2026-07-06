module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [9:0] __in0,
  output logic [15:0] __out0);
  logic [29:0] __st0;
  logic [29:0] __st0_next;
  logic [3:0] zi1;
  logic [0:0] zi2;
  logic [7:0] zi3;
  logic [0:0] zi4;
  logic [4:0] zi5;
  logic [0:0] zi6;
  logic [8:0] zi7;
  logic [8:0] zi9;
  logic [3:0] zi10;
  logic [44:0] zi11;
  logic [14:0] zi12;
  logic [29:0] zi13;
  logic [29:0] slice_in;
  logic [44:0] zi14;
  logic [14:0] zi15;
  logic [14:0] zi16;
  logic [0:0] zi17;
  logic [4:0] zi18;
  logic [8:0] zi19;
  logic [3:0] zi20;
  logic [7:0] zi21;
  logic [29:0] zi22;
  logic [45:0] zi23;
  logic [15:0] zi24;
  logic [29:0] zi25;
  logic [45:0] zres;
  assign zi1 = __in0[3:0];
  assign zi2 = __st0[29];
  assign zi3 = __in0[7:0];
  assign zi4 = __st0[29];
  assign zi5 = __st0[28:24];
  assign zi6 = __in0[0];
  assign zi7 = __st0[23:15];
  assign zi9 = __st0[23:15];
  assign zi10 = __st0[27:24];
  assign zi11 = {(__in0[9:8] == 2'h0) ? {zi2, 1'h1, zi1, 5'h0, zi1} : ((__in0[9:8] == 2'h1) ? {zi4, zi5, 1'h1, zi3} : ((zi6 == 1'h0) ? {6'h0, zi7} : ((__st0[28] == 1'h0) ? {6'h20, zi9} : {11'h400, zi10}))), __st0};
  assign zi12 = zi11[44:30];
  assign zi13 = zi11[29:0];
  assign slice_in = zi13 >> {8'h80{1'h0}};
  assign zi14 = {zi12, zi11[29:15], slice_in[14:0]};
  assign zi15 = zi14[14:0];
  assign zi16 = zi15;
  assign zi17 = zi16[14];
  assign zi18 = zi16[13:9];
  assign zi19 = zi16[8:0];
  assign zi20 = zi16[3:0];
  assign zi21 = zi16[7:0];
  assign zi22 = zi14[44:15];
  assign zi23 = {(zi17 == 1'h0) ? {7'h0, zi19} : ((zi16[8] == 1'h0) ? {2'h3, zi18, 5'h0, zi20} : {2'h1, zi18, 1'h1, zi21}), zi22};
  assign zi24 = zi23[45:30];
  assign zi25 = zi23[29:0];
  assign zres = {zi24, zi25};
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