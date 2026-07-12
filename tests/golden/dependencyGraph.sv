module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [9:0] __in0,
  output logic [15:0] __out0);
  // state registers
  // __st0: 30 bits, init 0x0
  logic [29:0] __st0;
  logic [29:0] __st0_next;
  // combinational logic
  wire [3:0] w4 = __in0[3:0];
  wire [0:0] b = __st0[29];
  wire [7:0] w8 = __in0[7:0];
  wire [4:0] w4R1 = __st0[28:24];
  wire [0:0] Zds2 = __in0[0];
  wire [8:0] w8R1 = __st0[23:15];
  wire [3:0] w4R2 = __st0[27:24];
  wire [44:0] Zds = {(__in0[9:8] == 2'h0) ? {b, 1'h1, w4, 5'h0, w4} :
    ((__in0[9:8] == 2'h1) ? {b, w4R1, 1'h1, w8} :
      ((~Zds2) ? {6'h0, w8R1} :
        ((~__st0[28]) ? {6'h20, w8R1} : {11'h400, w4R2}))), __st0};
  wire [14:0] s = Zds[44:30];
  wire [29:0] ps = Zds[29:0];
  wire [29:0] slice_in = ps >> {8'h80{1'h0}};
  wire [44:0] ZdsR1 = {s, Zds[29:15], slice_in[14:0]};
  wire [14:0] out = ZdsR1[14:0];
  wire [0:0] Zds1 = out[14];
  wire [4:0] mw4 = out[13:9];
  wire [8:0] Zds2R1 = out[8:0];
  wire [3:0] w4R3 = out[3:0];
  wire [7:0] w8R2 = out[7:0];
  wire [29:0] s$ = ZdsR1[44:15];
  wire [45:0] Za = {(~Zds1) ? {7'h0, Zds2R1} :
    ((~out[8]) ? {2'h3, mw4, 5'h0, w4R3} : {2'h1, mw4, 1'h1, w8R2}), s$};
  wire [15:0] o = Za[45:30];
  wire [29:0] sR1 = Za[29:0];
  wire [45:0] Zres = {o, sR1};
  assign __st0_next = Zres[29:0];
  // outputs
  assign __out0 = Zres[45:30];
  // state register update
  initial __st0 = 30'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 30'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule