module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] main_$ds_out;
  logic [2:0] main_$ds_outR1;
  // state registers
  // __resumption_tag: 1 bits, init 0x1
  //   states: 0=$ds2 1=i
  // __st0: 1 bits, init 0x0
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  // combinational logic
  wire [0:0] Za = ~__st0;
  main_$ds  Zds_i (Za, main_$ds_out);
  main_$ds  Zds_iR1 (__in0, main_$ds_outR1);
  wire [2:0] Zres = (~__resumption_tag) ? {2'h3, __st0} :
    ((~__in0) ? main_$ds_out : main_$ds_outR1);
  assign __resumption_tag_next = Zres[1];
  assign __st0_next = Zres[0];
  // outputs
  assign __out0 = Zres[2];
  // state register update
  initial __resumption_tag = 1'h1;
  initial __st0 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
      __st0 <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
      __st0 <= __st0_next;
    end
  end
endmodule

// main.$ds
// block '$L.$ds' of process main
module main_$ds (input logic [0:0] s0,
  output logic [2:0] res);
  assign res = {s0, 1'h0, s0};
endmodule