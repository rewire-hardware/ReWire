module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  // state registers
  // __resumption_tag: 1 bits, init 0x1
  //   states: 0=$ds 1=i
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  // combinational logic
  wire [1:0] Zres = (~__resumption_tag) ? 2'h3 : {__in0, 1'h0};
  assign __resumption_tag_next = Zres[0];
  // outputs
  assign __out0 = Zres[1];
  // state register update
  initial __resumption_tag = 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule