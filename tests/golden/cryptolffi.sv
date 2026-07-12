module top_level (input logic [7:0] __in0,
  input logic [7:0] __in1,
  output logic [7:0] __out0);
  // combinational logic
  wire [3:0] hi$4700 = __in1[7:4];
  wire [3:0] lo$4698 = __in1[3:0];
  wire [7:0] x$1 = {lo$4698, hi$4700};
  wire [7:0] Za = (x$1 > __in0) ? __in0 : x$1;
  // outputs
  assign __out0 = Za;
endmodule