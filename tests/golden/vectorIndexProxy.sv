module top_level (input logic [1023:0] __in0,
  input logic [31:0] __in1,
  output logic [63:0] __out0);
  // combinational logic
  wire [63:0] Za = {__in1[7:0] + __in0[999:992], __in1[7:0] + __in0[935:928], __in1[7:0] + __in0[871:864],
    __in1[7:0] + __in0[807:800], __in1[7:0] + __in0[743:736], __in1[7:0] + __in0[679:672], __in1[7:0] + __in0[615:608],
    __in1[7:0] + __in0[551:544]};
  // outputs
  assign __out0 = Za;
endmodule