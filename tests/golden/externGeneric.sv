module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] extres;
  logic [7:0] extresR1;
  // combinational logic
  addk #(.K(32'h3)) addk_i (__in0, extres[7:0]);
  addk #(.K(32'h5)) addk_iR1 (__in0, extresR1[7:0]);
  wire [7:0] a = extres + extresR1;
  // outputs
  assign __out0 = a;
endmodule