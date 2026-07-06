module top_level (input logic [7:0] __in0,
  output logic [5:0] __out0);
  logic [5:0] zi0;
  logic [5:0] zres;
  assign zi0 = {&__in0, ~(&__in0), |__in0, ~(|__in0), ^__in0, ~(^__in0)};
  assign zres = zi0;
  assign __out0 = zres;
endmodule