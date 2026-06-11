module top_level (input logic [7:0] __in0,
  output logic [5:0] __out0);
  logic [7:0] zin;
  logic [5:0] zres;
  assign zin = __in0;
  assign zres = {&zin, ~(&zin), |zin, ~(|zin), ^zin, ~(^zin)};
  assign __out0 = zres;
endmodule