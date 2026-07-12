module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0,
  output logic [63:0] __out1);
  // combinational logic
  wire [63:0] slice_in = __in0 >> 128'h38;
  wire [63:0] slice_inR1 = __in0 >> 128'h28;
  wire [63:0] slice_inR2 = __in0 >> 128'h18;
  wire [63:0] slice_inR3 = __in0 >> 128'h8;
  wire [63:0] slice_inR4 = __in1 >> 128'h38;
  wire [63:0] slice_inR5 = __in1 >> 128'h28;
  wire [63:0] slice_inR6 = __in1 >> 128'h18;
  wire [63:0] slice_inR7 = __in1 >> 128'h8;
  wire [63:0] slice_inR8 = __in0 >> 128'h30;
  wire [63:0] slice_inR9 = __in0 >> 128'h20;
  wire [63:0] slice_inR10 = __in0 >> 128'h10;
  wire [63:0] slice_inR11 = __in0 >> {8'h80{1'h0}};
  wire [63:0] slice_inR12 = __in1 >> 128'h30;
  wire [63:0] slice_inR13 = __in1 >> 128'h20;
  wire [63:0] slice_inR14 = __in1 >> 128'h10;
  wire [63:0] slice_inR15 = __in1 >> {8'h80{1'h0}};
  wire [127:0] Zds1 = {slice_in[7:0], slice_inR1[7:0], slice_inR2[7:0], slice_inR3[7:0], slice_inR4[7:0],
    slice_inR5[7:0], slice_inR6[7:0], slice_inR7[7:0], slice_inR8[7:0], slice_inR9[7:0], slice_inR10[7:0],
    slice_inR11[7:0], slice_inR12[7:0], slice_inR13[7:0], slice_inR14[7:0], slice_inR15[7:0]};
  wire [63:0] lo = Zds1[127:64];
  wire [63:0] hi = Zds1[63:0];
  wire [63:0] slice_inR16 = hi >> 128'h38;
  wire [63:0] slice_inR17 = hi >> 128'h28;
  wire [63:0] slice_inR18 = hi >> 128'h18;
  wire [63:0] slice_inR19 = hi >> 128'h8;
  wire [63:0] slice_inR20 = lo >> 128'h38;
  wire [63:0] slice_inR21 = lo >> 128'h28;
  wire [63:0] slice_inR22 = lo >> 128'h18;
  wire [63:0] slice_inR23 = lo >> 128'h8;
  wire [63:0] slice_inR24 = hi >> 128'h30;
  wire [63:0] slice_inR25 = hi >> 128'h20;
  wire [63:0] slice_inR26 = hi >> 128'h10;
  wire [63:0] slice_inR27 = hi >> {8'h80{1'h0}};
  wire [63:0] slice_inR28 = lo >> 128'h30;
  wire [63:0] slice_inR29 = lo >> 128'h20;
  wire [63:0] slice_inR30 = lo >> 128'h10;
  wire [63:0] slice_inR31 = lo >> {8'h80{1'h0}};
  wire [127:0] Zds2 = {slice_inR16[7:0], slice_inR17[7:0], slice_inR18[7:0], slice_inR19[7:0], slice_inR20[7:0],
    slice_inR21[7:0], slice_inR22[7:0], slice_inR23[7:0], slice_inR24[7:0], slice_inR25[7:0], slice_inR26[7:0],
    slice_inR27[7:0], slice_inR28[7:0], slice_inR29[7:0], slice_inR30[7:0], slice_inR31[7:0]};
  wire [63:0] lo$ = Zds2[127:64];
  wire [63:0] hi$ = Zds2[63:0];
  wire [63:0] slice_inR32 = lo$ >> 128'h38;
  wire [63:0] slice_inR33 = hi$ >> 128'h38;
  wire [63:0] slice_inR34 = lo$ >> 128'h30;
  wire [63:0] slice_inR35 = hi$ >> 128'h30;
  wire [63:0] slice_inR36 = lo$ >> 128'h28;
  wire [63:0] slice_inR37 = hi$ >> 128'h28;
  wire [63:0] slice_inR38 = lo$ >> 128'h20;
  wire [63:0] slice_inR39 = hi$ >> 128'h20;
  wire [63:0] slice_inR40 = lo$ >> 128'h18;
  wire [63:0] slice_inR41 = hi$ >> 128'h18;
  wire [63:0] slice_inR42 = lo$ >> 128'h10;
  wire [63:0] slice_inR43 = hi$ >> 128'h10;
  wire [63:0] slice_inR44 = lo$ >> 128'h8;
  wire [63:0] slice_inR45 = hi$ >> 128'h8;
  wire [63:0] slice_inR46 = lo$ >> {8'h80{1'h0}};
  wire [63:0] slice_inR47 = hi$ >> {8'h80{1'h0}};
  wire [127:0] Zds3 = {slice_inR32[7:0], slice_inR33[7:0], slice_inR34[7:0], slice_inR35[7:0], slice_inR36[7:0],
    slice_inR37[7:0], slice_inR38[7:0], slice_inR39[7:0], slice_inR40[7:0], slice_inR41[7:0], slice_inR42[7:0],
    slice_inR43[7:0], slice_inR44[7:0], slice_inR45[7:0], slice_inR46[7:0], slice_inR47[7:0]};
  wire [63:0] v$ = Zds3[127:64];
  wire [63:0] w$ = Zds3[63:0];
  wire [127:0] Za = {v$, w$};
  // outputs
  assign __out0 = Za[127:64];
  assign __out1 = Za[63:0];
endmodule