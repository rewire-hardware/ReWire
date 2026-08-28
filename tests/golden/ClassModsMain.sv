module top_level (input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] ClassMods_Impl_$cflick_out;
  logic [7:0] ClassMods_Impl_$cinv_out;
  logic [7:0] ClassMods_Impl_$cinv_outR1;
  // combinational logic
  wire [0:0] Zt0 = __in0[0];
  ClassMods_Impl_$cflick  Zcflick_i (__in0, ClassMods_Impl_$cflick_out);
  ClassMods_Impl_$cinv  Zcinv_i (__in0 | ClassMods_Impl_$cflick_out, ClassMods_Impl_$cinv_out);
  ClassMods_Impl_$cinv  Zcinv_iR1 (__in0, ClassMods_Impl_$cinv_outR1);
  wire [7:0] Za = (~Zt0) ? ClassMods_Impl_$cinv_out : (ClassMods_Impl_$cinv_outR1 | ClassMods_Impl_$cflick_out);
  // outputs
  assign __out0 = Za;
endmodule

// ClassMods.Impl.$cinv
module ClassMods_Impl_$cinv (input logic [7:0] w,
  output logic [7:0] res);
  assign res = w ^ 8'hff;
endmodule

// ClassMods.Impl.$cflick
module ClassMods_Impl_$cflick (input logic [7:0] w,
  output logic [7:0] res);
  assign res = w ^ 8'hf;
endmodule