// Hand-written implementation of the parameterized extern in
// externGeneric.hs: adds the module parameter K to the input. Ports
// connect positionally from the generated instantiations.
module addk #(parameter K = 0)
             (input logic [7:0] p0, output logic [7:0] p1);
      assign p1 = p0 + 8'(K);
endmodule
