module mymod (input logic [0:0] clk,
  input logic [0:0] rst,

  input logic [15:0] x,
  output logic [7:0] out);

  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      out <= 8'h0;
    end else begin
      out <= x[7:0] + 8'h1;
    end
  end
endmodule
