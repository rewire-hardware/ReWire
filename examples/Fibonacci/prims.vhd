-- prims.vhd for Fibonacci.rw

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package prims is
  pure function plusW8 (x : std_logic_vector; y : std_logic_vector) return std_logic_vector;
end prims;

package body prims is
  pure function plusW8 (x : std_logic_vector; y : std_logic_vector) return std_logic_vector is
  begin
	return (std_logic_vector(unsigned(x)+unsigned(y)));
  end plusW8;
end prims;