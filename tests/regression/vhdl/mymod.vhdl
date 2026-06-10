-- VHDL equivalent of the extern module in ../verilog/mymod.sv, for the
-- rwc-test cosimulation check.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mymod is
  port (clk : in std_logic_vector (0 downto 0);
        rst : in std_logic_vector (0 downto 0);
        x : in std_logic_vector (15 downto 0);
        \out\ : out std_logic_vector (7 downto 0));
end entity;
architecture rtl of mymod is
begin
  process (clk, rst)
  begin
    if rst = "1" then
      \out\ <= x"00";
    elsif rising_edge(clk(0)) then
      \out\ <= std_logic_vector(unsigned(x(7 downto 0)) + 1);
    end if;
  end process;
end architecture;
