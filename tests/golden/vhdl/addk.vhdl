-- Hand-written implementation of the parameterized extern in
-- externGeneric.hs: adds the generic K to the input. Port names match
-- the generated component declaration (p<i> for anonymous ports).
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity addk is
      generic (\K\ : integer := 0);
      port (p0 : in std_logic_vector(7 downto 0);
            p1 : out std_logic_vector(7 downto 0));
end addk;

architecture behavioral of addk is
begin
      p1 <= std_logic_vector(unsigned(p0) + to_unsigned(\K\, 8));
end behavioral;
