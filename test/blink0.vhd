library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity rewire is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to -1);
         output : out std_logic_vector (0 to 0));
end rewire;

architecture behavioral of rewire is
  type control_state is (STATE0,STATE6,STATE10);
  
  
begin
  process (clk)
    variable goto_L11 : boolean := false;
    variable goto_L10 : boolean := false;
    variable goto_L6 : boolean := false;
    variable goto_L0 : boolean := false;
    variable goto_L1 : boolean := false;
    variable goto_L12 : boolean := false;
    variable r7 : std_logic_vector(0 to 0) := (others => '0');
    variable r3 : std_logic_vector(0 to 0) := (others => '0');
    variable state : control_state := STATE0;
  begin
    if clk'event and clk='1' then
      goto_L11 := false;
      goto_L10 := false;
      goto_L6 := false;
      goto_L0 := false;
      goto_L1 := false;
      goto_L12 := false;
      null; -- label L11
      -- ENTER
      goto_L0 := (state = STATE0);
      if (NOT goto_L0) then
        goto_L6 := (state = STATE6);
        if (NOT goto_L6) then
          goto_L10 := (state = STATE10);
          null; -- label L10
          null;
          goto_L1 := true;
        end if;
        goto_L1 := goto_L1;
        if (NOT goto_L1) then
          null; -- label L6
          null;
          r7 := "1";
          output <= r7;
          state := STATE10;
          goto_L12 := true;
        end if;
        goto_L12 := goto_L12;
      end if;
      goto_L12 := goto_L12;
      if (NOT goto_L12) then
        goto_L1 := goto_L1;
        if (NOT goto_L1) then
          null; -- label L0
          -- START
          goto_L1 := true;
        end if;
        goto_L1 := goto_L1;
        null; -- label L1
        -- blink0 in
        r3 := "0";
        output <= r3;
        state := STATE6;
        goto_L12 := true;
      end if;
      goto_L12 := goto_L12;
      null; -- label L12
      -- EXIT
    end if;
  end process;
end behavioral;
