library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity rwcomp0 is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to -1);
         output : out std_logic_vector (0 to 0));
end rwcomp0;

architecture behavioral of rwcomp0 is
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
    variable statevar0 : std_logic_vector(0 to -1) := (others => '0');
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
library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity rwcomp1 is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to -1);
         output : out std_logic_vector (0 to 0));
end rwcomp1;

architecture behavioral of rwcomp1 is
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
    variable statevar0 : std_logic_vector(0 to -1) := (others => '0');
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
          r7 := "0";
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
        -- blink1 in
        r3 := "1";
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
library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity rwcomp2 is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to -1);
         output : out std_logic_vector (0 to 0));
end rwcomp2;

architecture behavioral of rwcomp2 is
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
    variable statevar0 : std_logic_vector(0 to -1) := (others => '0');
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
          r7 := "0";
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
        -- blink1 in
        r3 := "1";
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
library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity rwcomp3 is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to -1);
         output : out std_logic_vector (0 to 0));
end rwcomp3;

architecture behavioral of rwcomp3 is
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
    variable statevar0 : std_logic_vector(0 to -1) := (others => '0');
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
library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity rwcomp4 is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to -1);
         output : out std_logic_vector (0 to 3));
end rwcomp4;
architecture behavioral of rwcomp4 is
  signal rwcomp0input  : std_logic_vector(0 to -1);
  signal rwcomp0output : std_logic_vector(0 to 0);
  signal rwcomp1input  : std_logic_vector(0 to -1);
  signal rwcomp1output : std_logic_vector(0 to 0);
  signal rwcomp2input  : std_logic_vector(0 to -1);
  signal rwcomp2output : std_logic_vector(0 to 0);
  signal rwcomp3input  : std_logic_vector(0 to -1);
  signal rwcomp3output : std_logic_vector(0 to 0);
begin
  rwcomp0input <= input(0 TO -1);
  rwcomp1input <= input(0 TO -1);
  rwcomp2input <= input(0 TO -1);
  rwcomp3input <= input(0 TO -1);
  rwcomp0dev : entity work.rwcomp0(behavioral)
    port map (clk,rwcomp0input,rwcomp0output);
  
  rwcomp1dev : entity work.rwcomp1(behavioral)
    port map (clk,rwcomp1input,rwcomp1output);
  
  rwcomp2dev : entity work.rwcomp2(behavioral)
    port map (clk,rwcomp2input,rwcomp2output);
  
  rwcomp3dev : entity work.rwcomp3(behavioral)
    port map (clk,rwcomp3input,rwcomp3output);
  
  output <= rwcomp0output & rwcomp1output & rwcomp2output & rwcomp3output;
end behavioral;
library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity main is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to -1);
         output : out std_logic_vector (0 to 3));
end main;
architecture structural of main is
begin
  dev : entity work.rwcomp4(behavioral)
    port map (clk,input,output);
    

end structural;
