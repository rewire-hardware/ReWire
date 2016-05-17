--- plusW8 :: W8 -> W8 -> W8
--- {-# INLINE plusW8 #-}
--- plusW8 = nativeVhdl "plusW8" plusW8
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity plusW8 is
  port (arg1 : in std_logic_vector(0 to 7);
        arg2 : in std_logic_vector(0 to 7);
        res : out std_logic_vector(0 to 7));
end plusW8;

architecture impl of plusW8 is
begin
  res <= std_logic_vector(unsigned(arg1)+unsigned(arg2));
end impl;

--- zeroW8 :: W8
--- zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity zeroW8 is
  port (res : out std_logic_vector(0 to 7));
end zeroW8;

architecture impl of zeroW8 is
begin
  res <= "00000000";
end impl;

--- oneW8 :: W8
--- oneW8 = W8 Zero Zero Zero Zero Zero Zero Zero One
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity oneW8 is
  port (res : out std_logic_vector(0 to 7));
end oneW8;

architecture impl of oneW8 is
begin
  res <= "00000001";
end impl;

--- start_pure :: Either () (W8,R)
--- start_pure = begin_pure
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity start_pure is
  port (res : out std_logic_vector(0 to 24));
end start_pure;

architecture impl of start_pure is
  component begin_pure
    port (res : out std_logic_vector(0 to 24));
  end component;
begin
  begin_pure_call : begin_pure port map(res => res);
end impl;

--- begin_pure :: Either () (W8,R)
--- begin_pure = loop_pure zeroW8 oneW8
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity begin_pure is
  port (res : out std_logic_vector(0 to 24));
end begin_pure;

architecture impl of begin_pure is
  signal loop_pure_call_arg_1 : std_logic_vector(0 to 7);
  signal loop_pure_call_arg_2 : std_logic_vector(0 to 7);
  component zeroW8
    port (res : out std_logic_vector(0 to 7));
  end component;
  component oneW8
    port (res : out std_logic_vector(0 to 7));
  end component;
  component loop_pure
    port (arg1 : in std_logic_vector(0 to 7);
		    arg2 : in std_logic_vector(0 to 7);
	  	    res  : out std_logic_vector(0 to 24));
  end component;
begin
  zeroW8_call : zeroW8 port map (res => loop_pure_call_arg_1);
  oneW8_call  : oneW8  port map (res => loop_pure_call_arg_2);
  loop_pure_call : loop_pure port map (arg1 => loop_pure_call_arg_1,
													arg2 => loop_pure_call_arg_2,
													res  => res);
end impl;

--- loop_pure :: W8 -> W8 -> Either () (W8,R)
--- loop_pure n m = (Right (n,R_k n m))
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity loop_pure is
  port (arg1 : in std_logic_vector(0 to 7);
		  arg2 : in std_logic_vector(0 to 7);
		  res  : out std_logic_vector(0 to 24));
end loop_pure;

architecture impl of loop_pure is
begin
  res <= "1" & arg1 & arg1 & arg2;
end impl;

--- k_pure :: W8 -> W8 -> Bit -> Either () (W8,R)
--- k_pure n m b = case b of
---                  One  -> loop_pure n m
---                  Zero -> loop_pure m (plusW8 n m)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity k_pure is
  port (arg1 : in std_logic_vector(0 to 7);
	     arg2 : in std_logic_vector(0 to 7);
		  arg3 : in std_logic_vector(0 to 0);
		  res  : out std_logic_vector(0 to 24));
end k_pure;

architecture impl of k_pure is
  signal loop_pure_call_1_res : std_logic_vector(0 to 24);
  signal loop_pure_call_2_res : std_logic_vector(0 to 24);
  signal plusW8_call_res : std_logic_vector(0 to 7);
  component loop_pure
    port (arg1 : in std_logic_vector(0 to 7);
	 	    arg2 : in std_logic_vector(0 to 7);
		    res  : out std_logic_vector(0 to 24));
  end component;
  component plusW8
    port (arg1 : in std_logic_vector(0 to 7);
          arg2 : in std_logic_vector(0 to 7);
          res : out std_logic_vector(0 to 7));
  end component;
begin
  with arg3 select res <=
    loop_pure_call_1_res when "1",
	 loop_pure_call_2_res when others;
	 
  loop_pure_call_1 : loop_pure port map (arg1 => arg1,
													  arg2 => arg2,
													  res  => loop_pure_call_1_res);

  loop_pure_call_2 : loop_pure port map (arg1 => arg2,
													  arg2 => plusW8_call_res,
													  res  => loop_pure_call_2_res);

  plusW8_call : plusW8 port map (arg1 => arg1,
											arg2 => arg2,
										   res  => plusW8_call_res);											
end impl;

---
--- And now, the boilerplate arising from the following:
---    "start_pure" has type Either () (W8,R)
---    "R" has one constructor, R_k W8 W8
---
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity top_level is
  port (clk  : in std_logic;
        rst  : in std_logic;
        inp  : in std_logic_vector(0 to 0);
        outp : out std_logic_vector(0 to 7));
end top_level;

architecture impl of top_level is
  component start_pure
    port (res : out std_logic_vector(0 to 24));
  end component;
  component k_pure
    port (arg1 : in std_logic_vector(0 to 7);
	       arg2 : in std_logic_vector(0 to 7);
		    arg3 : in std_logic_vector(0 to 0);
		    res  : out std_logic_vector(0 to 24));
  end component;
  signal circuit_state : std_logic_vector(0 to 15);
  signal start_pure_call_res : std_logic_vector(0 to 24);
  signal k_pure_call_res : std_logic_vector(0 to 24);
  signal case_res : std_logic_vector(0 to 24);
begin
  start_pure_call : start_pure port map (res => start_pure_call_res);
  k_pure_call : k_pure port map (arg1 => circuit_state(0 to 7),
										   arg2 => circuit_state(8 to 15),
											arg3 => inp,
											res  => k_pure_call_res);
  with rst select case_res <= start_pure_call_res when '1',
                              k_pure_call_res when others;

  outp <= case_res (1 to 8);

  process(clk)
  begin
    if clk'event and clk='1' then
	   circuit_state <= case_res(9 to 24);
	 end if;
  end process;
end impl;
