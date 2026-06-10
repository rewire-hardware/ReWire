library ieee;
use ieee.std_logic_1164.all;
entity top_level is
  port (clk: in std_logic;
        rst: in std_logic;
        __in0: in std_logic_vector (0 to 0);
        __out0: out std_logic_vector (0 to 7));
end top_level;
architecture top_level_impl of top_level is
  signal start_state: std_logic_vector (0 to 17);
  signal loop_out: std_logic_vector (0 to 17);
  signal current_state: std_logic_vector (0 to 17);
  signal done_or_next_state: std_logic_vector (0 to 17);
  signal next_state: std_logic_vector (0 to 17);
  signal inp: std_logic_vector (0 to 0);
  component zdPurezidispatch
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 17));
  end component;
  component zdPurezistart
    port (res: out std_logic_vector (0 to 17));
  end component;
begin
  start_call: zdPurezistart port map (res => start_state);
  loop_call: zdPurezidispatch port map (arg0 => current_state(9 to 16),
                                        arg1 => inp,
                                        res => loop_out);
  with rst
    select next_state <= start_state when '1',
                         done_or_next_state when others;
  with current_state(0 to 0)
    select done_or_next_state <= loop_out when "1",
                                 current_state when others;
  process(clk)
  begin
    if clk'event and clk = '1' then
      current_state <= next_state;
      __out0 <= current_state(1 to 8);
    end if;
  end process;
  inp <= ("" & __in0);
end top_level_impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezidispatch is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 17));
end zdPurezidispatch;
architecture zdPurezidispatchImpl of zdPurezidispatch is
  signal slice0Res: std_logic_vector (0 to 8);
  signal zdLLziMainziincr161Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr16
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainziincr162Call: zdLLziMainziincr16 port map (arg0 => slice0Res(0 to 0),
                                                        arg1 => slice0Res(1 to 8),
                                                        res => zdLLziMainziincr161Res);
  res <= zdLLziMainziincr161Res;
end zdPurezidispatchImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezistart is
  port (res: out std_logic_vector (0 to 17));
end zdPurezistart;
architecture zdPurezistartImpl of zdPurezistart is
  signal lit131072x180Res: std_logic_vector (0 to 17);
begin
  lit131072x180Res <= "100000000000000000";
  res <= lit131072x180Res;
end zdPurezistartImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr17 is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr17;
architecture zdLLziMainziincr17Impl of zdLLziMainziincr17 is
  signal slice0Res: std_logic_vector (0 to 17);
  signal lit256x101Res: std_logic_vector (0 to 9);
begin
  lit256x101Res <= "0100000000";
  slice0Res <= (lit256x101Res & arg0);
  res <= slice0Res;
end zdLLziMainziincr17Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr16 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr16;
architecture zdLLziMainziincr16Impl of zdLLziMainziincr16 is
  signal Mainziincr0Res: std_logic_vector (0 to 17);
  component Mainziincr
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  Mainziincr1Call: Mainziincr port map (arg0 => arg1(0 to 7),
                                        res => Mainziincr0Res);
  res <= Mainziincr0Res;
end zdLLziMainziincr16Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr13 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr13;
architecture zdLLziMainziincr13Impl of zdLLziMainziincr13 is
  signal slice0Res: std_logic_vector (0 to 17);
  signal lit0x21Res: std_logic_vector (0 to 1);
begin
  lit0x21Res <= "00";
  slice0Res <= ((lit0x21Res & arg0) & arg1);
  res <= slice0Res;
end zdLLziMainziincr13Impl;

library ieee;
use ieee.std_logic_1164.all;
entity Mainziincr is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end Mainziincr;
architecture MainziincrImpl of Mainziincr is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainziincr41Res: std_logic_vector (0 to 17);
  signal zdLLziMainziincr63Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr6
    port (arg0: in std_logic_vector (0 to 17);
          res: out std_logic_vector (0 to 17));
  end component;
  component zdLLziMainziincr4
    port (arg0: in std_logic_vector (0 to 15);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg0 & arg0);
  zdLLziMainziincr42Call: zdLLziMainziincr4 port map (arg0 => slice0Res(0 to 15),
                                                      res => zdLLziMainziincr41Res);
  zdLLziMainziincr64Call: zdLLziMainziincr6 port map (arg0 => zdLLziMainziincr41Res(0 to 17),
                                                      res => zdLLziMainziincr63Res);
  res <= zdLLziMainziincr63Res;
end MainziincrImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr12 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr12;
architecture zdLLziMainziincr12Impl of zdLLziMainziincr12 is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainziincr101Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr10
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziMainziincr102Call: zdLLziMainziincr10 port map (arg0 => slice0Res(0 to 7),
                                                        arg1 => slice0Res(8 to 15),
                                                        res => zdLLziMainziincr101Res);
  res <= zdLLziMainziincr101Res;
end zdLLziMainziincr12Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr10 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr10;
architecture zdLLziMainziincr10Impl of zdLLziMainziincr10 is
  signal slice0Res: std_logic_vector (0 to 17);
  signal lit2x21Res: std_logic_vector (0 to 1);
begin
  lit2x21Res <= "10";
  slice0Res <= ((lit2x21Res & arg0) & arg1);
  res <= slice0Res;
end zdLLziMainziincr10Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr9 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 17);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr9;
architecture zdLLziMainziincr9Impl of zdLLziMainziincr9 is
  signal slice0Res: std_logic_vector (0 to 25);
  signal zdLLziMainziincr121Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr12
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziMainziincr122Call: zdLLziMainziincr12 port map (arg0 => slice0Res(0 to 7),
                                                        arg1 => slice0Res(18 to 25),
                                                        res => zdLLziMainziincr121Res);
  res <= zdLLziMainziincr121Res;
end zdLLziMainziincr9Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr8 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr8;
architecture zdLLziMainziincr8Impl of zdLLziMainziincr8 is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainziincr51Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr5
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziMainziincr52Call: zdLLziMainziincr5 port map (arg0 => slice0Res(0 to 7),
                                                      arg1 => slice0Res(8 to 15),
                                                      res => zdLLziMainziincr51Res);
  res <= zdLLziMainziincr51Res;
end zdLLziMainziincr8Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr6 is
  port (arg0: in std_logic_vector (0 to 17);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr6;
architecture zdLLziMainziincr6Impl of zdLLziMainziincr6 is
  signal zdLLziMainziincr80Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr8
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  zdLLziMainziincr81Call: zdLLziMainziincr8 port map (arg0 => arg0(2 to 9),
                                                      arg1 => arg0(10 to 17),
                                                      res => zdLLziMainziincr80Res);
  res <= zdLLziMainziincr80Res;
end zdLLziMainziincr6Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr5 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr5;
architecture zdLLziMainziincr5Impl of zdLLziMainziincr5 is
  signal slice0Res: std_logic_vector (0 to 25);
  signal zdLLziMainziincr171Res: std_logic_vector (0 to 17);
  signal zdLLziMainziincr93Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr9
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 17);
          res: out std_logic_vector (0 to 17));
  end component;
  component zdLLziMainziincr17
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  zdLLziMainziincr172Call: zdLLziMainziincr17 port map (arg0 => arg0(0 to 7),
                                                        res => zdLLziMainziincr171Res);
  slice0Res <= (arg0 & zdLLziMainziincr171Res);
  zdLLziMainziincr94Call: zdLLziMainziincr9 port map (arg0 => slice0Res(0 to 7),
                                                      arg1 => slice0Res(8 to 25),
                                                      res => zdLLziMainziincr93Res);
  res <= zdLLziMainziincr93Res;
end zdLLziMainziincr5Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr4 is
  port (arg0: in std_logic_vector (0 to 15);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr4;
architecture zdLLziMainziincr4Impl of zdLLziMainziincr4 is
  signal zdLLziMainziincr130Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr13
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  zdLLziMainziincr131Call: zdLLziMainziincr13 port map (arg0 => arg0(0 to 7),
                                                        arg1 => arg0(8 to 15),
                                                        res => zdLLziMainziincr130Res);
  res <= zdLLziMainziincr130Res;
end zdLLziMainziincr4Impl;