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
  signal zdLLziMainziincr31Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr3
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainziincr32Call: zdLLziMainziincr3 port map (arg0 => slice0Res(0 to 0),
                                                      arg1 => slice0Res(1 to 8),
                                                      res => zdLLziMainziincr31Res);
  res <= zdLLziMainziincr31Res;
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
entity zdLLziMainziincr12 is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr12;
architecture zdLLziMainziincr12Impl of zdLLziMainziincr12 is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainziincr101Res: std_logic_vector (0 to 17);
  signal zdLLziMainziincr13Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr1
    port (arg0: in std_logic_vector (0 to 17);
          res: out std_logic_vector (0 to 17));
  end component;
  component zdLLziMainziincr10
    port (arg0: in std_logic_vector (0 to 15);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg0 & arg0);
  zdLLziMainziincr102Call: zdLLziMainziincr10 port map (arg0 => slice0Res(0 to 15),
                                                        res => zdLLziMainziincr101Res);
  zdLLziMainziincr14Call: zdLLziMainziincr1 port map (arg0 => zdLLziMainziincr101Res(0 to 17),
                                                      res => zdLLziMainziincr13Res);
  res <= zdLLziMainziincr13Res;
end zdLLziMainziincr12Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr11 is
  port (arg0: in std_logic_vector (0 to 17);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr11;
architecture zdLLziMainziincr11Impl of zdLLziMainziincr11 is
  signal zdLLziMainziincr20Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr2
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  zdLLziMainziincr21Call: zdLLziMainziincr2 port map (arg0 => arg0(10 to 17),
                                                      res => zdLLziMainziincr20Res);
  res <= zdLLziMainziincr20Res;
end zdLLziMainziincr11Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr10 is
  port (arg0: in std_logic_vector (0 to 15);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr10;
architecture zdLLziMainziincr10Impl of zdLLziMainziincr10 is
  signal zdLLziMainziincr50Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr5
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  zdLLziMainziincr51Call: zdLLziMainziincr5 port map (arg0 => arg0(0 to 7),
                                                      arg1 => arg0(8 to 15),
                                                      res => zdLLziMainziincr50Res);
  res <= zdLLziMainziincr50Res;
end zdLLziMainziincr10Impl;

library ieee;
use ieee.std_logic_1164.all;
entity Mainziincr is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end Mainziincr;
architecture MainziincrImpl of Mainziincr is
  signal Mainzigrunt0Res: std_logic_vector (0 to 7);
  signal zdLLziMainziincr92Res: std_logic_vector (0 to 17);
  signal zdLLziMainziincr114Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr11
    port (arg0: in std_logic_vector (0 to 17);
          res: out std_logic_vector (0 to 17));
  end component;
  component zdLLziMainziincr9
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
  component Mainzigrunt
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 7));
  end component;
begin
  Mainzigrunt1Call: Mainzigrunt port map (arg0 => arg0(0 to 7),
                                          res => Mainzigrunt0Res);
  zdLLziMainziincr93Call: zdLLziMainziincr9 port map (arg0 => Mainzigrunt0Res(0 to 7),
                                                      res => zdLLziMainziincr92Res);
  zdLLziMainziincr115Call: zdLLziMainziincr11 port map (arg0 => zdLLziMainziincr92Res(0 to 17),
                                                        res => zdLLziMainziincr114Res);
  res <= zdLLziMainziincr114Res;
end MainziincrImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr9 is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr9;
architecture zdLLziMainziincr9Impl of zdLLziMainziincr9 is
  signal slice0Res: std_logic_vector (0 to 17);
  signal lit256x101Res: std_logic_vector (0 to 9);
begin
  lit256x101Res <= "0100000000";
  slice0Res <= (lit256x101Res & arg0);
  res <= slice0Res;
end zdLLziMainziincr9Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigrunt1 is
  port (arg0: in std_logic_vector (0 to 15);
        res: out std_logic_vector (0 to 7));
end zdLLziMainzigrunt1;
architecture zdLLziMainzigrunt1Impl of zdLLziMainzigrunt1 is
  signal zdLLziMainzigrunt0Res: std_logic_vector (0 to 7);
  component zdLLziMainzigrunt
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 7));
  end component;
begin
  zdLLziMainzigrunt1Call: zdLLziMainzigrunt port map (arg0 => arg0(0 to 7),
                                                      arg1 => arg0(8 to 15),
                                                      res => zdLLziMainzigrunt0Res);
  res <= zdLLziMainzigrunt0Res;
end zdLLziMainzigrunt1Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigrunt is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 7));
end zdLLziMainzigrunt;
architecture zdLLziMainzigruntImpl of zdLLziMainzigrunt is
begin
  res <= arg0;
end zdLLziMainzigruntImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr8 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr8;
architecture zdLLziMainziincr8Impl of zdLLziMainziincr8 is
  signal slice0Res: std_logic_vector (0 to 17);
  signal lit2x21Res: std_logic_vector (0 to 1);
begin
  lit2x21Res <= "10";
  slice0Res <= ((lit2x21Res & arg0) & arg1);
  res <= slice0Res;
end zdLLziMainziincr8Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr5 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr5;
architecture zdLLziMainziincr5Impl of zdLLziMainziincr5 is
  signal slice0Res: std_logic_vector (0 to 17);
  signal lit0x21Res: std_logic_vector (0 to 1);
begin
  lit0x21Res <= "00";
  slice0Res <= ((lit0x21Res & arg0) & arg1);
  res <= slice0Res;
end zdLLziMainziincr5Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr4 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr4;
architecture zdLLziMainziincr4Impl of zdLLziMainziincr4 is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainziincr81Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr8
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziMainziincr82Call: zdLLziMainziincr8 port map (arg0 => slice0Res(0 to 7),
                                                      arg1 => slice0Res(8 to 15),
                                                      res => zdLLziMainziincr81Res);
  res <= zdLLziMainziincr81Res;
end zdLLziMainziincr4Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr3 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr3;
architecture zdLLziMainziincr3Impl of zdLLziMainziincr3 is
  signal Mainziincr0Res: std_logic_vector (0 to 17);
  component Mainziincr
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  Mainziincr1Call: Mainziincr port map (arg0 => arg1(0 to 7),
                                        res => Mainziincr0Res);
  res <= Mainziincr0Res;
end zdLLziMainziincr3Impl;

library ieee;
use ieee.std_logic_1164.all;
entity Mainzigrunt is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 7));
end Mainzigrunt;
architecture MainzigruntImpl of Mainzigrunt is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainzigrunt11Res: std_logic_vector (0 to 7);
  component zdLLziMainzigrunt1
    port (arg0: in std_logic_vector (0 to 15);
          res: out std_logic_vector (0 to 7));
  end component;
begin
  slice0Res <= (arg0 & arg0);
  zdLLziMainzigrunt12Call: zdLLziMainzigrunt1 port map (arg0 => slice0Res(0 to 15),
                                                        res => zdLLziMainzigrunt11Res);
  res <= zdLLziMainzigrunt11Res;
end MainzigruntImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr2 is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr2;
architecture zdLLziMainziincr2Impl of zdLLziMainziincr2 is
  signal zdLLziMainziincr120Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr12
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  zdLLziMainziincr121Call: zdLLziMainziincr12 port map (arg0 => arg0(0 to 7),
                                                        res => zdLLziMainziincr120Res);
  res <= zdLLziMainziincr120Res;
end zdLLziMainziincr2Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziincr1 is
  port (arg0: in std_logic_vector (0 to 17);
        res: out std_logic_vector (0 to 17));
end zdLLziMainziincr1;
architecture zdLLziMainziincr1Impl of zdLLziMainziincr1 is
  signal zdLLziMainziincr40Res: std_logic_vector (0 to 17);
  component zdLLziMainziincr4
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 17));
  end component;
begin
  zdLLziMainziincr41Call: zdLLziMainziincr4 port map (arg0 => arg0(2 to 9),
                                                      arg1 => arg0(10 to 17),
                                                      res => zdLLziMainziincr40Res);
  res <= zdLLziMainziincr40Res;
end zdLLziMainziincr1Impl;