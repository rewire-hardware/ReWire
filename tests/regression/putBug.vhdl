library ieee;
use ieee.std_logic_1164.all;
entity top_level is
  port (clk: in std_logic;
        rst: in std_logic;
        __in0: in std_logic_vector (0 to 0);
        __out0: out std_logic_vector (0 to 0));
end top_level;
architecture top_level_impl of top_level is
  signal start_state: std_logic_vector (0 to 3);
  signal loop_out: std_logic_vector (0 to 3);
  signal current_state: std_logic_vector (0 to 3);
  signal done_or_next_state: std_logic_vector (0 to 3);
  signal next_state: std_logic_vector (0 to 3);
  signal inp: std_logic_vector (0 to 0);
  component zdPurezidispatch
    port (arg0: in std_logic_vector (0 to 1);
          arg1: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 3));
  end component;
  component zdPurezistart
    port (res: out std_logic_vector (0 to 3));
  end component;
begin
  start_call: zdPurezistart port map (res => start_state);
  loop_call: zdPurezidispatch port map (arg0 => current_state(2 to 3),
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
      __out0 <= current_state(1 to 1);
    end if;
  end process;
  inp <= ("" & __in0);
end top_level_impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezidispatch is
  port (arg0: in std_logic_vector (0 to 1);
        arg1: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 3));
end zdPurezidispatch;
architecture zdPurezidispatchImpl of zdPurezidispatch is
  signal slice0Res: std_logic_vector (0 to 2);
  signal zdLLziPurezidispatch1Res: std_logic_vector (0 to 3);
  component zdLLziPurezidispatch
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 0);
          arg2: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 3));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziPurezidispatch2Call: zdLLziPurezidispatch port map (arg0 => slice0Res(0 to 0),
                                                            arg1 => slice0Res(1 to 1),
                                                            arg2 => slice0Res(2 to 2),
                                                            res => zdLLziPurezidispatch1Res);
  res <= zdLLziPurezidispatch1Res;
end zdPurezidispatchImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezistart is
  port (res: out std_logic_vector (0 to 3));
end zdPurezistart;
architecture zdPurezistartImpl of zdPurezistart is
  signal lit8x40Res: std_logic_vector (0 to 3);
begin
  lit8x40Res <= "1000";
  res <= lit8x40Res;
end zdPurezistartImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziconvtest15 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 3));
end zdLLziMainziconvtest15;
architecture zdLLziMainziconvtest15Impl of zdLLziMainziconvtest15 is
  signal slice0Res: std_logic_vector (0 to 1);
  signal zdLLziMainziconvtest141Res: std_logic_vector (0 to 3);
  component zdLLziMainziconvtest14
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 3));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziMainziconvtest142Call: zdLLziMainziconvtest14 port map (arg0 => slice0Res(0 to 0),
                                                                arg1 => slice0Res(1 to 1),
                                                                res => zdLLziMainziconvtest141Res);
  res <= zdLLziMainziconvtest141Res;
end zdLLziMainziconvtest15Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziconvtest14 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 3));
end zdLLziMainziconvtest14;
architecture zdLLziMainziconvtest14Impl of zdLLziMainziconvtest14 is
  signal slice0Res: std_logic_vector (0 to 3);
  signal lit1x11Res: std_logic_vector (0 to 0);
begin
  lit1x11Res <= "1";
  slice0Res <= (((lit1x11Res & arg0) & arg0) & arg1);
  res <= slice0Res;
end zdLLziMainziconvtest14Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziconvtest10 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 0);
        arg2: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 3));
end zdLLziMainziconvtest10;
architecture zdLLziMainziconvtest10Impl of zdLLziMainziconvtest10 is
  signal slice0Res: std_logic_vector (0 to 4);
  signal zdLLziMainziconvtest41Res: std_logic_vector (0 to 3);
  signal zdLLziMainziconvtest83Res: std_logic_vector (0 to 3);
  component zdLLziMainziconvtest8
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 3);
          res: out std_logic_vector (0 to 3));
  end component;
  component zdLLziMainziconvtest4
    port (arg0: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 3));
  end component;
begin
  zdLLziMainziconvtest42Call: zdLLziMainziconvtest4 port map (arg0 => arg0(0 to 0),
                                                              res => zdLLziMainziconvtest41Res);
  slice0Res <= (arg0 & zdLLziMainziconvtest41Res);
  zdLLziMainziconvtest84Call: zdLLziMainziconvtest8 port map (arg0 => slice0Res(0 to 0),
                                                              arg1 => slice0Res(1 to 4),
                                                              res => zdLLziMainziconvtest83Res);
  res <= zdLLziMainziconvtest83Res;
end zdLLziMainziconvtest10Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziconvtest8 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 3);
        res: out std_logic_vector (0 to 3));
end zdLLziMainziconvtest8;
architecture zdLLziMainziconvtest8Impl of zdLLziMainziconvtest8 is
  signal slice0Res: std_logic_vector (0 to 4);
  signal zdLLziMainziconvtest151Res: std_logic_vector (0 to 3);
  component zdLLziMainziconvtest15
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 3));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziMainziconvtest152Call: zdLLziMainziconvtest15 port map (arg0 => slice0Res(0 to 0),
                                                                arg1 => slice0Res(4 to 4),
                                                                res => zdLLziMainziconvtest151Res);
  res <= zdLLziMainziconvtest151Res;
end zdLLziMainziconvtest8Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainziconvtest4 is
  port (arg0: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 3));
end zdLLziMainziconvtest4;
architecture zdLLziMainziconvtest4Impl of zdLLziMainziconvtest4 is
  signal slice0Res: std_logic_vector (0 to 3);
  signal lit0x31Res: std_logic_vector (0 to 2);
begin
  lit0x31Res <= "000";
  slice0Res <= (lit0x31Res & arg0);
  res <= slice0Res;
end zdLLziMainziconvtest4Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziPurezidispatch is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 0);
        arg2: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 3));
end zdLLziPurezidispatch;
architecture zdLLziPurezidispatchImpl of zdLLziPurezidispatch is
  signal slice0Res: std_logic_vector (0 to 2);
  signal zdLLziMainziconvtest101Res: std_logic_vector (0 to 3);
  component zdLLziMainziconvtest10
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 0);
          arg2: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 3));
  end component;
begin
  slice0Res <= ((arg1 & arg0) & arg2);
  zdLLziMainziconvtest102Call: zdLLziMainziconvtest10 port map (arg0 => slice0Res(0 to 0),
                                                                arg1 => slice0Res(1 to 1),
                                                                arg2 => slice0Res(2 to 2),
                                                                res => zdLLziMainziconvtest101Res);
  res <= zdLLziMainziconvtest101Res;
end zdLLziPurezidispatchImpl;