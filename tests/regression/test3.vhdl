library ieee;
use ieee.std_logic_1164.all;
entity top_level is
  port (clk: in std_logic;
        rst: in std_logic;
        __in0: in std_logic_vector (0 to 0);
        __out0: out std_logic_vector (0 to 7));
end top_level;
architecture top_level_impl of top_level is
  signal start_state: std_logic_vector (0 to 16);
  signal loop_out: std_logic_vector (0 to 16);
  signal current_state: std_logic_vector (0 to 16);
  signal done_or_next_state: std_logic_vector (0 to 16);
  signal next_state: std_logic_vector (0 to 16);
  signal inp: std_logic_vector (0 to 0);
  component zdPurezidispatch
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 16));
  end component;
  component zdPurezistart
    port (res: out std_logic_vector (0 to 16));
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
        res: out std_logic_vector (0 to 16));
end zdPurezidispatch;
architecture zdPurezidispatchImpl of zdPurezidispatch is
  signal slice0Res: std_logic_vector (0 to 8);
  signal zdLLziMainzigo31Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo3
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainzigo32Call: zdLLziMainzigo3 port map (arg0 => slice0Res(0 to 0),
                                                  arg1 => slice0Res(1 to 8),
                                                  res => zdLLziMainzigo31Res);
  res <= zdLLziMainzigo31Res;
end zdPurezidispatchImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezistart is
  port (res: out std_logic_vector (0 to 16));
end zdPurezistart;
architecture zdPurezistartImpl of zdPurezistart is
  signal lit65536x170Res: std_logic_vector (0 to 16);
begin
  lit65536x170Res <= "10000000000000000";
  res <= lit65536x170Res;
end zdPurezistartImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo10 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo10;
architecture zdLLziMainzigo10Impl of zdLLziMainzigo10 is
  signal slice0Res: std_logic_vector (0 to 8);
  signal zdLLziMainzigo81Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo8
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainzigo82Call: zdLLziMainzigo8 port map (arg0 => slice0Res(0 to 7),
                                                  res => zdLLziMainzigo81Res);
  res <= zdLLziMainzigo81Res;
end zdLLziMainzigo10Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo9 is
  port (arg0: in std_logic_vector (0 to 15);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo9;
architecture zdLLziMainzigo9Impl of zdLLziMainzigo9 is
  signal zdLLziMainzigo20Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo2
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  zdLLziMainzigo21Call: zdLLziMainzigo2 port map (arg0 => arg0(0 to 7),
                                                  arg1 => arg0(8 to 15),
                                                  res => zdLLziMainzigo20Res);
  res <= zdLLziMainzigo20Res;
end zdLLziMainzigo9Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo8 is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo8;
architecture zdLLziMainzigo8Impl of zdLLziMainzigo8 is
  signal Mainzigo0Res: std_logic_vector (0 to 16);
  component Mainzigo
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  Mainzigo1Call: Mainzigo port map (arg0 => arg0(0 to 7),
                                    res => Mainzigo0Res);
  res <= Mainzigo0Res;
end zdLLziMainzigo8Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo7 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo7;
architecture zdLLziMainzigo7Impl of zdLLziMainzigo7 is
  signal slice0Res: std_logic_vector (0 to 16);
  signal lit1x11Res: std_logic_vector (0 to 0);
begin
  lit1x11Res <= "1";
  slice0Res <= ((lit1x11Res & arg0) & arg1);
  res <= slice0Res;
end zdLLziMainzigo7Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo5 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo5;
architecture zdLLziMainzigo5Impl of zdLLziMainzigo5 is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainzigo71Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo7
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziMainzigo72Call: zdLLziMainzigo7 port map (arg0 => slice0Res(0 to 7),
                                                  arg1 => slice0Res(8 to 15),
                                                  res => zdLLziMainzigo71Res);
  res <= zdLLziMainzigo71Res;
end zdLLziMainzigo5Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo4 is
  port (arg0: in std_logic_vector (0 to 16);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo4;
architecture zdLLziMainzigo4Impl of zdLLziMainzigo4 is
  signal zdLLziMainzigo50Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo5
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  zdLLziMainzigo51Call: zdLLziMainzigo5 port map (arg0 => arg0(1 to 8),
                                                  arg1 => arg0(9 to 16),
                                                  res => zdLLziMainzigo50Res);
  res <= zdLLziMainzigo50Res;
end zdLLziMainzigo4Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo3 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo3;
architecture zdLLziMainzigo3Impl of zdLLziMainzigo3 is
  signal slice0Res: std_logic_vector (0 to 9);
  signal zdLLziMainzigo11Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo1
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 0);
          arg2: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  slice0Res <= ((arg0 & arg0) & arg1);
  zdLLziMainzigo12Call: zdLLziMainzigo1 port map (arg0 => slice0Res(0 to 0),
                                                  arg1 => slice0Res(1 to 1),
                                                  arg2 => slice0Res(2 to 9),
                                                  res => zdLLziMainzigo11Res);
  res <= zdLLziMainzigo11Res;
end zdLLziMainzigo3Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo2 is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo2;
architecture zdLLziMainzigo2Impl of zdLLziMainzigo2 is
  signal slice0Res: std_logic_vector (0 to 16);
  signal lit0x11Res: std_logic_vector (0 to 0);
begin
  lit0x11Res <= "0";
  slice0Res <= ((lit0x11Res & arg0) & arg1);
  res <= slice0Res;
end zdLLziMainzigo2Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzigo1 is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 0);
        arg2: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzigo1;
architecture zdLLziMainzigo1Impl of zdLLziMainzigo1 is
  signal match0Res: std_logic_vector (0 to 16);
  signal slice1Res: std_logic_vector (0 to 8);
  signal zdLLziMainzigo82Res: std_logic_vector (0 to 16);
  signal slice4Res: std_logic_vector (0 to 8);
  signal zdLLziMainzigo105Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo8
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
  component zdLLziMainzigo10
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  slice1Res <= (arg2 & arg1);
  with ((TRUE AND TRUE) AND (slice1Res(8 to 8) = "1"))
    select match0Res <= zdLLziMainzigo82Res when TRUE,
                        zdLLziMainzigo105Res when others;
  zdLLziMainzigo83Call: zdLLziMainzigo8 port map (arg0 => slice1Res(0 to 7),
                                                  res => zdLLziMainzigo82Res);
  slice4Res <= (arg0 & arg2);
  zdLLziMainzigo106Call: zdLLziMainzigo10 port map (arg0 => slice4Res(0 to 0),
                                                    arg1 => slice4Res(1 to 8),
                                                    res => zdLLziMainzigo105Res);
  res <= match0Res;
end zdLLziMainzigo1Impl;

library ieee;
use ieee.std_logic_1164.all;
entity Mainzigo is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end Mainzigo;
architecture MainzigoImpl of Mainzigo is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainzigo91Res: std_logic_vector (0 to 16);
  signal zdLLziMainzigo43Res: std_logic_vector (0 to 16);
  component zdLLziMainzigo4
    port (arg0: in std_logic_vector (0 to 16);
          res: out std_logic_vector (0 to 16));
  end component;
  component zdLLziMainzigo9
    port (arg0: in std_logic_vector (0 to 15);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  slice0Res <= (arg0 & arg0);
  zdLLziMainzigo92Call: zdLLziMainzigo9 port map (arg0 => slice0Res(0 to 15),
                                                  res => zdLLziMainzigo91Res);
  zdLLziMainzigo44Call: zdLLziMainzigo4 port map (arg0 => zdLLziMainzigo91Res(0 to 16),
                                                  res => zdLLziMainzigo43Res);
  res <= zdLLziMainzigo43Res;
end MainzigoImpl;