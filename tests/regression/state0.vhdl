library ieee;
use ieee.std_logic_1164.all;
entity top_level is
  port (clk: in std_logic;
        rst: in std_logic;
        __in0: in std_logic_vector (0 to 2);
        __out0: out std_logic_vector (0 to 0);
        __out1: out std_logic_vector (0 to 2));
end top_level;
architecture top_level_impl of top_level is
  signal start_state: std_logic_vector (0 to 8);
  signal loop_out: std_logic_vector (0 to 8);
  signal current_state: std_logic_vector (0 to 8);
  signal done_or_next_state: std_logic_vector (0 to 8);
  signal next_state: std_logic_vector (0 to 8);
  signal inp: std_logic_vector (0 to 2);
  component zdPurezidispatch
    port (arg0: in std_logic_vector (0 to 4);
          arg1: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
  component zdPurezistart
    port (res: out std_logic_vector (0 to 8));
  end component;
begin
  start_call: zdPurezistart port map (res => start_state);
  loop_call: zdPurezidispatch port map (arg0 => current_state(5 to 9),
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
      __out1 <= current_state(2 to 4);
    end if;
  end process;
  inp <= ("" & __in0);
end top_level_impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezidispatch is
  port (arg0: in std_logic_vector (0 to 4);
        arg1: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdPurezidispatch;
architecture zdPurezidispatchImpl of zdPurezidispatch is
  signal match0Res: std_logic_vector (0 to 8);
  signal slice1Res: std_logic_vector (0 to 7);
  signal zdLLziPurezidispatch12Res: std_logic_vector (0 to 8);
  signal match4Res: std_logic_vector (0 to 8);
  signal slice5Res: std_logic_vector (0 to 7);
  signal zdLLziPurezidispatch36Res: std_logic_vector (0 to 8);
  signal slice8Res: std_logic_vector (0 to 7);
  signal Mainzidev9Res: std_logic_vector (0 to 8);
  component zdLLziPurezidispatch1
    port (arg0: in std_logic_vector (0 to 2);
          arg1: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
  component zdLLziPurezidispatch3
    port (arg0: in std_logic_vector (0 to 2);
          arg1: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
  component Mainzidev
    port (arg0: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
begin
  slice1Res <= (arg1 & arg0);
  with (((TRUE AND TRUE) AND (slice1Res(3 to 4) = "01")) AND TRUE)
    select match0Res <= zdLLziPurezidispatch12Res when TRUE,
                        match4Res when others;
  zdLLziPurezidispatch13Call: zdLLziPurezidispatch1 port map (arg0 => slice1Res(0 to 2),
                                                              arg1 => slice1Res(5 to 7),
                                                              res => zdLLziPurezidispatch12Res);
  slice5Res <= (arg1 & arg0);
  with (((TRUE AND TRUE) AND (slice5Res(3 to 4) = "10")) AND TRUE)
    select match4Res <= zdLLziPurezidispatch36Res when TRUE,
                        Mainzidev9Res when others;
  zdLLziPurezidispatch37Call: zdLLziPurezidispatch3 port map (arg0 => slice5Res(0 to 2),
                                                              arg1 => slice5Res(5 to 7),
                                                              res => zdLLziPurezidispatch36Res);
  slice8Res <= (arg1 & arg0);
  Mainzidev10Call: Mainzidev port map (arg0 => slice8Res(0 to 2),
                                       res => Mainzidev9Res);
  res <= match0Res;
end zdPurezidispatchImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezistart is
  port (res: out std_logic_vector (0 to 8));
end zdPurezistart;
architecture zdPurezistartImpl of zdPurezistart is
  signal lit16x90Res: std_logic_vector (0 to 8);
begin
  lit16x90Res <= "000010000";
  res <= lit16x90Res;
end zdPurezistartImpl;

library ieee;
use ieee.std_logic_1164.all;
entity Mainzidev is
  port (arg0: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end Mainzidev;
architecture MainzidevImpl of Mainzidev is
  signal zdLLziMainzidev30Res: std_logic_vector (0 to 8);
  component zdLLziMainzidev3
    port (arg0: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
begin
  zdLLziMainzidev31Call: zdLLziMainzidev3 port map (arg0 => arg0(0 to 2),
                                                    res => zdLLziMainzidev30Res);
  res <= zdLLziMainzidev30Res;
end MainzidevImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziPurezidispatch4 is
  port (arg0: in std_logic_vector (0 to 2);
        arg1: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdLLziPurezidispatch4;
architecture zdLLziPurezidispatch4Impl of zdLLziPurezidispatch4 is
  signal slice0Res: std_logic_vector (0 to 5);
  signal zdLLziMainzidev11Res: std_logic_vector (0 to 8);
  component zdLLziMainzidev1
    port (arg0: in std_logic_vector (0 to 2);
          arg1: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainzidev12Call: zdLLziMainzidev1 port map (arg0 => slice0Res(0 to 2),
                                                    arg1 => slice0Res(3 to 5),
                                                    res => zdLLziMainzidev11Res);
  res <= zdLLziMainzidev11Res;
end zdLLziPurezidispatch4Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzidev3 is
  port (arg0: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdLLziMainzidev3;
architecture zdLLziMainzidev3Impl of zdLLziMainzidev3 is
  signal slice0Res: std_logic_vector (0 to 8);
  signal lit2x61Res: std_logic_vector (0 to 5);
begin
  lit2x61Res <= "000010";
  slice0Res <= (lit2x61Res & arg0);
  res <= slice0Res;
end zdLLziMainzidev3Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziPurezidispatch3 is
  port (arg0: in std_logic_vector (0 to 2);
        arg1: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdLLziPurezidispatch3;
architecture zdLLziPurezidispatch3Impl of zdLLziPurezidispatch3 is
  signal slice0Res: std_logic_vector (0 to 5);
  signal zdLLziPurezidispatch41Res: std_logic_vector (0 to 8);
  component zdLLziPurezidispatch4
    port (arg0: in std_logic_vector (0 to 2);
          arg1: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziPurezidispatch42Call: zdLLziPurezidispatch4 port map (arg0 => slice0Res(0 to 2),
                                                              arg1 => slice0Res(3 to 5),
                                                              res => zdLLziPurezidispatch41Res);
  res <= zdLLziPurezidispatch41Res;
end zdLLziPurezidispatch3Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzidev2 is
  port (arg0: in std_logic_vector (0 to 2);
        arg1: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdLLziMainzidev2;
architecture zdLLziMainzidev2Impl of zdLLziMainzidev2 is
  signal slice0Res: std_logic_vector (0 to 8);
  signal lit1x11Res: std_logic_vector (0 to 0);
  signal lit0x52Res: std_logic_vector (0 to 4);
begin
  lit1x11Res <= "1";
  lit0x52Res <= "00000";
  slice0Res <= ((lit1x11Res & arg0) & lit0x52Res);
  res <= slice0Res;
end zdLLziMainzidev2Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzidev1 is
  port (arg0: in std_logic_vector (0 to 2);
        arg1: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdLLziMainzidev1;
architecture zdLLziMainzidev1Impl of zdLLziMainzidev1 is
  signal slice0Res: std_logic_vector (0 to 8);
  signal lit1x61Res: std_logic_vector (0 to 5);
begin
  lit1x61Res <= "000001";
  slice0Res <= (lit1x61Res & arg0);
  res <= slice0Res;
end zdLLziMainzidev1Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziPurezidispatch1 is
  port (arg0: in std_logic_vector (0 to 2);
        arg1: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdLLziPurezidispatch1;
architecture zdLLziPurezidispatch1Impl of zdLLziPurezidispatch1 is
  signal slice0Res: std_logic_vector (0 to 5);
  signal zdLLziPurezidispatch1Res: std_logic_vector (0 to 8);
  component zdLLziPurezidispatch
    port (arg0: in std_logic_vector (0 to 2);
          arg1: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
begin
  slice0Res <= (arg0 & arg1);
  zdLLziPurezidispatch2Call: zdLLziPurezidispatch port map (arg0 => slice0Res(0 to 2),
                                                            arg1 => slice0Res(3 to 5),
                                                            res => zdLLziPurezidispatch1Res);
  res <= zdLLziPurezidispatch1Res;
end zdLLziPurezidispatch1Impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziPurezidispatch is
  port (arg0: in std_logic_vector (0 to 2);
        arg1: in std_logic_vector (0 to 2);
        res: out std_logic_vector (0 to 8));
end zdLLziPurezidispatch;
architecture zdLLziPurezidispatchImpl of zdLLziPurezidispatch is
  signal slice0Res: std_logic_vector (0 to 5);
  signal zdLLziMainzidev21Res: std_logic_vector (0 to 8);
  component zdLLziMainzidev2
    port (arg0: in std_logic_vector (0 to 2);
          arg1: in std_logic_vector (0 to 2);
          res: out std_logic_vector (0 to 8));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainzidev22Call: zdLLziMainzidev2 port map (arg0 => slice0Res(0 to 2),
                                                    arg1 => slice0Res(3 to 5),
                                                    res => zdLLziMainzidev21Res);
  res <= zdLLziMainzidev21Res;
end zdLLziPurezidispatchImpl;