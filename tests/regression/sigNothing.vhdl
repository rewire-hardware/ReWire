library ieee;
use ieee.std_logic_1164.all;
entity top_level is
  port (clk: in std_logic;
        rst: in std_logic;
        __in0: in std_logic_vector (0 to 7);
        __out0: out std_logic_vector (0 to 0);
        __out1: out std_logic_vector (0 to 7));
end top_level;
architecture top_level_impl of top_level is
  signal start_state: std_logic_vector (0 to 16);
  signal loop_out: std_logic_vector (0 to 16);
  signal current_state: std_logic_vector (0 to 16);
  signal done_or_next_state: std_logic_vector (0 to 16);
  signal next_state: std_logic_vector (0 to 16);
  signal inp: std_logic_vector (0 to 7);
  component zdPurezidispatch
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
  component zdPurezistart
    port (res: out std_logic_vector (0 to 16));
  end component;
begin
  start_call: zdPurezistart port map (res => start_state);
  loop_call: zdPurezidispatch port map (arg0 => current_state(10 to 17),
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
      __out1 <= current_state(2 to 9);
    end if;
  end process;
  inp <= ("" & __in0);
end top_level_impl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezidispatch is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdPurezidispatch;
architecture zdPurezidispatchImpl of zdPurezidispatch is
  signal slice0Res: std_logic_vector (0 to 15);
  signal zdLLziMainzistartzq1Res: std_logic_vector (0 to 16);
  component zdLLziMainzistartzq
    port (arg0: in std_logic_vector (0 to 7);
          arg1: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainzistartzq2Call: zdLLziMainzistartzq port map (arg0 => slice0Res(0 to 7),
                                                          arg1 => slice0Res(8 to 15),
                                                          res => zdLLziMainzistartzq1Res);
  res <= zdLLziMainzistartzq1Res;
end zdPurezidispatchImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezistart is
  port (res: out std_logic_vector (0 to 16));
end zdPurezistart;
architecture zdPurezistartImpl of zdPurezistart is
  signal lit0x170Res: std_logic_vector (0 to 16);
begin
  lit0x170Res <= "00000000000000000";
  res <= lit0x170Res;
end zdPurezistartImpl;

library ieee;
use ieee.std_logic_1164.all;
entity Mainzistartzq is
  port (arg0: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end Mainzistartzq;
architecture MainzistartzqImpl of Mainzistartzq is
  signal slice0Res: std_logic_vector (0 to 16);
  signal lit0x91Res: std_logic_vector (0 to 8);
begin
  lit0x91Res <= "000000000";
  slice0Res <= (lit0x91Res & arg0);
  res <= slice0Res;
end MainzistartzqImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzistartzq is
  port (arg0: in std_logic_vector (0 to 7);
        arg1: in std_logic_vector (0 to 7);
        res: out std_logic_vector (0 to 16));
end zdLLziMainzistartzq;
architecture zdLLziMainzistartzqImpl of zdLLziMainzistartzq is
  signal Mainzistartzq0Res: std_logic_vector (0 to 16);
  component Mainzistartzq
    port (arg0: in std_logic_vector (0 to 7);
          res: out std_logic_vector (0 to 16));
  end component;
begin
  Mainzistartzq1Call: Mainzistartzq port map (arg0 => arg1(0 to 7),
                                              res => Mainzistartzq0Res);
  res <= Mainzistartzq0Res;
end zdLLziMainzistartzqImpl;