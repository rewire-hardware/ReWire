library ieee;
use ieee.std_logic_1164.all;
entity top_level is
  port (clk: in std_logic;
        rst: in std_logic;
        __in0: in std_logic_vector (0 to 0);
        __out0: out std_logic_vector (0 to 0));
end top_level;
architecture top_level_impl of top_level is
  signal start_state: std_logic_vector (0 to 2);
  signal loop_out: std_logic_vector (0 to 2);
  signal current_state: std_logic_vector (0 to 2);
  signal done_or_next_state: std_logic_vector (0 to 2);
  signal next_state: std_logic_vector (0 to 2);
  signal inp: std_logic_vector (0 to 0);
  component zdPurezidispatch
    port (arg0: in std_logic_vector (0 to 1);
          arg1: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 2));
  end component;
  component zdPurezistart
    port (res: out std_logic_vector (0 to 2));
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
        res: out std_logic_vector (0 to 2));
end zdPurezidispatch;
architecture zdPurezidispatchImpl of zdPurezidispatch is
  signal slice0Res: std_logic_vector (0 to 2);
  signal zdLLziMainzirepl1Res: std_logic_vector (0 to 2);
  component zdLLziMainzirepl
    port (arg0: in std_logic_vector (0 to 0);
          arg1: in std_logic_vector (0 to 1);
          res: out std_logic_vector (0 to 2));
  end component;
begin
  slice0Res <= (arg1 & arg0);
  zdLLziMainzirepl2Call: zdLLziMainzirepl port map (arg0 => slice0Res(0 to 0),
                                                    arg1 => slice0Res(1 to 2),
                                                    res => zdLLziMainzirepl1Res);
  res <= zdLLziMainzirepl1Res;
end zdPurezidispatchImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezistart is
  port (res: out std_logic_vector (0 to 2));
end zdPurezistart;
architecture zdPurezistartImpl of zdPurezistart is
  signal lit0x30Res: std_logic_vector (0 to 2);
begin
  lit0x30Res <= "000";
  res <= lit0x30Res;
end zdPurezistartImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdLLziMainzirepl is
  port (arg0: in std_logic_vector (0 to 0);
        arg1: in std_logic_vector (0 to 1);
        res: out std_logic_vector (0 to 2));
end zdLLziMainzirepl;
architecture zdLLziMainzireplImpl of zdLLziMainzirepl is
  signal slice0Res: std_logic_vector (0 to 2);
  signal lit0x11Res: std_logic_vector (0 to 0);
begin
  lit0x11Res <= "0";
  slice0Res <= (lit0x11Res & arg1);
  res <= slice0Res;
end zdLLziMainzireplImpl;