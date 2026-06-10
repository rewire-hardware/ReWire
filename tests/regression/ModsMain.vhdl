library ieee;
use ieee.std_logic_1164.all;
entity top_level is
  port (clk: in std_logic;
        rst: in std_logic;
        __in0: in std_logic_vector (0 to 0);
        __out0: out std_logic_vector (0 to 0));
end top_level;
architecture top_level_impl of top_level is
  signal start_state: std_logic_vector (0 to 0);
  signal loop_out: std_logic_vector (0 to 0);
  signal current_state: std_logic_vector (0 to 0);
  signal done_or_next_state: std_logic_vector (0 to 0);
  signal next_state: std_logic_vector (0 to 0);
  signal inp: std_logic_vector (0 to 0);
  component zdPurezidispatch
    port (arg0: in std_logic_vector (0 to 0);
          res: out std_logic_vector (0 to 0));
  end component;
  component zdPurezistart
    port (res: out std_logic_vector (0 to 0));
  end component;
begin
  start_call: zdPurezistart port map (res => start_state);
  loop_call: zdPurezidispatch port map (arg0 => current_state(2 to 2),
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
  port (arg0: in std_logic_vector (0 to 0);
        res: out std_logic_vector (0 to 0));
end zdPurezidispatch;
architecture zdPurezidispatchImpl of zdPurezidispatch is
  signal lit0x10Res: std_logic_vector (0 to 0);
begin
  lit0x10Res <= "0";
  res <= lit0x10Res;
end zdPurezidispatchImpl;

library ieee;
use ieee.std_logic_1164.all;
entity zdPurezistart is
  port (res: out std_logic_vector (0 to 0));
end zdPurezistart;
architecture zdPurezistartImpl of zdPurezistart is
  signal lit0x10Res: std_logic_vector (0 to 0);
begin
  lit0x10Res <= "0";
  res <= lit0x10Res;
end zdPurezistartImpl;