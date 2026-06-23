library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package rw_helpers is
  function rw_resize (v : std_logic_vector; n : natural) return std_logic_vector;
  function rw_add (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_sub (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_mul (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_div (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_mod (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_pow (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_and (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_or (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_xor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_not (a : std_logic_vector) return std_logic_vector;
  function rw_shiftl (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_shiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_ashiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_rand (a : std_logic_vector) return std_logic_vector;
  function rw_ror (a : std_logic_vector) return std_logic_vector;
  function rw_rxor (a : std_logic_vector) return std_logic_vector;
  function rw_eq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_neq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_lt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_gt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_lteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_gteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_cond (c : std_logic_vector; a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_repl (n : natural; v : std_logic_vector) return std_logic_vector;
  function rw_sext (v : std_logic_vector; n : natural) return std_logic_vector;
  function rw_lts (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_lteqs (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_gts (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_gteqs (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
end package;

package body rw_helpers is
  function rw_max (a : natural; b : natural) return natural is
  begin
    if a > b then return a; else return b; end if;
  end;
  function rw_b2v (b : boolean) return std_logic_vector is
  begin
    if b then return "1"; else return "0"; end if;
  end;
  function rw_resize (v : std_logic_vector; n : natural) return std_logic_vector is
  begin
    return std_logic_vector(resize(unsigned(v), n));
  end;
  function rw_add (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return std_logic_vector(resize(unsigned(a), n) + resize(unsigned(b), n));
  end;
  function rw_sub (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return std_logic_vector(resize(unsigned(a), n) - resize(unsigned(b), n));
  end;
  function rw_mul (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return std_logic_vector(resize(resize(unsigned(a), n) * resize(unsigned(b), n), n));
  end;
  function rw_div (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    if unsigned(b) = 0 then return std_logic_vector(to_unsigned(0, n) - 1); end if;
    return std_logic_vector(resize(resize(unsigned(a), n) / resize(unsigned(b), n), n));
  end;
  function rw_mod (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    if unsigned(b) = 0 then return std_logic_vector(resize(unsigned(a), n)); end if;
    return std_logic_vector(resize(resize(unsigned(a), n) mod resize(unsigned(b), n), n));
  end;
  function rw_pow (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := a'length;
    variable r : unsigned(n - 1 downto 0) := to_unsigned(1, n);
  begin
    for i in 1 to to_integer(unsigned(b)) loop
      r := resize(r * unsigned(a), n);
    end loop;
    return std_logic_vector(r);
  end;
  function rw_and (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_resize(a, n) and rw_resize(b, n);
  end;
  function rw_or (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_resize(a, n) or rw_resize(b, n);
  end;
  function rw_xor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_resize(a, n) xor rw_resize(b, n);
  end;
  function rw_not (a : std_logic_vector) return std_logic_vector is
  begin
    return not a;
  end;
  function rw_shiftl (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    if unsigned(b) >= a'length then return std_logic_vector(to_unsigned(0, a'length)); end if;
    return std_logic_vector(shift_left(unsigned(a), to_integer(unsigned(b))));
  end;
  function rw_shiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    if unsigned(b) >= a'length then return std_logic_vector(to_unsigned(0, a'length)); end if;
    return std_logic_vector(shift_right(unsigned(a), to_integer(unsigned(b))));
  end;
  function rw_ashiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    variable sh : natural;
  begin
    if unsigned(b) >= a'length then sh := a'length; else sh := to_integer(unsigned(b)); end if;
    return std_logic_vector(shift_right(signed(a), sh));
  end;
  function rw_rand (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((and a) = '1');
  end;
  function rw_ror (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((or a) = '1');
  end;
  function rw_rxor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((xor a) = '1');
  end;
  function rw_eq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_b2v(resize(unsigned(a), n) = resize(unsigned(b), n));
  end;
  function rw_neq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_b2v(resize(unsigned(a), n) /= resize(unsigned(b), n));
  end;
  function rw_lt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_b2v(resize(unsigned(a), n) < resize(unsigned(b), n));
  end;
  function rw_gt (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_b2v(resize(unsigned(a), n) > resize(unsigned(b), n));
  end;
  function rw_lteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_b2v(resize(unsigned(a), n) <= resize(unsigned(b), n));
  end;
  function rw_gteq (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_b2v(resize(unsigned(a), n) >= resize(unsigned(b), n));
  end;
  function rw_cond (c : std_logic_vector; a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    if unsigned(c) /= 0 then return rw_resize(a, n); else return rw_resize(b, n); end if;
  end;
  function rw_repl (n : natural; v : std_logic_vector) return std_logic_vector is
    variable r : std_logic_vector(n * v'length - 1 downto 0);
  begin
    for i in 0 to n - 1 loop
      r((i + 1) * v'length - 1 downto i * v'length) := v;
    end loop;
    return r;
  end;
  function rw_sext (v : std_logic_vector; n : natural) return std_logic_vector is
  begin
    return std_logic_vector(resize(signed(v), n));
  end;
  function rw_lts (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(signed(a) < signed(b));
  end;
  function rw_lteqs (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(signed(a) <= signed(b));
  end;
  function rw_gts (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(signed(a) > signed(b));
  end;
  function rw_gteqs (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(signed(a) >= signed(b));
  end;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity top_level is
port (clk : in std_logic_vector (0 downto 0);
      rst : in std_logic_vector (0 downto 0);
      \__in0\ : in std_logic_vector (513 downto 0);
      \__out0\ : out std_logic_vector (257 downto 0));
end entity;

architecture rtl of top_level is
component \Main_dev\ is
      port (arg0 : in std_logic_vector (513 downto 0);
            arg1 : in std_logic_vector (255 downto 0);
            arg2 : in std_logic_vector (511 downto 0);
            arg3 : in std_logic_vector (255 downto 0);
            arg4 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1296 downto 0));
      end component;
      component \Main_loop\ is
      port (arg0 : in std_logic_vector (255 downto 0);
            arg1 : in std_logic_vector (511 downto 0);
            arg2 : in std_logic_vector (255 downto 0);
            arg3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1296 downto 0));
      end component;
      component \ZLL_Pure_dispatch8\ is
      port (arg0 : in std_logic_vector (513 downto 0);
            arg1 : in std_logic_vector (255 downto 0);
            arg2 : in std_logic_vector (511 downto 0);
            arg3 : in std_logic_vector (255 downto 0);
            arg4 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1296 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (7 downto 0) := std_logic_vector'(B"01000000");
      signal \__resumption_tag_next\ : std_logic_vector (7 downto 0);
      signal \__st0\ : std_logic_vector (223 downto 0) := std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (223 downto 0);
      signal \__st1\ : std_logic_vector (31 downto 0) := std_logic_vector'(B"00000000000000000000000000000000");
      signal \__st1_next\ : std_logic_vector (31 downto 0);
      signal \__st2\ : std_logic_vector (511 downto 0) := std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st2_next\ : std_logic_vector (511 downto 0);
      signal \__st3\ : std_logic_vector (261 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st3_next\ : std_logic_vector (261 downto 0);
      signal zi1 : std_logic_vector (255 downto 0);
      signal zi3 : std_logic_vector (255 downto 0);
      signal zi4 : std_logic_vector (5 downto 0);
      signal main_dev_out : std_logic_vector (1296 downto 0);
      signal conn : std_logic_vector (255 downto 0);
      signal zll_pure_dispatch8_out : std_logic_vector (1296 downto 0);
      signal \connR1\ : std_logic_vector (255 downto 0);
      signal \zll_pure_dispatch8_outR1\ : std_logic_vector (1296 downto 0);
      signal zi5 : std_logic_vector (5 downto 0);
      signal zi6 : std_logic_vector (255 downto 0);
      signal zi8 : std_logic_vector (255 downto 0);
      signal zi9 : std_logic_vector (5 downto 0);
      signal zi16 : std_logic_vector (31 downto 0);
      signal zi17 : std_logic_vector (31 downto 0);
      signal zi18 : std_logic_vector (31 downto 0);
      signal zi19 : std_logic_vector (31 downto 0);
      signal zi20 : std_logic_vector (31 downto 0);
      signal zi21 : std_logic_vector (31 downto 0);
      signal zi22 : std_logic_vector (31 downto 0);
      signal zi23 : std_logic_vector (31 downto 0);
      signal zi30 : std_logic_vector (31 downto 0);
      signal zi31 : std_logic_vector (31 downto 0);
      signal zi32 : std_logic_vector (31 downto 0);
      signal zi33 : std_logic_vector (31 downto 0);
      signal zi34 : std_logic_vector (31 downto 0);
      signal zi35 : std_logic_vector (31 downto 0);
      signal zi36 : std_logic_vector (31 downto 0);
      signal zi38 : std_logic_vector (1029 downto 0);
      signal zi39 : std_logic_vector (255 downto 0);
      signal zi40 : std_logic_vector (511 downto 0);
      signal zi41 : std_logic_vector (255 downto 0);
      signal zi42 : std_logic_vector (5 downto 0);
      signal zi43 : std_logic_vector (1296 downto 0);
      signal zi44 : std_logic_vector (255 downto 0);
      signal zi45 : std_logic_vector (511 downto 0);
      signal zi46 : std_logic_vector (255 downto 0);
      signal zi47 : std_logic_vector (5 downto 0);
      signal \main_dev_outR1\ : std_logic_vector (1296 downto 0);
      signal main_loop_out : std_logic_vector (1296 downto 0);
      signal zres : std_logic_vector (1296 downto 0);
begin
zi1 <= (\__st0\ & \__st1\);
      zi3 <= \__st3\(261 downto 6);
      zi4 <= \__st3\(5 downto 0);
      inst : \Main_dev\ port map (\__in0\, zi1, \__st2\, zi3, zi4, main_dev_out);
      conn <= (\__st0\ & \__st1\);
      \instR1\ : \ZLL_Pure_dispatch8\ port map (\__in0\, conn, \__st2\, \__st3\(261 downto 6), \__st3\(5 downto 0), zll_pure_dispatch8_out);
      \connR1\ <= (\__st0\ & \__st1\);
      \instR2\ : \ZLL_Pure_dispatch8\ port map (\__in0\, \connR1\, \__st2\, \__st3\(261 downto 6), \__st3\(5 downto 0), \zll_pure_dispatch8_outR1\);
      zi5 <= \__resumption_tag\(5 downto 0);
      zi6 <= (\__st0\ & \__st1\);
      zi8 <= \__st3\(261 downto 6);
      zi9 <= \__st3\(5 downto 0);
      zi16 <= \__st3\(261 downto 230);
      zi17 <= \__st3\(229 downto 198);
      zi18 <= \__st3\(197 downto 166);
      zi19 <= \__st3\(165 downto 134);
      zi20 <= \__st3\(133 downto 102);
      zi21 <= \__st3\(101 downto 70);
      zi22 <= \__st3\(69 downto 38);
      zi23 <= \__st3\(37 downto 6);
      zi30 <= \__st0\(223 downto 192);
      zi31 <= \__st0\(191 downto 160);
      zi32 <= \__st0\(159 downto 128);
      zi33 <= \__st0\(127 downto 96);
      zi34 <= \__st0\(95 downto 64);
      zi35 <= \__st0\(63 downto 32);
      zi36 <= \__st0\(31 downto 0);
      zi38 <= (zi6 & \__st2\ & rw_add(zi30, zi16) & rw_add(zi31, zi17) & rw_add(zi32, zi18) & rw_add(zi33, zi19) & rw_add(zi34, zi20) & rw_add(zi35, zi21) & rw_add(zi36, zi22) & rw_add(\__st1\, zi23) & zi9);
      zi39 <= zi38(1029 downto 774);
      zi40 <= zi38(773 downto 262);
      zi41 <= zi38(261 downto 6);
      zi42 <= zi38(5 downto 0);
      zi43 <= ((std_logic_vector'(B"0000000001") & rw_repl(257, std_logic_vector'(B"0"))) & zi39 & zi40 & zi41 & zi42);
      zi44 <= zi43(1029 downto 774);
      zi45 <= zi43(773 downto 262);
      zi46 <= zi43(261 downto 6);
      zi47 <= zi43(5 downto 0);
      \instR3\ : \Main_dev\ port map (\__in0\, zi44, zi45, zi46, zi47, \main_dev_outR1\);
      \instR4\ : \Main_loop\ port map (zi6, \__st2\, zi8, zi9, main_loop_out);
      zres <= rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"01")), main_dev_out, rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"10")), zll_pure_dispatch8_out, rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"11")), \zll_pure_dispatch8_outR1\, rw_cond(rw_eq(zi5, std_logic_vector'(B"111111")), \main_dev_outR1\, main_loop_out))));
      \__resumption_tag_next\ <= zres(1037 downto 1030);
      \__st0_next\ <= zres(1029 downto 806);
      \__st1_next\ <= zres(805 downto 774);
      \__st2_next\ <= zres(773 downto 262);
      \__st3_next\ <= zres(261 downto 0);
      \__out0\ <= zres(1295 downto 1038);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"01000000");
                  \__st0\ <= std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
                  \__st1\ <= std_logic_vector'(B"00000000000000000000000000000000");
                  \__st2\ <= std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
                  \__st3\ <= std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__resumption_tag\ <= \__resumption_tag_next\;
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
                  \__st2\ <= \__st2_next\;
                  \__st3\ <= \__st3_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_sigma115\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (31 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \ZLL_Main_sigma115\ is

begin
res <= rw_or(rw_shiftr(arg1, arg0), rw_shiftl(arg1, rw_sub(std_logic_vector'(B"00000000000000000000000000100000"), arg0)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev104\ is
port (arg0 : in std_logic_vector (1029 downto 0);
      res : out std_logic_vector (1296 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev104\ is
signal zi0 : std_logic_vector (255 downto 0);
      signal zi1 : std_logic_vector (511 downto 0);
      signal zi2 : std_logic_vector (255 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
begin
zi0 <= arg0(1029 downto 774);
      zi1 <= arg0(773 downto 262);
      zi2 <= arg0(261 downto 6);
      zi3 <= arg0(5 downto 0);
      res <= ((std_logic_vector'(B"0000000001") & rw_repl(257, std_logic_vector'(B"0"))) & zi0 & zi1 & zi2 & zi3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_dev\ is
port (arg0 : in std_logic_vector (513 downto 0);
      arg1 : in std_logic_vector (255 downto 0);
      arg2 : in std_logic_vector (511 downto 0);
      arg3 : in std_logic_vector (255 downto 0);
      arg4 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1296 downto 0));
end entity;

architecture rtl of \Main_dev\ is
component \ZLL_Main_dev104\ is
      port (arg0 : in std_logic_vector (1029 downto 0);
            res : out std_logic_vector (1296 downto 0));
      end component;
      component \ZLL_Main_dev50\ is
      port (arg0 : in std_logic_vector (511 downto 0);
            arg1 : in std_logic_vector (255 downto 0);
            arg2 : in std_logic_vector (511 downto 0);
            arg3 : in std_logic_vector (255 downto 0);
            arg4 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1029 downto 0));
      end component;
      signal zi0 : std_logic_vector (511 downto 0);
      signal zll_main_dev50_out : std_logic_vector (1029 downto 0);
      signal zll_main_dev104_out : std_logic_vector (1296 downto 0);
      signal zi22 : std_logic_vector (1296 downto 0);
      signal zi23 : std_logic_vector (255 downto 0);
      signal zi24 : std_logic_vector (511 downto 0);
      signal zi25 : std_logic_vector (255 downto 0);
      signal zi26 : std_logic_vector (5 downto 0);
      signal zi27 : std_logic_vector (511 downto 0);
      signal \zll_main_dev50_outR1\ : std_logic_vector (1029 downto 0);
      signal \zll_main_dev104_outR1\ : std_logic_vector (1296 downto 0);
      signal zi44 : std_logic_vector (1296 downto 0);
      signal zi45 : std_logic_vector (255 downto 0);
      signal zi46 : std_logic_vector (511 downto 0);
      signal zi47 : std_logic_vector (255 downto 0);
      signal zi48 : std_logic_vector (5 downto 0);
begin
zi0 <= arg0(511 downto 0);
      inst : \ZLL_Main_dev50\ port map (zi0, std_logic_vector'(B"0110101000001001111001100110011110111011011001111010111010000101001111000110111011110011011100101010010101001111111101010011101001010001000011100101001001111111100110110000010101101000100011000001111110000011110110011010101101011011111000001100110100011001"), arg2, std_logic_vector'(B"0110101000001001111001100110011110111011011001111010111010000101001111000110111011110011011100101010010101001111111101010011101001010001000011100101001001111111100110110000010101101000100011000001111110000011110110011010101101011011111000001100110100011001"), std_logic_vector'(B"000000"), zll_main_dev50_out);
      \instR1\ : \ZLL_Main_dev104\ port map (zll_main_dev50_out, zll_main_dev104_out);
      zi22 <= zll_main_dev104_out;
      zi23 <= zi22(1029 downto 774);
      zi24 <= zi22(773 downto 262);
      zi25 <= zi22(261 downto 6);
      zi26 <= zi22(5 downto 0);
      zi27 <= arg0(511 downto 0);
      \instR2\ : \ZLL_Main_dev50\ port map (zi27, arg3, arg2, arg3, std_logic_vector'(B"000000"), \zll_main_dev50_outR1\);
      \instR3\ : \ZLL_Main_dev104\ port map (\zll_main_dev50_outR1\, \zll_main_dev104_outR1\);
      zi44 <= \zll_main_dev104_outR1\;
      zi45 <= zi44(1029 downto 774);
      zi46 <= zi44(773 downto 262);
      zi47 <= zi44(261 downto 6);
      zi48 <= zi44(5 downto 0);
      res <= rw_cond(rw_eq(arg0(513 downto 512), std_logic_vector'(B"00")), (std_logic_vector'(B"101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000") & zi23 & zi24 & zi25 & zi26), rw_cond(rw_eq(arg0(513 downto 512), std_logic_vector'(B"01")), (std_logic_vector'(B"101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011000000") & zi45 & zi46 & zi47 & zi48), (std_logic_vector'(B"100") & arg3 & std_logic_vector'(B"01000000") & arg1 & arg2 & arg3 & arg4)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_loop\ is
port (arg0 : in std_logic_vector (255 downto 0);
      arg1 : in std_logic_vector (511 downto 0);
      arg2 : in std_logic_vector (255 downto 0);
      arg3 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1296 downto 0));
end entity;

architecture rtl of \Main_loop\ is
component \ZLL_Main_sigma115\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal zi12 : std_logic_vector (31 downto 0);
      signal zi28 : std_logic_vector (31 downto 0);
      signal zi29 : std_logic_vector (31 downto 0);
      signal zi30 : std_logic_vector (31 downto 0);
      signal zi31 : std_logic_vector (31 downto 0);
      signal zi32 : std_logic_vector (31 downto 0);
      signal zi33 : std_logic_vector (31 downto 0);
      signal zi34 : std_logic_vector (31 downto 0);
      signal zi35 : std_logic_vector (31 downto 0);
      signal zi36 : std_logic_vector (31 downto 0);
      signal zi37 : std_logic_vector (31 downto 0);
      signal zi38 : std_logic_vector (31 downto 0);
      signal zi39 : std_logic_vector (31 downto 0);
      signal zi40 : std_logic_vector (31 downto 0);
      signal zi41 : std_logic_vector (31 downto 0);
      signal zi42 : std_logic_vector (31 downto 0);
      signal zi43 : std_logic_vector (31 downto 0);
      signal zll_main_sigma115_out : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR1\ : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR2\ : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR3\ : std_logic_vector (31 downto 0);
      signal zi44 : std_logic_vector (31 downto 0);
      signal zi45 : std_logic_vector (1029 downto 0);
      signal zi46 : std_logic_vector (255 downto 0);
      signal zi47 : std_logic_vector (511 downto 0);
      signal zi48 : std_logic_vector (255 downto 0);
      signal zi49 : std_logic_vector (5 downto 0);
      signal zi50 : std_logic_vector (1061 downto 0);
      signal zi51 : std_logic_vector (31 downto 0);
      signal zi53 : std_logic_vector (511 downto 0);
      signal zi54 : std_logic_vector (255 downto 0);
      signal zi55 : std_logic_vector (5 downto 0);
      signal zi56 : std_logic_vector (31 downto 0);
      signal zi69 : std_logic_vector (31 downto 0);
      signal zi70 : std_logic_vector (31 downto 0);
      signal zi71 : std_logic_vector (31 downto 0);
      signal zi72 : std_logic_vector (31 downto 0);
      signal zi73 : std_logic_vector (31 downto 0);
      signal zi74 : std_logic_vector (31 downto 0);
      signal zi75 : std_logic_vector (31 downto 0);
      signal zi76 : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR4\ : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR5\ : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR6\ : std_logic_vector (31 downto 0);
      signal zi81 : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR7\ : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR8\ : std_logic_vector (31 downto 0);
      signal \zll_main_sigma115_outR9\ : std_logic_vector (31 downto 0);
      signal zi86 : std_logic_vector (31 downto 0);
      signal zi87 : std_logic_vector (31 downto 0);
      signal zi88 : std_logic_vector (31 downto 0);
      signal zi89 : std_logic_vector (1029 downto 0);
      signal zi90 : std_logic_vector (255 downto 0);
      signal zi91 : std_logic_vector (511 downto 0);
      signal zi92 : std_logic_vector (255 downto 0);
      signal zi94 : std_logic_vector (1029 downto 0);
      signal zi95 : std_logic_vector (255 downto 0);
      signal zi96 : std_logic_vector (511 downto 0);
      signal zi97 : std_logic_vector (255 downto 0);
      signal zi98 : std_logic_vector (5 downto 0);
      signal zi99 : std_logic_vector (1035 downto 0);
      signal zi100 : std_logic_vector (5 downto 0);
      signal zi101 : std_logic_vector (255 downto 0);
      signal zi102 : std_logic_vector (511 downto 0);
      signal zi103 : std_logic_vector (255 downto 0);
      signal zi104 : std_logic_vector (5 downto 0);
      signal zi105 : std_logic_vector (1296 downto 0);
      signal zi106 : std_logic_vector (5 downto 0);
      signal zi107 : std_logic_vector (255 downto 0);
      signal zi108 : std_logic_vector (511 downto 0);
      signal zi109 : std_logic_vector (255 downto 0);
      signal zi110 : std_logic_vector (5 downto 0);
begin
zi12 <= arg1(511 downto 480);
      zi28 <= arg1(511 downto 480);
      zi29 <= arg1(479 downto 448);
      zi30 <= arg1(447 downto 416);
      zi31 <= arg1(415 downto 384);
      zi32 <= arg1(383 downto 352);
      zi33 <= arg1(351 downto 320);
      zi34 <= arg1(319 downto 288);
      zi35 <= arg1(287 downto 256);
      zi36 <= arg1(255 downto 224);
      zi37 <= arg1(223 downto 192);
      zi38 <= arg1(191 downto 160);
      zi39 <= arg1(159 downto 128);
      zi40 <= arg1(127 downto 96);
      zi41 <= arg1(95 downto 64);
      zi42 <= arg1(63 downto 32);
      zi43 <= arg1(31 downto 0);
      inst : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000010001"), zi42, zll_main_sigma115_out);
      \instR1\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000010011"), zi42, \zll_main_sigma115_outR1\);
      \instR2\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi29, \zll_main_sigma115_outR2\);
      \instR3\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000010010"), zi29, \zll_main_sigma115_outR3\);
      zi44 <= rw_add(rw_add(rw_xor(rw_xor(zll_main_sigma115_out, \zll_main_sigma115_outR1\), rw_shiftr(zi42, std_logic_vector'(B"00000000000000000000000000001010"))), zi37), rw_add(rw_xor(rw_xor(\zll_main_sigma115_outR2\, \zll_main_sigma115_outR3\), rw_shiftr(zi29, std_logic_vector'(B"00000000000000000000000000000011"))), zi28));
      zi45 <= (arg0 & (zi29 & zi30 & zi31 & zi32 & zi33 & zi34 & zi35 & zi36 & zi37 & zi38 & zi39 & zi40 & zi41 & zi42 & zi43 & zi44) & arg2 & arg3);
      zi46 <= zi45(1029 downto 774);
      zi47 <= zi45(773 downto 262);
      zi48 <= zi45(261 downto 6);
      zi49 <= zi45(5 downto 0);
      zi50 <= (zi12 & zi46 & zi47 & zi48 & zi49);
      zi51 <= zi50(1061 downto 1030);
      zi53 <= zi50(773 downto 262);
      zi54 <= zi50(261 downto 6);
      zi55 <= zi50(5 downto 0);
      zi56 <= rw_cond(rw_eq(arg3, std_logic_vector'(B"000000")), std_logic_vector'(B"01000010100010100010111110011000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000001")), std_logic_vector'(B"01110001001101110100010010010001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000010")), std_logic_vector'(B"10110101110000001111101111001111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000011")), std_logic_vector'(B"11101001101101011101101110100101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000100")), std_logic_vector'(B"00111001010101101100001001011011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000101")), std_logic_vector'(B"01011001111100010001000111110001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000110")), std_logic_vector'(B"10010010001111111000001010100100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000111")), std_logic_vector'(B"10101011000111000101111011010101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001000")), std_logic_vector'(B"11011000000001111010101010011000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001001")), std_logic_vector'(B"00010010100000110101101100000001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001010")), std_logic_vector'(B"00100100001100011000010110111110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001011")), std_logic_vector'(B"01010101000011000111110111000011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001100")), std_logic_vector'(B"01110010101111100101110101110100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001101")), std_logic_vector'(B"10000000110111101011000111111110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001110")), std_logic_vector'(B"10011011110111000000011010100111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001111")), std_logic_vector'(B"11000001100110111111000101110100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010000")), std_logic_vector'(B"11100100100110110110100111000001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010001")), std_logic_vector'(B"11101111101111100100011110000110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010010")), std_logic_vector'(B"00001111110000011001110111000110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010011")), std_logic_vector'(B"00100100000011001010000111001100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010100")), std_logic_vector'(B"00101101111010010010110001101111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010101")), std_logic_vector'(B"01001010011101001000010010101010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010110")), std_logic_vector'(B"01011100101100001010100111011100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010111")), std_logic_vector'(B"01110110111110011000100011011010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011000")), std_logic_vector'(B"10011000001111100101000101010010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011001")), std_logic_vector'(B"10101000001100011100011001101101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011010")), std_logic_vector'(B"10110000000000110010011111001000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011011")), std_logic_vector'(B"10111111010110010111111111000111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011100")), std_logic_vector'(B"11000110111000000000101111110011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011101")), std_logic_vector'(B"11010101101001111001000101000111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011110")), std_logic_vector'(B"00000110110010100110001101010001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011111")), std_logic_vector'(B"00010100001010010010100101100111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100000")), std_logic_vector'(B"00100111101101110000101010000101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100001")), std_logic_vector'(B"00101110000110110010000100111000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100010")), std_logic_vector'(B"01001101001011000110110111111100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100011")), std_logic_vector'(B"01010011001110000000110100010011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100100")), std_logic_vector'(B"01100101000010100111001101010100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100101")), std_logic_vector'(B"01110110011010100000101010111011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100110")), std_logic_vector'(B"10000001110000101100100100101110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100111")), std_logic_vector'(B"10010010011100100010110010000101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101000")), std_logic_vector'(B"10100010101111111110100010100001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101001")), std_logic_vector'(B"10101000000110100110011001001011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101010")), std_logic_vector'(B"11000010010010111000101101110000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101011")), std_logic_vector'(B"11000111011011000101000110100011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101100")), std_logic_vector'(B"11010001100100101110100000011001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101101")), std_logic_vector'(B"11010110100110010000011000100100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101110")), std_logic_vector'(B"11110100000011100011010110000101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101111")), std_logic_vector'(B"00010000011010101010000001110000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110000")), std_logic_vector'(B"00011001101001001100000100010110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110001")), std_logic_vector'(B"00011110001101110110110000001000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110010")), std_logic_vector'(B"00100111010010000111011101001100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110011")), std_logic_vector'(B"00110100101100001011110010110101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110100")), std_logic_vector'(B"00111001000111000000110010110011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110101")), std_logic_vector'(B"01001110110110001010101001001010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110110")), std_logic_vector'(B"01011011100111001100101001001111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110111")), std_logic_vector'(B"01101000001011100110111111110011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111000")), std_logic_vector'(B"01110100100011111000001011101110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111001")), std_logic_vector'(B"01111000101001010110001101101111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111010")), std_logic_vector'(B"10000100110010000111100000010100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111011")), std_logic_vector'(B"10001100110001110000001000001000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111100")), std_logic_vector'(B"10010000101111101111111111111010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111101")), std_logic_vector'(B"10100100010100000110110011101011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111110")), std_logic_vector'(B"10111110111110011010001111110111"), std_logic_vector'(B"11000110011100010111100011110010"))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      zi69 <= zi50(1029 downto 998);
      zi70 <= zi50(997 downto 966);
      zi71 <= zi50(965 downto 934);
      zi72 <= zi50(933 downto 902);
      zi73 <= zi50(901 downto 870);
      zi74 <= zi50(869 downto 838);
      zi75 <= zi50(837 downto 806);
      zi76 <= zi50(805 downto 774);
      \instR4\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000000110"), zi73, \zll_main_sigma115_outR4\);
      \instR5\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi73, \zll_main_sigma115_outR5\);
      \instR6\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000011001"), zi73, \zll_main_sigma115_outR6\);
      zi81 <= rw_add(zi76, rw_add(rw_add(rw_xor(rw_xor(\zll_main_sigma115_outR4\, \zll_main_sigma115_outR5\), \zll_main_sigma115_outR6\), rw_xor(rw_and(zi73, zi74), rw_and(rw_not(zi73), zi75))), rw_add(zi56, zi51)));
      \instR7\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000000010"), zi69, \zll_main_sigma115_outR7\);
      \instR8\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000001101"), zi69, \zll_main_sigma115_outR8\);
      \instR9\ : \ZLL_Main_sigma115\ port map (std_logic_vector'(B"00000000000000000000000000010110"), zi69, \zll_main_sigma115_outR9\);
      zi86 <= rw_add(rw_xor(rw_xor(\zll_main_sigma115_outR7\, \zll_main_sigma115_outR8\), \zll_main_sigma115_outR9\), rw_xor(rw_xor(rw_and(zi69, zi70), rw_and(zi69, zi71)), rw_and(zi70, zi71)));
      zi87 <= rw_add(zi72, zi81);
      zi88 <= rw_add(zi81, zi86);
      zi89 <= ((zi88 & zi69 & zi70 & zi71 & zi87 & zi73 & zi74 & zi75) & zi53 & zi54 & zi55);
      zi90 <= zi89(1029 downto 774);
      zi91 <= zi89(773 downto 262);
      zi92 <= zi89(261 downto 6);
      zi94 <= (zi90 & zi91 & zi92 & rw_cond(rw_eq(arg3, std_logic_vector'(B"000000")), std_logic_vector'(B"000001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000001")), std_logic_vector'(B"000010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000010")), std_logic_vector'(B"000011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000011")), std_logic_vector'(B"000100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000100")), std_logic_vector'(B"000101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000101")), std_logic_vector'(B"000110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000110")), std_logic_vector'(B"000111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"000111")), std_logic_vector'(B"001000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001000")), std_logic_vector'(B"001001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001001")), std_logic_vector'(B"001010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001010")), std_logic_vector'(B"001011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001011")), std_logic_vector'(B"001100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001100")), std_logic_vector'(B"001101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001101")), std_logic_vector'(B"001110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001110")), std_logic_vector'(B"001111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"001111")), std_logic_vector'(B"010000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010000")), std_logic_vector'(B"010001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010001")), std_logic_vector'(B"010010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010010")), std_logic_vector'(B"010011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010011")), std_logic_vector'(B"010100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010100")), std_logic_vector'(B"010101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010101")), std_logic_vector'(B"010110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010110")), std_logic_vector'(B"010111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"010111")), std_logic_vector'(B"011000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011000")), std_logic_vector'(B"011001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011001")), std_logic_vector'(B"011010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011010")), std_logic_vector'(B"011011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011011")), std_logic_vector'(B"011100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011100")), std_logic_vector'(B"011101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011101")), std_logic_vector'(B"011110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011110")), std_logic_vector'(B"011111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"011111")), std_logic_vector'(B"100000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100000")), std_logic_vector'(B"100001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100001")), std_logic_vector'(B"100010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100010")), std_logic_vector'(B"100011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100011")), std_logic_vector'(B"100100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100100")), std_logic_vector'(B"100101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100101")), std_logic_vector'(B"100110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100110")), std_logic_vector'(B"100111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"100111")), std_logic_vector'(B"101000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101000")), std_logic_vector'(B"101001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101001")), std_logic_vector'(B"101010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101010")), std_logic_vector'(B"101011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101011")), std_logic_vector'(B"101100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101100")), std_logic_vector'(B"101101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101101")), std_logic_vector'(B"101110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101110")), std_logic_vector'(B"101111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"101111")), std_logic_vector'(B"110000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110000")), std_logic_vector'(B"110001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110001")), std_logic_vector'(B"110010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110010")), std_logic_vector'(B"110011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110011")), std_logic_vector'(B"110100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110100")), std_logic_vector'(B"110101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110101")), std_logic_vector'(B"110110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110110")), std_logic_vector'(B"110111"), rw_cond(rw_eq(arg3, std_logic_vector'(B"110111")), std_logic_vector'(B"111000"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111000")), std_logic_vector'(B"111001"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111001")), std_logic_vector'(B"111010"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111010")), std_logic_vector'(B"111011"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111011")), std_logic_vector'(B"111100"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111100")), std_logic_vector'(B"111101"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111101")), std_logic_vector'(B"111110"), rw_cond(rw_eq(arg3, std_logic_vector'(B"111110")), std_logic_vector'(B"111111"), std_logic_vector'(B"000000")))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      zi95 <= zi94(1029 downto 774);
      zi96 <= zi94(773 downto 262);
      zi97 <= zi94(261 downto 6);
      zi98 <= zi94(5 downto 0);
      zi99 <= (arg3 & zi95 & zi96 & zi97 & zi98);
      zi100 <= zi99(1035 downto 1030);
      zi101 <= zi99(1029 downto 774);
      zi102 <= zi99(773 downto 262);
      zi103 <= zi99(261 downto 6);
      zi104 <= zi99(5 downto 0);
      zi105 <= ((std_logic_vector'(B"00000000001") & rw_repl(250, std_logic_vector'(B"0"))) & zi100 & zi101 & zi102 & zi103 & zi104);
      zi106 <= zi105(1035 downto 1030);
      zi107 <= zi105(1029 downto 774);
      zi108 <= zi105(773 downto 262);
      zi109 <= zi105(261 downto 6);
      zi110 <= zi105(5 downto 0);
      res <= ((std_logic_vector'(B"11") & rw_repl(259, std_logic_vector'(B"0"))) & zi106 & zi107 & zi108 & zi109 & zi110);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev50\ is
port (arg0 : in std_logic_vector (511 downto 0);
      arg1 : in std_logic_vector (255 downto 0);
      arg2 : in std_logic_vector (511 downto 0);
      arg3 : in std_logic_vector (255 downto 0);
      arg4 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1029 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev50\ is

begin
res <= (arg1 & arg0 & arg3 & arg4);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Pure_dispatch8\ is
port (arg0 : in std_logic_vector (513 downto 0);
      arg1 : in std_logic_vector (255 downto 0);
      arg2 : in std_logic_vector (511 downto 0);
      arg3 : in std_logic_vector (255 downto 0);
      arg4 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1296 downto 0));
end entity;

architecture rtl of \ZLL_Pure_dispatch8\ is
component \Main_loop\ is
      port (arg0 : in std_logic_vector (255 downto 0);
            arg1 : in std_logic_vector (511 downto 0);
            arg2 : in std_logic_vector (255 downto 0);
            arg3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1296 downto 0));
      end component;
      signal main_loop_out : std_logic_vector (1296 downto 0);
begin
inst : \Main_loop\ port map (arg1, arg2, arg3, arg4, main_loop_out);
      res <= main_loop_out;
end architecture;