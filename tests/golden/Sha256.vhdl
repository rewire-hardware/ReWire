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
      signal zi5 : std_logic_vector (5 downto 0);
      signal zi6 : std_logic_vector (255 downto 0);
      signal zi8 : std_logic_vector (255 downto 0);
      signal zi9 : std_logic_vector (5 downto 0);
      signal zi10 : std_logic_vector (0 downto 0);
      signal zi17 : std_logic_vector (31 downto 0);
      signal zi18 : std_logic_vector (31 downto 0);
      signal zi19 : std_logic_vector (31 downto 0);
      signal zi20 : std_logic_vector (31 downto 0);
      signal zi21 : std_logic_vector (31 downto 0);
      signal zi22 : std_logic_vector (31 downto 0);
      signal zi23 : std_logic_vector (31 downto 0);
      signal zi24 : std_logic_vector (31 downto 0);
      signal zi31 : std_logic_vector (31 downto 0);
      signal zi32 : std_logic_vector (31 downto 0);
      signal zi33 : std_logic_vector (31 downto 0);
      signal zi34 : std_logic_vector (31 downto 0);
      signal zi35 : std_logic_vector (31 downto 0);
      signal zi36 : std_logic_vector (31 downto 0);
      signal zi37 : std_logic_vector (31 downto 0);
      signal zi39 : std_logic_vector (1029 downto 0);
      signal zi40 : std_logic_vector (255 downto 0);
      signal zi41 : std_logic_vector (511 downto 0);
      signal zi42 : std_logic_vector (255 downto 0);
      signal zi43 : std_logic_vector (5 downto 0);
      signal zi44 : std_logic_vector (1296 downto 0);
      signal zi45 : std_logic_vector (255 downto 0);
      signal zi46 : std_logic_vector (511 downto 0);
      signal zi47 : std_logic_vector (255 downto 0);
      signal zi48 : std_logic_vector (5 downto 0);
      signal \main_dev_outR1\ : std_logic_vector (1296 downto 0);
      signal main_loop_out : std_logic_vector (1296 downto 0);
      signal zi50 : std_logic_vector (255 downto 0);
      signal zi52 : std_logic_vector (255 downto 0);
      signal zi53 : std_logic_vector (5 downto 0);
      signal \main_loop_outR1\ : std_logic_vector (1296 downto 0);
      signal zi54 : std_logic_vector (255 downto 0);
      signal zi56 : std_logic_vector (255 downto 0);
      signal zi57 : std_logic_vector (5 downto 0);
      signal \main_loop_outR2\ : std_logic_vector (1296 downto 0);
      signal zres : std_logic_vector (1296 downto 0);
begin
zi1 <= (\__st0\ & \__st1\);
      zi3 <= \__st3\(261 downto 6);
      zi4 <= \__st3\(5 downto 0);
      inst : \Main_dev\ port map (\__in0\, zi1, \__st2\, zi3, zi4, main_dev_out);
      zi5 <= \__resumption_tag\(5 downto 0);
      zi6 <= (\__st0\ & \__st1\);
      zi8 <= \__st3\(261 downto 6);
      zi9 <= \__st3\(5 downto 0);
      zi10 <= rw_eq(zi5, std_logic_vector'(B"111111"));
      zi17 <= \__st3\(261 downto 230);
      zi18 <= \__st3\(229 downto 198);
      zi19 <= \__st3\(197 downto 166);
      zi20 <= \__st3\(165 downto 134);
      zi21 <= \__st3\(133 downto 102);
      zi22 <= \__st3\(101 downto 70);
      zi23 <= \__st3\(69 downto 38);
      zi24 <= \__st3\(37 downto 6);
      zi31 <= \__st0\(223 downto 192);
      zi32 <= \__st0\(191 downto 160);
      zi33 <= \__st0\(159 downto 128);
      zi34 <= \__st0\(127 downto 96);
      zi35 <= \__st0\(95 downto 64);
      zi36 <= \__st0\(63 downto 32);
      zi37 <= \__st0\(31 downto 0);
      zi39 <= (zi6 & \__st2\ & rw_add(zi31, zi17) & rw_add(zi32, zi18) & rw_add(zi33, zi19) & rw_add(zi34, zi20) & rw_add(zi35, zi21) & rw_add(zi36, zi22) & rw_add(zi37, zi23) & rw_add(\__st1\, zi24) & zi9);
      zi40 <= zi39(1029 downto 774);
      zi41 <= zi39(773 downto 262);
      zi42 <= zi39(261 downto 6);
      zi43 <= zi39(5 downto 0);
      zi44 <= ((std_logic_vector'(B"0000000001") & rw_repl(257, std_logic_vector'(B"0"))) & zi40 & zi41 & zi42 & zi43);
      zi45 <= zi44(1029 downto 774);
      zi46 <= zi44(773 downto 262);
      zi47 <= zi44(261 downto 6);
      zi48 <= zi44(5 downto 0);
      \instR1\ : \Main_dev\ port map (\__in0\, zi45, zi46, zi47, zi48, \main_dev_outR1\);
      \instR2\ : \Main_loop\ port map (zi6, \__st2\, zi8, zi9, main_loop_out);
      zi50 <= (\__st0\ & \__st1\);
      zi52 <= \__st3\(261 downto 6);
      zi53 <= \__st3\(5 downto 0);
      \instR3\ : \Main_loop\ port map (zi50, \__st2\, zi52, zi53, \main_loop_outR1\);
      zi54 <= (\__st0\ & \__st1\);
      zi56 <= \__st3\(261 downto 6);
      zi57 <= \__st3\(5 downto 0);
      \instR4\ : \Main_loop\ port map (zi54, \__st2\, zi56, zi57, \main_loop_outR2\);
      zres <= rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"01")), main_dev_out, rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"10")), rw_cond(rw_eq(zi10, std_logic_vector'(B"1")), \main_dev_outR1\, main_loop_out), rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"11")), \main_loop_outR1\, \main_loop_outR2\)));
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
entity \Main_dev\ is
port (arg0 : in std_logic_vector (513 downto 0);
      arg1 : in std_logic_vector (255 downto 0);
      arg2 : in std_logic_vector (511 downto 0);
      arg3 : in std_logic_vector (255 downto 0);
      arg4 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1296 downto 0));
end entity;

architecture rtl of \Main_dev\ is
component \ZLL_Main_dev80\ is
      port (arg0 : in std_logic_vector (511 downto 0);
            arg1 : in std_logic_vector (255 downto 0);
            arg2 : in std_logic_vector (511 downto 0);
            arg3 : in std_logic_vector (255 downto 0);
            arg4 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1029 downto 0));
      end component;
      signal zi0 : std_logic_vector (511 downto 0);
      signal zll_main_dev80_out : std_logic_vector (1029 downto 0);
      signal zi22 : std_logic_vector (1029 downto 0);
      signal zi23 : std_logic_vector (255 downto 0);
      signal zi24 : std_logic_vector (511 downto 0);
      signal zi25 : std_logic_vector (255 downto 0);
      signal zi26 : std_logic_vector (5 downto 0);
      signal zi27 : std_logic_vector (1296 downto 0);
      signal zi28 : std_logic_vector (255 downto 0);
      signal zi29 : std_logic_vector (511 downto 0);
      signal zi30 : std_logic_vector (255 downto 0);
      signal zi31 : std_logic_vector (5 downto 0);
      signal zi32 : std_logic_vector (511 downto 0);
      signal \zll_main_dev80_outR1\ : std_logic_vector (1029 downto 0);
      signal zi49 : std_logic_vector (1029 downto 0);
      signal zi50 : std_logic_vector (255 downto 0);
      signal zi51 : std_logic_vector (511 downto 0);
      signal zi52 : std_logic_vector (255 downto 0);
      signal zi53 : std_logic_vector (5 downto 0);
      signal zi54 : std_logic_vector (1296 downto 0);
      signal zi55 : std_logic_vector (255 downto 0);
      signal zi56 : std_logic_vector (511 downto 0);
      signal zi57 : std_logic_vector (255 downto 0);
      signal zi58 : std_logic_vector (5 downto 0);
begin
zi0 <= arg0(511 downto 0);
      inst : \ZLL_Main_dev80\ port map (zi0, std_logic_vector'(B"0110101000001001111001100110011110111011011001111010111010000101001111000110111011110011011100101010010101001111111101010011101001010001000011100101001001111111100110110000010101101000100011000001111110000011110110011010101101011011111000001100110100011001"), arg2, std_logic_vector'(B"0110101000001001111001100110011110111011011001111010111010000101001111000110111011110011011100101010010101001111111101010011101001010001000011100101001001111111100110110000010101101000100011000001111110000011110110011010101101011011111000001100110100011001"), std_logic_vector'(B"000000"), zll_main_dev80_out);
      zi22 <= zll_main_dev80_out;
      zi23 <= zi22(1029 downto 774);
      zi24 <= zi22(773 downto 262);
      zi25 <= zi22(261 downto 6);
      zi26 <= zi22(5 downto 0);
      zi27 <= ((std_logic_vector'(B"0000000001") & rw_repl(257, std_logic_vector'(B"0"))) & zi23 & zi24 & zi25 & zi26);
      zi28 <= zi27(1029 downto 774);
      zi29 <= zi27(773 downto 262);
      zi30 <= zi27(261 downto 6);
      zi31 <= zi27(5 downto 0);
      zi32 <= arg0(511 downto 0);
      \instR1\ : \ZLL_Main_dev80\ port map (zi32, arg3, arg2, arg3, std_logic_vector'(B"000000"), \zll_main_dev80_outR1\);
      zi49 <= \zll_main_dev80_outR1\;
      zi50 <= zi49(1029 downto 774);
      zi51 <= zi49(773 downto 262);
      zi52 <= zi49(261 downto 6);
      zi53 <= zi49(5 downto 0);
      zi54 <= ((std_logic_vector'(B"0000000001") & rw_repl(257, std_logic_vector'(B"0"))) & zi50 & zi51 & zi52 & zi53);
      zi55 <= zi54(1029 downto 774);
      zi56 <= zi54(773 downto 262);
      zi57 <= zi54(261 downto 6);
      zi58 <= zi54(5 downto 0);
      res <= rw_cond(rw_eq(arg0(513 downto 512), std_logic_vector'(B"00")), ((std_logic_vector'(B"101") & rw_repl(264, std_logic_vector'(B"0"))) & zi28 & zi29 & zi30 & zi31), rw_cond(rw_eq(arg0(513 downto 512), std_logic_vector'(B"01")), (std_logic_vector'(B"101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011000000") & zi55 & zi56 & zi57 & zi58), (std_logic_vector'(B"100") & arg3 & std_logic_vector'(B"01000000") & arg1 & arg2 & arg3 & arg4)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev80\ is
port (arg0 : in std_logic_vector (511 downto 0);
      arg1 : in std_logic_vector (255 downto 0);
      arg2 : in std_logic_vector (511 downto 0);
      arg3 : in std_logic_vector (255 downto 0);
      arg4 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1029 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev80\ is

begin
res <= (arg1 & arg0 & arg3 & arg4);
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
      zi56 <= rw_resize(rw_shiftr(std_logic_vector'(B"01000010100010100010111110011000011100010011011101000100100100011011010111000000111110111100111111101001101101011101101110100101001110010101011011000010010110110101100111110001000100011111000110010010001111111000001010100100101010110001110001011110110101011101100000000111101010101001100000010010100000110101101100000001001001000011000110000101101111100101010100001100011111011100001101110010101111100101110101110100100000001101111010110001111111101001101111011100000001101010011111000001100110111111000101110100111001001001101101101001110000011110111110111110010001111000011000001111110000011001110111000110001001000000110010100001110011000010110111101001001011000110111101001010011101001000010010101010010111001011000010101001110111000111011011111001100010001101101010011000001111100101000101010010101010000011000111000110011011011011000000000011001001111100100010111111010110010111111111000111110001101110000000001011111100111101010110100111100100010100011100000110110010100110001101010001000101000010100100101001011001110010011110110111000010101000010100101110000110110010000100111000010011010010110001101101111111000101001100111000000011010001001101100101000010100111001101010100011101100110101000001010101110111000000111000010110010010010111010010010011100100010110010000101101000101011111111101000101000011010100000011010011001100100101111000010010010111000101101110000110001110110110001010001101000111101000110010010111010000001100111010110100110010000011000100100111101000000111000110101100001010001000001101010101000000111000000011001101001001100000100010110000111100011011101101100000010000010011101001000011101110100110000110100101100001011110010110101001110010001110000001100101100110100111011011000101010100100101001011011100111001100101001001111011010000010111001101111111100110111010010001111100000101110111001111000101001010110001101101111100001001100100001111000000101001000110011000111000000100000100010010000101111101111111111111010101001000101000001101100111010111011111011111001101000111111011111000110011100010111100011110010"), rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000"), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32);
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
      zi94 <= (zi90 & zi91 & zi92 & rw_add(arg3, std_logic_vector'(B"000001")));
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
      res <= (std_logic_vector'(B"110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010") & zi106 & zi107 & zi108 & zi109 & zi110);
end architecture;