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
      signal main_loop_out : std_logic_vector (1296 downto 0);
      signal zi11 : std_logic_vector (31 downto 0);
      signal zi12 : std_logic_vector (31 downto 0);
      signal zi13 : std_logic_vector (31 downto 0);
      signal zi14 : std_logic_vector (31 downto 0);
      signal zi15 : std_logic_vector (31 downto 0);
      signal zi16 : std_logic_vector (31 downto 0);
      signal zi17 : std_logic_vector (31 downto 0);
      signal zi18 : std_logic_vector (31 downto 0);
      signal zi19 : std_logic_vector (31 downto 0);
      signal zi20 : std_logic_vector (31 downto 0);
      signal zi21 : std_logic_vector (31 downto 0);
      signal zi22 : std_logic_vector (31 downto 0);
      signal zi23 : std_logic_vector (31 downto 0);
      signal zi24 : std_logic_vector (31 downto 0);
      signal zi25 : std_logic_vector (31 downto 0);
      signal zi27 : std_logic_vector (1029 downto 0);
      signal zi28 : std_logic_vector (255 downto 0);
      signal zi29 : std_logic_vector (511 downto 0);
      signal zi30 : std_logic_vector (255 downto 0);
      signal zi31 : std_logic_vector (5 downto 0);
      signal zi32 : std_logic_vector (1296 downto 0);
      signal zi33 : std_logic_vector (255 downto 0);
      signal zi34 : std_logic_vector (511 downto 0);
      signal zi35 : std_logic_vector (255 downto 0);
      signal zi36 : std_logic_vector (5 downto 0);
      signal \main_dev_outR1\ : std_logic_vector (1296 downto 0);
      signal zi37 : std_logic_vector (255 downto 0);
      signal zi39 : std_logic_vector (255 downto 0);
      signal zi40 : std_logic_vector (5 downto 0);
      signal \main_loop_outR1\ : std_logic_vector (1296 downto 0);
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
      \instR1\ : \Main_loop\ port map (zi6, \__st2\, zi8, zi9, main_loop_out);
      zi11 <= \__st3\(261 downto 230);
      zi12 <= \__st3\(229 downto 198);
      zi13 <= \__st3\(197 downto 166);
      zi14 <= \__st3\(165 downto 134);
      zi15 <= \__st3\(133 downto 102);
      zi16 <= \__st3\(101 downto 70);
      zi17 <= \__st3\(69 downto 38);
      zi18 <= \__st3\(37 downto 6);
      zi19 <= \__st0\(223 downto 192);
      zi20 <= \__st0\(191 downto 160);
      zi21 <= \__st0\(159 downto 128);
      zi22 <= \__st0\(127 downto 96);
      zi23 <= \__st0\(95 downto 64);
      zi24 <= \__st0\(63 downto 32);
      zi25 <= \__st0\(31 downto 0);
      zi27 <= (zi6 & \__st2\ & rw_add(zi19, zi11) & rw_add(zi20, zi12) & rw_add(zi21, zi13) & rw_add(zi22, zi14) & rw_add(zi23, zi15) & rw_add(zi24, zi16) & rw_add(zi25, zi17) & rw_add(\__st1\, zi18) & zi9);
      zi28 <= zi27(1029 downto 774);
      zi29 <= zi27(773 downto 262);
      zi30 <= zi27(261 downto 6);
      zi31 <= zi27(5 downto 0);
      zi32 <= (std_logic_vector'(B"000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000") & zi28 & zi29 & zi30 & zi31);
      zi33 <= zi32(1029 downto 774);
      zi34 <= zi32(773 downto 262);
      zi35 <= zi32(261 downto 6);
      zi36 <= zi32(5 downto 0);
      \instR2\ : \Main_dev\ port map (\__in0\, zi33, zi34, zi35, zi36, \main_dev_outR1\);
      zi37 <= (\__st0\ & \__st1\);
      zi39 <= \__st3\(261 downto 6);
      zi40 <= \__st3\(5 downto 0);
      \instR3\ : \Main_loop\ port map (zi37, \__st2\, zi39, zi40, \main_loop_outR1\);
      zres <= rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"01")), main_dev_out, rw_cond(rw_eq(\__resumption_tag\(7 downto 6), std_logic_vector'(B"10")), rw_cond(rw_eq(zi10, std_logic_vector'(B"0")), main_loop_out, \main_dev_outR1\), \main_loop_outR1\));
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
entity \ZLL_Main_bigsigma04\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (31 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \ZLL_Main_bigsigma04\ is

begin
res <= rw_or(rw_shiftr(arg1, arg0), rw_shiftl(arg1, rw_sub(std_logic_vector'(B"00000000000000000000000000100000"), arg0)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev10\ is
port (arg0 : in std_logic_vector (255 downto 0);
      arg1 : in std_logic_vector (511 downto 0);
      arg2 : in std_logic_vector (255 downto 0);
      arg3 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1296 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev10\ is

begin
res <= ((std_logic_vector'(B"101") & rw_repl(264, std_logic_vector'(B"0"))) & arg0 & arg1 & arg2 & arg3);
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
component \ZLL_Main_dev10\ is
      port (arg0 : in std_logic_vector (255 downto 0);
            arg1 : in std_logic_vector (511 downto 0);
            arg2 : in std_logic_vector (255 downto 0);
            arg3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1296 downto 0));
      end component;
      component \ZLL_Main_dev2\ is
      port (arg0 : in std_logic_vector (511 downto 0);
            arg1 : in std_logic_vector (255 downto 0);
            arg2 : in std_logic_vector (511 downto 0);
            arg3 : in std_logic_vector (255 downto 0);
            arg4 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1029 downto 0));
      end component;
      signal zi0 : std_logic_vector (511 downto 0);
      signal zll_main_dev2_out : std_logic_vector (1029 downto 0);
      signal zi1 : std_logic_vector (1029 downto 0);
      signal zi2 : std_logic_vector (255 downto 0);
      signal zi3 : std_logic_vector (511 downto 0);
      signal zi4 : std_logic_vector (255 downto 0);
      signal zi5 : std_logic_vector (5 downto 0);
      signal zi6 : std_logic_vector (1296 downto 0);
      signal zi7 : std_logic_vector (255 downto 0);
      signal zi8 : std_logic_vector (511 downto 0);
      signal zi9 : std_logic_vector (255 downto 0);
      signal zi10 : std_logic_vector (5 downto 0);
      signal zll_main_dev10_out : std_logic_vector (1296 downto 0);
      signal zi11 : std_logic_vector (511 downto 0);
      signal \zll_main_dev2_outR1\ : std_logic_vector (1029 downto 0);
      signal zi12 : std_logic_vector (1029 downto 0);
      signal zi13 : std_logic_vector (255 downto 0);
      signal zi14 : std_logic_vector (511 downto 0);
      signal zi15 : std_logic_vector (255 downto 0);
      signal zi16 : std_logic_vector (5 downto 0);
      signal zi17 : std_logic_vector (1296 downto 0);
      signal zi18 : std_logic_vector (255 downto 0);
      signal zi19 : std_logic_vector (511 downto 0);
      signal zi20 : std_logic_vector (255 downto 0);
      signal zi21 : std_logic_vector (5 downto 0);
      signal \zll_main_dev10_outR1\ : std_logic_vector (1296 downto 0);
begin
zi0 <= arg0(511 downto 0);
      inst : \ZLL_Main_dev2\ port map (zi0, std_logic_vector'(B"0110101000001001111001100110011110111011011001111010111010000101001111000110111011110011011100101010010101001111111101010011101001010001000011100101001001111111100110110000010101101000100011000001111110000011110110011010101101011011111000001100110100011001"), arg2, std_logic_vector'(B"0110101000001001111001100110011110111011011001111010111010000101001111000110111011110011011100101010010101001111111101010011101001010001000011100101001001111111100110110000010101101000100011000001111110000011110110011010101101011011111000001100110100011001"), std_logic_vector'(B"000000"), zll_main_dev2_out);
      zi1 <= zll_main_dev2_out;
      zi2 <= zi1(1029 downto 774);
      zi3 <= zi1(773 downto 262);
      zi4 <= zi1(261 downto 6);
      zi5 <= zi1(5 downto 0);
      zi6 <= (std_logic_vector'(B"000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000") & zi2 & zi3 & zi4 & zi5);
      zi7 <= zi6(1029 downto 774);
      zi8 <= zi6(773 downto 262);
      zi9 <= zi6(261 downto 6);
      zi10 <= zi6(5 downto 0);
      \instR1\ : \ZLL_Main_dev10\ port map (zi7, zi8, zi9, zi10, zll_main_dev10_out);
      zi11 <= arg0(511 downto 0);
      \instR2\ : \ZLL_Main_dev2\ port map (zi11, arg3, arg2, arg3, std_logic_vector'(B"000000"), \zll_main_dev2_outR1\);
      zi12 <= \zll_main_dev2_outR1\;
      zi13 <= zi12(1029 downto 774);
      zi14 <= zi12(773 downto 262);
      zi15 <= zi12(261 downto 6);
      zi16 <= zi12(5 downto 0);
      zi17 <= (std_logic_vector'(B"000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000") & zi13 & zi14 & zi15 & zi16);
      zi18 <= zi17(1029 downto 774);
      zi19 <= zi17(773 downto 262);
      zi20 <= zi17(261 downto 6);
      zi21 <= zi17(5 downto 0);
      \instR3\ : \ZLL_Main_dev10\ port map (zi18, zi19, zi20, zi21, \zll_main_dev10_outR1\);
      res <= rw_cond(rw_eq(arg0(513 downto 512), std_logic_vector'(B"00")), zll_main_dev10_out, rw_cond(rw_eq(arg0(513 downto 512), std_logic_vector'(B"01")), \zll_main_dev10_outR1\, (std_logic_vector'(B"100") & arg3 & std_logic_vector'(B"01000000") & arg1 & arg2 & arg3 & arg4)));
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
component \ZLL_Main_bigsigma04\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal zi0 : std_logic_vector (31 downto 0);
      signal zi1 : std_logic_vector (31 downto 0);
      signal zi2 : std_logic_vector (31 downto 0);
      signal zi3 : std_logic_vector (31 downto 0);
      signal zi4 : std_logic_vector (31 downto 0);
      signal zi5 : std_logic_vector (31 downto 0);
      signal zi6 : std_logic_vector (31 downto 0);
      signal zi7 : std_logic_vector (31 downto 0);
      signal zi8 : std_logic_vector (31 downto 0);
      signal zi9 : std_logic_vector (31 downto 0);
      signal zi10 : std_logic_vector (31 downto 0);
      signal zi11 : std_logic_vector (31 downto 0);
      signal zi12 : std_logic_vector (31 downto 0);
      signal zi13 : std_logic_vector (31 downto 0);
      signal zi14 : std_logic_vector (31 downto 0);
      signal zi15 : std_logic_vector (31 downto 0);
      signal zi16 : std_logic_vector (31 downto 0);
      signal zll_main_bigsigma04_out : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR1\ : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR2\ : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR3\ : std_logic_vector (31 downto 0);
      signal zi17 : std_logic_vector (511 downto 0);
      signal zi18 : std_logic_vector (1061 downto 0);
      signal zi19 : std_logic_vector (31 downto 0);
      signal zi21 : std_logic_vector (511 downto 0);
      signal zi22 : std_logic_vector (255 downto 0);
      signal zi23 : std_logic_vector (5 downto 0);
      signal zi24 : std_logic_vector (31 downto 0);
      signal zi25 : std_logic_vector (31 downto 0);
      signal zi26 : std_logic_vector (31 downto 0);
      signal zi27 : std_logic_vector (31 downto 0);
      signal zi28 : std_logic_vector (31 downto 0);
      signal zi29 : std_logic_vector (31 downto 0);
      signal zi30 : std_logic_vector (31 downto 0);
      signal zi31 : std_logic_vector (31 downto 0);
      signal zi32 : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR4\ : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR5\ : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR6\ : std_logic_vector (31 downto 0);
      signal zi33 : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR7\ : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR8\ : std_logic_vector (31 downto 0);
      signal \zll_main_bigsigma04_outR9\ : std_logic_vector (31 downto 0);
      signal zi34 : std_logic_vector (1029 downto 0);
      signal zi35 : std_logic_vector (255 downto 0);
      signal zi36 : std_logic_vector (511 downto 0);
      signal zi37 : std_logic_vector (255 downto 0);
      signal zi39 : std_logic_vector (5 downto 0);
      signal zt0 : std_logic_vector (1035 downto 0);
      signal v : std_logic_vector (5 downto 0);
      signal s0 : std_logic_vector (255 downto 0);
      signal s1 : std_logic_vector (511 downto 0);
      signal s2 : std_logic_vector (255 downto 0);
      signal s3 : std_logic_vector (5 downto 0);
      signal zt1 : std_logic_vector (1296 downto 0);
      signal \vR1\ : std_logic_vector (5 downto 0);
      signal \s0R1\ : std_logic_vector (255 downto 0);
      signal \s1R1\ : std_logic_vector (511 downto 0);
      signal \s2R1\ : std_logic_vector (255 downto 0);
      signal \s3R1\ : std_logic_vector (5 downto 0);
begin
zi0 <= arg1(511 downto 480);
      zi1 <= arg1(511 downto 480);
      zi2 <= arg1(479 downto 448);
      zi3 <= arg1(447 downto 416);
      zi4 <= arg1(415 downto 384);
      zi5 <= arg1(383 downto 352);
      zi6 <= arg1(351 downto 320);
      zi7 <= arg1(319 downto 288);
      zi8 <= arg1(287 downto 256);
      zi9 <= arg1(255 downto 224);
      zi10 <= arg1(223 downto 192);
      zi11 <= arg1(191 downto 160);
      zi12 <= arg1(159 downto 128);
      zi13 <= arg1(127 downto 96);
      zi14 <= arg1(95 downto 64);
      zi15 <= arg1(63 downto 32);
      zi16 <= arg1(31 downto 0);
      inst : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000010001"), zi15, zll_main_bigsigma04_out);
      \instR1\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000010011"), zi15, \zll_main_bigsigma04_outR1\);
      \instR2\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi2, \zll_main_bigsigma04_outR2\);
      \instR3\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000010010"), zi2, \zll_main_bigsigma04_outR3\);
      zi17 <= (zi2 & zi3 & zi4 & zi5 & zi6 & zi7 & zi8 & zi9 & zi10 & zi11 & zi12 & zi13 & zi14 & zi15 & zi16 & rw_add(rw_add(rw_xor(rw_xor(zll_main_bigsigma04_out, \zll_main_bigsigma04_outR1\), rw_shiftr(zi15, std_logic_vector'(B"00000000000000000000000000001010"))), zi10), rw_add(rw_xor(rw_xor(\zll_main_bigsigma04_outR2\, \zll_main_bigsigma04_outR3\), rw_shiftr(zi2, std_logic_vector'(B"00000000000000000000000000000011"))), zi1)));
      zi18 <= (zi0 & arg0 & zi17 & arg2 & arg3);
      zi19 <= zi18(1061 downto 1030);
      zi21 <= zi18(773 downto 262);
      zi22 <= zi18(261 downto 6);
      zi23 <= zi18(5 downto 0);
      zi24 <= rw_resize(rw_shiftr(std_logic_vector'(B"01000010100010100010111110011000011100010011011101000100100100011011010111000000111110111100111111101001101101011101101110100101001110010101011011000010010110110101100111110001000100011111000110010010001111111000001010100100101010110001110001011110110101011101100000000111101010101001100000010010100000110101101100000001001001000011000110000101101111100101010100001100011111011100001101110010101111100101110101110100100000001101111010110001111111101001101111011100000001101010011111000001100110111111000101110100111001001001101101101001110000011110111110111110010001111000011000001111110000011001110111000110001001000000110010100001110011000010110111101001001011000110111101001010011101001000010010101010010111001011000010101001110111000111011011111001100010001101101010011000001111100101000101010010101010000011000111000110011011011011000000000011001001111100100010111111010110010111111111000111110001101110000000001011111100111101010110100111100100010100011100000110110010100110001101010001000101000010100100101001011001110010011110110111000010101000010100101110000110110010000100111000010011010010110001101101111111000101001100111000000011010001001101100101000010100111001101010100011101100110101000001010101110111000000111000010110010010010111010010010011100100010110010000101101000101011111111101000101000011010100000011010011001100100101111000010010010111000101101110000110001110110110001010001101000111101000110010010111010000001100111010110100110010000011000100100111101000000111000110101100001010001000001101010101000000111000000011001101001001100000100010110000111100011011101101100000010000010011101001000011101110100110000110100101100001011110010110101001110010001110000001100101100110100111011011000101010100100101001011011100111001100101001001111011010000010111001101111111100110111010010001111100000101110111001111000101001010110001101101111100001001100100001111000000101001000110011000111000000100000100010010000101111101111111111111010101001000101000001101100111010111011111011111001101000111111011111000110011100010111100011110010"), rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000"), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32);
      zi25 <= zi18(1029 downto 998);
      zi26 <= zi18(997 downto 966);
      zi27 <= zi18(965 downto 934);
      zi28 <= zi18(933 downto 902);
      zi29 <= zi18(901 downto 870);
      zi30 <= zi18(869 downto 838);
      zi31 <= zi18(837 downto 806);
      zi32 <= zi18(805 downto 774);
      \instR4\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000000110"), zi29, \zll_main_bigsigma04_outR4\);
      \instR5\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi29, \zll_main_bigsigma04_outR5\);
      \instR6\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000011001"), zi29, \zll_main_bigsigma04_outR6\);
      zi33 <= rw_add(zi32, rw_add(rw_add(rw_xor(rw_xor(\zll_main_bigsigma04_outR4\, \zll_main_bigsigma04_outR5\), \zll_main_bigsigma04_outR6\), rw_xor(rw_and(zi29, zi30), rw_and(rw_not(zi29), zi31))), rw_add(zi24, zi19)));
      \instR7\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000000010"), zi25, \zll_main_bigsigma04_outR7\);
      \instR8\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000001101"), zi25, \zll_main_bigsigma04_outR8\);
      \instR9\ : \ZLL_Main_bigsigma04\ port map (std_logic_vector'(B"00000000000000000000000000010110"), zi25, \zll_main_bigsigma04_outR9\);
      zi34 <= ((rw_add(zi33, rw_add(rw_xor(rw_xor(\zll_main_bigsigma04_outR7\, \zll_main_bigsigma04_outR8\), \zll_main_bigsigma04_outR9\), rw_xor(rw_xor(rw_and(zi25, zi26), rw_and(zi25, zi27)), rw_and(zi26, zi27)))) & zi25 & zi26 & zi27 & rw_add(zi28, zi33) & zi29 & zi30 & zi31) & zi21 & zi22 & zi23);
      zi35 <= zi34(1029 downto 774);
      zi36 <= zi34(773 downto 262);
      zi37 <= zi34(261 downto 6);
      zi39 <= rw_add(arg3, std_logic_vector'(B"000001"));
      zt0 <= (arg3 & zi35 & zi36 & zi37 & zi39);
      v <= zt0(1035 downto 1030);
      s0 <= zt0(1029 downto 774);
      s1 <= zt0(773 downto 262);
      s2 <= zt0(261 downto 6);
      s3 <= zt0(5 downto 0);
      zt1 <= (rw_repl(261, std_logic_vector'(B"0")) & v & s0 & s1 & s2 & s3);
      \vR1\ <= zt1(1035 downto 1030);
      \s0R1\ <= zt1(1029 downto 774);
      \s1R1\ <= zt1(773 downto 262);
      \s2R1\ <= zt1(261 downto 6);
      \s3R1\ <= zt1(5 downto 0);
      res <= (std_logic_vector'(B"110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010") & \vR1\ & \s0R1\ & \s1R1\ & \s2R1\ & \s3R1\);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev2\ is
port (arg0 : in std_logic_vector (511 downto 0);
      arg1 : in std_logic_vector (255 downto 0);
      arg2 : in std_logic_vector (511 downto 0);
      arg3 : in std_logic_vector (255 downto 0);
      arg4 : in std_logic_vector (5 downto 0);
      res : out std_logic_vector (1029 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev2\ is

begin
res <= (arg1 & arg0 & arg3 & arg4);
end architecture;