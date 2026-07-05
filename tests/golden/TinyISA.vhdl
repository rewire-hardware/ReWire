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
      \__in0\ : in std_logic_vector (16 downto 0);
      \__out0\ : out std_logic_vector (14 downto 0));
end entity;

architecture rtl of top_level is
component \Main_getIns\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (86 downto 0));
      end component;
      component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \Main_getPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (75 downto 0));
      end component;
      component \Main_incrPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \Main_putIns\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_finishInstr1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop12\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (88 downto 0));
      end component;
      component \ZLL_Main_loop15\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (88 downto 0));
      end component;
      component \ZLL_Main_putReg\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"001");
      signal \__resumption_tag_next\ : std_logic_vector (2 downto 0);
      signal \__st0\ : std_logic_vector (69 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (88 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal zll_main_loop12_out : std_logic_vector (88 downto 0);
      signal \main_putins_outR1\ : std_logic_vector (69 downto 0);
      signal zi6 : std_logic_vector (69 downto 0);
      signal zi7 : std_logic_vector (88 downto 0);
      signal zi8 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zi9 : std_logic_vector (69 downto 0);
      signal zi10 : std_logic_vector (88 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zi12 : std_logic_vector (75 downto 0);
      signal zi13 : std_logic_vector (5 downto 0);
      signal zi14 : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr1_out : std_logic_vector (69 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal zi16 : std_logic_vector (88 downto 0);
      signal zi17 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi18 : std_logic_vector (84 downto 0);
      signal zi19 : std_logic_vector (14 downto 0);
      signal zi20 : std_logic_vector (69 downto 0);
      signal zi21 : std_logic_vector (88 downto 0);
      signal zi22 : std_logic_vector (14 downto 0);
      signal zi23 : std_logic_vector (69 downto 0);
      signal \main_putins_outR2\ : std_logic_vector (69 downto 0);
      signal zi25 : std_logic_vector (69 downto 0);
      signal zi26 : std_logic_vector (88 downto 0);
      signal zi27 : std_logic_vector (69 downto 0);
      signal zll_main_loop15_out : std_logic_vector (88 downto 0);
      signal \main_putins_outR3\ : std_logic_vector (69 downto 0);
      signal zi29 : std_logic_vector (69 downto 0);
      signal zi30 : std_logic_vector (88 downto 0);
      signal zi31 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal zi32 : std_logic_vector (69 downto 0);
      signal zi33 : std_logic_vector (88 downto 0);
      signal zi34 : std_logic_vector (69 downto 0);
      signal \zll_main_loop12_outR1\ : std_logic_vector (88 downto 0);
      signal \main_putins_outR4\ : std_logic_vector (69 downto 0);
      signal zi36 : std_logic_vector (69 downto 0);
      signal zi37 : std_logic_vector (88 downto 0);
      signal zi38 : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi39 : std_logic_vector (86 downto 0);
      signal zi41 : std_logic_vector (69 downto 0);
      signal zi42 : std_logic_vector (7 downto 0);
      signal zi43 : std_logic_vector (77 downto 0);
      signal zi44 : std_logic_vector (7 downto 0);
      signal zi45 : std_logic_vector (69 downto 0);
      signal zll_main_putreg_out : std_logic_vector (69 downto 0);
      signal zi46 : std_logic_vector (69 downto 0);
      signal zi47 : std_logic_vector (88 downto 0);
      signal zi48 : std_logic_vector (69 downto 0);
      signal \zll_main_loop15_outR1\ : std_logic_vector (88 downto 0);
      signal zres : std_logic_vector (88 downto 0);
begin
inst : \Main_putIns\ port map (\__in0\, \__st0\, main_putins_out);
      zi2 <= main_putins_out;
      zi3 <= (std_logic_vector'(B"0010000000000000000") & zi2);
      zi4 <= zi3(69 downto 0);
      \instR1\ : \ZLL_Main_loop12\ port map (zi4, zll_main_loop12_out);
      \instR2\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR1\);
      zi6 <= \main_putins_outR1\;
      zi7 <= (std_logic_vector'(B"0010000000000000000") & zi6);
      zi8 <= zi7(69 downto 0);
      \instR3\ : \Main_incrPC\ port map (zi8, main_incrpc_out);
      zi9 <= main_incrpc_out;
      zi10 <= (std_logic_vector'(B"0010000000000000000") & zi9);
      zi11 <= zi10(69 downto 0);
      \instR4\ : \Main_getPC\ port map (zi11, main_getpc_out);
      zi12 <= main_getpc_out;
      zi13 <= zi12(75 downto 70);
      zi14 <= zi12(69 downto 0);
      \instR5\ : \ZLL_Main_finishInstr1\ port map (zi13, zi14, zll_main_finishinstr1_out);
      zi15 <= zll_main_finishinstr1_out;
      zi16 <= (std_logic_vector'(B"0010000000000000000") & zi15);
      zi17 <= zi16(69 downto 0);
      \instR6\ : \Main_getOut\ port map (zi17, main_getout_out);
      zi18 <= main_getout_out;
      zi19 <= zi18(84 downto 70);
      zi20 <= zi18(69 downto 0);
      zi21 <= (std_logic_vector'(B"0001") & zi19 & zi20);
      zi22 <= zi21(84 downto 70);
      zi23 <= zi21(69 downto 0);
      \instR7\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR2\);
      zi25 <= \main_putins_outR2\;
      zi26 <= (std_logic_vector'(B"0010000000000000000") & zi25);
      zi27 <= zi26(69 downto 0);
      \instR8\ : \ZLL_Main_loop15\ port map (zi27, zll_main_loop15_out);
      \instR9\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR3\);
      zi29 <= \main_putins_outR3\;
      zi30 <= (std_logic_vector'(B"0010000000000000000") & zi29);
      zi31 <= zi30(69 downto 0);
      \instR10\ : \Main_incrPC\ port map (zi31, \main_incrpc_outR1\);
      zi32 <= \main_incrpc_outR1\;
      zi33 <= (std_logic_vector'(B"0010000000000000000") & zi32);
      zi34 <= zi33(69 downto 0);
      \instR11\ : \ZLL_Main_loop12\ port map (zi34, \zll_main_loop12_outR1\);
      \instR12\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR4\);
      zi36 <= \main_putins_outR4\;
      zi37 <= (std_logic_vector'(B"0010000000000000000") & zi36);
      zi38 <= zi37(69 downto 0);
      \instR13\ : \Main_getIns\ port map (zi38, main_getins_out);
      zi39 <= main_getins_out;
      zi41 <= zi39(69 downto 0);
      zi42 <= zi39(77 downto 70);
      zi43 <= (zi42 & zi41);
      zi44 <= zi43(77 downto 70);
      zi45 <= zi43(69 downto 0);
      \instR14\ : \ZLL_Main_putReg\ port map (zi44, zi45, zi45, zll_main_putreg_out);
      zi46 <= zll_main_putreg_out;
      zi47 <= (std_logic_vector'(B"0010000000000000000") & zi46);
      zi48 <= zi47(69 downto 0);
      \instR15\ : \ZLL_Main_loop15\ port map (zi48, \zll_main_loop15_outR1\);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"001")), zll_main_loop12_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"010")), (std_logic_vector'(B"1") & zi22 & std_logic_vector'(B"000") & zi23), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"011")), zll_main_loop15_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"100")), \zll_main_loop12_outR1\, \zll_main_loop15_outR1\))));
      \__resumption_tag_next\ <= zres(72 downto 70);
      \__st0_next\ <= zres(69 downto 0);
      \__out0\ <= zres(87 downto 73);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"001");
                  \__st0\ <= std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__resumption_tag\ <= \__resumption_tag_next\;
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_putPC\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_putPC\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
begin
zi0 <= arg1(69 downto 62);
      zi1 <= arg1(61 downto 54);
      zi2 <= arg1(53 downto 46);
      zi3 <= arg1(45 downto 38);
      zi4 <= arg1(31 downto 15);
      zi5 <= arg1(14 downto 0);
      res <= (zi0 & zi1 & zi2 & zi3 & arg0 & zi4 & zi5);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getReg$s1\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \Main_getReg$s1\ is
component \ZLL_Main_getReg3\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg3_out : std_logic_vector (77 downto 0);
begin
inst : \ZLL_Main_getReg3\ port map (arg0, arg0, zll_main_getreg3_out);
      res <= zll_main_getreg3_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_putIns\ is
port (arg0 : in std_logic_vector (16 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_putIns\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (5 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
begin
zi0 <= arg1(69 downto 62);
      zi1 <= arg1(61 downto 54);
      zi2 <= arg1(53 downto 46);
      zi3 <= arg1(45 downto 38);
      zi4 <= arg1(37 downto 32);
      zi5 <= arg1(14 downto 0);
      res <= (zi0 & zi1 & zi2 & zi3 & zi4 & arg0 & zi5);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getReg\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \Main_getReg\ is
component \ZLL_Main_getReg3\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg3_out : std_logic_vector (77 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
begin
inst : \ZLL_Main_getReg3\ port map (arg1, arg1, zll_main_getreg3_out);
      zi0 <= arg1(61 downto 54);
      zi1 <= arg1(53 downto 46);
      zi2 <= arg1(45 downto 38);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), zll_main_getreg3_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), (zi0 & arg1), rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), (zi1 & arg1), (zi2 & arg1))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_getReg3\ is
port (arg0 : in std_logic_vector (69 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \ZLL_Main_getReg3\ is
signal zi0 : std_logic_vector (7 downto 0);
begin
zi0 <= arg0(69 downto 62);
      res <= (zi0 & arg1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_finishInstr1\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_finishInstr1\ is
component \Main_putAddrOut\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_finishInstr\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zt0 : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr_out : std_logic_vector (69 downto 0);
begin
inst : \Main_putAddrOut\ port map (arg0, arg1, main_putaddrout_out);
      zt0 <= main_putaddrout_out;
      \instR1\ : \ZLL_Main_finishInstr\ port map (zt0, zll_main_finishinstr_out);
      res <= zll_main_finishinstr_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop15\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (88 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop15\ is
component \Main_getIns\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (86 downto 0));
      end component;
      component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \Main_getReg\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      component \Main_getReg$s1\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      component \Main_incrPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \Main_putAddrOut\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \Main_putOut\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \Main_putPC\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_finishInstr\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop12\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (88 downto 0));
      end component;
      component \ZLL_Main_putReg\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi0 : std_logic_vector (86 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (8 downto 0);
      signal zi4 : std_logic_vector (78 downto 0);
      signal zi5 : std_logic_vector (8 downto 0);
      signal zi6 : std_logic_vector (69 downto 0);
      signal zi7 : std_logic_vector (88 downto 0);
      signal zi9 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zi10 : std_logic_vector (69 downto 0);
      signal zi11 : std_logic_vector (88 downto 0);
      signal zi12 : std_logic_vector (69 downto 0);
      signal zll_main_loop12_out : std_logic_vector (88 downto 0);
      signal zi13 : std_logic_vector (5 downto 0);
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zi14 : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr_out : std_logic_vector (69 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal zi16 : std_logic_vector (88 downto 0);
      signal zi17 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi18 : std_logic_vector (84 downto 0);
      signal zi19 : std_logic_vector (14 downto 0);
      signal zi20 : std_logic_vector (69 downto 0);
      signal zi21 : std_logic_vector (88 downto 0);
      signal zi22 : std_logic_vector (14 downto 0);
      signal zi23 : std_logic_vector (69 downto 0);
      signal zi24 : std_logic_vector (5 downto 0);
      signal \main_getreg$s1_out\ : std_logic_vector (77 downto 0);
      signal zi25 : std_logic_vector (77 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (69 downto 0);
      signal \main_putaddrout_outR1\ : std_logic_vector (69 downto 0);
      signal zi28 : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal zi29 : std_logic_vector (84 downto 0);
      signal zi31 : std_logic_vector (69 downto 0);
      signal zi32 : std_logic_vector (0 downto 0);
      signal zi33 : std_logic_vector (5 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
      signal zi34 : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal zi35 : std_logic_vector (84 downto 0);
      signal zi37 : std_logic_vector (69 downto 0);
      signal zi38 : std_logic_vector (5 downto 0);
      signal zi39 : std_logic_vector (7 downto 0);
      signal \connR1\ : std_logic_vector (14 downto 0);
      signal \main_putout_outR1\ : std_logic_vector (69 downto 0);
      signal zi40 : std_logic_vector (69 downto 0);
      signal zi41 : std_logic_vector (88 downto 0);
      signal zi42 : std_logic_vector (69 downto 0);
      signal \main_getout_outR3\ : std_logic_vector (84 downto 0);
      signal zi43 : std_logic_vector (84 downto 0);
      signal zi44 : std_logic_vector (14 downto 0);
      signal zi45 : std_logic_vector (69 downto 0);
      signal zi46 : std_logic_vector (88 downto 0);
      signal zi47 : std_logic_vector (14 downto 0);
      signal zi48 : std_logic_vector (69 downto 0);
      signal zi49 : std_logic_vector (1 downto 0);
      signal zi50 : std_logic_vector (1 downto 0);
      signal zi51 : std_logic_vector (1 downto 0);
      signal main_getreg_out : std_logic_vector (77 downto 0);
      signal zi52 : std_logic_vector (77 downto 0);
      signal zi53 : std_logic_vector (7 downto 0);
      signal zi54 : std_logic_vector (69 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (77 downto 0);
      signal zi55 : std_logic_vector (77 downto 0);
      signal zi56 : std_logic_vector (7 downto 0);
      signal zi57 : std_logic_vector (69 downto 0);
      signal zi58 : std_logic_vector (7 downto 0);
      signal zll_main_putreg_out : std_logic_vector (69 downto 0);
      signal zi59 : std_logic_vector (7 downto 0);
      signal zi60 : std_logic_vector (7 downto 0);
      signal zi61 : std_logic_vector (7 downto 0);
      signal zi62 : std_logic_vector (5 downto 0);
      signal zi63 : std_logic_vector (16 downto 0);
      signal zi64 : std_logic_vector (14 downto 0);
      signal zi65 : std_logic_vector (7 downto 0);
      signal zi66 : std_logic_vector (7 downto 0);
      signal zi67 : std_logic_vector (7 downto 0);
      signal zi68 : std_logic_vector (5 downto 0);
      signal zi69 : std_logic_vector (16 downto 0);
      signal zi70 : std_logic_vector (14 downto 0);
      signal zi71 : std_logic_vector (7 downto 0);
      signal zi72 : std_logic_vector (7 downto 0);
      signal zi73 : std_logic_vector (7 downto 0);
      signal zi74 : std_logic_vector (5 downto 0);
      signal zi75 : std_logic_vector (16 downto 0);
      signal zi76 : std_logic_vector (14 downto 0);
      signal zi77 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal zi78 : std_logic_vector (69 downto 0);
      signal zi79 : std_logic_vector (88 downto 0);
      signal zi80 : std_logic_vector (69 downto 0);
      signal \zll_main_loop12_outR1\ : std_logic_vector (88 downto 0);
      signal zi81 : std_logic_vector (5 downto 0);
      signal \main_getreg$s1_outR1\ : std_logic_vector (77 downto 0);
      signal zi82 : std_logic_vector (77 downto 0);
      signal zi83 : std_logic_vector (7 downto 0);
      signal zi84 : std_logic_vector (69 downto 0);
      signal zi85 : std_logic_vector (0 downto 0);
      signal main_putpc_out : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR2\ : std_logic_vector (69 downto 0);
      signal zi86 : std_logic_vector (69 downto 0);
      signal zi87 : std_logic_vector (88 downto 0);
      signal zi88 : std_logic_vector (69 downto 0);
      signal \zll_main_loop12_outR2\ : std_logic_vector (88 downto 0);
begin
inst : \Main_getIns\ port map (arg0, main_getins_out);
      zi0 <= main_getins_out;
      zi2 <= zi0(69 downto 0);
      zi3 <= zi0(86 downto 78);
      zi4 <= (zi3 & zi2);
      zi5 <= zi4(78 downto 70);
      zi6 <= zi4(69 downto 0);
      zi7 <= (std_logic_vector'(B"0000000000") & zi5 & zi6);
      zi9 <= zi7(69 downto 0);
      \instR1\ : \Main_incrPC\ port map (zi9, main_incrpc_out);
      zi10 <= main_incrpc_out;
      zi11 <= (std_logic_vector'(B"0010000000000000000") & zi10);
      zi12 <= zi11(69 downto 0);
      \instR2\ : \ZLL_Main_loop12\ port map (zi12, zll_main_loop12_out);
      zi13 <= zi7(75 downto 70);
      \instR3\ : \Main_putAddrOut\ port map (zi13, zi9, main_putaddrout_out);
      zi14 <= main_putaddrout_out;
      \instR4\ : \ZLL_Main_finishInstr\ port map (zi14, zll_main_finishinstr_out);
      zi15 <= zll_main_finishinstr_out;
      zi16 <= (std_logic_vector'(B"0010000000000000000") & zi15);
      zi17 <= zi16(69 downto 0);
      \instR5\ : \Main_getOut\ port map (zi17, main_getout_out);
      zi18 <= main_getout_out;
      zi19 <= zi18(84 downto 70);
      zi20 <= zi18(69 downto 0);
      zi21 <= (std_logic_vector'(B"0001") & zi19 & zi20);
      zi22 <= zi21(84 downto 70);
      zi23 <= zi21(69 downto 0);
      zi24 <= zi7(75 downto 70);
      \instR6\ : \Main_getReg$s1\ port map (zi9, \main_getreg$s1_out\);
      zi25 <= \main_getreg$s1_out\;
      zi26 <= zi25(77 downto 70);
      zi27 <= zi25(69 downto 0);
      \instR7\ : \Main_putAddrOut\ port map (zi24, zi27, \main_putaddrout_outR1\);
      zi28 <= \main_putaddrout_outR1\;
      \instR8\ : \Main_getOut\ port map (zi28, \main_getout_outR1\);
      zi29 <= \main_getout_outR1\;
      zi31 <= zi29(69 downto 0);
      zi32 <= zi29(84 downto 84);
      zi33 <= zi29(83 downto 78);
      conn <= (zi32 & zi33 & zi26);
      \instR9\ : \Main_putOut\ port map (conn, zi31, main_putout_out);
      zi34 <= main_putout_out;
      \instR10\ : \Main_getOut\ port map (zi34, \main_getout_outR2\);
      zi35 <= \main_getout_outR2\;
      zi37 <= zi35(69 downto 0);
      zi38 <= zi35(83 downto 78);
      zi39 <= zi35(77 downto 70);
      \connR1\ <= (std_logic_vector'(B"1") & zi38 & zi39);
      \instR11\ : \Main_putOut\ port map (\connR1\, zi37, \main_putout_outR1\);
      zi40 <= \main_putout_outR1\;
      zi41 <= (std_logic_vector'(B"0010000000000000000") & zi40);
      zi42 <= zi41(69 downto 0);
      \instR12\ : \Main_getOut\ port map (zi42, \main_getout_outR3\);
      zi43 <= \main_getout_outR3\;
      zi44 <= zi43(84 downto 70);
      zi45 <= zi43(69 downto 0);
      zi46 <= (std_logic_vector'(B"0001") & zi44 & zi45);
      zi47 <= zi46(84 downto 70);
      zi48 <= zi46(69 downto 0);
      zi49 <= zi7(75 downto 74);
      zi50 <= zi7(73 downto 72);
      zi51 <= zi7(71 downto 70);
      \instR13\ : \Main_getReg\ port map (zi50, zi9, main_getreg_out);
      zi52 <= main_getreg_out;
      zi53 <= zi52(77 downto 70);
      zi54 <= zi52(69 downto 0);
      \instR14\ : \Main_getReg\ port map (zi51, zi54, \main_getreg_outR1\);
      zi55 <= \main_getreg_outR1\;
      zi56 <= zi55(77 downto 70);
      zi57 <= zi55(69 downto 0);
      zi58 <= rw_not(rw_and(zi53, zi56));
      \instR15\ : \ZLL_Main_putReg\ port map (zi58, zi57, zi57, zll_main_putreg_out);
      zi59 <= zi55(69 downto 62);
      zi60 <= zi55(53 downto 46);
      zi61 <= zi55(45 downto 38);
      zi62 <= zi55(37 downto 32);
      zi63 <= zi55(31 downto 15);
      zi64 <= zi55(14 downto 0);
      zi65 <= zi55(69 downto 62);
      zi66 <= zi55(61 downto 54);
      zi67 <= zi55(45 downto 38);
      zi68 <= zi55(37 downto 32);
      zi69 <= zi55(31 downto 15);
      zi70 <= zi55(14 downto 0);
      zi71 <= zi55(69 downto 62);
      zi72 <= zi55(61 downto 54);
      zi73 <= zi55(53 downto 46);
      zi74 <= zi55(37 downto 32);
      zi75 <= zi55(31 downto 15);
      zi76 <= zi55(14 downto 0);
      zi77 <= rw_cond(rw_eq(zi49, std_logic_vector'(B"00")), zll_main_putreg_out, rw_cond(rw_eq(zi49, std_logic_vector'(B"01")), (zi59 & zi58 & zi60 & zi61 & zi62 & zi63 & zi64), rw_cond(rw_eq(zi49, std_logic_vector'(B"10")), (zi65 & zi66 & zi58 & zi67 & zi68 & zi69 & zi70), (zi71 & zi72 & zi73 & zi58 & zi74 & zi75 & zi76))));
      \instR16\ : \Main_incrPC\ port map (zi77, \main_incrpc_outR1\);
      zi78 <= \main_incrpc_outR1\;
      zi79 <= (std_logic_vector'(B"0010000000000000000") & zi78);
      zi80 <= zi79(69 downto 0);
      \instR17\ : \ZLL_Main_loop12\ port map (zi80, \zll_main_loop12_outR1\);
      zi81 <= zi7(75 downto 70);
      \instR18\ : \Main_getReg$s1\ port map (zi9, \main_getreg$s1_outR1\);
      zi82 <= \main_getreg$s1_outR1\;
      zi83 <= zi82(77 downto 70);
      zi84 <= zi82(69 downto 0);
      zi85 <= rw_eq(zi83, std_logic_vector'(B"00000000"));
      \instR19\ : \Main_putPC\ port map (zi81, zi84, main_putpc_out);
      \instR20\ : \Main_incrPC\ port map (zi84, \main_incrpc_outR2\);
      zi86 <= rw_cond(rw_eq(zi85, std_logic_vector'(B"0")), main_putpc_out, \main_incrpc_outR2\);
      zi87 <= (std_logic_vector'(B"0010000000000000000") & zi86);
      zi88 <= zi87(69 downto 0);
      \instR21\ : \ZLL_Main_loop12\ port map (zi88, \zll_main_loop12_outR2\);
      res <= rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"000")), zll_main_loop12_out, rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"001")), (std_logic_vector'(B"1") & zi22 & std_logic_vector'(B"010") & zi23), rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"010")), (std_logic_vector'(B"1") & zi47 & std_logic_vector'(B"100") & zi48), rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"011")), \zll_main_loop12_outR1\, \zll_main_loop12_outR2\))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getIns\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (86 downto 0));
end entity;

architecture rtl of \Main_getIns\ is
signal zi0 : std_logic_vector (16 downto 0);
begin
zi0 <= arg0(31 downto 15);
      res <= (zi0 & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop12\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (88 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop12\ is
component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \Main_getPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (75 downto 0));
      end component;
      component \ZLL_Main_finishInstr1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zt0 : std_logic_vector (75 downto 0);
      signal st0 : std_logic_vector (5 downto 0);
      signal st1 : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr1_out : std_logic_vector (69 downto 0);
      signal zt1 : std_logic_vector (69 downto 0);
      signal zt2 : std_logic_vector (88 downto 0);
      signal s0 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi0 : std_logic_vector (84 downto 0);
      signal zi1 : std_logic_vector (14 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (88 downto 0);
      signal zi4 : std_logic_vector (14 downto 0);
      signal zi5 : std_logic_vector (69 downto 0);
begin
inst : \Main_getPC\ port map (arg0, main_getpc_out);
      zt0 <= main_getpc_out;
      st0 <= zt0(75 downto 70);
      st1 <= zt0(69 downto 0);
      \instR1\ : \ZLL_Main_finishInstr1\ port map (st0, st1, zll_main_finishinstr1_out);
      zt1 <= zll_main_finishinstr1_out;
      zt2 <= (std_logic_vector'(B"0010000000000000000") & zt1);
      s0 <= zt2(69 downto 0);
      \instR2\ : \Main_getOut\ port map (s0, main_getout_out);
      zi0 <= main_getout_out;
      zi1 <= zi0(84 downto 70);
      zi2 <= zi0(69 downto 0);
      zi3 <= (std_logic_vector'(B"0001") & zi1 & zi2);
      zi4 <= zi3(84 downto 70);
      zi5 <= zi3(69 downto 0);
      res <= (std_logic_vector'(B"1") & zi4 & std_logic_vector'(B"011") & zi5);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getOut\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (84 downto 0));
end entity;

architecture rtl of \Main_getOut\ is
signal zi0 : std_logic_vector (14 downto 0);
begin
zi0 <= arg0(14 downto 0);
      res <= (zi0 & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putReg\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putReg\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
begin
zi0 <= arg1(61 downto 54);
      zi1 <= arg1(53 downto 46);
      zi2 <= arg1(45 downto 38);
      zi3 <= arg1(37 downto 32);
      zi4 <= arg1(31 downto 15);
      zi5 <= arg1(14 downto 0);
      res <= (arg0 & zi0 & zi1 & zi2 & zi3 & zi4 & zi5);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_putAddrOut\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_putAddrOut\ is
component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \Main_putOut\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zt0 : std_logic_vector (84 downto 0);
      signal st1 : std_logic_vector (69 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getOut\ port map (arg1, main_getout_out);
      zt0 <= main_getout_out;
      st1 <= zt0(69 downto 0);
      zi0 <= zt0(84 downto 84);
      zi1 <= zt0(77 downto 70);
      conn <= (zi0 & arg0 & zi1);
      \instR1\ : \Main_putOut\ port map (conn, st1, main_putout_out);
      res <= main_putout_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_incrPC\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_incrPC\ is
component \Main_getPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (75 downto 0));
      end component;
      component \Main_putPC\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zt0 : std_logic_vector (75 downto 0);
      signal st0 : std_logic_vector (5 downto 0);
      signal st1 : std_logic_vector (69 downto 0);
      signal conn : std_logic_vector (5 downto 0);
      signal main_putpc_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getPC\ port map (arg0, main_getpc_out);
      zt0 <= main_getpc_out;
      st0 <= zt0(75 downto 70);
      st1 <= zt0(69 downto 0);
      conn <= rw_add(st0, std_logic_vector'(B"000001"));
      \instR1\ : \Main_putPC\ port map (conn, st1, main_putpc_out);
      res <= main_putpc_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_finishInstr\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_finishInstr\ is
component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \Main_putOut\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi0 : std_logic_vector (84 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getOut\ port map (arg0, main_getout_out);
      zi0 <= main_getout_out;
      zi2 <= zi0(69 downto 0);
      zi3 <= zi0(83 downto 78);
      zi4 <= zi0(77 downto 70);
      conn <= (std_logic_vector'(B"0") & zi3 & zi4);
      \instR1\ : \Main_putOut\ port map (conn, zi2, main_putout_out);
      res <= main_putout_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getPC\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (75 downto 0));
end entity;

architecture rtl of \Main_getPC\ is
signal zi0 : std_logic_vector (5 downto 0);
begin
zi0 <= arg0(37 downto 32);
      res <= (zi0 & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_putOut\ is
port (arg0 : in std_logic_vector (14 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_putOut\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (5 downto 0);
      signal zi5 : std_logic_vector (16 downto 0);
begin
zi0 <= arg1(69 downto 62);
      zi1 <= arg1(61 downto 54);
      zi2 <= arg1(53 downto 46);
      zi3 <= arg1(45 downto 38);
      zi4 <= arg1(37 downto 32);
      zi5 <= arg1(31 downto 15);
      res <= (zi0 & zi1 & zi2 & zi3 & zi4 & zi5 & arg0);
end architecture;