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
      component \ZLL_Main_loop11\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (88 downto 0));
      end component;
      component \ZLL_Main_loop13\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (88 downto 0));
      end component;
      component \ZLL_Main_putReg3\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"000");
      signal \__resumption_tag_next\ : std_logic_vector (2 downto 0);
      signal \__st0\ : std_logic_vector (69 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (88 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zi5 : std_logic_vector (69 downto 0);
      signal zi6 : std_logic_vector (88 downto 0);
      signal zi7 : std_logic_vector (69 downto 0);
      signal zll_main_loop11_out : std_logic_vector (88 downto 0);
      signal \main_putins_outR1\ : std_logic_vector (69 downto 0);
      signal zi9 : std_logic_vector (69 downto 0);
      signal zi10 : std_logic_vector (88 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal zll_main_loop13_out : std_logic_vector (88 downto 0);
      signal \main_putins_outR2\ : std_logic_vector (69 downto 0);
      signal zi13 : std_logic_vector (69 downto 0);
      signal zi14 : std_logic_vector (88 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal zi16 : std_logic_vector (69 downto 0);
      signal zi17 : std_logic_vector (88 downto 0);
      signal zi18 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zi19 : std_logic_vector (75 downto 0);
      signal zi20 : std_logic_vector (5 downto 0);
      signal zi21 : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr1_out : std_logic_vector (69 downto 0);
      signal zi22 : std_logic_vector (69 downto 0);
      signal zi23 : std_logic_vector (88 downto 0);
      signal zi24 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi25 : std_logic_vector (84 downto 0);
      signal zi26 : std_logic_vector (14 downto 0);
      signal zi27 : std_logic_vector (69 downto 0);
      signal zi28 : std_logic_vector (88 downto 0);
      signal zi29 : std_logic_vector (14 downto 0);
      signal zi30 : std_logic_vector (69 downto 0);
      signal \main_putins_outR3\ : std_logic_vector (69 downto 0);
      signal zi32 : std_logic_vector (69 downto 0);
      signal zi33 : std_logic_vector (88 downto 0);
      signal zi34 : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi35 : std_logic_vector (86 downto 0);
      signal zi37 : std_logic_vector (69 downto 0);
      signal zi38 : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (77 downto 0);
      signal zi40 : std_logic_vector (7 downto 0);
      signal zi41 : std_logic_vector (69 downto 0);
      signal zll_main_putreg3_out : std_logic_vector (69 downto 0);
      signal zi42 : std_logic_vector (69 downto 0);
      signal zi43 : std_logic_vector (88 downto 0);
      signal zi44 : std_logic_vector (69 downto 0);
      signal \zll_main_loop13_outR1\ : std_logic_vector (88 downto 0);
      signal \main_putins_outR4\ : std_logic_vector (69 downto 0);
      signal zi46 : std_logic_vector (69 downto 0);
      signal zi47 : std_logic_vector (88 downto 0);
      signal zi48 : std_logic_vector (69 downto 0);
      signal \zll_main_loop11_outR1\ : std_logic_vector (88 downto 0);
      signal zres : std_logic_vector (88 downto 0);
begin
inst : \Main_putIns\ port map (\__in0\, \__st0\, main_putins_out);
      zi2 <= main_putins_out;
      zi3 <= (std_logic_vector'(B"0010000000000000000") & zi2);
      zi4 <= zi3(69 downto 0);
      \instR1\ : \Main_incrPC\ port map (zi4, main_incrpc_out);
      zi5 <= main_incrpc_out;
      zi6 <= (std_logic_vector'(B"0010000000000000000") & zi5);
      zi7 <= zi6(69 downto 0);
      \instR2\ : \ZLL_Main_loop11\ port map (zi7, zll_main_loop11_out);
      \instR3\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR1\);
      zi9 <= \main_putins_outR1\;
      zi10 <= (std_logic_vector'(B"0010000000000000000") & zi9);
      zi11 <= zi10(69 downto 0);
      \instR4\ : \ZLL_Main_loop13\ port map (zi11, zll_main_loop13_out);
      \instR5\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR2\);
      zi13 <= \main_putins_outR2\;
      zi14 <= (std_logic_vector'(B"0010000000000000000") & zi13);
      zi15 <= zi14(69 downto 0);
      \instR6\ : \Main_incrPC\ port map (zi15, \main_incrpc_outR1\);
      zi16 <= \main_incrpc_outR1\;
      zi17 <= (std_logic_vector'(B"0010000000000000000") & zi16);
      zi18 <= zi17(69 downto 0);
      \instR7\ : \Main_getPC\ port map (zi18, main_getpc_out);
      zi19 <= main_getpc_out;
      zi20 <= zi19(75 downto 70);
      zi21 <= zi19(69 downto 0);
      \instR8\ : \ZLL_Main_finishInstr1\ port map (zi20, zi21, zll_main_finishinstr1_out);
      zi22 <= zll_main_finishinstr1_out;
      zi23 <= (std_logic_vector'(B"0010000000000000000") & zi22);
      zi24 <= zi23(69 downto 0);
      \instR9\ : \Main_getOut\ port map (zi24, main_getout_out);
      zi25 <= main_getout_out;
      zi26 <= zi25(84 downto 70);
      zi27 <= zi25(69 downto 0);
      zi28 <= (std_logic_vector'(B"0001") & zi26 & zi27);
      zi29 <= zi28(84 downto 70);
      zi30 <= zi28(69 downto 0);
      \instR10\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR3\);
      zi32 <= \main_putins_outR3\;
      zi33 <= (std_logic_vector'(B"0010000000000000000") & zi32);
      zi34 <= zi33(69 downto 0);
      \instR11\ : \Main_getIns\ port map (zi34, main_getins_out);
      zi35 <= main_getins_out;
      zi37 <= zi35(69 downto 0);
      zi38 <= zi35(77 downto 70);
      zi39 <= (zi38 & zi37);
      zi40 <= zi39(77 downto 70);
      zi41 <= zi39(69 downto 0);
      \instR12\ : \ZLL_Main_putReg3\ port map (zi40, std_logic_vector'(B"00"), zi41, zll_main_putreg3_out);
      zi42 <= zll_main_putreg3_out;
      zi43 <= (std_logic_vector'(B"0010000000000000000") & zi42);
      zi44 <= zi43(69 downto 0);
      \instR13\ : \ZLL_Main_loop13\ port map (zi44, \zll_main_loop13_outR1\);
      \instR14\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR4\);
      zi46 <= \main_putins_outR4\;
      zi47 <= (std_logic_vector'(B"0010000000000000000") & zi46);
      zi48 <= zi47(69 downto 0);
      \instR15\ : \ZLL_Main_loop11\ port map (zi48, \zll_main_loop11_outR1\);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"001")), zll_main_loop11_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"010")), zll_main_loop13_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"011")), (std_logic_vector'(B"1") & zi29 & std_logic_vector'(B"100") & zi30), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"100")), \zll_main_loop13_outR1\, \zll_main_loop11_outR1\))));
      \__resumption_tag_next\ <= zres(72 downto 70);
      \__st0_next\ <= zres(69 downto 0);
      \__out0\ <= zres(87 downto 73);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"000");
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
entity \Main_putPC1\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_putPC1\ is
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
entity \Main_getReg1\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \Main_getReg1\ is
component \ZLL_Main_getReg3\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg3_out : std_logic_vector (77 downto 0);
begin
inst : \ZLL_Main_getReg3\ port map (arg0, arg1, zll_main_getreg3_out);
      res <= zll_main_getreg3_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop13\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (88 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop13\ is
component \Main_getIns\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (86 downto 0));
      end component;
      component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \Main_getReg\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      component \Main_getReg1\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
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
      component \Main_putPC1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_finishInstr\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop11\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (88 downto 0));
      end component;
      component \ZLL_Main_putReg3\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putWeOut\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (14 downto 0);
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
      signal zll_main_loop11_out : std_logic_vector (88 downto 0);
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
      signal main_getreg_out : std_logic_vector (77 downto 0);
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
      signal zi36 : std_logic_vector (14 downto 0);
      signal zi37 : std_logic_vector (69 downto 0);
      signal zll_main_putweout_out : std_logic_vector (69 downto 0);
      signal zi38 : std_logic_vector (69 downto 0);
      signal zi39 : std_logic_vector (88 downto 0);
      signal zi40 : std_logic_vector (69 downto 0);
      signal \main_getout_outR3\ : std_logic_vector (84 downto 0);
      signal zi41 : std_logic_vector (84 downto 0);
      signal zi42 : std_logic_vector (14 downto 0);
      signal zi43 : std_logic_vector (69 downto 0);
      signal zi44 : std_logic_vector (88 downto 0);
      signal zi45 : std_logic_vector (14 downto 0);
      signal zi46 : std_logic_vector (69 downto 0);
      signal zi47 : std_logic_vector (1 downto 0);
      signal zi48 : std_logic_vector (1 downto 0);
      signal zi49 : std_logic_vector (1 downto 0);
      signal main_getreg1_out : std_logic_vector (77 downto 0);
      signal zi50 : std_logic_vector (77 downto 0);
      signal zi51 : std_logic_vector (7 downto 0);
      signal zi52 : std_logic_vector (69 downto 0);
      signal \main_getreg1_outR1\ : std_logic_vector (77 downto 0);
      signal zi53 : std_logic_vector (77 downto 0);
      signal zi54 : std_logic_vector (7 downto 0);
      signal zi55 : std_logic_vector (69 downto 0);
      signal zi56 : std_logic_vector (7 downto 0);
      signal zll_main_putreg3_out : std_logic_vector (69 downto 0);
      signal zi57 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal zi58 : std_logic_vector (69 downto 0);
      signal zi59 : std_logic_vector (88 downto 0);
      signal zi60 : std_logic_vector (69 downto 0);
      signal \zll_main_loop11_outR1\ : std_logic_vector (88 downto 0);
      signal zi61 : std_logic_vector (5 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (77 downto 0);
      signal zi62 : std_logic_vector (77 downto 0);
      signal zi63 : std_logic_vector (7 downto 0);
      signal zi64 : std_logic_vector (69 downto 0);
      signal zi65 : std_logic_vector (0 downto 0);
      signal main_putpc1_out : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR2\ : std_logic_vector (69 downto 0);
      signal zi66 : std_logic_vector (69 downto 0);
      signal zi67 : std_logic_vector (88 downto 0);
      signal zi68 : std_logic_vector (69 downto 0);
      signal \zll_main_loop11_outR2\ : std_logic_vector (88 downto 0);
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
      \instR2\ : \ZLL_Main_loop11\ port map (zi12, zll_main_loop11_out);
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
      \instR6\ : \Main_getReg\ port map (zi9, main_getreg_out);
      zi25 <= main_getreg_out;
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
      zi36 <= zi35(84 downto 70);
      zi37 <= zi35(69 downto 0);
      \instR11\ : \ZLL_Main_putWeOut\ port map (std_logic_vector'(B"1"), zi36, zi37, zll_main_putweout_out);
      zi38 <= zll_main_putweout_out;
      zi39 <= (std_logic_vector'(B"0010000000000000000") & zi38);
      zi40 <= zi39(69 downto 0);
      \instR12\ : \Main_getOut\ port map (zi40, \main_getout_outR3\);
      zi41 <= \main_getout_outR3\;
      zi42 <= zi41(84 downto 70);
      zi43 <= zi41(69 downto 0);
      zi44 <= (std_logic_vector'(B"0001") & zi42 & zi43);
      zi45 <= zi44(84 downto 70);
      zi46 <= zi44(69 downto 0);
      zi47 <= zi7(75 downto 74);
      zi48 <= zi7(73 downto 72);
      zi49 <= zi7(71 downto 70);
      \instR13\ : \Main_getReg1\ port map (zi48, zi9, main_getreg1_out);
      zi50 <= main_getreg1_out;
      zi51 <= zi50(77 downto 70);
      zi52 <= zi50(69 downto 0);
      \instR14\ : \Main_getReg1\ port map (zi49, zi52, \main_getreg1_outR1\);
      zi53 <= \main_getreg1_outR1\;
      zi54 <= zi53(77 downto 70);
      zi55 <= zi53(69 downto 0);
      zi56 <= rw_not(rw_and(zi51, zi54));
      \instR15\ : \ZLL_Main_putReg3\ port map (zi56, zi47, zi55, zll_main_putreg3_out);
      zi57 <= zll_main_putreg3_out;
      \instR16\ : \Main_incrPC\ port map (zi57, \main_incrpc_outR1\);
      zi58 <= \main_incrpc_outR1\;
      zi59 <= (std_logic_vector'(B"0010000000000000000") & zi58);
      zi60 <= zi59(69 downto 0);
      \instR17\ : \ZLL_Main_loop11\ port map (zi60, \zll_main_loop11_outR1\);
      zi61 <= zi7(75 downto 70);
      \instR18\ : \Main_getReg\ port map (zi9, \main_getreg_outR1\);
      zi62 <= \main_getreg_outR1\;
      zi63 <= zi62(77 downto 70);
      zi64 <= zi62(69 downto 0);
      zi65 <= rw_eq(zi63, std_logic_vector'(B"00000000"));
      \instR19\ : \Main_putPC1\ port map (zi61, zi64, main_putpc1_out);
      \instR20\ : \Main_incrPC\ port map (zi64, \main_incrpc_outR2\);
      zi66 <= rw_cond(rw_eq(zi65, std_logic_vector'(B"0")), main_putpc1_out, \main_incrpc_outR2\);
      zi67 <= (std_logic_vector'(B"0010000000000000000") & zi66);
      zi68 <= zi67(69 downto 0);
      \instR21\ : \ZLL_Main_loop11\ port map (zi68, \zll_main_loop11_outR2\);
      res <= rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"000")), zll_main_loop11_out, rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"001")), (std_logic_vector'(B"1") & zi22 & std_logic_vector'(B"011") & zi23), rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"010")), (std_logic_vector'(B"1") & zi45 & std_logic_vector'(B"001") & zi46), rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"011")), \zll_main_loop11_outR1\, \zll_main_loop11_outR2\))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_getReg3\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \ZLL_Main_getReg3\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
begin
zi0 <= arg1(69 downto 62);
      zi1 <= arg1(61 downto 54);
      zi2 <= arg1(53 downto 46);
      zi3 <= arg1(45 downto 38);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), (zi0 & arg1), rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), (zi1 & arg1), rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), (zi2 & arg1), (zi3 & arg1))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop11\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (88 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop11\ is
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
      res <= (std_logic_vector'(B"1") & zi4 & std_logic_vector'(B"010") & zi5);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putWeOut\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (14 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putWeOut\ is
component \Main_putOut\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal addrout : std_logic_vector (5 downto 0);
      signal dataout : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
begin
addrout <= arg1(13 downto 8);
      dataout <= arg1(7 downto 0);
      conn <= (arg0 & addrout & dataout);
      inst : \Main_putOut\ port map (conn, arg2, main_putout_out);
      res <= main_putout_out;
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
entity \ZLL_Main_putReg3\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putReg3\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (5 downto 0);
      signal zi10 : std_logic_vector (16 downto 0);
      signal zi11 : std_logic_vector (14 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal zi15 : std_logic_vector (5 downto 0);
      signal zi16 : std_logic_vector (16 downto 0);
      signal zi17 : std_logic_vector (14 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (5 downto 0);
      signal zi22 : std_logic_vector (16 downto 0);
      signal zi23 : std_logic_vector (14 downto 0);
begin
zi0 <= arg2(61 downto 54);
      zi1 <= arg2(53 downto 46);
      zi2 <= arg2(45 downto 38);
      zi3 <= arg2(37 downto 32);
      zi4 <= arg2(31 downto 15);
      zi5 <= arg2(14 downto 0);
      zi6 <= arg2(69 downto 62);
      zi7 <= arg2(53 downto 46);
      zi8 <= arg2(45 downto 38);
      zi9 <= arg2(37 downto 32);
      zi10 <= arg2(31 downto 15);
      zi11 <= arg2(14 downto 0);
      zi12 <= arg2(69 downto 62);
      zi13 <= arg2(61 downto 54);
      zi14 <= arg2(45 downto 38);
      zi15 <= arg2(37 downto 32);
      zi16 <= arg2(31 downto 15);
      zi17 <= arg2(14 downto 0);
      zi18 <= arg2(69 downto 62);
      zi19 <= arg2(61 downto 54);
      zi20 <= arg2(53 downto 46);
      zi21 <= arg2(37 downto 32);
      zi22 <= arg2(31 downto 15);
      zi23 <= arg2(14 downto 0);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), (arg0 & zi0 & zi1 & zi2 & zi3 & zi4 & zi5), rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), (zi6 & arg0 & zi7 & zi8 & zi9 & zi10 & zi11), rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), (zi12 & zi13 & arg0 & zi14 & zi15 & zi16 & zi17), (zi18 & zi19 & zi20 & arg0 & zi21 & zi22 & zi23))));
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
entity \Main_incrPC\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_incrPC\ is
component \Main_getPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (75 downto 0));
      end component;
      component \Main_putPC1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zt0 : std_logic_vector (75 downto 0);
      signal st0 : std_logic_vector (5 downto 0);
      signal st1 : std_logic_vector (69 downto 0);
      signal conn : std_logic_vector (5 downto 0);
      signal main_putpc1_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getPC\ port map (arg0, main_getpc_out);
      zt0 <= main_getpc_out;
      st0 <= zt0(75 downto 70);
      st1 <= zt0(69 downto 0);
      conn <= rw_add(st0, std_logic_vector'(B"000001"));
      \instR1\ : \Main_putPC1\ port map (conn, st1, main_putpc1_out);
      res <= main_putpc1_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getReg\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \Main_getReg\ is
component \ZLL_Main_getReg3\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg3_out : std_logic_vector (77 downto 0);
begin
inst : \ZLL_Main_getReg3\ port map (std_logic_vector'(B"00"), arg0, zll_main_getreg3_out);
      res <= zll_main_getreg3_out;
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
      component \ZLL_Main_putWeOut\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (14 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi0 : std_logic_vector (84 downto 0);
      signal zi1 : std_logic_vector (14 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zll_main_putweout_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getOut\ port map (arg0, main_getout_out);
      zi0 <= main_getout_out;
      zi1 <= zi0(84 downto 70);
      zi2 <= zi0(69 downto 0);
      \instR1\ : \ZLL_Main_putWeOut\ port map (std_logic_vector'(B"0"), zi1, zi2, zll_main_putweout_out);
      res <= zll_main_putweout_out;
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