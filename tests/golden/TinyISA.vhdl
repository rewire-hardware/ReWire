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
      component \ZLL_Main_loop23\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (89 downto 0));
      end component;
      component \ZLL_Main_loop31\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (89 downto 0));
      end component;
      component \ZLL_Main_putReg3\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (3 downto 0) := std_logic_vector'(B"0000");
      signal \__resumption_tag_next\ : std_logic_vector (3 downto 0);
      signal \__st0\ : std_logic_vector (69 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (89 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zi5 : std_logic_vector (69 downto 0);
      signal zi6 : std_logic_vector (89 downto 0);
      signal zi7 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zi8 : std_logic_vector (75 downto 0);
      signal zi9 : std_logic_vector (5 downto 0);
      signal zi10 : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr1_out : std_logic_vector (69 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal zi12 : std_logic_vector (89 downto 0);
      signal zi13 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi14 : std_logic_vector (84 downto 0);
      signal zi15 : std_logic_vector (14 downto 0);
      signal zi16 : std_logic_vector (69 downto 0);
      signal zi17 : std_logic_vector (89 downto 0);
      signal zi18 : std_logic_vector (14 downto 0);
      signal zi19 : std_logic_vector (69 downto 0);
      signal zll_main_loop31_out : std_logic_vector (89 downto 0);
      signal \main_putins_outR1\ : std_logic_vector (69 downto 0);
      signal zi22 : std_logic_vector (69 downto 0);
      signal zi23 : std_logic_vector (89 downto 0);
      signal zi24 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal zi25 : std_logic_vector (69 downto 0);
      signal zi26 : std_logic_vector (89 downto 0);
      signal zi27 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal zi28 : std_logic_vector (75 downto 0);
      signal zi29 : std_logic_vector (5 downto 0);
      signal zi30 : std_logic_vector (69 downto 0);
      signal \zll_main_finishinstr1_outR1\ : std_logic_vector (69 downto 0);
      signal zi31 : std_logic_vector (69 downto 0);
      signal zi32 : std_logic_vector (89 downto 0);
      signal zi33 : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal zi34 : std_logic_vector (84 downto 0);
      signal zi35 : std_logic_vector (14 downto 0);
      signal zi36 : std_logic_vector (69 downto 0);
      signal zi37 : std_logic_vector (89 downto 0);
      signal zi38 : std_logic_vector (14 downto 0);
      signal zi39 : std_logic_vector (69 downto 0);
      signal \main_putins_outR2\ : std_logic_vector (69 downto 0);
      signal zi41 : std_logic_vector (69 downto 0);
      signal zi42 : std_logic_vector (89 downto 0);
      signal zi43 : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi44 : std_logic_vector (86 downto 0);
      signal zi46 : std_logic_vector (69 downto 0);
      signal zi47 : std_logic_vector (7 downto 0);
      signal zi48 : std_logic_vector (77 downto 0);
      signal zi49 : std_logic_vector (7 downto 0);
      signal zi50 : std_logic_vector (69 downto 0);
      signal zll_main_putreg3_out : std_logic_vector (69 downto 0);
      signal zi51 : std_logic_vector (69 downto 0);
      signal zi52 : std_logic_vector (89 downto 0);
      signal zi53 : std_logic_vector (69 downto 0);
      signal zll_main_loop23_out : std_logic_vector (89 downto 0);
      signal \zll_main_loop31_outR1\ : std_logic_vector (89 downto 0);
      signal \zll_main_loop31_outR2\ : std_logic_vector (89 downto 0);
      signal \zll_main_loop31_outR3\ : std_logic_vector (89 downto 0);
      signal \zll_main_loop31_outR4\ : std_logic_vector (89 downto 0);
      signal \main_putins_outR3\ : std_logic_vector (69 downto 0);
      signal zi59 : std_logic_vector (69 downto 0);
      signal zi60 : std_logic_vector (89 downto 0);
      signal zi61 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal zi62 : std_logic_vector (75 downto 0);
      signal zi63 : std_logic_vector (5 downto 0);
      signal zi64 : std_logic_vector (69 downto 0);
      signal \zll_main_finishinstr1_outR2\ : std_logic_vector (69 downto 0);
      signal zi65 : std_logic_vector (69 downto 0);
      signal zi66 : std_logic_vector (89 downto 0);
      signal zi67 : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal zi68 : std_logic_vector (84 downto 0);
      signal zi69 : std_logic_vector (14 downto 0);
      signal zi70 : std_logic_vector (69 downto 0);
      signal zi71 : std_logic_vector (89 downto 0);
      signal zi72 : std_logic_vector (14 downto 0);
      signal zi73 : std_logic_vector (69 downto 0);
      signal zres : std_logic_vector (89 downto 0);
begin
inst : \Main_putIns\ port map (\__in0\, \__st0\, main_putins_out);
      zi2 <= main_putins_out;
      zi3 <= (std_logic_vector'(B"00010000000000000000") & zi2);
      zi4 <= zi3(69 downto 0);
      \instR1\ : \Main_incrPC\ port map (zi4, main_incrpc_out);
      zi5 <= main_incrpc_out;
      zi6 <= (std_logic_vector'(B"00010000000000000000") & zi5);
      zi7 <= zi6(69 downto 0);
      \instR2\ : \Main_getPC\ port map (zi7, main_getpc_out);
      zi8 <= main_getpc_out;
      zi9 <= zi8(75 downto 70);
      zi10 <= zi8(69 downto 0);
      \instR3\ : \ZLL_Main_finishInstr1\ port map (zi9, zi10, zll_main_finishinstr1_out);
      zi11 <= zll_main_finishinstr1_out;
      zi12 <= (std_logic_vector'(B"00010000000000000000") & zi11);
      zi13 <= zi12(69 downto 0);
      \instR4\ : \Main_getOut\ port map (zi13, main_getout_out);
      zi14 <= main_getout_out;
      zi15 <= zi14(84 downto 70);
      zi16 <= zi14(69 downto 0);
      zi17 <= (std_logic_vector'(B"00001") & zi15 & zi16);
      zi18 <= zi17(84 downto 70);
      zi19 <= zi17(69 downto 0);
      \instR5\ : \ZLL_Main_loop31\ port map (\__in0\, \__st0\, zll_main_loop31_out);
      \instR6\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR1\);
      zi22 <= \main_putins_outR1\;
      zi23 <= (std_logic_vector'(B"00010000000000000000") & zi22);
      zi24 <= zi23(69 downto 0);
      \instR7\ : \Main_incrPC\ port map (zi24, \main_incrpc_outR1\);
      zi25 <= \main_incrpc_outR1\;
      zi26 <= (std_logic_vector'(B"00010000000000000000") & zi25);
      zi27 <= zi26(69 downto 0);
      \instR8\ : \Main_getPC\ port map (zi27, \main_getpc_outR1\);
      zi28 <= \main_getpc_outR1\;
      zi29 <= zi28(75 downto 70);
      zi30 <= zi28(69 downto 0);
      \instR9\ : \ZLL_Main_finishInstr1\ port map (zi29, zi30, \zll_main_finishinstr1_outR1\);
      zi31 <= \zll_main_finishinstr1_outR1\;
      zi32 <= (std_logic_vector'(B"00010000000000000000") & zi31);
      zi33 <= zi32(69 downto 0);
      \instR10\ : \Main_getOut\ port map (zi33, \main_getout_outR1\);
      zi34 <= \main_getout_outR1\;
      zi35 <= zi34(84 downto 70);
      zi36 <= zi34(69 downto 0);
      zi37 <= (std_logic_vector'(B"00001") & zi35 & zi36);
      zi38 <= zi37(84 downto 70);
      zi39 <= zi37(69 downto 0);
      \instR11\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR2\);
      zi41 <= \main_putins_outR2\;
      zi42 <= (std_logic_vector'(B"00010000000000000000") & zi41);
      zi43 <= zi42(69 downto 0);
      \instR12\ : \Main_getIns\ port map (zi43, main_getins_out);
      zi44 <= main_getins_out;
      zi46 <= zi44(69 downto 0);
      zi47 <= zi44(77 downto 70);
      zi48 <= (zi47 & zi46);
      zi49 <= zi48(77 downto 70);
      zi50 <= zi48(69 downto 0);
      \instR13\ : \ZLL_Main_putReg3\ port map (zi49, std_logic_vector'(B"00"), zi50, zll_main_putreg3_out);
      zi51 <= zll_main_putreg3_out;
      zi52 <= (std_logic_vector'(B"00010000000000000000") & zi51);
      zi53 <= zi52(69 downto 0);
      \instR14\ : \ZLL_Main_loop23\ port map (zi53, zll_main_loop23_out);
      \instR15\ : \ZLL_Main_loop31\ port map (\__in0\, \__st0\, \zll_main_loop31_outR1\);
      \instR16\ : \ZLL_Main_loop31\ port map (\__in0\, \__st0\, \zll_main_loop31_outR2\);
      \instR17\ : \ZLL_Main_loop31\ port map (\__in0\, \__st0\, \zll_main_loop31_outR3\);
      \instR18\ : \ZLL_Main_loop31\ port map (\__in0\, \__st0\, \zll_main_loop31_outR4\);
      \instR19\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR3\);
      zi59 <= \main_putins_outR3\;
      zi60 <= (std_logic_vector'(B"00010000000000000000") & zi59);
      zi61 <= zi60(69 downto 0);
      \instR20\ : \Main_getPC\ port map (zi61, \main_getpc_outR2\);
      zi62 <= \main_getpc_outR2\;
      zi63 <= zi62(75 downto 70);
      zi64 <= zi62(69 downto 0);
      \instR21\ : \ZLL_Main_finishInstr1\ port map (zi63, zi64, \zll_main_finishinstr1_outR2\);
      zi65 <= \zll_main_finishinstr1_outR2\;
      zi66 <= (std_logic_vector'(B"00010000000000000000") & zi65);
      zi67 <= zi66(69 downto 0);
      \instR22\ : \Main_getOut\ port map (zi67, \main_getout_outR2\);
      zi68 <= \main_getout_outR2\;
      zi69 <= zi68(84 downto 70);
      zi70 <= zi68(69 downto 0);
      zi71 <= (std_logic_vector'(B"00001") & zi69 & zi70);
      zi72 <= zi71(84 downto 70);
      zi73 <= zi71(69 downto 0);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001")), (std_logic_vector'(B"1") & zi18 & std_logic_vector'(B"0101") & zi19), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010")), zll_main_loop31_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011")), (std_logic_vector'(B"1") & zi38 & std_logic_vector'(B"0100") & zi39), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100")), zll_main_loop23_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101")), \zll_main_loop31_outR1\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110")), \zll_main_loop31_outR2\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111")), \zll_main_loop31_outR3\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000")), \zll_main_loop31_outR4\, (std_logic_vector'(B"1") & zi72 & std_logic_vector'(B"0111") & zi73)))))))));
      \__resumption_tag_next\ <= zres(73 downto 70);
      \__st0_next\ <= zres(69 downto 0);
      \__out0\ <= zres(88 downto 74);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"0000");
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
entity \ZLL_Main_loop31\ is
port (arg0 : in std_logic_vector (16 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (89 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop31\ is
component \Main_putIns\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop23\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (89 downto 0));
      end component;
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zt0 : std_logic_vector (69 downto 0);
      signal zt1 : std_logic_vector (89 downto 0);
      signal s0 : std_logic_vector (69 downto 0);
      signal zll_main_loop23_out : std_logic_vector (89 downto 0);
begin
inst : \Main_putIns\ port map (arg0, arg1, main_putins_out);
      zt0 <= main_putins_out;
      zt1 <= (std_logic_vector'(B"00010000000000000000") & zt0);
      s0 <= zt1(69 downto 0);
      \instR1\ : \ZLL_Main_loop23\ port map (s0, zll_main_loop23_out);
      res <= zll_main_loop23_out;
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
entity \ZLL_Main_loop23\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (89 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop23\ is
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
      component \ZLL_Main_finishInstr1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_ld\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
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
      signal zi7 : std_logic_vector (89 downto 0);
      signal zi9 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zi10 : std_logic_vector (69 downto 0);
      signal zi11 : std_logic_vector (89 downto 0);
      signal zi12 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zi13 : std_logic_vector (75 downto 0);
      signal zi14 : std_logic_vector (5 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr1_out : std_logic_vector (69 downto 0);
      signal zi16 : std_logic_vector (69 downto 0);
      signal zi17 : std_logic_vector (89 downto 0);
      signal zi18 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zi19 : std_logic_vector (84 downto 0);
      signal zi20 : std_logic_vector (14 downto 0);
      signal zi21 : std_logic_vector (69 downto 0);
      signal zi22 : std_logic_vector (89 downto 0);
      signal zi23 : std_logic_vector (14 downto 0);
      signal zi24 : std_logic_vector (69 downto 0);
      signal zi25 : std_logic_vector (5 downto 0);
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zi26 : std_logic_vector (69 downto 0);
      signal zll_main_ld_out : std_logic_vector (69 downto 0);
      signal zi27 : std_logic_vector (69 downto 0);
      signal zi28 : std_logic_vector (89 downto 0);
      signal zi29 : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal zi30 : std_logic_vector (84 downto 0);
      signal zi31 : std_logic_vector (14 downto 0);
      signal zi32 : std_logic_vector (69 downto 0);
      signal zi33 : std_logic_vector (89 downto 0);
      signal zi34 : std_logic_vector (14 downto 0);
      signal zi35 : std_logic_vector (69 downto 0);
      signal zi36 : std_logic_vector (5 downto 0);
      signal main_getreg_out : std_logic_vector (77 downto 0);
      signal zi37 : std_logic_vector (77 downto 0);
      signal zi38 : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (69 downto 0);
      signal \main_putaddrout_outR1\ : std_logic_vector (69 downto 0);
      signal zi40 : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal zi41 : std_logic_vector (84 downto 0);
      signal zi43 : std_logic_vector (69 downto 0);
      signal zi44 : std_logic_vector (0 downto 0);
      signal zi45 : std_logic_vector (5 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
      signal zi46 : std_logic_vector (69 downto 0);
      signal \main_getout_outR3\ : std_logic_vector (84 downto 0);
      signal zi47 : std_logic_vector (84 downto 0);
      signal zi48 : std_logic_vector (14 downto 0);
      signal zi49 : std_logic_vector (69 downto 0);
      signal zll_main_putweout_out : std_logic_vector (69 downto 0);
      signal zi50 : std_logic_vector (69 downto 0);
      signal zi51 : std_logic_vector (89 downto 0);
      signal zi52 : std_logic_vector (69 downto 0);
      signal \main_getout_outR4\ : std_logic_vector (84 downto 0);
      signal zi53 : std_logic_vector (84 downto 0);
      signal zi54 : std_logic_vector (14 downto 0);
      signal zi55 : std_logic_vector (69 downto 0);
      signal zi56 : std_logic_vector (89 downto 0);
      signal zi57 : std_logic_vector (14 downto 0);
      signal zi58 : std_logic_vector (69 downto 0);
      signal zi59 : std_logic_vector (1 downto 0);
      signal zi60 : std_logic_vector (1 downto 0);
      signal zi61 : std_logic_vector (1 downto 0);
      signal main_getreg1_out : std_logic_vector (77 downto 0);
      signal zi62 : std_logic_vector (77 downto 0);
      signal zi63 : std_logic_vector (7 downto 0);
      signal zi64 : std_logic_vector (69 downto 0);
      signal \main_getreg1_outR1\ : std_logic_vector (77 downto 0);
      signal zi65 : std_logic_vector (77 downto 0);
      signal zi66 : std_logic_vector (7 downto 0);
      signal zi67 : std_logic_vector (69 downto 0);
      signal zi68 : std_logic_vector (7 downto 0);
      signal zll_main_putreg3_out : std_logic_vector (69 downto 0);
      signal zi69 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal zi70 : std_logic_vector (69 downto 0);
      signal zi71 : std_logic_vector (89 downto 0);
      signal zi72 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal zi73 : std_logic_vector (75 downto 0);
      signal zi74 : std_logic_vector (5 downto 0);
      signal zi75 : std_logic_vector (69 downto 0);
      signal \zll_main_finishinstr1_outR1\ : std_logic_vector (69 downto 0);
      signal zi76 : std_logic_vector (69 downto 0);
      signal zi77 : std_logic_vector (89 downto 0);
      signal zi78 : std_logic_vector (69 downto 0);
      signal \main_getout_outR5\ : std_logic_vector (84 downto 0);
      signal zi79 : std_logic_vector (84 downto 0);
      signal zi80 : std_logic_vector (14 downto 0);
      signal zi81 : std_logic_vector (69 downto 0);
      signal zi82 : std_logic_vector (89 downto 0);
      signal zi83 : std_logic_vector (14 downto 0);
      signal zi84 : std_logic_vector (69 downto 0);
      signal zi85 : std_logic_vector (5 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (77 downto 0);
      signal zi86 : std_logic_vector (77 downto 0);
      signal zi87 : std_logic_vector (7 downto 0);
      signal zi88 : std_logic_vector (69 downto 0);
      signal zi89 : std_logic_vector (0 downto 0);
      signal main_putpc1_out : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR2\ : std_logic_vector (69 downto 0);
      signal zi90 : std_logic_vector (69 downto 0);
      signal zi91 : std_logic_vector (89 downto 0);
      signal zi92 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal zi93 : std_logic_vector (75 downto 0);
      signal zi94 : std_logic_vector (5 downto 0);
      signal zi95 : std_logic_vector (69 downto 0);
      signal \zll_main_finishinstr1_outR2\ : std_logic_vector (69 downto 0);
      signal zi96 : std_logic_vector (69 downto 0);
      signal zi97 : std_logic_vector (89 downto 0);
      signal zi98 : std_logic_vector (69 downto 0);
      signal \main_getout_outR6\ : std_logic_vector (84 downto 0);
      signal zi99 : std_logic_vector (84 downto 0);
      signal zi100 : std_logic_vector (14 downto 0);
      signal zi101 : std_logic_vector (69 downto 0);
      signal zi102 : std_logic_vector (89 downto 0);
      signal zi103 : std_logic_vector (14 downto 0);
      signal zi104 : std_logic_vector (69 downto 0);
begin
inst : \Main_getIns\ port map (arg0, main_getins_out);
      zi0 <= main_getins_out;
      zi2 <= zi0(69 downto 0);
      zi3 <= zi0(86 downto 78);
      zi4 <= (zi3 & zi2);
      zi5 <= zi4(78 downto 70);
      zi6 <= zi4(69 downto 0);
      zi7 <= (std_logic_vector'(B"00000000000") & zi5 & zi6);
      zi9 <= zi7(69 downto 0);
      \instR1\ : \Main_incrPC\ port map (zi9, main_incrpc_out);
      zi10 <= main_incrpc_out;
      zi11 <= (std_logic_vector'(B"00010000000000000000") & zi10);
      zi12 <= zi11(69 downto 0);
      \instR2\ : \Main_getPC\ port map (zi12, main_getpc_out);
      zi13 <= main_getpc_out;
      zi14 <= zi13(75 downto 70);
      zi15 <= zi13(69 downto 0);
      \instR3\ : \ZLL_Main_finishInstr1\ port map (zi14, zi15, zll_main_finishinstr1_out);
      zi16 <= zll_main_finishinstr1_out;
      zi17 <= (std_logic_vector'(B"00010000000000000000") & zi16);
      zi18 <= zi17(69 downto 0);
      \instR4\ : \Main_getOut\ port map (zi18, main_getout_out);
      zi19 <= main_getout_out;
      zi20 <= zi19(84 downto 70);
      zi21 <= zi19(69 downto 0);
      zi22 <= (std_logic_vector'(B"00001") & zi20 & zi21);
      zi23 <= zi22(84 downto 70);
      zi24 <= zi22(69 downto 0);
      zi25 <= zi7(75 downto 70);
      \instR5\ : \Main_putAddrOut\ port map (zi25, zi9, main_putaddrout_out);
      zi26 <= main_putaddrout_out;
      \instR6\ : \ZLL_Main_ld\ port map (zi26, zll_main_ld_out);
      zi27 <= zll_main_ld_out;
      zi28 <= (std_logic_vector'(B"00010000000000000000") & zi27);
      zi29 <= zi28(69 downto 0);
      \instR7\ : \Main_getOut\ port map (zi29, \main_getout_outR1\);
      zi30 <= \main_getout_outR1\;
      zi31 <= zi30(84 downto 70);
      zi32 <= zi30(69 downto 0);
      zi33 <= (std_logic_vector'(B"00001") & zi31 & zi32);
      zi34 <= zi33(84 downto 70);
      zi35 <= zi33(69 downto 0);
      zi36 <= zi7(75 downto 70);
      \instR8\ : \Main_getReg\ port map (zi9, main_getreg_out);
      zi37 <= main_getreg_out;
      zi38 <= zi37(77 downto 70);
      zi39 <= zi37(69 downto 0);
      \instR9\ : \Main_putAddrOut\ port map (zi36, zi39, \main_putaddrout_outR1\);
      zi40 <= \main_putaddrout_outR1\;
      \instR10\ : \Main_getOut\ port map (zi40, \main_getout_outR2\);
      zi41 <= \main_getout_outR2\;
      zi43 <= zi41(69 downto 0);
      zi44 <= zi41(84 downto 84);
      zi45 <= zi41(83 downto 78);
      conn <= (zi44 & zi45 & zi38);
      \instR11\ : \Main_putOut\ port map (conn, zi43, main_putout_out);
      zi46 <= main_putout_out;
      \instR12\ : \Main_getOut\ port map (zi46, \main_getout_outR3\);
      zi47 <= \main_getout_outR3\;
      zi48 <= zi47(84 downto 70);
      zi49 <= zi47(69 downto 0);
      \instR13\ : \ZLL_Main_putWeOut\ port map (std_logic_vector'(B"1"), zi48, zi49, zll_main_putweout_out);
      zi50 <= zll_main_putweout_out;
      zi51 <= (std_logic_vector'(B"00010000000000000000") & zi50);
      zi52 <= zi51(69 downto 0);
      \instR14\ : \Main_getOut\ port map (zi52, \main_getout_outR4\);
      zi53 <= \main_getout_outR4\;
      zi54 <= zi53(84 downto 70);
      zi55 <= zi53(69 downto 0);
      zi56 <= (std_logic_vector'(B"00001") & zi54 & zi55);
      zi57 <= zi56(84 downto 70);
      zi58 <= zi56(69 downto 0);
      zi59 <= zi7(75 downto 74);
      zi60 <= zi7(73 downto 72);
      zi61 <= zi7(71 downto 70);
      \instR15\ : \Main_getReg1\ port map (zi60, zi9, main_getreg1_out);
      zi62 <= main_getreg1_out;
      zi63 <= zi62(77 downto 70);
      zi64 <= zi62(69 downto 0);
      \instR16\ : \Main_getReg1\ port map (zi61, zi64, \main_getreg1_outR1\);
      zi65 <= \main_getreg1_outR1\;
      zi66 <= zi65(77 downto 70);
      zi67 <= zi65(69 downto 0);
      zi68 <= rw_not(rw_and(zi63, zi66));
      \instR17\ : \ZLL_Main_putReg3\ port map (zi68, zi59, zi67, zll_main_putreg3_out);
      zi69 <= zll_main_putreg3_out;
      \instR18\ : \Main_incrPC\ port map (zi69, \main_incrpc_outR1\);
      zi70 <= \main_incrpc_outR1\;
      zi71 <= (std_logic_vector'(B"00010000000000000000") & zi70);
      zi72 <= zi71(69 downto 0);
      \instR19\ : \Main_getPC\ port map (zi72, \main_getpc_outR1\);
      zi73 <= \main_getpc_outR1\;
      zi74 <= zi73(75 downto 70);
      zi75 <= zi73(69 downto 0);
      \instR20\ : \ZLL_Main_finishInstr1\ port map (zi74, zi75, \zll_main_finishinstr1_outR1\);
      zi76 <= \zll_main_finishinstr1_outR1\;
      zi77 <= (std_logic_vector'(B"00010000000000000000") & zi76);
      zi78 <= zi77(69 downto 0);
      \instR21\ : \Main_getOut\ port map (zi78, \main_getout_outR5\);
      zi79 <= \main_getout_outR5\;
      zi80 <= zi79(84 downto 70);
      zi81 <= zi79(69 downto 0);
      zi82 <= (std_logic_vector'(B"00001") & zi80 & zi81);
      zi83 <= zi82(84 downto 70);
      zi84 <= zi82(69 downto 0);
      zi85 <= zi7(75 downto 70);
      \instR22\ : \Main_getReg\ port map (zi9, \main_getreg_outR1\);
      zi86 <= \main_getreg_outR1\;
      zi87 <= zi86(77 downto 70);
      zi88 <= zi86(69 downto 0);
      zi89 <= rw_eq(zi87, std_logic_vector'(B"00000000"));
      \instR23\ : \Main_putPC1\ port map (zi85, zi88, main_putpc1_out);
      \instR24\ : \Main_incrPC\ port map (zi88, \main_incrpc_outR2\);
      zi90 <= rw_cond(rw_eq(zi89, std_logic_vector'(B"0")), main_putpc1_out, \main_incrpc_outR2\);
      zi91 <= (std_logic_vector'(B"00010000000000000000") & zi90);
      zi92 <= zi91(69 downto 0);
      \instR25\ : \Main_getPC\ port map (zi92, \main_getpc_outR2\);
      zi93 <= \main_getpc_outR2\;
      zi94 <= zi93(75 downto 70);
      zi95 <= zi93(69 downto 0);
      \instR26\ : \ZLL_Main_finishInstr1\ port map (zi94, zi95, \zll_main_finishinstr1_outR2\);
      zi96 <= \zll_main_finishinstr1_outR2\;
      zi97 <= (std_logic_vector'(B"00010000000000000000") & zi96);
      zi98 <= zi97(69 downto 0);
      \instR27\ : \Main_getOut\ port map (zi98, \main_getout_outR6\);
      zi99 <= \main_getout_outR6\;
      zi100 <= zi99(84 downto 70);
      zi101 <= zi99(69 downto 0);
      zi102 <= (std_logic_vector'(B"00001") & zi100 & zi101);
      zi103 <= zi102(84 downto 70);
      zi104 <= zi102(69 downto 0);
      res <= rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"000")), (std_logic_vector'(B"1") & zi23 & std_logic_vector'(B"0010") & zi24), rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"001")), (std_logic_vector'(B"1") & zi34 & std_logic_vector'(B"0011") & zi35), rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"010")), (std_logic_vector'(B"1") & zi57 & std_logic_vector'(B"0001") & zi58), rw_cond(rw_eq(zi7(78 downto 76), std_logic_vector'(B"011")), (std_logic_vector'(B"1") & zi83 & std_logic_vector'(B"1000") & zi84), (std_logic_vector'(B"1") & zi103 & std_logic_vector'(B"0110") & zi104)))));
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
entity \ZLL_Main_ld\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_ld\ is
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
      component \ZLL_Main_ld\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zt0 : std_logic_vector (69 downto 0);
      signal zll_main_ld_out : std_logic_vector (69 downto 0);
begin
inst : \Main_putAddrOut\ port map (arg0, arg1, main_putaddrout_out);
      zt0 <= main_putaddrout_out;
      \instR1\ : \ZLL_Main_ld\ port map (zt0, zll_main_ld_out);
      res <= zll_main_ld_out;
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