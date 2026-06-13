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
      component \ZLL_Main_loop163\ is
      port (arg0 : in std_logic_vector (142 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop217\ is
      port (arg0 : in std_logic_vector (84 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop218\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_putReg67\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (9 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_reset32\ is
      port (arg0 : in std_logic_vector (75 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Pure_dispatch7\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (3 downto 0) := std_logic_vector'(B"0001");
      signal \__resumption_tag_next\ : std_logic_vector (3 downto 0);
      signal \__st0\ : std_logic_vector (69 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zll_main_loop218_out : std_logic_vector (142 downto 0);
      signal zi2 : std_logic_vector (142 downto 0);
      signal zi3 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zll_main_reset32_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR1\ : std_logic_vector (142 downto 0);
      signal zi4 : std_logic_vector (142 downto 0);
      signal zi5 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_loop217_out : std_logic_vector (142 downto 0);
      signal zi6 : std_logic_vector (142 downto 0);
      signal zi7 : std_logic_vector (14 downto 0);
      signal zi8 : std_logic_vector (69 downto 0);
      signal \main_putins_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR2\ : std_logic_vector (142 downto 0);
      signal zi10 : std_logic_vector (142 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR3\ : std_logic_vector (142 downto 0);
      signal zi12 : std_logic_vector (142 downto 0);
      signal zi13 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_reset32_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR4\ : std_logic_vector (142 downto 0);
      signal zi14 : std_logic_vector (142 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop217_outR1\ : std_logic_vector (142 downto 0);
      signal zi16 : std_logic_vector (142 downto 0);
      signal zi17 : std_logic_vector (14 downto 0);
      signal zi18 : std_logic_vector (69 downto 0);
      signal zll_pure_dispatch7_out : std_logic_vector (142 downto 0);
      signal \zll_pure_dispatch7_outR1\ : std_logic_vector (142 downto 0);
      signal \zll_pure_dispatch7_outR2\ : std_logic_vector (142 downto 0);
      signal \main_putins_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR5\ : std_logic_vector (142 downto 0);
      signal zi20 : std_logic_vector (142 downto 0);
      signal zi21 : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi22 : std_logic_vector (86 downto 0);
      signal zi24 : std_logic_vector (69 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (77 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal zi29 : std_logic_vector (69 downto 0);
      signal conn : std_logic_vector (9 downto 0);
      signal zll_main_putreg67_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR6\ : std_logic_vector (142 downto 0);
      signal zll_main_loop163_out : std_logic_vector (142 downto 0);
      signal \zll_pure_dispatch7_outR3\ : std_logic_vector (142 downto 0);
      signal \main_putins_outR3\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR7\ : std_logic_vector (142 downto 0);
      signal zi31 : std_logic_vector (142 downto 0);
      signal zi32 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR8\ : std_logic_vector (142 downto 0);
      signal zi33 : std_logic_vector (142 downto 0);
      signal zi34 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_reset32_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR9\ : std_logic_vector (142 downto 0);
      signal zi35 : std_logic_vector (142 downto 0);
      signal zi36 : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop217_outR2\ : std_logic_vector (142 downto 0);
      signal zi37 : std_logic_vector (142 downto 0);
      signal zi38 : std_logic_vector (14 downto 0);
      signal zi39 : std_logic_vector (69 downto 0);
      signal \zll_pure_dispatch7_outR4\ : std_logic_vector (142 downto 0);
      signal zres : std_logic_vector (142 downto 0);
begin
inst : \Main_putIns\ port map (\__in0\, \__st0\, main_putins_out);
      \instR1\ : \ZLL_Main_loop218\ port map (main_putins_out, zll_main_loop218_out);
      zi2 <= zll_main_loop218_out;
      zi3 <= zi2(69 downto 0);
      \instR2\ : \Main_getPC\ port map (zi3, main_getpc_out);
      \instR3\ : \ZLL_Main_reset32\ port map (main_getpc_out, zll_main_reset32_out);
      \instR4\ : \ZLL_Main_loop218\ port map (zll_main_reset32_out, \zll_main_loop218_outR1\);
      zi4 <= \zll_main_loop218_outR1\;
      zi5 <= zi4(69 downto 0);
      \instR5\ : \Main_getOut\ port map (zi5, main_getout_out);
      \instR6\ : \ZLL_Main_loop217\ port map (main_getout_out, zll_main_loop217_out);
      zi6 <= zll_main_loop217_out;
      zi7 <= zi6(84 downto 70);
      zi8 <= zi6(69 downto 0);
      \instR7\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR1\);
      \instR8\ : \ZLL_Main_loop218\ port map (\main_putins_outR1\, \zll_main_loop218_outR2\);
      zi10 <= \zll_main_loop218_outR2\;
      zi11 <= zi10(69 downto 0);
      \instR9\ : \Main_incrPC\ port map (zi11, main_incrpc_out);
      \instR10\ : \ZLL_Main_loop218\ port map (main_incrpc_out, \zll_main_loop218_outR3\);
      zi12 <= \zll_main_loop218_outR3\;
      zi13 <= zi12(69 downto 0);
      \instR11\ : \Main_getPC\ port map (zi13, \main_getpc_outR1\);
      \instR12\ : \ZLL_Main_reset32\ port map (\main_getpc_outR1\, \zll_main_reset32_outR1\);
      \instR13\ : \ZLL_Main_loop218\ port map (\zll_main_reset32_outR1\, \zll_main_loop218_outR4\);
      zi14 <= \zll_main_loop218_outR4\;
      zi15 <= zi14(69 downto 0);
      \instR14\ : \Main_getOut\ port map (zi15, \main_getout_outR1\);
      \instR15\ : \ZLL_Main_loop217\ port map (\main_getout_outR1\, \zll_main_loop217_outR1\);
      zi16 <= \zll_main_loop217_outR1\;
      zi17 <= zi16(84 downto 70);
      zi18 <= zi16(69 downto 0);
      \instR16\ : \ZLL_Pure_dispatch7\ port map (\__in0\, \__st0\, zll_pure_dispatch7_out);
      \instR17\ : \ZLL_Pure_dispatch7\ port map (\__in0\, \__st0\, \zll_pure_dispatch7_outR1\);
      \instR18\ : \ZLL_Pure_dispatch7\ port map (\__in0\, \__st0\, \zll_pure_dispatch7_outR2\);
      \instR19\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR2\);
      \instR20\ : \ZLL_Main_loop218\ port map (\main_putins_outR2\, \zll_main_loop218_outR5\);
      zi20 <= \zll_main_loop218_outR5\;
      zi21 <= zi20(69 downto 0);
      \instR21\ : \Main_getIns\ port map (zi21, main_getins_out);
      zi22 <= main_getins_out;
      zi24 <= zi22(69 downto 0);
      zi26 <= zi22(77 downto 70);
      zi27 <= (zi26 & zi24);
      zi28 <= zi27(77 downto 70);
      zi29 <= zi27(69 downto 0);
      conn <= (std_logic_vector'(B"00") & zi28);
      \instR22\ : \ZLL_Main_putReg67\ port map (zi28, std_logic_vector'(B"00"), conn, zi29, zll_main_putreg67_out);
      \instR23\ : \ZLL_Main_loop218\ port map (zll_main_putreg67_out, \zll_main_loop218_outR6\);
      \instR24\ : \ZLL_Main_loop163\ port map (\zll_main_loop218_outR6\, zll_main_loop163_out);
      \instR25\ : \ZLL_Pure_dispatch7\ port map (\__in0\, \__st0\, \zll_pure_dispatch7_outR3\);
      \instR26\ : \Main_putIns\ port map (\__in0\, \__st0\, \main_putins_outR3\);
      \instR27\ : \ZLL_Main_loop218\ port map (\main_putins_outR3\, \zll_main_loop218_outR7\);
      zi31 <= \zll_main_loop218_outR7\;
      zi32 <= zi31(69 downto 0);
      \instR28\ : \Main_incrPC\ port map (zi32, \main_incrpc_outR1\);
      \instR29\ : \ZLL_Main_loop218\ port map (\main_incrpc_outR1\, \zll_main_loop218_outR8\);
      zi33 <= \zll_main_loop218_outR8\;
      zi34 <= zi33(69 downto 0);
      \instR30\ : \Main_getPC\ port map (zi34, \main_getpc_outR2\);
      \instR31\ : \ZLL_Main_reset32\ port map (\main_getpc_outR2\, \zll_main_reset32_outR2\);
      \instR32\ : \ZLL_Main_loop218\ port map (\zll_main_reset32_outR2\, \zll_main_loop218_outR9\);
      zi35 <= \zll_main_loop218_outR9\;
      zi36 <= zi35(69 downto 0);
      \instR33\ : \Main_getOut\ port map (zi36, \main_getout_outR2\);
      \instR34\ : \ZLL_Main_loop217\ port map (\main_getout_outR2\, \zll_main_loop217_outR2\);
      zi37 <= \zll_main_loop217_outR2\;
      zi38 <= zi37(84 downto 70);
      zi39 <= zi37(69 downto 0);
      \instR35\ : \ZLL_Pure_dispatch7\ port map (\__in0\, \__st0\, \zll_pure_dispatch7_outR4\);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi7 & std_logic_vector'(B"0011") & zi8), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi17 & std_logic_vector'(B"0110") & zi18), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011")), zll_pure_dispatch7_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100")), \zll_pure_dispatch7_outR1\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101")), \zll_pure_dispatch7_outR2\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110")), zll_main_loop163_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111")), \zll_pure_dispatch7_outR3\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi38 & std_logic_vector'(B"0101") & zi39), \zll_pure_dispatch7_outR4\))))))));
      \__resumption_tag_next\ <= zres(73 downto 70);
      \__st0_next\ <= zres(69 downto 0);
      \__out0\ <= zres(88 downto 74);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"0001");
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
entity \ZLL_Main_putReg67\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (9 downto 0);
      arg3 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putReg67\ is
component \ZLL_Main_putReg48\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            arg4 : in std_logic_vector (7 downto 0);
            arg5 : in std_logic_vector (16 downto 0);
            arg6 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (5 downto 0);
      signal zi9 : std_logic_vector (16 downto 0);
      signal zi10 : std_logic_vector (14 downto 0);
      signal zll_main_putreg48_out : std_logic_vector (69 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (5 downto 0);
      signal zi21 : std_logic_vector (16 downto 0);
      signal zi22 : std_logic_vector (14 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal zi29 : std_logic_vector (7 downto 0);
      signal zi31 : std_logic_vector (7 downto 0);
      signal zi32 : std_logic_vector (5 downto 0);
      signal zi33 : std_logic_vector (16 downto 0);
      signal zi34 : std_logic_vector (14 downto 0);
      signal zi40 : std_logic_vector (7 downto 0);
      signal zi41 : std_logic_vector (7 downto 0);
      signal zi42 : std_logic_vector (7 downto 0);
      signal zi44 : std_logic_vector (5 downto 0);
      signal zi45 : std_logic_vector (16 downto 0);
      signal zi46 : std_logic_vector (14 downto 0);
begin
zi0 <= arg2(7 downto 0);
      zi5 <= arg3(61 downto 54);
      zi6 <= arg3(53 downto 46);
      zi7 <= arg3(45 downto 38);
      zi8 <= arg3(37 downto 32);
      zi9 <= arg3(31 downto 15);
      zi10 <= arg3(14 downto 0);
      inst : \ZLL_Main_putReg48\ port map (zi8, zi0, zi7, zi5, zi6, zi9, zi10, zll_main_putreg48_out);
      zi16 <= arg3(69 downto 62);
      zi18 <= arg3(53 downto 46);
      zi19 <= arg3(45 downto 38);
      zi20 <= arg3(37 downto 32);
      zi21 <= arg3(31 downto 15);
      zi22 <= arg3(14 downto 0);
      zi28 <= arg3(69 downto 62);
      zi29 <= arg3(61 downto 54);
      zi31 <= arg3(45 downto 38);
      zi32 <= arg3(37 downto 32);
      zi33 <= arg3(31 downto 15);
      zi34 <= arg3(14 downto 0);
      zi40 <= arg3(69 downto 62);
      zi41 <= arg3(61 downto 54);
      zi42 <= arg3(53 downto 46);
      zi44 <= arg3(37 downto 32);
      zi45 <= arg3(31 downto 15);
      zi46 <= arg3(14 downto 0);
      res <= rw_cond(rw_eq(arg2(9 downto 8), std_logic_vector'(B"00")), zll_main_putreg48_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), (zi16 & arg0 & zi18 & zi19 & zi20 & zi21 & zi22), rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), (zi28 & zi29 & arg0 & zi31 & zi32 & zi33 & zi34), (zi40 & zi41 & zi42 & arg0 & zi44 & zi45 & zi46))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop218\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop218\ is

begin
res <= ((std_logic_vector'(B"01") & rw_repl(71, std_logic_vector'(B"0"))) & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_getReg23\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \ZLL_Main_getReg23\ is
component \ZLL_Main_r05\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (5 downto 0);
            arg4 : in std_logic_vector (16 downto 0);
            arg5 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_r15\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (5 downto 0);
            arg3 : in std_logic_vector (16 downto 0);
            arg4 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_r37\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (5 downto 0);
            arg2 : in std_logic_vector (16 downto 0);
            arg3 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (5 downto 0);
      signal zi8 : std_logic_vector (16 downto 0);
      signal zi9 : std_logic_vector (14 downto 0);
      signal zll_main_r05_out : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal zi15 : std_logic_vector (7 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (5 downto 0);
      signal zi18 : std_logic_vector (16 downto 0);
      signal zi19 : std_logic_vector (14 downto 0);
      signal \zll_main_r05_outR1\ : std_logic_vector (7 downto 0);
      signal zi25 : std_logic_vector (7 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (5 downto 0);
      signal zi28 : std_logic_vector (16 downto 0);
      signal zi29 : std_logic_vector (14 downto 0);
      signal zll_main_r15_out : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (7 downto 0);
      signal zi37 : std_logic_vector (5 downto 0);
      signal zi38 : std_logic_vector (16 downto 0);
      signal zi39 : std_logic_vector (14 downto 0);
      signal zll_main_r37_out : std_logic_vector (7 downto 0);
begin
zi3 <= arg2(69 downto 62);
      zi5 <= arg2(53 downto 46);
      zi6 <= arg2(45 downto 38);
      zi7 <= arg2(37 downto 32);
      zi8 <= arg2(31 downto 15);
      zi9 <= arg2(14 downto 0);
      inst : \ZLL_Main_r05\ port map (zi3, zi5, zi6, zi7, zi8, zi9, zll_main_r05_out);
      zi14 <= arg2(61 downto 54);
      zi15 <= arg2(53 downto 46);
      zi16 <= arg2(45 downto 38);
      zi17 <= arg2(37 downto 32);
      zi18 <= arg2(31 downto 15);
      zi19 <= arg2(14 downto 0);
      \instR1\ : \ZLL_Main_r05\ port map (zi14, zi15, zi16, zi17, zi18, zi19, \zll_main_r05_outR1\);
      zi25 <= arg2(53 downto 46);
      zi26 <= arg2(45 downto 38);
      zi27 <= arg2(37 downto 32);
      zi28 <= arg2(31 downto 15);
      zi29 <= arg2(14 downto 0);
      \instR2\ : \ZLL_Main_r15\ port map (zi25, zi26, zi27, zi28, zi29, zll_main_r15_out);
      zi36 <= arg2(45 downto 38);
      zi37 <= arg2(37 downto 32);
      zi38 <= arg2(31 downto 15);
      zi39 <= arg2(14 downto 0);
      \instR3\ : \ZLL_Main_r37\ port map (zi36, zi37, zi38, zi39, zll_main_r37_out);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), (zll_main_r05_out & arg2), rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), (\zll_main_r05_outR1\ & arg2), rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), (zll_main_r15_out & arg2), (zll_main_r37_out & arg2))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop217\ is
port (arg0 : in std_logic_vector (84 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop217\ is
signal zi0 : std_logic_vector (14 downto 0);
      signal zi1 : std_logic_vector (69 downto 0);
begin
zi0 <= arg0(84 downto 70);
      zi1 <= arg0(69 downto 0);
      res <= ((std_logic_vector'(B"001") & rw_repl(55, std_logic_vector'(B"0"))) & zi0 & zi1);
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
component \ZLL_Main_putReg48\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            arg4 : in std_logic_vector (7 downto 0);
            arg5 : in std_logic_vector (16 downto 0);
            arg6 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (16 downto 0);
      signal zi9 : std_logic_vector (14 downto 0);
      signal zll_main_putreg48_out : std_logic_vector (69 downto 0);
begin
zi3 <= arg1(69 downto 62);
      zi4 <= arg1(61 downto 54);
      zi5 <= arg1(53 downto 46);
      zi6 <= arg1(45 downto 38);
      zi8 <= arg1(31 downto 15);
      zi9 <= arg1(14 downto 0);
      inst : \ZLL_Main_putReg48\ port map (arg0, zi3, zi6, zi4, zi5, zi8, zi9, zll_main_putreg48_out);
      res <= zll_main_putreg48_out;
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
signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (5 downto 0);
      signal zi9 : std_logic_vector (14 downto 0);
begin
zi3 <= arg1(69 downto 62);
      zi4 <= arg1(61 downto 54);
      zi5 <= arg1(53 downto 46);
      zi6 <= arg1(45 downto 38);
      zi7 <= arg1(37 downto 32);
      zi9 <= arg1(14 downto 0);
      res <= (zi3 & zi4 & zi5 & zi6 & zi7 & arg0 & zi9);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_r37\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (5 downto 0);
      arg2 : in std_logic_vector (16 downto 0);
      arg3 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_r37\ is

begin
res <= arg0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_r15\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (5 downto 0);
      arg3 : in std_logic_vector (16 downto 0);
      arg4 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_r15\ is
component \ZLL_Main_r37\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (5 downto 0);
            arg2 : in std_logic_vector (16 downto 0);
            arg3 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zll_main_r37_out : std_logic_vector (7 downto 0);
begin
inst : \ZLL_Main_r37\ port map (arg0, arg2, arg3, arg4, zll_main_r37_out);
      res <= zll_main_r37_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Pure_dispatch7\ is
port (arg0 : in std_logic_vector (16 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Pure_dispatch7\ is
component \Main_putIns\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop163\ is
      port (arg0 : in std_logic_vector (142 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop218\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zll_main_loop218_out : std_logic_vector (142 downto 0);
      signal zll_main_loop163_out : std_logic_vector (142 downto 0);
begin
inst : \Main_putIns\ port map (arg0, arg1, main_putins_out);
      \instR1\ : \ZLL_Main_loop218\ port map (main_putins_out, zll_main_loop218_out);
      \instR2\ : \ZLL_Main_loop163\ port map (zll_main_loop218_out, zll_main_loop163_out);
      res <= zll_main_loop163_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putReg48\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      arg4 : in std_logic_vector (7 downto 0);
      arg5 : in std_logic_vector (16 downto 0);
      arg6 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putReg48\ is

begin
res <= (arg1 & arg3 & arg4 & arg2 & arg0 & arg5 & arg6);
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
component \ZLL_Main_getReg23\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg23_out : std_logic_vector (77 downto 0);
begin
inst : \ZLL_Main_getReg23\ port map (arg0, arg0, arg1, zll_main_getreg23_out);
      res <= zll_main_getreg23_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_reset32\ is
port (arg0 : in std_logic_vector (75 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_reset32\ is
component \Main_putAddrOut\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_finishInstr3\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zi0 : std_logic_vector (5 downto 0);
      signal zi1 : std_logic_vector (69 downto 0);
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr3_out : std_logic_vector (69 downto 0);
begin
zi0 <= arg0(75 downto 70);
      zi1 <= arg0(69 downto 0);
      inst : \Main_putAddrOut\ port map (zi0, zi1, main_putaddrout_out);
      \instR1\ : \ZLL_Main_finishInstr3\ port map (main_putaddrout_out, zll_main_finishinstr3_out);
      res <= zll_main_finishinstr3_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_finishInstr3\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_finishInstr3\ is
component \ZLL_Main_putWeOut4\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zll_main_putweout4_out : std_logic_vector (69 downto 0);
begin
inst : \ZLL_Main_putWeOut4\ port map (std_logic_vector'(B"0"), arg0, zll_main_putweout4_out);
      res <= zll_main_putweout4_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop163\ is
port (arg0 : in std_logic_vector (142 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop163\ is
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
      component \ZLL_Main_finishInstr3\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop217\ is
      port (arg0 : in std_logic_vector (84 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop218\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_putReg67\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (9 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putWeOut4\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_reset32\ is
      port (arg0 : in std_logic_vector (75 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zi0 : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi1 : std_logic_vector (86 downto 0);
      signal zi3 : std_logic_vector (69 downto 0);
      signal zi4 : std_logic_vector (8 downto 0);
      signal zi6 : std_logic_vector (78 downto 0);
      signal zi7 : std_logic_vector (8 downto 0);
      signal zi8 : std_logic_vector (69 downto 0);
      signal zi9 : std_logic_vector (142 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zll_main_loop218_out : std_logic_vector (142 downto 0);
      signal zi18 : std_logic_vector (142 downto 0);
      signal zi19 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zll_main_reset32_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR1\ : std_logic_vector (142 downto 0);
      signal zi20 : std_logic_vector (142 downto 0);
      signal zi21 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_loop217_out : std_logic_vector (142 downto 0);
      signal zi22 : std_logic_vector (142 downto 0);
      signal zi23 : std_logic_vector (14 downto 0);
      signal zi24 : std_logic_vector (69 downto 0);
      signal zi25 : std_logic_vector (5 downto 0);
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr3_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR2\ : std_logic_vector (142 downto 0);
      signal zi26 : std_logic_vector (142 downto 0);
      signal zi27 : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop217_outR1\ : std_logic_vector (142 downto 0);
      signal zi28 : std_logic_vector (142 downto 0);
      signal zi29 : std_logic_vector (14 downto 0);
      signal zi30 : std_logic_vector (69 downto 0);
      signal zi31 : std_logic_vector (5 downto 0);
      signal main_getreg_out : std_logic_vector (77 downto 0);
      signal zi32 : std_logic_vector (77 downto 0);
      signal zi33 : std_logic_vector (7 downto 0);
      signal zi34 : std_logic_vector (69 downto 0);
      signal \main_putaddrout_outR1\ : std_logic_vector (69 downto 0);
      signal zi35 : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal zi36 : std_logic_vector (84 downto 0);
      signal zi38 : std_logic_vector (69 downto 0);
      signal zi39 : std_logic_vector (0 downto 0);
      signal zi40 : std_logic_vector (5 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
      signal zi42 : std_logic_vector (69 downto 0);
      signal zll_main_putweout4_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR3\ : std_logic_vector (142 downto 0);
      signal zi43 : std_logic_vector (142 downto 0);
      signal zi44 : std_logic_vector (69 downto 0);
      signal \main_getout_outR3\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop217_outR2\ : std_logic_vector (142 downto 0);
      signal zi45 : std_logic_vector (142 downto 0);
      signal zi46 : std_logic_vector (14 downto 0);
      signal zi47 : std_logic_vector (69 downto 0);
      signal zi48 : std_logic_vector (1 downto 0);
      signal zi49 : std_logic_vector (1 downto 0);
      signal zi50 : std_logic_vector (1 downto 0);
      signal main_getreg1_out : std_logic_vector (77 downto 0);
      signal zi51 : std_logic_vector (77 downto 0);
      signal zi52 : std_logic_vector (7 downto 0);
      signal zi53 : std_logic_vector (69 downto 0);
      signal \main_getreg1_outR1\ : std_logic_vector (77 downto 0);
      signal zi54 : std_logic_vector (77 downto 0);
      signal zi55 : std_logic_vector (7 downto 0);
      signal zi56 : std_logic_vector (69 downto 0);
      signal zi57 : std_logic_vector (7 downto 0);
      signal \connR1\ : std_logic_vector (9 downto 0);
      signal zll_main_putreg67_out : std_logic_vector (69 downto 0);
      signal zi58 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR4\ : std_logic_vector (142 downto 0);
      signal zi59 : std_logic_vector (142 downto 0);
      signal zi60 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_reset32_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR5\ : std_logic_vector (142 downto 0);
      signal zi61 : std_logic_vector (142 downto 0);
      signal zi62 : std_logic_vector (69 downto 0);
      signal \main_getout_outR4\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop217_outR3\ : std_logic_vector (142 downto 0);
      signal zi63 : std_logic_vector (142 downto 0);
      signal zi64 : std_logic_vector (14 downto 0);
      signal zi65 : std_logic_vector (69 downto 0);
      signal zi66 : std_logic_vector (5 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (77 downto 0);
      signal zi67 : std_logic_vector (77 downto 0);
      signal zi68 : std_logic_vector (7 downto 0);
      signal zi69 : std_logic_vector (69 downto 0);
      signal zi70 : std_logic_vector (0 downto 0);
      signal \main_incrpc_outR2\ : std_logic_vector (69 downto 0);
      signal main_putpc1_out : std_logic_vector (69 downto 0);
      signal \connR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR6\ : std_logic_vector (142 downto 0);
      signal zi72 : std_logic_vector (142 downto 0);
      signal zi73 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_reset32_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop218_outR7\ : std_logic_vector (142 downto 0);
      signal zi74 : std_logic_vector (142 downto 0);
      signal zi75 : std_logic_vector (69 downto 0);
      signal \main_getout_outR5\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop217_outR4\ : std_logic_vector (142 downto 0);
      signal zi76 : std_logic_vector (142 downto 0);
      signal zi77 : std_logic_vector (14 downto 0);
      signal zi78 : std_logic_vector (69 downto 0);
begin
zi0 <= arg0(69 downto 0);
      inst : \Main_getIns\ port map (zi0, main_getins_out);
      zi1 <= main_getins_out;
      zi3 <= zi1(69 downto 0);
      zi4 <= zi1(86 downto 78);
      zi6 <= (zi4 & zi3);
      zi7 <= zi6(78 downto 70);
      zi8 <= zi6(69 downto 0);
      zi9 <= (rw_repl(64, std_logic_vector'(B"0")) & zi7 & zi8);
      zi11 <= zi9(69 downto 0);
      \instR1\ : \Main_incrPC\ port map (zi11, main_incrpc_out);
      \instR2\ : \ZLL_Main_loop218\ port map (main_incrpc_out, zll_main_loop218_out);
      zi18 <= zll_main_loop218_out;
      zi19 <= zi18(69 downto 0);
      \instR3\ : \Main_getPC\ port map (zi19, main_getpc_out);
      \instR4\ : \ZLL_Main_reset32\ port map (main_getpc_out, zll_main_reset32_out);
      \instR5\ : \ZLL_Main_loop218\ port map (zll_main_reset32_out, \zll_main_loop218_outR1\);
      zi20 <= \zll_main_loop218_outR1\;
      zi21 <= zi20(69 downto 0);
      \instR6\ : \Main_getOut\ port map (zi21, main_getout_out);
      \instR7\ : \ZLL_Main_loop217\ port map (main_getout_out, zll_main_loop217_out);
      zi22 <= zll_main_loop217_out;
      zi23 <= zi22(84 downto 70);
      zi24 <= zi22(69 downto 0);
      zi25 <= zi9(75 downto 70);
      \instR8\ : \Main_putAddrOut\ port map (zi25, zi11, main_putaddrout_out);
      \instR9\ : \ZLL_Main_finishInstr3\ port map (main_putaddrout_out, zll_main_finishinstr3_out);
      \instR10\ : \ZLL_Main_loop218\ port map (zll_main_finishinstr3_out, \zll_main_loop218_outR2\);
      zi26 <= \zll_main_loop218_outR2\;
      zi27 <= zi26(69 downto 0);
      \instR11\ : \Main_getOut\ port map (zi27, \main_getout_outR1\);
      \instR12\ : \ZLL_Main_loop217\ port map (\main_getout_outR1\, \zll_main_loop217_outR1\);
      zi28 <= \zll_main_loop217_outR1\;
      zi29 <= zi28(84 downto 70);
      zi30 <= zi28(69 downto 0);
      zi31 <= zi9(75 downto 70);
      \instR13\ : \Main_getReg\ port map (zi11, main_getreg_out);
      zi32 <= main_getreg_out;
      zi33 <= zi32(77 downto 70);
      zi34 <= zi32(69 downto 0);
      \instR14\ : \Main_putAddrOut\ port map (zi31, zi34, \main_putaddrout_outR1\);
      zi35 <= \main_putaddrout_outR1\;
      \instR15\ : \Main_getOut\ port map (zi35, \main_getout_outR2\);
      zi36 <= \main_getout_outR2\;
      zi38 <= zi36(69 downto 0);
      zi39 <= zi36(84 downto 84);
      zi40 <= zi36(83 downto 78);
      conn <= (zi39 & zi40 & zi33);
      \instR16\ : \Main_putOut\ port map (conn, zi38, main_putout_out);
      zi42 <= main_putout_out;
      \instR17\ : \ZLL_Main_putWeOut4\ port map (std_logic_vector'(B"1"), zi42, zll_main_putweout4_out);
      \instR18\ : \ZLL_Main_loop218\ port map (zll_main_putweout4_out, \zll_main_loop218_outR3\);
      zi43 <= \zll_main_loop218_outR3\;
      zi44 <= zi43(69 downto 0);
      \instR19\ : \Main_getOut\ port map (zi44, \main_getout_outR3\);
      \instR20\ : \ZLL_Main_loop217\ port map (\main_getout_outR3\, \zll_main_loop217_outR2\);
      zi45 <= \zll_main_loop217_outR2\;
      zi46 <= zi45(84 downto 70);
      zi47 <= zi45(69 downto 0);
      zi48 <= zi9(75 downto 74);
      zi49 <= zi9(73 downto 72);
      zi50 <= zi9(71 downto 70);
      \instR21\ : \Main_getReg1\ port map (zi49, zi11, main_getreg1_out);
      zi51 <= main_getreg1_out;
      zi52 <= zi51(77 downto 70);
      zi53 <= zi51(69 downto 0);
      \instR22\ : \Main_getReg1\ port map (zi50, zi53, \main_getreg1_outR1\);
      zi54 <= \main_getreg1_outR1\;
      zi55 <= zi54(77 downto 70);
      zi56 <= zi54(69 downto 0);
      zi57 <= rw_not(rw_and(zi52, zi55));
      \connR1\ <= (zi48 & zi57);
      \instR23\ : \ZLL_Main_putReg67\ port map (zi57, zi48, \connR1\, zi56, zll_main_putreg67_out);
      zi58 <= zll_main_putreg67_out;
      \instR24\ : \Main_incrPC\ port map (zi58, \main_incrpc_outR1\);
      \instR25\ : \ZLL_Main_loop218\ port map (\main_incrpc_outR1\, \zll_main_loop218_outR4\);
      zi59 <= \zll_main_loop218_outR4\;
      zi60 <= zi59(69 downto 0);
      \instR26\ : \Main_getPC\ port map (zi60, \main_getpc_outR1\);
      \instR27\ : \ZLL_Main_reset32\ port map (\main_getpc_outR1\, \zll_main_reset32_outR1\);
      \instR28\ : \ZLL_Main_loop218\ port map (\zll_main_reset32_outR1\, \zll_main_loop218_outR5\);
      zi61 <= \zll_main_loop218_outR5\;
      zi62 <= zi61(69 downto 0);
      \instR29\ : \Main_getOut\ port map (zi62, \main_getout_outR4\);
      \instR30\ : \ZLL_Main_loop217\ port map (\main_getout_outR4\, \zll_main_loop217_outR3\);
      zi63 <= \zll_main_loop217_outR3\;
      zi64 <= zi63(84 downto 70);
      zi65 <= zi63(69 downto 0);
      zi66 <= zi9(75 downto 70);
      \instR31\ : \Main_getReg\ port map (zi11, \main_getreg_outR1\);
      zi67 <= \main_getreg_outR1\;
      zi68 <= zi67(77 downto 70);
      zi69 <= zi67(69 downto 0);
      zi70 <= rw_eq(zi68, std_logic_vector'(B"00000000"));
      \instR32\ : \Main_incrPC\ port map (zi69, \main_incrpc_outR2\);
      \instR33\ : \Main_putPC1\ port map (zi66, zi69, main_putpc1_out);
      \connR2\ <= rw_cond(rw_eq(zi70, std_logic_vector'(B"1")), \main_incrpc_outR2\, main_putpc1_out);
      \instR34\ : \ZLL_Main_loop218\ port map (\connR2\, \zll_main_loop218_outR6\);
      zi72 <= \zll_main_loop218_outR6\;
      zi73 <= zi72(69 downto 0);
      \instR35\ : \Main_getPC\ port map (zi73, \main_getpc_outR2\);
      \instR36\ : \ZLL_Main_reset32\ port map (\main_getpc_outR2\, \zll_main_reset32_outR2\);
      \instR37\ : \ZLL_Main_loop218\ port map (\zll_main_reset32_outR2\, \zll_main_loop218_outR7\);
      zi74 <= \zll_main_loop218_outR7\;
      zi75 <= zi74(69 downto 0);
      \instR38\ : \Main_getOut\ port map (zi75, \main_getout_outR5\);
      \instR39\ : \ZLL_Main_loop217\ port map (\main_getout_outR5\, \zll_main_loop217_outR4\);
      zi76 <= \zll_main_loop217_outR4\;
      zi77 <= zi76(84 downto 70);
      zi78 <= zi76(69 downto 0);
      res <= rw_cond(rw_eq(zi9(78 downto 76), std_logic_vector'(B"000")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi23 & std_logic_vector'(B"0100") & zi24), rw_cond(rw_eq(zi9(78 downto 76), std_logic_vector'(B"001")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi29 & std_logic_vector'(B"0010") & zi30), rw_cond(rw_eq(zi9(78 downto 76), std_logic_vector'(B"010")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi46 & std_logic_vector'(B"1000") & zi47), rw_cond(rw_eq(zi9(78 downto 76), std_logic_vector'(B"011")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi64 & std_logic_vector'(B"0000") & zi65), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi77 & std_logic_vector'(B"0111") & zi78)))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_r05\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (5 downto 0);
      arg4 : in std_logic_vector (16 downto 0);
      arg5 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_r05\ is
component \ZLL_Main_r15\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (5 downto 0);
            arg3 : in std_logic_vector (16 downto 0);
            arg4 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zll_main_r15_out : std_logic_vector (7 downto 0);
begin
inst : \ZLL_Main_r15\ port map (arg0, arg2, arg3, arg4, arg5, zll_main_r15_out);
      res <= zll_main_r15_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putWeOut4\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putWeOut4\ is
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
      signal zi4 : std_logic_vector (5 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getOut\ port map (arg1, main_getout_out);
      zi0 <= main_getout_out;
      zi2 <= zi0(69 downto 0);
      zi4 <= zi0(83 downto 78);
      zi5 <= zi0(77 downto 70);
      conn <= (arg0 & zi4 & zi5);
      \instR1\ : \Main_putOut\ port map (conn, zi2, main_putout_out);
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
signal zi8 : std_logic_vector (16 downto 0);
begin
zi8 <= arg0(31 downto 15);
      res <= (zi8 & arg0);
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
component \ZLL_Main_getReg23\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg23_out : std_logic_vector (77 downto 0);
begin
inst : \ZLL_Main_getReg23\ port map (std_logic_vector'(B"00"), std_logic_vector'(B"00"), arg0, zll_main_getreg23_out);
      res <= zll_main_getreg23_out;
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
signal zi9 : std_logic_vector (14 downto 0);
begin
zi9 <= arg0(14 downto 0);
      res <= (zi9 & arg0);
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
      signal zi0 : std_logic_vector (84 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getOut\ port map (arg1, main_getout_out);
      zi0 <= main_getout_out;
      zi2 <= zi0(69 downto 0);
      zi3 <= zi0(84 downto 84);
      zi5 <= zi0(77 downto 70);
      conn <= (zi3 & arg0 & zi5);
      \instR1\ : \Main_putOut\ port map (conn, zi2, main_putout_out);
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
      component \Main_putPC1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zi0 : std_logic_vector (75 downto 0);
      signal zi1 : std_logic_vector (5 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal conn : std_logic_vector (5 downto 0);
      signal main_putpc1_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getPC\ port map (arg0, main_getpc_out);
      zi0 <= main_getpc_out;
      zi1 <= zi0(75 downto 70);
      zi2 <= zi0(69 downto 0);
      conn <= rw_add(zi1, std_logic_vector'(B"000001"));
      \instR1\ : \Main_putPC1\ port map (conn, zi2, main_putpc1_out);
      res <= main_putpc1_out;
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
signal zi7 : std_logic_vector (5 downto 0);
begin
zi7 <= arg0(37 downto 32);
      res <= (zi7 & arg0);
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
signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (5 downto 0);
      signal zi8 : std_logic_vector (16 downto 0);
begin
zi3 <= arg1(69 downto 62);
      zi4 <= arg1(61 downto 54);
      zi5 <= arg1(53 downto 46);
      zi6 <= arg1(45 downto 38);
      zi7 <= arg1(37 downto 32);
      zi8 <= arg1(31 downto 15);
      res <= (zi3 & zi4 & zi5 & zi6 & zi7 & zi8 & arg0);
end architecture;