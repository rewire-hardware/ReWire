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
  function rw_xnor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_not (a : std_logic_vector) return std_logic_vector;
  function rw_shiftl (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_shiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_ashiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_land (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_lor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_lnot (a : std_logic_vector) return std_logic_vector;
  function rw_rand (a : std_logic_vector) return std_logic_vector;
  function rw_rnand (a : std_logic_vector) return std_logic_vector;
  function rw_ror (a : std_logic_vector) return std_logic_vector;
  function rw_rnor (a : std_logic_vector) return std_logic_vector;
  function rw_rxor (a : std_logic_vector) return std_logic_vector;
  function rw_rxnor (a : std_logic_vector) return std_logic_vector;
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
  function rw_xnor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_resize(a, n) xnor rw_resize(b, n);
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
  function rw_land (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(unsigned(a) /= 0 and unsigned(b) /= 0);
  end;
  function rw_lor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(unsigned(a) /= 0 or unsigned(b) /= 0);
  end;
  function rw_lnot (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(unsigned(a) = 0);
  end;
  function rw_rand (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((and a) = '1');
  end;
  function rw_rnand (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((and a) /= '1');
  end;
  function rw_ror (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((or a) = '1');
  end;
  function rw_rnor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((or a) /= '1');
  end;
  function rw_rxor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((xor a) = '1');
  end;
  function rw_rxnor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((xor a) /= '1');
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
      component \ZLL_Main_loop112\ is
      port (arg0 : in std_logic_vector (75 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop140\ is
      port (arg0 : in std_logic_vector (84 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop210\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop221\ is
      port (arg0 : in std_logic_vector (142 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_putReg27\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (9 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Pure_dispatch7\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (3 downto 0) := std_logic_vector'(B"0101");
      signal \__resumption_tag_next\ : std_logic_vector (3 downto 0);
      signal \__st0\ : std_logic_vector (69 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal zin : std_logic_vector (90 downto 0);
      signal zi0 : std_logic_vector (73 downto 0);
      signal zi1 : std_logic_vector (16 downto 0);
      signal zi2 : std_logic_vector (90 downto 0);
      signal zll_pure_dispatch7_out : std_logic_vector (142 downto 0);
      signal zi3 : std_logic_vector (90 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (69 downto 0);
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zll_main_loop210_out : std_logic_vector (142 downto 0);
      signal zi6 : std_logic_vector (142 downto 0);
      signal zi7 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR1\ : std_logic_vector (142 downto 0);
      signal zi8 : std_logic_vector (142 downto 0);
      signal zi9 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zll_main_loop112_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR2\ : std_logic_vector (142 downto 0);
      signal zi10 : std_logic_vector (142 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_loop140_out : std_logic_vector (142 downto 0);
      signal zi12 : std_logic_vector (142 downto 0);
      signal zi13 : std_logic_vector (14 downto 0);
      signal zi14 : std_logic_vector (69 downto 0);
      signal zi15 : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR1\ : std_logic_vector (142 downto 0);
      signal zi16 : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR2\ : std_logic_vector (142 downto 0);
      signal zi17 : std_logic_vector (90 downto 0);
      signal zi18 : std_logic_vector (16 downto 0);
      signal zi19 : std_logic_vector (69 downto 0);
      signal \main_putins_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR3\ : std_logic_vector (142 downto 0);
      signal zi20 : std_logic_vector (142 downto 0);
      signal zi21 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR4\ : std_logic_vector (142 downto 0);
      signal zi22 : std_logic_vector (142 downto 0);
      signal zi23 : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR1\ : std_logic_vector (142 downto 0);
      signal zi24 : std_logic_vector (142 downto 0);
      signal zi25 : std_logic_vector (14 downto 0);
      signal zi26 : std_logic_vector (69 downto 0);
      signal zi27 : std_logic_vector (90 downto 0);
      signal zi28 : std_logic_vector (16 downto 0);
      signal zi29 : std_logic_vector (69 downto 0);
      signal \main_putins_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR5\ : std_logic_vector (142 downto 0);
      signal zi30 : std_logic_vector (142 downto 0);
      signal zi31 : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi32 : std_logic_vector (86 downto 0);
      signal zi33 : std_logic_vector (16 downto 0);
      signal zi34 : std_logic_vector (69 downto 0);
      signal zi35 : std_logic_vector (8 downto 0);
      signal zi36 : std_logic_vector (7 downto 0);
      signal zi37 : std_logic_vector (77 downto 0);
      signal zi38 : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (69 downto 0);
      signal conn : std_logic_vector (9 downto 0);
      signal zll_main_putreg27_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR6\ : std_logic_vector (142 downto 0);
      signal zll_main_loop221_out : std_logic_vector (142 downto 0);
      signal zi40 : std_logic_vector (90 downto 0);
      signal zi41 : std_logic_vector (16 downto 0);
      signal zi42 : std_logic_vector (69 downto 0);
      signal \main_putins_outR3\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR7\ : std_logic_vector (142 downto 0);
      signal zi43 : std_logic_vector (142 downto 0);
      signal zi44 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR8\ : std_logic_vector (142 downto 0);
      signal zi45 : std_logic_vector (142 downto 0);
      signal zi46 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR9\ : std_logic_vector (142 downto 0);
      signal zi47 : std_logic_vector (142 downto 0);
      signal zi48 : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR2\ : std_logic_vector (142 downto 0);
      signal zi49 : std_logic_vector (142 downto 0);
      signal zi50 : std_logic_vector (14 downto 0);
      signal zi51 : std_logic_vector (69 downto 0);
      signal zi52 : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR3\ : std_logic_vector (142 downto 0);
      signal zi53 : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR4\ : std_logic_vector (142 downto 0);
      signal zres : std_logic_vector (142 downto 0);
begin
zin <= (\__resumption_tag\ & \__st0\ & \__in0\);
      zi0 <= zin(90 downto 17);
      zi1 <= zin(16 downto 0);
      zi2 <= (zi1 & zi0);
      inst : \ZLL_Pure_dispatch7\ port map (zi2(90 downto 74), zi2(69 downto 0), zll_pure_dispatch7_out);
      zi3 <= (zi1 & zi0);
      zi4 <= zi3(90 downto 74);
      zi5 <= zi3(69 downto 0);
      \instR1\ : \Main_putIns\ port map (zi4, zi5, main_putins_out);
      \instR2\ : \ZLL_Main_loop210\ port map (main_putins_out, zll_main_loop210_out);
      zi6 <= zll_main_loop210_out;
      zi7 <= zi6(69 downto 0);
      \instR3\ : \Main_incrPC\ port map (zi7, main_incrpc_out);
      \instR4\ : \ZLL_Main_loop210\ port map (main_incrpc_out, \zll_main_loop210_outR1\);
      zi8 <= \zll_main_loop210_outR1\;
      zi9 <= zi8(69 downto 0);
      \instR5\ : \Main_getPC\ port map (zi9, main_getpc_out);
      \instR6\ : \ZLL_Main_loop112\ port map (main_getpc_out, zll_main_loop112_out);
      \instR7\ : \ZLL_Main_loop210\ port map (zll_main_loop112_out, \zll_main_loop210_outR2\);
      zi10 <= \zll_main_loop210_outR2\;
      zi11 <= zi10(69 downto 0);
      \instR8\ : \Main_getOut\ port map (zi11, main_getout_out);
      \instR9\ : \ZLL_Main_loop140\ port map (main_getout_out, zll_main_loop140_out);
      zi12 <= zll_main_loop140_out;
      zi13 <= zi12(84 downto 70);
      zi14 <= zi12(69 downto 0);
      zi15 <= (zi1 & zi0);
      \instR10\ : \ZLL_Pure_dispatch7\ port map (zi15(90 downto 74), zi15(69 downto 0), \zll_pure_dispatch7_outR1\);
      zi16 <= (zi1 & zi0);
      \instR11\ : \ZLL_Pure_dispatch7\ port map (zi16(90 downto 74), zi16(69 downto 0), \zll_pure_dispatch7_outR2\);
      zi17 <= (zi1 & zi0);
      zi18 <= zi17(90 downto 74);
      zi19 <= zi17(69 downto 0);
      \instR12\ : \Main_putIns\ port map (zi18, zi19, \main_putins_outR1\);
      \instR13\ : \ZLL_Main_loop210\ port map (\main_putins_outR1\, \zll_main_loop210_outR3\);
      zi20 <= \zll_main_loop210_outR3\;
      zi21 <= zi20(69 downto 0);
      \instR14\ : \Main_getPC\ port map (zi21, \main_getpc_outR1\);
      \instR15\ : \ZLL_Main_loop112\ port map (\main_getpc_outR1\, \zll_main_loop112_outR1\);
      \instR16\ : \ZLL_Main_loop210\ port map (\zll_main_loop112_outR1\, \zll_main_loop210_outR4\);
      zi22 <= \zll_main_loop210_outR4\;
      zi23 <= zi22(69 downto 0);
      \instR17\ : \Main_getOut\ port map (zi23, \main_getout_outR1\);
      \instR18\ : \ZLL_Main_loop140\ port map (\main_getout_outR1\, \zll_main_loop140_outR1\);
      zi24 <= \zll_main_loop140_outR1\;
      zi25 <= zi24(84 downto 70);
      zi26 <= zi24(69 downto 0);
      zi27 <= (zi1 & zi0);
      zi28 <= zi27(90 downto 74);
      zi29 <= zi27(69 downto 0);
      \instR19\ : \Main_putIns\ port map (zi28, zi29, \main_putins_outR2\);
      \instR20\ : \ZLL_Main_loop210\ port map (\main_putins_outR2\, \zll_main_loop210_outR5\);
      zi30 <= \zll_main_loop210_outR5\;
      zi31 <= zi30(69 downto 0);
      \instR21\ : \Main_getIns\ port map (zi31, main_getins_out);
      zi32 <= main_getins_out;
      zi33 <= zi32(86 downto 70);
      zi34 <= zi32(69 downto 0);
      zi35 <= zi33(16 downto 8);
      zi36 <= zi33(7 downto 0);
      zi37 <= (zi36 & zi34);
      zi38 <= zi37(77 downto 70);
      zi39 <= zi37(69 downto 0);
      conn <= (std_logic_vector'(B"00") & zi38);
      \instR22\ : \ZLL_Main_putReg27\ port map (zi38, std_logic_vector'(B"00"), conn, zi39, zll_main_putreg27_out);
      \instR23\ : \ZLL_Main_loop210\ port map (zll_main_putreg27_out, \zll_main_loop210_outR6\);
      \instR24\ : \ZLL_Main_loop221\ port map (\zll_main_loop210_outR6\, zll_main_loop221_out);
      zi40 <= (zi1 & zi0);
      zi41 <= zi40(90 downto 74);
      zi42 <= zi40(69 downto 0);
      \instR25\ : \Main_putIns\ port map (zi41, zi42, \main_putins_outR3\);
      \instR26\ : \ZLL_Main_loop210\ port map (\main_putins_outR3\, \zll_main_loop210_outR7\);
      zi43 <= \zll_main_loop210_outR7\;
      zi44 <= zi43(69 downto 0);
      \instR27\ : \Main_incrPC\ port map (zi44, \main_incrpc_outR1\);
      \instR28\ : \ZLL_Main_loop210\ port map (\main_incrpc_outR1\, \zll_main_loop210_outR8\);
      zi45 <= \zll_main_loop210_outR8\;
      zi46 <= zi45(69 downto 0);
      \instR29\ : \Main_getPC\ port map (zi46, \main_getpc_outR2\);
      \instR30\ : \ZLL_Main_loop112\ port map (\main_getpc_outR2\, \zll_main_loop112_outR2\);
      \instR31\ : \ZLL_Main_loop210\ port map (\zll_main_loop112_outR2\, \zll_main_loop210_outR9\);
      zi47 <= \zll_main_loop210_outR9\;
      zi48 <= zi47(69 downto 0);
      \instR32\ : \Main_getOut\ port map (zi48, \main_getout_outR2\);
      \instR33\ : \ZLL_Main_loop140\ port map (\main_getout_outR2\, \zll_main_loop140_outR2\);
      zi49 <= \zll_main_loop140_outR2\;
      zi50 <= zi49(84 downto 70);
      zi51 <= zi49(69 downto 0);
      zi52 <= (zi1 & zi0);
      \instR34\ : \ZLL_Pure_dispatch7\ port map (zi52(90 downto 74), zi52(69 downto 0), \zll_pure_dispatch7_outR3\);
      zi53 <= (zi1 & zi0);
      \instR35\ : \ZLL_Pure_dispatch7\ port map (zi53(90 downto 74), zi53(69 downto 0), \zll_pure_dispatch7_outR4\);
      zres <= rw_cond(rw_eq(zi2(73 downto 70), std_logic_vector'(B"0001")), zll_pure_dispatch7_out, rw_cond(rw_eq(zi3(73 downto 70), std_logic_vector'(B"0010")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi13 & std_logic_vector'(B"1000") & zi14), rw_cond(rw_eq(zi15(73 downto 70), std_logic_vector'(B"0011")), \zll_pure_dispatch7_outR1\, rw_cond(rw_eq(zi16(73 downto 70), std_logic_vector'(B"0100")), \zll_pure_dispatch7_outR2\, rw_cond(rw_eq(zi17(73 downto 70), std_logic_vector'(B"0101")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi25 & std_logic_vector'(B"0001") & zi26), rw_cond(rw_eq(zi27(73 downto 70), std_logic_vector'(B"0110")), zll_main_loop221_out, rw_cond(rw_eq(zi40(73 downto 70), std_logic_vector'(B"0111")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi50 & std_logic_vector'(B"0110") & zi51), rw_cond(rw_eq(zi52(73 downto 70), std_logic_vector'(B"1000")), \zll_pure_dispatch7_outR3\, \zll_pure_dispatch7_outR4\))))))));
      \__resumption_tag_next\ <= zres(73 downto 70);
      \__st0_next\ <= zres(69 downto 0);
      \__out0\ <= zres(88 downto 74);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"0101");
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
entity \ZLL_Main_finishInstr4\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_finishInstr4\ is
component \ZLL_Main_putWeOut5\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zll_main_putweout5_out : std_logic_vector (69 downto 0);
begin
inst : \ZLL_Main_putWeOut5\ port map (std_logic_vector'(B"0"), arg0, zll_main_putweout5_out);
      res <= zll_main_putweout5_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_r27\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (5 downto 0);
      arg3 : in std_logic_vector (16 downto 0);
      arg4 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_r27\ is
component \ZLL_Main_r35\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (5 downto 0);
            arg2 : in std_logic_vector (16 downto 0);
            arg3 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zll_main_r35_out : std_logic_vector (7 downto 0);
begin
inst : \ZLL_Main_r35\ port map (arg0, arg2, arg3, arg4, zll_main_r35_out);
      res <= zll_main_r35_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop221\ is
port (arg0 : in std_logic_vector (142 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop221\ is
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
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      component \Main_getReg1\ is
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
      component \Main_putPC1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_finishInstr4\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop112\ is
      port (arg0 : in std_logic_vector (75 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop140\ is
      port (arg0 : in std_logic_vector (84 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop210\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_putReg27\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (9 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putWeOut5\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zi0 : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zi1 : std_logic_vector (86 downto 0);
      signal zi2 : std_logic_vector (16 downto 0);
      signal zi3 : std_logic_vector (69 downto 0);
      signal zi4 : std_logic_vector (8 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (78 downto 0);
      signal zi7 : std_logic_vector (8 downto 0);
      signal zi8 : std_logic_vector (69 downto 0);
      signal zi9 : std_logic_vector (142 downto 0);
      signal zi10 : std_logic_vector (8 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal zi12 : std_logic_vector (139 downto 0);
      signal zi13 : std_logic_vector (69 downto 0);
      signal zi14 : std_logic_vector (69 downto 0);
      signal zi15 : std_logic_vector (142 downto 0);
      signal zi16 : std_logic_vector (151 downto 0);
      signal zi17 : std_logic_vector (8 downto 0);
      signal zi18 : std_logic_vector (69 downto 0);
      signal zi19 : std_logic_vector (69 downto 0);
      signal zi20 : std_logic_vector (78 downto 0);
      signal zi21 : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zll_main_loop210_out : std_logic_vector (142 downto 0);
      signal zi22 : std_logic_vector (142 downto 0);
      signal zi23 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zll_main_loop112_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR1\ : std_logic_vector (142 downto 0);
      signal zi24 : std_logic_vector (142 downto 0);
      signal zi25 : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_loop140_out : std_logic_vector (142 downto 0);
      signal zi26 : std_logic_vector (142 downto 0);
      signal zi27 : std_logic_vector (14 downto 0);
      signal zi28 : std_logic_vector (69 downto 0);
      signal zi29 : std_logic_vector (78 downto 0);
      signal zi30 : std_logic_vector (69 downto 0);
      signal zi31 : std_logic_vector (5 downto 0);
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr4_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR2\ : std_logic_vector (142 downto 0);
      signal zi32 : std_logic_vector (142 downto 0);
      signal zi33 : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR1\ : std_logic_vector (142 downto 0);
      signal zi34 : std_logic_vector (142 downto 0);
      signal zi35 : std_logic_vector (14 downto 0);
      signal zi36 : std_logic_vector (69 downto 0);
      signal zi37 : std_logic_vector (78 downto 0);
      signal zi38 : std_logic_vector (69 downto 0);
      signal zi39 : std_logic_vector (5 downto 0);
      signal main_getreg1_out : std_logic_vector (77 downto 0);
      signal zi40 : std_logic_vector (77 downto 0);
      signal zi41 : std_logic_vector (83 downto 0);
      signal zi42 : std_logic_vector (5 downto 0);
      signal zi43 : std_logic_vector (7 downto 0);
      signal zi44 : std_logic_vector (69 downto 0);
      signal \main_putaddrout_outR1\ : std_logic_vector (69 downto 0);
      signal zi45 : std_logic_vector (69 downto 0);
      signal zi46 : std_logic_vector (77 downto 0);
      signal zi47 : std_logic_vector (7 downto 0);
      signal zi48 : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal zi49 : std_logic_vector (84 downto 0);
      signal zi50 : std_logic_vector (92 downto 0);
      signal zi51 : std_logic_vector (7 downto 0);
      signal zi52 : std_logic_vector (14 downto 0);
      signal zi53 : std_logic_vector (69 downto 0);
      signal zi54 : std_logic_vector (22 downto 0);
      signal zi55 : std_logic_vector (7 downto 0);
      signal zi56 : std_logic_vector (0 downto 0);
      signal zi57 : std_logic_vector (5 downto 0);
      signal zi58 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
      signal zi59 : std_logic_vector (69 downto 0);
      signal zll_main_putweout5_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR3\ : std_logic_vector (142 downto 0);
      signal zi60 : std_logic_vector (142 downto 0);
      signal zi61 : std_logic_vector (69 downto 0);
      signal \main_getout_outR3\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR2\ : std_logic_vector (142 downto 0);
      signal zi62 : std_logic_vector (142 downto 0);
      signal zi63 : std_logic_vector (14 downto 0);
      signal zi64 : std_logic_vector (69 downto 0);
      signal zi65 : std_logic_vector (78 downto 0);
      signal zi66 : std_logic_vector (69 downto 0);
      signal zi67 : std_logic_vector (1 downto 0);
      signal zi68 : std_logic_vector (1 downto 0);
      signal zi69 : std_logic_vector (1 downto 0);
      signal main_getreg_out : std_logic_vector (77 downto 0);
      signal zi70 : std_logic_vector (77 downto 0);
      signal zi71 : std_logic_vector (81 downto 0);
      signal zi72 : std_logic_vector (1 downto 0);
      signal zi73 : std_logic_vector (1 downto 0);
      signal zi74 : std_logic_vector (7 downto 0);
      signal zi75 : std_logic_vector (69 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (77 downto 0);
      signal zi76 : std_logic_vector (77 downto 0);
      signal zi77 : std_logic_vector (87 downto 0);
      signal zi78 : std_logic_vector (7 downto 0);
      signal zi79 : std_logic_vector (1 downto 0);
      signal zi80 : std_logic_vector (7 downto 0);
      signal zi81 : std_logic_vector (69 downto 0);
      signal zi82 : std_logic_vector (7 downto 0);
      signal \connR1\ : std_logic_vector (9 downto 0);
      signal zll_main_putreg27_out : std_logic_vector (69 downto 0);
      signal zi83 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR4\ : std_logic_vector (142 downto 0);
      signal zi84 : std_logic_vector (142 downto 0);
      signal zi85 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR5\ : std_logic_vector (142 downto 0);
      signal zi86 : std_logic_vector (142 downto 0);
      signal zi87 : std_logic_vector (69 downto 0);
      signal \main_getout_outR4\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR3\ : std_logic_vector (142 downto 0);
      signal zi88 : std_logic_vector (142 downto 0);
      signal zi89 : std_logic_vector (14 downto 0);
      signal zi90 : std_logic_vector (69 downto 0);
      signal zi91 : std_logic_vector (78 downto 0);
      signal zi92 : std_logic_vector (69 downto 0);
      signal zi93 : std_logic_vector (5 downto 0);
      signal \main_getreg1_outR1\ : std_logic_vector (77 downto 0);
      signal zi94 : std_logic_vector (77 downto 0);
      signal zi95 : std_logic_vector (83 downto 0);
      signal zi96 : std_logic_vector (5 downto 0);
      signal zi97 : std_logic_vector (7 downto 0);
      signal zi98 : std_logic_vector (69 downto 0);
      signal zi99 : std_logic_vector (0 downto 0);
      signal zi100 : std_logic_vector (70 downto 0);
      signal zi101 : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR2\ : std_logic_vector (69 downto 0);
      signal zi102 : std_logic_vector (0 downto 0);
      signal zi103 : std_logic_vector (76 downto 0);
      signal zi104 : std_logic_vector (69 downto 0);
      signal zi105 : std_logic_vector (5 downto 0);
      signal main_putpc1_out : std_logic_vector (69 downto 0);
      signal \connR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR6\ : std_logic_vector (142 downto 0);
      signal zi106 : std_logic_vector (142 downto 0);
      signal zi107 : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop210_outR7\ : std_logic_vector (142 downto 0);
      signal zi108 : std_logic_vector (142 downto 0);
      signal zi109 : std_logic_vector (69 downto 0);
      signal \main_getout_outR5\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR4\ : std_logic_vector (142 downto 0);
      signal zi110 : std_logic_vector (142 downto 0);
      signal zi111 : std_logic_vector (14 downto 0);
      signal zi112 : std_logic_vector (69 downto 0);
begin
zi0 <= arg0(69 downto 0);
      inst : \Main_getIns\ port map (zi0, main_getins_out);
      zi1 <= main_getins_out;
      zi2 <= zi1(86 downto 70);
      zi3 <= zi1(69 downto 0);
      zi4 <= zi2(16 downto 8);
      zi5 <= zi2(7 downto 0);
      zi6 <= (zi4 & zi3);
      zi7 <= zi6(78 downto 70);
      zi8 <= zi6(69 downto 0);
      zi9 <= (rw_repl(64, std_logic_vector'(B"0")) & zi7 & zi8);
      zi10 <= zi9(78 downto 70);
      zi11 <= zi9(69 downto 0);
      zi12 <= (zi11 & zi11);
      zi13 <= zi12(139 downto 70);
      zi14 <= zi12(69 downto 0);
      zi15 <= (std_logic_vector'(B"011") & zi13 & zi14);
      zi16 <= (zi10 & zi15);
      zi17 <= zi16(151 downto 143);
      zi18 <= zi16(139 downto 70);
      zi19 <= zi16(69 downto 0);
      zi20 <= (zi19 & zi17);
      zi21 <= zi20(78 downto 9);
      \instR1\ : \Main_incrPC\ port map (zi21, main_incrpc_out);
      \instR2\ : \ZLL_Main_loop210\ port map (main_incrpc_out, zll_main_loop210_out);
      zi22 <= zll_main_loop210_out;
      zi23 <= zi22(69 downto 0);
      \instR3\ : \Main_getPC\ port map (zi23, main_getpc_out);
      \instR4\ : \ZLL_Main_loop112\ port map (main_getpc_out, zll_main_loop112_out);
      \instR5\ : \ZLL_Main_loop210\ port map (zll_main_loop112_out, \zll_main_loop210_outR1\);
      zi24 <= \zll_main_loop210_outR1\;
      zi25 <= zi24(69 downto 0);
      \instR6\ : \Main_getOut\ port map (zi25, main_getout_out);
      \instR7\ : \ZLL_Main_loop140\ port map (main_getout_out, zll_main_loop140_out);
      zi26 <= zll_main_loop140_out;
      zi27 <= zi26(84 downto 70);
      zi28 <= zi26(69 downto 0);
      zi29 <= (zi19 & zi17);
      zi30 <= zi29(78 downto 9);
      zi31 <= zi29(5 downto 0);
      \instR8\ : \Main_putAddrOut\ port map (zi31, zi30, main_putaddrout_out);
      \instR9\ : \ZLL_Main_finishInstr4\ port map (main_putaddrout_out, zll_main_finishinstr4_out);
      \instR10\ : \ZLL_Main_loop210\ port map (zll_main_finishinstr4_out, \zll_main_loop210_outR2\);
      zi32 <= \zll_main_loop210_outR2\;
      zi33 <= zi32(69 downto 0);
      \instR11\ : \Main_getOut\ port map (zi33, \main_getout_outR1\);
      \instR12\ : \ZLL_Main_loop140\ port map (\main_getout_outR1\, \zll_main_loop140_outR1\);
      zi34 <= \zll_main_loop140_outR1\;
      zi35 <= zi34(84 downto 70);
      zi36 <= zi34(69 downto 0);
      zi37 <= (zi19 & zi17);
      zi38 <= zi37(78 downto 9);
      zi39 <= zi37(5 downto 0);
      \instR13\ : \Main_getReg1\ port map (zi38, main_getreg1_out);
      zi40 <= main_getreg1_out;
      zi41 <= (zi39 & zi40);
      zi42 <= zi41(83 downto 78);
      zi43 <= zi41(77 downto 70);
      zi44 <= zi41(69 downto 0);
      \instR14\ : \Main_putAddrOut\ port map (zi42, zi44, \main_putaddrout_outR1\);
      zi45 <= \main_putaddrout_outR1\;
      zi46 <= (zi43 & zi45);
      zi47 <= zi46(77 downto 70);
      zi48 <= zi46(69 downto 0);
      \instR15\ : \Main_getOut\ port map (zi48, \main_getout_outR2\);
      zi49 <= \main_getout_outR2\;
      zi50 <= (zi47 & zi49);
      zi51 <= zi50(92 downto 85);
      zi52 <= zi50(84 downto 70);
      zi53 <= zi50(69 downto 0);
      zi54 <= (zi51 & zi52);
      zi55 <= zi54(22 downto 15);
      zi56 <= zi54(14 downto 14);
      zi57 <= zi54(13 downto 8);
      zi58 <= zi54(7 downto 0);
      conn <= (zi56 & zi57 & zi55);
      \instR16\ : \Main_putOut\ port map (conn, zi53, main_putout_out);
      zi59 <= main_putout_out;
      \instR17\ : \ZLL_Main_putWeOut5\ port map (std_logic_vector'(B"1"), zi59, zll_main_putweout5_out);
      \instR18\ : \ZLL_Main_loop210\ port map (zll_main_putweout5_out, \zll_main_loop210_outR3\);
      zi60 <= \zll_main_loop210_outR3\;
      zi61 <= zi60(69 downto 0);
      \instR19\ : \Main_getOut\ port map (zi61, \main_getout_outR3\);
      \instR20\ : \ZLL_Main_loop140\ port map (\main_getout_outR3\, \zll_main_loop140_outR2\);
      zi62 <= \zll_main_loop140_outR2\;
      zi63 <= zi62(84 downto 70);
      zi64 <= zi62(69 downto 0);
      zi65 <= (zi19 & zi17);
      zi66 <= zi65(78 downto 9);
      zi67 <= zi65(5 downto 4);
      zi68 <= zi65(3 downto 2);
      zi69 <= zi65(1 downto 0);
      \instR21\ : \Main_getReg\ port map (zi68, zi66, main_getreg_out);
      zi70 <= main_getreg_out;
      zi71 <= (zi69 & zi67 & zi70);
      zi72 <= zi71(81 downto 80);
      zi73 <= zi71(79 downto 78);
      zi74 <= zi71(77 downto 70);
      zi75 <= zi71(69 downto 0);
      \instR22\ : \Main_getReg\ port map (zi72, zi75, \main_getreg_outR1\);
      zi76 <= \main_getreg_outR1\;
      zi77 <= (zi74 & zi73 & zi76);
      zi78 <= zi77(87 downto 80);
      zi79 <= zi77(79 downto 78);
      zi80 <= zi77(77 downto 70);
      zi81 <= zi77(69 downto 0);
      zi82 <= rw_not(rw_and(zi78, zi80));
      \connR1\ <= (zi79 & zi82);
      \instR23\ : \ZLL_Main_putReg27\ port map (zi82, zi79, \connR1\, zi81, zll_main_putreg27_out);
      zi83 <= zll_main_putreg27_out;
      \instR24\ : \Main_incrPC\ port map (zi83, \main_incrpc_outR1\);
      \instR25\ : \ZLL_Main_loop210\ port map (\main_incrpc_outR1\, \zll_main_loop210_outR4\);
      zi84 <= \zll_main_loop210_outR4\;
      zi85 <= zi84(69 downto 0);
      \instR26\ : \Main_getPC\ port map (zi85, \main_getpc_outR1\);
      \instR27\ : \ZLL_Main_loop112\ port map (\main_getpc_outR1\, \zll_main_loop112_outR1\);
      \instR28\ : \ZLL_Main_loop210\ port map (\zll_main_loop112_outR1\, \zll_main_loop210_outR5\);
      zi86 <= \zll_main_loop210_outR5\;
      zi87 <= zi86(69 downto 0);
      \instR29\ : \Main_getOut\ port map (zi87, \main_getout_outR4\);
      \instR30\ : \ZLL_Main_loop140\ port map (\main_getout_outR4\, \zll_main_loop140_outR3\);
      zi88 <= \zll_main_loop140_outR3\;
      zi89 <= zi88(84 downto 70);
      zi90 <= zi88(69 downto 0);
      zi91 <= (zi19 & zi17);
      zi92 <= zi91(78 downto 9);
      zi93 <= zi91(5 downto 0);
      \instR31\ : \Main_getReg1\ port map (zi92, \main_getreg1_outR1\);
      zi94 <= \main_getreg1_outR1\;
      zi95 <= (zi93 & zi94);
      zi96 <= zi95(83 downto 78);
      zi97 <= zi95(77 downto 70);
      zi98 <= zi95(69 downto 0);
      zi99 <= rw_eq(zi97, std_logic_vector'(B"00000000"));
      zi100 <= (zi98 & zi99);
      zi101 <= zi100(70 downto 1);
      \instR32\ : \Main_incrPC\ port map (zi101, \main_incrpc_outR2\);
      zi102 <= rw_eq(zi97, std_logic_vector'(B"00000000"));
      zi103 <= (zi98 & zi96 & zi102);
      zi104 <= zi103(76 downto 7);
      zi105 <= zi103(6 downto 1);
      \instR33\ : \Main_putPC1\ port map (zi105, zi104, main_putpc1_out);
      \connR2\ <= rw_cond(rw_eq(zi100(0 downto 0), std_logic_vector'(B"1")), \main_incrpc_outR2\, main_putpc1_out);
      \instR34\ : \ZLL_Main_loop210\ port map (\connR2\, \zll_main_loop210_outR6\);
      zi106 <= \zll_main_loop210_outR6\;
      zi107 <= zi106(69 downto 0);
      \instR35\ : \Main_getPC\ port map (zi107, \main_getpc_outR2\);
      \instR36\ : \ZLL_Main_loop112\ port map (\main_getpc_outR2\, \zll_main_loop112_outR2\);
      \instR37\ : \ZLL_Main_loop210\ port map (\zll_main_loop112_outR2\, \zll_main_loop210_outR7\);
      zi108 <= \zll_main_loop210_outR7\;
      zi109 <= zi108(69 downto 0);
      \instR38\ : \Main_getOut\ port map (zi109, \main_getout_outR5\);
      \instR39\ : \ZLL_Main_loop140\ port map (\main_getout_outR5\, \zll_main_loop140_outR4\);
      zi110 <= \zll_main_loop140_outR4\;
      zi111 <= zi110(84 downto 70);
      zi112 <= zi110(69 downto 0);
      res <= rw_cond(rw_eq(zi20(8 downto 6), std_logic_vector'(B"000")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi27 & std_logic_vector'(B"0000") & zi28), rw_cond(rw_eq(zi29(8 downto 6), std_logic_vector'(B"001")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi35 & std_logic_vector'(B"0111") & zi36), rw_cond(rw_eq(zi37(8 downto 6), std_logic_vector'(B"010")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi63 & std_logic_vector'(B"0010") & zi64), rw_cond(rw_eq(zi65(8 downto 6), std_logic_vector'(B"011")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi89 & std_logic_vector'(B"0100") & zi90), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zi111 & std_logic_vector'(B"0011") & zi112)))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop210\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop210\ is

begin
res <= ((std_logic_vector'(B"01") & rw_repl(71, std_logic_vector'(B"0"))) & arg0);
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
signal zi0 : std_logic_vector (139 downto 0);
      signal zi1 : std_logic_vector (145 downto 0);
      signal zi2 : std_logic_vector (5 downto 0);
      signal zi3 : std_logic_vector (69 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal zi5 : std_logic_vector (75 downto 0);
      signal zi6 : std_logic_vector (5 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (5 downto 0);
      signal zi12 : std_logic_vector (16 downto 0);
      signal zi13 : std_logic_vector (14 downto 0);
begin
zi0 <= (arg1 & arg1);
      zi1 <= (arg0 & zi0);
      zi2 <= zi1(145 downto 140);
      zi3 <= zi1(139 downto 70);
      zi4 <= zi1(69 downto 0);
      zi5 <= (zi2 & zi3);
      zi6 <= zi5(75 downto 70);
      zi7 <= zi5(69 downto 62);
      zi8 <= zi5(61 downto 54);
      zi9 <= zi5(53 downto 46);
      zi10 <= zi5(45 downto 38);
      zi11 <= zi5(37 downto 32);
      zi12 <= zi5(31 downto 15);
      zi13 <= zi5(14 downto 0);
      res <= (zi7 & zi8 & zi9 & zi10 & zi6 & zi12 & zi13);
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
signal zi0 : std_logic_vector (139 downto 0);
      signal zi1 : std_logic_vector (156 downto 0);
      signal zi2 : std_logic_vector (16 downto 0);
      signal zi3 : std_logic_vector (69 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal zi5 : std_logic_vector (86 downto 0);
      signal zi6 : std_logic_vector (16 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (5 downto 0);
      signal zi12 : std_logic_vector (16 downto 0);
      signal zi13 : std_logic_vector (14 downto 0);
begin
zi0 <= (arg1 & arg1);
      zi1 <= (arg0 & zi0);
      zi2 <= zi1(156 downto 140);
      zi3 <= zi1(139 downto 70);
      zi4 <= zi1(69 downto 0);
      zi5 <= (zi2 & zi3);
      zi6 <= zi5(86 downto 70);
      zi7 <= zi5(69 downto 62);
      zi8 <= zi5(61 downto 54);
      zi9 <= zi5(53 downto 46);
      zi10 <= zi5(45 downto 38);
      zi11 <= zi5(37 downto 32);
      zi12 <= zi5(31 downto 15);
      zi13 <= zi5(14 downto 0);
      res <= (zi7 & zi8 & zi9 & zi10 & zi11 & zi6 & zi13);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getReg1\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \Main_getReg1\ is
component \ZLL_Main_getReg6\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg6_out : std_logic_vector (77 downto 0);
begin
inst : \ZLL_Main_getReg6\ port map (std_logic_vector'(B"00"), std_logic_vector'(B"00"), arg0, zll_main_getreg6_out);
      res <= zll_main_getreg6_out;
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
      component \ZLL_Main_loop210\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop221\ is
      port (arg0 : in std_logic_vector (142 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      signal main_putins_out : std_logic_vector (69 downto 0);
      signal zll_main_loop210_out : std_logic_vector (142 downto 0);
      signal zll_main_loop221_out : std_logic_vector (142 downto 0);
begin
inst : \Main_putIns\ port map (arg0, arg1, main_putins_out);
      \instR1\ : \ZLL_Main_loop210\ port map (main_putins_out, zll_main_loop210_out);
      \instR2\ : \ZLL_Main_loop221\ port map (zll_main_loop210_out, zll_main_loop221_out);
      res <= zll_main_loop221_out;
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
component \ZLL_Main_getReg6\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (77 downto 0));
      end component;
      signal zll_main_getreg6_out : std_logic_vector (77 downto 0);
begin
inst : \ZLL_Main_getReg6\ port map (arg0, arg0, arg1, zll_main_getreg6_out);
      res <= zll_main_getreg6_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_r35\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (5 downto 0);
      arg2 : in std_logic_vector (16 downto 0);
      arg3 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_r35\ is

begin
res <= arg0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop140\ is
port (arg0 : in std_logic_vector (84 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop140\ is
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
entity \Main_getIns\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (86 downto 0));
end entity;

architecture rtl of \Main_getIns\ is
signal zi0 : std_logic_vector (139 downto 0);
      signal zi1 : std_logic_vector (69 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (5 downto 0);
      signal zi8 : std_logic_vector (16 downto 0);
      signal zi9 : std_logic_vector (14 downto 0);
begin
zi0 <= (arg0 & arg0);
      zi1 <= zi0(139 downto 70);
      zi2 <= zi0(69 downto 0);
      zi3 <= zi1(69 downto 62);
      zi4 <= zi1(61 downto 54);
      zi5 <= zi1(53 downto 46);
      zi6 <= zi1(45 downto 38);
      zi7 <= zi1(37 downto 32);
      zi8 <= zi1(31 downto 15);
      zi9 <= zi1(14 downto 0);
      res <= (zi8 & zi2);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_loop112\ is
port (arg0 : in std_logic_vector (75 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop112\ is
component \Main_putAddrOut\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_finishInstr4\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zi0 : std_logic_vector (5 downto 0);
      signal zi1 : std_logic_vector (69 downto 0);
      signal main_putaddrout_out : std_logic_vector (69 downto 0);
      signal zll_main_finishinstr4_out : std_logic_vector (69 downto 0);
begin
zi0 <= arg0(75 downto 70);
      zi1 <= arg0(69 downto 0);
      inst : \Main_putAddrOut\ port map (zi0, zi1, main_putaddrout_out);
      \instR1\ : \ZLL_Main_finishInstr4\ port map (main_putaddrout_out, zll_main_finishinstr4_out);
      res <= zll_main_finishinstr4_out;
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
signal zi0 : std_logic_vector (139 downto 0);
      signal zi1 : std_logic_vector (69 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (5 downto 0);
      signal zi8 : std_logic_vector (16 downto 0);
      signal zi9 : std_logic_vector (14 downto 0);
begin
zi0 <= (arg0 & arg0);
      zi1 <= zi0(139 downto 70);
      zi2 <= zi0(69 downto 0);
      zi3 <= zi1(69 downto 62);
      zi4 <= zi1(61 downto 54);
      zi5 <= zi1(53 downto 46);
      zi6 <= zi1(45 downto 38);
      zi7 <= zi1(37 downto 32);
      zi8 <= zi1(31 downto 15);
      zi9 <= zi1(14 downto 0);
      res <= (zi9 & zi2);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putWeOut5\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putWeOut5\ is
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
      signal zi1 : std_logic_vector (85 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (14 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal zi5 : std_logic_vector (15 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (5 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getOut\ port map (arg1, main_getout_out);
      zi0 <= main_getout_out;
      zi1 <= (arg0 & zi0);
      zi2 <= zi1(85 downto 85);
      zi3 <= zi1(84 downto 70);
      zi4 <= zi1(69 downto 0);
      zi5 <= (zi2 & zi3);
      zi6 <= zi5(15 downto 15);
      zi7 <= zi5(14 downto 14);
      zi8 <= zi5(13 downto 8);
      zi9 <= zi5(7 downto 0);
      conn <= (zi6 & zi8 & zi9);
      \instR1\ : \Main_putOut\ port map (conn, zi4, main_putout_out);
      res <= main_putout_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_r13\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (5 downto 0);
      arg4 : in std_logic_vector (16 downto 0);
      arg5 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_r13\ is
component \ZLL_Main_r27\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (5 downto 0);
            arg3 : in std_logic_vector (16 downto 0);
            arg4 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zll_main_r27_out : std_logic_vector (7 downto 0);
begin
inst : \ZLL_Main_r27\ port map (arg0, arg2, arg3, arg4, arg5, zll_main_r27_out);
      res <= zll_main_r27_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putReg27\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (9 downto 0);
      arg3 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putReg27\ is
signal zt0 : std_logic_vector (79 downto 0);
      signal zi0 : std_logic_vector (69 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (139 downto 0);
      signal zi3 : std_logic_vector (147 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (69 downto 0);
      signal zi6 : std_logic_vector (69 downto 0);
      signal zi7 : std_logic_vector (77 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (5 downto 0);
      signal zi14 : std_logic_vector (16 downto 0);
      signal zi15 : std_logic_vector (14 downto 0);
      signal zi16 : std_logic_vector (9 downto 0);
      signal zi17 : std_logic_vector (79 downto 0);
      signal zi18 : std_logic_vector (69 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (139 downto 0);
      signal zi21 : std_logic_vector (147 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (69 downto 0);
      signal zi24 : std_logic_vector (69 downto 0);
      signal zi25 : std_logic_vector (77 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (7 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal zi29 : std_logic_vector (7 downto 0);
      signal zi30 : std_logic_vector (7 downto 0);
      signal zi31 : std_logic_vector (5 downto 0);
      signal zi32 : std_logic_vector (16 downto 0);
      signal zi33 : std_logic_vector (14 downto 0);
      signal zi34 : std_logic_vector (9 downto 0);
      signal zi35 : std_logic_vector (79 downto 0);
      signal zi36 : std_logic_vector (69 downto 0);
      signal zi37 : std_logic_vector (7 downto 0);
      signal zi38 : std_logic_vector (139 downto 0);
      signal zi39 : std_logic_vector (147 downto 0);
      signal zi40 : std_logic_vector (7 downto 0);
      signal zi41 : std_logic_vector (69 downto 0);
      signal zi42 : std_logic_vector (69 downto 0);
      signal zi43 : std_logic_vector (77 downto 0);
      signal zi44 : std_logic_vector (7 downto 0);
      signal zi45 : std_logic_vector (7 downto 0);
      signal zi46 : std_logic_vector (7 downto 0);
      signal zi47 : std_logic_vector (7 downto 0);
      signal zi48 : std_logic_vector (7 downto 0);
      signal zi49 : std_logic_vector (5 downto 0);
      signal zi50 : std_logic_vector (16 downto 0);
      signal zi51 : std_logic_vector (14 downto 0);
      signal zi52 : std_logic_vector (9 downto 0);
      signal zi53 : std_logic_vector (79 downto 0);
      signal zi54 : std_logic_vector (69 downto 0);
      signal zi55 : std_logic_vector (7 downto 0);
      signal zi56 : std_logic_vector (139 downto 0);
      signal zi57 : std_logic_vector (147 downto 0);
      signal zi58 : std_logic_vector (7 downto 0);
      signal zi59 : std_logic_vector (69 downto 0);
      signal zi60 : std_logic_vector (69 downto 0);
      signal zi61 : std_logic_vector (77 downto 0);
      signal zi62 : std_logic_vector (7 downto 0);
      signal zi63 : std_logic_vector (7 downto 0);
      signal zi64 : std_logic_vector (7 downto 0);
      signal zi65 : std_logic_vector (7 downto 0);
      signal zi66 : std_logic_vector (7 downto 0);
      signal zi67 : std_logic_vector (5 downto 0);
      signal zi68 : std_logic_vector (16 downto 0);
      signal zi69 : std_logic_vector (14 downto 0);
begin
zt0 <= (arg3 & arg2);
      zi0 <= zt0(79 downto 10);
      zi1 <= zt0(7 downto 0);
      zi2 <= (zi0 & zi0);
      zi3 <= (zi1 & zi2);
      zi4 <= zi3(147 downto 140);
      zi5 <= zi3(139 downto 70);
      zi6 <= zi3(69 downto 0);
      zi7 <= (zi4 & zi5);
      zi8 <= zi7(77 downto 70);
      zi9 <= zi7(69 downto 62);
      zi10 <= zi7(61 downto 54);
      zi11 <= zi7(53 downto 46);
      zi12 <= zi7(45 downto 38);
      zi13 <= zi7(37 downto 32);
      zi14 <= zi7(31 downto 15);
      zi15 <= zi7(14 downto 0);
      zi16 <= (arg1 & arg0);
      zi17 <= (arg3 & zi16);
      zi18 <= zi17(79 downto 10);
      zi19 <= zi17(7 downto 0);
      zi20 <= (zi18 & zi18);
      zi21 <= (zi19 & zi20);
      zi22 <= zi21(147 downto 140);
      zi23 <= zi21(139 downto 70);
      zi24 <= zi21(69 downto 0);
      zi25 <= (zi22 & zi23);
      zi26 <= zi25(77 downto 70);
      zi27 <= zi25(69 downto 62);
      zi28 <= zi25(61 downto 54);
      zi29 <= zi25(53 downto 46);
      zi30 <= zi25(45 downto 38);
      zi31 <= zi25(37 downto 32);
      zi32 <= zi25(31 downto 15);
      zi33 <= zi25(14 downto 0);
      zi34 <= (arg1 & arg0);
      zi35 <= (arg3 & zi34);
      zi36 <= zi35(79 downto 10);
      zi37 <= zi35(7 downto 0);
      zi38 <= (zi36 & zi36);
      zi39 <= (zi37 & zi38);
      zi40 <= zi39(147 downto 140);
      zi41 <= zi39(139 downto 70);
      zi42 <= zi39(69 downto 0);
      zi43 <= (zi40 & zi41);
      zi44 <= zi43(77 downto 70);
      zi45 <= zi43(69 downto 62);
      zi46 <= zi43(61 downto 54);
      zi47 <= zi43(53 downto 46);
      zi48 <= zi43(45 downto 38);
      zi49 <= zi43(37 downto 32);
      zi50 <= zi43(31 downto 15);
      zi51 <= zi43(14 downto 0);
      zi52 <= (arg1 & arg0);
      zi53 <= (arg3 & zi52);
      zi54 <= zi53(79 downto 10);
      zi55 <= zi53(7 downto 0);
      zi56 <= (zi54 & zi54);
      zi57 <= (zi55 & zi56);
      zi58 <= zi57(147 downto 140);
      zi59 <= zi57(139 downto 70);
      zi60 <= zi57(69 downto 0);
      zi61 <= (zi58 & zi59);
      zi62 <= zi61(77 downto 70);
      zi63 <= zi61(69 downto 62);
      zi64 <= zi61(61 downto 54);
      zi65 <= zi61(53 downto 46);
      zi66 <= zi61(45 downto 38);
      zi67 <= zi61(37 downto 32);
      zi68 <= zi61(31 downto 15);
      zi69 <= zi61(14 downto 0);
      res <= rw_cond(rw_eq(zt0(9 downto 8), std_logic_vector'(B"00")), (zi8 & zi10 & zi11 & zi12 & zi13 & zi14 & zi15), rw_cond(rw_eq(zi17(9 downto 8), std_logic_vector'(B"01")), (zi27 & zi26 & zi29 & zi30 & zi31 & zi32 & zi33), rw_cond(rw_eq(zi35(9 downto 8), std_logic_vector'(B"10")), (zi45 & zi46 & zi44 & zi48 & zi49 & zi50 & zi51), (zi63 & zi64 & zi65 & zi62 & zi67 & zi68 & zi69))));
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
      signal zi1 : std_logic_vector (90 downto 0);
      signal zi2 : std_logic_vector (5 downto 0);
      signal zi3 : std_logic_vector (14 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal zi5 : std_logic_vector (20 downto 0);
      signal zi6 : std_logic_vector (5 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (5 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal main_putout_out : std_logic_vector (69 downto 0);
begin
inst : \Main_getOut\ port map (arg1, main_getout_out);
      zi0 <= main_getout_out;
      zi1 <= (arg0 & zi0);
      zi2 <= zi1(90 downto 85);
      zi3 <= zi1(84 downto 70);
      zi4 <= zi1(69 downto 0);
      zi5 <= (zi2 & zi3);
      zi6 <= zi5(20 downto 15);
      zi7 <= zi5(14 downto 14);
      zi8 <= zi5(13 downto 8);
      zi9 <= zi5(7 downto 0);
      conn <= (zi7 & zi6 & zi9);
      \instR1\ : \Main_putOut\ port map (conn, zi4, main_putout_out);
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
entity \ZLL_Main_getReg6\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (77 downto 0));
end entity;

architecture rtl of \ZLL_Main_getReg6\ is
component \ZLL_Main_r13\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (5 downto 0);
            arg4 : in std_logic_vector (16 downto 0);
            arg5 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_r27\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (5 downto 0);
            arg3 : in std_logic_vector (16 downto 0);
            arg4 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_r35\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (5 downto 0);
            arg2 : in std_logic_vector (16 downto 0);
            arg3 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zt0 : std_logic_vector (71 downto 0);
      signal zi0 : std_logic_vector (69 downto 0);
      signal zi1 : std_logic_vector (139 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (69 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (5 downto 0);
      signal zi9 : std_logic_vector (16 downto 0);
      signal zi10 : std_logic_vector (14 downto 0);
      signal zll_main_r13_out : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (71 downto 0);
      signal zi12 : std_logic_vector (69 downto 0);
      signal zi13 : std_logic_vector (139 downto 0);
      signal zi14 : std_logic_vector (69 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (5 downto 0);
      signal zi21 : std_logic_vector (16 downto 0);
      signal zi22 : std_logic_vector (14 downto 0);
      signal \zll_main_r13_outR1\ : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (71 downto 0);
      signal zi24 : std_logic_vector (69 downto 0);
      signal zi25 : std_logic_vector (139 downto 0);
      signal zi26 : std_logic_vector (69 downto 0);
      signal zi27 : std_logic_vector (69 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal zi29 : std_logic_vector (7 downto 0);
      signal zi30 : std_logic_vector (7 downto 0);
      signal zi31 : std_logic_vector (7 downto 0);
      signal zi32 : std_logic_vector (5 downto 0);
      signal zi33 : std_logic_vector (16 downto 0);
      signal zi34 : std_logic_vector (14 downto 0);
      signal zll_main_r27_out : std_logic_vector (7 downto 0);
      signal zi35 : std_logic_vector (71 downto 0);
      signal zi36 : std_logic_vector (69 downto 0);
      signal zi37 : std_logic_vector (139 downto 0);
      signal zi38 : std_logic_vector (69 downto 0);
      signal zi39 : std_logic_vector (69 downto 0);
      signal zi40 : std_logic_vector (7 downto 0);
      signal zi41 : std_logic_vector (7 downto 0);
      signal zi42 : std_logic_vector (7 downto 0);
      signal zi43 : std_logic_vector (7 downto 0);
      signal zi44 : std_logic_vector (5 downto 0);
      signal zi45 : std_logic_vector (16 downto 0);
      signal zi46 : std_logic_vector (14 downto 0);
      signal zll_main_r35_out : std_logic_vector (7 downto 0);
begin
zt0 <= (arg2 & arg1);
      zi0 <= zt0(71 downto 2);
      zi1 <= (zi0 & zi0);
      zi2 <= zi1(139 downto 70);
      zi3 <= zi1(69 downto 0);
      zi4 <= zi2(69 downto 62);
      zi5 <= zi2(61 downto 54);
      zi6 <= zi2(53 downto 46);
      zi7 <= zi2(45 downto 38);
      zi8 <= zi2(37 downto 32);
      zi9 <= zi2(31 downto 15);
      zi10 <= zi2(14 downto 0);
      inst : \ZLL_Main_r13\ port map (zi4, zi6, zi7, zi8, zi9, zi10, zll_main_r13_out);
      zi11 <= (arg2 & arg0);
      zi12 <= zi11(71 downto 2);
      zi13 <= (zi12 & zi12);
      zi14 <= zi13(139 downto 70);
      zi15 <= zi13(69 downto 0);
      zi16 <= zi14(69 downto 62);
      zi17 <= zi14(61 downto 54);
      zi18 <= zi14(53 downto 46);
      zi19 <= zi14(45 downto 38);
      zi20 <= zi14(37 downto 32);
      zi21 <= zi14(31 downto 15);
      zi22 <= zi14(14 downto 0);
      \instR1\ : \ZLL_Main_r13\ port map (zi17, zi18, zi19, zi20, zi21, zi22, \zll_main_r13_outR1\);
      zi23 <= (arg2 & arg0);
      zi24 <= zi23(71 downto 2);
      zi25 <= (zi24 & zi24);
      zi26 <= zi25(139 downto 70);
      zi27 <= zi25(69 downto 0);
      zi28 <= zi26(69 downto 62);
      zi29 <= zi26(61 downto 54);
      zi30 <= zi26(53 downto 46);
      zi31 <= zi26(45 downto 38);
      zi32 <= zi26(37 downto 32);
      zi33 <= zi26(31 downto 15);
      zi34 <= zi26(14 downto 0);
      \instR2\ : \ZLL_Main_r27\ port map (zi30, zi31, zi32, zi33, zi34, zll_main_r27_out);
      zi35 <= (arg2 & arg0);
      zi36 <= zi35(71 downto 2);
      zi37 <= (zi36 & zi36);
      zi38 <= zi37(139 downto 70);
      zi39 <= zi37(69 downto 0);
      zi40 <= zi38(69 downto 62);
      zi41 <= zi38(61 downto 54);
      zi42 <= zi38(53 downto 46);
      zi43 <= zi38(45 downto 38);
      zi44 <= zi38(37 downto 32);
      zi45 <= zi38(31 downto 15);
      zi46 <= zi38(14 downto 0);
      \instR3\ : \ZLL_Main_r35\ port map (zi43, zi44, zi45, zi46, zll_main_r35_out);
      res <= rw_cond(rw_eq(zt0(1 downto 0), std_logic_vector'(B"00")), (zll_main_r13_out & zi3), rw_cond(rw_eq(zi11(1 downto 0), std_logic_vector'(B"01")), (\zll_main_r13_outR1\ & zi15), rw_cond(rw_eq(zi23(1 downto 0), std_logic_vector'(B"10")), (zll_main_r27_out & zi27), (zll_main_r35_out & zi39))));
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
signal zi0 : std_logic_vector (139 downto 0);
      signal zi1 : std_logic_vector (69 downto 0);
      signal zi2 : std_logic_vector (69 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (5 downto 0);
      signal zi8 : std_logic_vector (16 downto 0);
      signal zi9 : std_logic_vector (14 downto 0);
begin
zi0 <= (arg0 & arg0);
      zi1 <= zi0(139 downto 70);
      zi2 <= zi0(69 downto 0);
      zi3 <= zi1(69 downto 62);
      zi4 <= zi1(61 downto 54);
      zi5 <= zi1(53 downto 46);
      zi6 <= zi1(45 downto 38);
      zi7 <= zi1(37 downto 32);
      zi8 <= zi1(31 downto 15);
      zi9 <= zi1(14 downto 0);
      res <= (zi7 & zi2);
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
signal zi0 : std_logic_vector (139 downto 0);
      signal zi1 : std_logic_vector (154 downto 0);
      signal zi2 : std_logic_vector (14 downto 0);
      signal zi3 : std_logic_vector (69 downto 0);
      signal zi4 : std_logic_vector (69 downto 0);
      signal zi5 : std_logic_vector (84 downto 0);
      signal zi6 : std_logic_vector (14 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (5 downto 0);
      signal zi12 : std_logic_vector (16 downto 0);
      signal zi13 : std_logic_vector (14 downto 0);
begin
zi0 <= (arg1 & arg1);
      zi1 <= (arg0 & zi0);
      zi2 <= zi1(154 downto 140);
      zi3 <= zi1(139 downto 70);
      zi4 <= zi1(69 downto 0);
      zi5 <= (zi2 & zi3);
      zi6 <= zi5(84 downto 70);
      zi7 <= zi5(69 downto 62);
      zi8 <= zi5(61 downto 54);
      zi9 <= zi5(53 downto 46);
      zi10 <= zi5(45 downto 38);
      zi11 <= zi5(37 downto 32);
      zi12 <= zi5(31 downto 15);
      zi13 <= zi5(14 downto 0);
      res <= (zi7 & zi8 & zi9 & zi10 & zi11 & zi12 & zi6);
end architecture;