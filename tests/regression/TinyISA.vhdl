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
    if unsigned(b) = 0 then return std_logic_vector(to_unsigned(0, n) - 1); end if;
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
      component \ZLL_Main_loop112\ is
      port (arg0 : in std_logic_vector (75 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_loop140\ is
      port (arg0 : in std_logic_vector (84 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop221\ is
      port (arg0 : in std_logic_vector (142 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop225\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_putIns11\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
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
      signal zll_pure_dispatch7_in : std_logic_vector (90 downto 0);
      signal zll_pure_dispatch7_out : std_logic_vector (142 downto 0);
      signal \zll_pure_dispatch7_inR1\ : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR1\ : std_logic_vector (142 downto 0);
      signal zll_pure_dispatch8_in : std_logic_vector (90 downto 0);
      signal zll_main_loop81_in : std_logic_vector (86 downto 0);
      signal zll_main_putins11_in : std_logic_vector (86 downto 0);
      signal zll_main_putins11_out : std_logic_vector (69 downto 0);
      signal zll_main_loop225_in : std_logic_vector (69 downto 0);
      signal zll_main_loop225_out : std_logic_vector (142 downto 0);
      signal zll_main_loop169_in : std_logic_vector (142 downto 0);
      signal zll_main_loop63_in : std_logic_vector (142 downto 0);
      signal zll_main_loop193_in : std_logic_vector (69 downto 0);
      signal main_incrpc_in : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR1\ : std_logic_vector (142 downto 0);
      signal zll_main_loop197_in : std_logic_vector (142 downto 0);
      signal zll_main_loop166_in : std_logic_vector (142 downto 0);
      signal zll_main_loop163_in : std_logic_vector (69 downto 0);
      signal main_getpc_in : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zll_main_loop112_in : std_logic_vector (75 downto 0);
      signal zll_main_loop112_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR2\ : std_logic_vector (142 downto 0);
      signal zll_main_loop31_in : std_logic_vector (142 downto 0);
      signal zll_main_loop171_in : std_logic_vector (142 downto 0);
      signal zll_main_loop49_in : std_logic_vector (69 downto 0);
      signal main_getout_in : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_loop140_in : std_logic_vector (84 downto 0);
      signal zll_main_loop140_out : std_logic_vector (142 downto 0);
      signal zll_main_loop46_in : std_logic_vector (142 downto 0);
      signal zll_main_loop80_in : std_logic_vector (142 downto 0);
      signal zll_main_loop47_in : std_logic_vector (84 downto 0);
      signal zll_pure_dispatch3_in : std_logic_vector (90 downto 0);
      signal zll_main_loop86_in : std_logic_vector (86 downto 0);
      signal \zll_main_putins11_inR1\ : std_logic_vector (86 downto 0);
      signal \zll_main_putins11_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR3\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR3\ : std_logic_vector (142 downto 0);
      signal zll_main_loop5_in : std_logic_vector (142 downto 0);
      signal zll_main_loop19_in : std_logic_vector (142 downto 0);
      signal zll_main_loop113_in : std_logic_vector (69 downto 0);
      signal main_getdatain_in : std_logic_vector (69 downto 0);
      signal main_getins_in : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zll_main_getdatain1_in : std_logic_vector (86 downto 0);
      signal zll_main_getdatain2_in : std_logic_vector (86 downto 0);
      signal zll_main_datain_in : std_logic_vector (16 downto 0);
      signal zll_main_datain2_in : std_logic_vector (16 downto 0);
      signal zll_main_loop135_in : std_logic_vector (77 downto 0);
      signal main_putreg1_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg27_in : std_logic_vector (89 downto 0);
      signal zll_main_putreg27_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR4\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR4\ : std_logic_vector (142 downto 0);
      signal zll_main_loop221_in : std_logic_vector (142 downto 0);
      signal zll_main_loop221_out : std_logic_vector (142 downto 0);
      signal zll_pure_dispatch_in : std_logic_vector (90 downto 0);
      signal zll_main_reset11_in : std_logic_vector (86 downto 0);
      signal \zll_main_putins11_inR2\ : std_logic_vector (86 downto 0);
      signal \zll_main_putins11_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR5\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR5\ : std_logic_vector (142 downto 0);
      signal zll_main_reset4_in : std_logic_vector (142 downto 0);
      signal zll_main_reset39_in : std_logic_vector (142 downto 0);
      signal zll_main_reset10_in : std_logic_vector (69 downto 0);
      signal \main_getpc_inR1\ : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_inR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR6\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR6\ : std_logic_vector (142 downto 0);
      signal zll_main_reset29_in : std_logic_vector (142 downto 0);
      signal zll_main_reset45_in : std_logic_vector (142 downto 0);
      signal zll_main_reset37_in : std_logic_vector (69 downto 0);
      signal \main_getout_inR1\ : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_inR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR1\ : std_logic_vector (142 downto 0);
      signal zll_main_reset46_in : std_logic_vector (142 downto 0);
      signal zll_main_reset33_in : std_logic_vector (142 downto 0);
      signal zll_main_reset36_in : std_logic_vector (84 downto 0);
      signal \zll_pure_dispatch7_inR2\ : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR2\ : std_logic_vector (142 downto 0);
      signal \zll_pure_dispatch7_inR3\ : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR3\ : std_logic_vector (142 downto 0);
      signal zll_pure_dispatch2_in : std_logic_vector (90 downto 0);
      signal zll_main_loop213_in : std_logic_vector (86 downto 0);
      signal \zll_main_putins11_inR3\ : std_logic_vector (86 downto 0);
      signal \zll_main_putins11_outR3\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR7\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR7\ : std_logic_vector (142 downto 0);
      signal zll_main_loop181_in : std_logic_vector (142 downto 0);
      signal zll_main_loop129_in : std_logic_vector (142 downto 0);
      signal zll_main_loop20_in : std_logic_vector (69 downto 0);
      signal \main_incrpc_inR1\ : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR8\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR8\ : std_logic_vector (142 downto 0);
      signal zll_main_loop226_in : std_logic_vector (142 downto 0);
      signal zll_main_loop216_in : std_logic_vector (142 downto 0);
      signal zll_main_loop148_in : std_logic_vector (69 downto 0);
      signal \main_getpc_inR2\ : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_inR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR9\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR9\ : std_logic_vector (142 downto 0);
      signal zll_main_loop110_in : std_logic_vector (142 downto 0);
      signal zll_main_loop105_in : std_logic_vector (142 downto 0);
      signal zll_main_loop25_in : std_logic_vector (69 downto 0);
      signal \main_getout_inR2\ : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_inR2\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR2\ : std_logic_vector (142 downto 0);
      signal zll_main_loop92_in : std_logic_vector (142 downto 0);
      signal zll_main_loop2_in : std_logic_vector (142 downto 0);
      signal zll_main_loop24_in : std_logic_vector (84 downto 0);
      signal \zll_pure_dispatch7_inR4\ : std_logic_vector (90 downto 0);
      signal \zll_pure_dispatch7_outR4\ : std_logic_vector (142 downto 0);
      signal \__padding\ : std_logic_vector (53 downto 0);
      signal \__resumption_tag\ : std_logic_vector (3 downto 0) := std_logic_vector'(B"0101");
      signal \__st0\ : std_logic_vector (69 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__resumption_tag_next\ : std_logic_vector (3 downto 0);
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal rwtmp0 : std_logic_vector (142 downto 0);
begin
zll_pure_dispatch7_in <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      inst : \ZLL_Pure_dispatch7\ port map (zll_pure_dispatch7_in(90 downto 74), zll_pure_dispatch7_in(69 downto 0), zll_pure_dispatch7_out);
      \zll_pure_dispatch7_inR1\ <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      \instR1\ : \ZLL_Pure_dispatch7\ port map (\zll_pure_dispatch7_inR1\(90 downto 74), \zll_pure_dispatch7_inR1\(69 downto 0), \zll_pure_dispatch7_outR1\);
      zll_pure_dispatch8_in <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      zll_main_loop81_in <= (zll_pure_dispatch8_in(90 downto 74) & zll_pure_dispatch8_in(69 downto 0));
      zll_main_putins11_in <= (zll_main_loop81_in(86 downto 70) & zll_main_loop81_in(69 downto 0));
      \instR2\ : \ZLL_Main_putIns11\ port map (zll_main_putins11_in(86 downto 70), zll_main_putins11_in(69 downto 0), zll_main_putins11_out);
      zll_main_loop225_in <= zll_main_putins11_out;
      \instR3\ : \ZLL_Main_loop225\ port map (zll_main_loop225_in(69 downto 0), zll_main_loop225_out);
      zll_main_loop169_in <= zll_main_loop225_out;
      zll_main_loop63_in <= zll_main_loop169_in(142 downto 0);
      zll_main_loop193_in <= zll_main_loop63_in(69 downto 0);
      main_incrpc_in <= zll_main_loop193_in(69 downto 0);
      \instR4\ : \Main_incrPC\ port map (main_incrpc_in(69 downto 0), main_incrpc_out);
      \zll_main_loop225_inR1\ <= main_incrpc_out;
      \instR5\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR1\(69 downto 0), \zll_main_loop225_outR1\);
      zll_main_loop197_in <= \zll_main_loop225_outR1\;
      zll_main_loop166_in <= zll_main_loop197_in(142 downto 0);
      zll_main_loop163_in <= zll_main_loop166_in(69 downto 0);
      main_getpc_in <= zll_main_loop163_in(69 downto 0);
      \instR6\ : \Main_getPC\ port map (main_getpc_in(69 downto 0), main_getpc_out);
      zll_main_loop112_in <= main_getpc_out;
      \instR7\ : \ZLL_Main_loop112\ port map (zll_main_loop112_in(75 downto 0), zll_main_loop112_out);
      \zll_main_loop225_inR2\ <= zll_main_loop112_out;
      \instR8\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR2\(69 downto 0), \zll_main_loop225_outR2\);
      zll_main_loop31_in <= \zll_main_loop225_outR2\;
      zll_main_loop171_in <= zll_main_loop31_in(142 downto 0);
      zll_main_loop49_in <= zll_main_loop171_in(69 downto 0);
      main_getout_in <= zll_main_loop49_in(69 downto 0);
      \instR9\ : \Main_getOut\ port map (main_getout_in(69 downto 0), main_getout_out);
      zll_main_loop140_in <= main_getout_out;
      \instR10\ : \ZLL_Main_loop140\ port map (zll_main_loop140_in(84 downto 0), zll_main_loop140_out);
      zll_main_loop46_in <= zll_main_loop140_out;
      zll_main_loop80_in <= zll_main_loop46_in(142 downto 0);
      zll_main_loop47_in <= (zll_main_loop80_in(84 downto 70) & zll_main_loop80_in(69 downto 0));
      zll_pure_dispatch3_in <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      zll_main_loop86_in <= (zll_pure_dispatch3_in(90 downto 74) & zll_pure_dispatch3_in(69 downto 0));
      \zll_main_putins11_inR1\ <= (zll_main_loop86_in(86 downto 70) & zll_main_loop86_in(69 downto 0));
      \instR11\ : \ZLL_Main_putIns11\ port map (\zll_main_putins11_inR1\(86 downto 70), \zll_main_putins11_inR1\(69 downto 0), \zll_main_putins11_outR1\);
      \zll_main_loop225_inR3\ <= \zll_main_putins11_outR1\;
      \instR12\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR3\(69 downto 0), \zll_main_loop225_outR3\);
      zll_main_loop5_in <= \zll_main_loop225_outR3\;
      zll_main_loop19_in <= zll_main_loop5_in(142 downto 0);
      zll_main_loop113_in <= zll_main_loop19_in(69 downto 0);
      main_getdatain_in <= zll_main_loop113_in(69 downto 0);
      main_getins_in <= main_getdatain_in(69 downto 0);
      \instR13\ : \Main_getIns\ port map (main_getins_in(69 downto 0), main_getins_out);
      zll_main_getdatain1_in <= main_getins_out;
      zll_main_getdatain2_in <= zll_main_getdatain1_in(86 downto 0);
      zll_main_datain_in <= zll_main_getdatain2_in(86 downto 70);
      zll_main_datain2_in <= zll_main_datain_in(16 downto 0);
      zll_main_loop135_in <= (zll_main_datain2_in(7 downto 0) & zll_main_getdatain2_in(69 downto 0));
      main_putreg1_in <= zll_main_loop135_in(77 downto 0);
      zll_main_putreg27_in <= (main_putreg1_in(77 downto 70) & std_logic_vector'(B"0000") & main_putreg1_in(77 downto 70) & main_putreg1_in(69 downto 0));
      \instR14\ : \ZLL_Main_putReg27\ port map (zll_main_putreg27_in(89 downto 82), zll_main_putreg27_in(81 downto 80), zll_main_putreg27_in(79 downto 70), zll_main_putreg27_in(69 downto 0), zll_main_putreg27_out);
      \zll_main_loop225_inR4\ <= zll_main_putreg27_out;
      \instR15\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR4\(69 downto 0), \zll_main_loop225_outR4\);
      zll_main_loop221_in <= \zll_main_loop225_outR4\;
      \instR16\ : \ZLL_Main_loop221\ port map (zll_main_loop221_in(142 downto 0), zll_main_loop221_out);
      zll_pure_dispatch_in <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      zll_main_reset11_in <= (zll_pure_dispatch_in(90 downto 74) & zll_pure_dispatch_in(69 downto 0));
      \zll_main_putins11_inR2\ <= (zll_main_reset11_in(86 downto 70) & zll_main_reset11_in(69 downto 0));
      \instR17\ : \ZLL_Main_putIns11\ port map (\zll_main_putins11_inR2\(86 downto 70), \zll_main_putins11_inR2\(69 downto 0), \zll_main_putins11_outR2\);
      \zll_main_loop225_inR5\ <= \zll_main_putins11_outR2\;
      \instR18\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR5\(69 downto 0), \zll_main_loop225_outR5\);
      zll_main_reset4_in <= \zll_main_loop225_outR5\;
      zll_main_reset39_in <= zll_main_reset4_in(142 downto 0);
      zll_main_reset10_in <= zll_main_reset39_in(69 downto 0);
      \main_getpc_inR1\ <= zll_main_reset10_in(69 downto 0);
      \instR19\ : \Main_getPC\ port map (\main_getpc_inR1\(69 downto 0), \main_getpc_outR1\);
      \zll_main_loop112_inR1\ <= \main_getpc_outR1\;
      \instR20\ : \ZLL_Main_loop112\ port map (\zll_main_loop112_inR1\(75 downto 0), \zll_main_loop112_outR1\);
      \zll_main_loop225_inR6\ <= \zll_main_loop112_outR1\;
      \instR21\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR6\(69 downto 0), \zll_main_loop225_outR6\);
      zll_main_reset29_in <= \zll_main_loop225_outR6\;
      zll_main_reset45_in <= zll_main_reset29_in(142 downto 0);
      zll_main_reset37_in <= zll_main_reset45_in(69 downto 0);
      \main_getout_inR1\ <= zll_main_reset37_in(69 downto 0);
      \instR22\ : \Main_getOut\ port map (\main_getout_inR1\(69 downto 0), \main_getout_outR1\);
      \zll_main_loop140_inR1\ <= \main_getout_outR1\;
      \instR23\ : \ZLL_Main_loop140\ port map (\zll_main_loop140_inR1\(84 downto 0), \zll_main_loop140_outR1\);
      zll_main_reset46_in <= \zll_main_loop140_outR1\;
      zll_main_reset33_in <= zll_main_reset46_in(142 downto 0);
      zll_main_reset36_in <= (zll_main_reset33_in(84 downto 70) & zll_main_reset33_in(69 downto 0));
      \zll_pure_dispatch7_inR2\ <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      \instR24\ : \ZLL_Pure_dispatch7\ port map (\zll_pure_dispatch7_inR2\(90 downto 74), \zll_pure_dispatch7_inR2\(69 downto 0), \zll_pure_dispatch7_outR2\);
      \zll_pure_dispatch7_inR3\ <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      \instR25\ : \ZLL_Pure_dispatch7\ port map (\zll_pure_dispatch7_inR3\(90 downto 74), \zll_pure_dispatch7_inR3\(69 downto 0), \zll_pure_dispatch7_outR3\);
      zll_pure_dispatch2_in <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      zll_main_loop213_in <= (zll_pure_dispatch2_in(90 downto 74) & zll_pure_dispatch2_in(69 downto 0));
      \zll_main_putins11_inR3\ <= (zll_main_loop213_in(86 downto 70) & zll_main_loop213_in(69 downto 0));
      \instR26\ : \ZLL_Main_putIns11\ port map (\zll_main_putins11_inR3\(86 downto 70), \zll_main_putins11_inR3\(69 downto 0), \zll_main_putins11_outR3\);
      \zll_main_loop225_inR7\ <= \zll_main_putins11_outR3\;
      \instR27\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR7\(69 downto 0), \zll_main_loop225_outR7\);
      zll_main_loop181_in <= \zll_main_loop225_outR7\;
      zll_main_loop129_in <= zll_main_loop181_in(142 downto 0);
      zll_main_loop20_in <= zll_main_loop129_in(69 downto 0);
      \main_incrpc_inR1\ <= zll_main_loop20_in(69 downto 0);
      \instR28\ : \Main_incrPC\ port map (\main_incrpc_inR1\(69 downto 0), \main_incrpc_outR1\);
      \zll_main_loop225_inR8\ <= \main_incrpc_outR1\;
      \instR29\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR8\(69 downto 0), \zll_main_loop225_outR8\);
      zll_main_loop226_in <= \zll_main_loop225_outR8\;
      zll_main_loop216_in <= zll_main_loop226_in(142 downto 0);
      zll_main_loop148_in <= zll_main_loop216_in(69 downto 0);
      \main_getpc_inR2\ <= zll_main_loop148_in(69 downto 0);
      \instR30\ : \Main_getPC\ port map (\main_getpc_inR2\(69 downto 0), \main_getpc_outR2\);
      \zll_main_loop112_inR2\ <= \main_getpc_outR2\;
      \instR31\ : \ZLL_Main_loop112\ port map (\zll_main_loop112_inR2\(75 downto 0), \zll_main_loop112_outR2\);
      \zll_main_loop225_inR9\ <= \zll_main_loop112_outR2\;
      \instR32\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR9\(69 downto 0), \zll_main_loop225_outR9\);
      zll_main_loop110_in <= \zll_main_loop225_outR9\;
      zll_main_loop105_in <= zll_main_loop110_in(142 downto 0);
      zll_main_loop25_in <= zll_main_loop105_in(69 downto 0);
      \main_getout_inR2\ <= zll_main_loop25_in(69 downto 0);
      \instR33\ : \Main_getOut\ port map (\main_getout_inR2\(69 downto 0), \main_getout_outR2\);
      \zll_main_loop140_inR2\ <= \main_getout_outR2\;
      \instR34\ : \ZLL_Main_loop140\ port map (\zll_main_loop140_inR2\(84 downto 0), \zll_main_loop140_outR2\);
      zll_main_loop92_in <= \zll_main_loop140_outR2\;
      zll_main_loop2_in <= zll_main_loop92_in(142 downto 0);
      zll_main_loop24_in <= (zll_main_loop2_in(84 downto 70) & zll_main_loop2_in(69 downto 0));
      \zll_pure_dispatch7_inR4\ <= (\__in0\ & (\__resumption_tag\ & \__st0\));
      \instR35\ : \ZLL_Pure_dispatch7\ port map (\zll_pure_dispatch7_inR4\(90 downto 74), \zll_pure_dispatch7_inR4\(69 downto 0), \zll_pure_dispatch7_outR4\);
      rwtmp0 <= rw_resize(rw_cond(rw_eq(\zll_pure_dispatch7_inR4\(73 downto 70), std_logic_vector'(B"0001")), \zll_pure_dispatch7_outR4\, rw_cond(rw_eq(zll_pure_dispatch2_in(73 downto 70), std_logic_vector'(B"0010")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_loop24_in(84 downto 70) & std_logic_vector'(B"1000") & zll_main_loop24_in(69 downto 0)), rw_cond(rw_eq(\zll_pure_dispatch7_inR3\(73 downto 70), std_logic_vector'(B"0011")), \zll_pure_dispatch7_outR3\, rw_cond(rw_eq(\zll_pure_dispatch7_inR2\(73 downto 70), std_logic_vector'(B"0100")), \zll_pure_dispatch7_outR2\, rw_cond(rw_eq(zll_pure_dispatch_in(73 downto 70), std_logic_vector'(B"0101")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_reset36_in(84 downto 70) & std_logic_vector'(B"0001") & zll_main_reset36_in(69 downto 0)), rw_cond(rw_eq(zll_pure_dispatch3_in(73 downto 70), std_logic_vector'(B"0110")), zll_main_loop221_out, rw_cond(rw_eq(zll_pure_dispatch8_in(73 downto 70), std_logic_vector'(B"0111")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_loop47_in(84 downto 70) & std_logic_vector'(B"0110") & zll_main_loop47_in(69 downto 0)), rw_cond(rw_eq(\zll_pure_dispatch7_inR1\(73 downto 70), std_logic_vector'(B"1000")), \zll_pure_dispatch7_outR1\, zll_pure_dispatch7_out)))))))), 143);
      \__padding\ <= rwtmp0(142 downto 89);
      \__out0\ <= rwtmp0(88 downto 74);
      \__resumption_tag_next\ <= rwtmp0(73 downto 70);
      \__st0_next\ <= rwtmp0(69 downto 0);
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
entity \ZLL_Main_loop225\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (142 downto 0));
end entity;

architecture rtl of \ZLL_Main_loop225\ is

begin
res <= rw_resize(((std_logic_vector'(B"01") & rw_repl(71, std_logic_vector'(B"0"))) & arg0), 143);
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
      signal zll_main_r35_in : std_logic_vector (45 downto 0);
      signal zll_main_r35_out : std_logic_vector (7 downto 0);
begin
zll_main_r35_in <= (arg0 & arg2 & arg3 & arg4);
      inst : \ZLL_Main_r35\ port map (zll_main_r35_in(45 downto 38), zll_main_r35_in(37 downto 32), zll_main_r35_in(31 downto 15), zll_main_r35_in(14 downto 0), zll_main_r35_out);
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
      component \Main_putWeOut\ is
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
      component \ZLL_Main_loop225\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_putAddrOut1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putOut5\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putPC14\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putReg27\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (9 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putWeOut2\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zll_main_loop214_in : std_logic_vector (142 downto 0);
      signal main_loop_in : std_logic_vector (69 downto 0);
      signal main_getinstr_in : std_logic_vector (69 downto 0);
      signal main_getins_in : std_logic_vector (69 downto 0);
      signal main_getins_out : std_logic_vector (86 downto 0);
      signal zll_main_getinstr_in : std_logic_vector (86 downto 0);
      signal zll_main_getinstr1_in : std_logic_vector (86 downto 0);
      signal zll_main_instrin_in : std_logic_vector (16 downto 0);
      signal zll_main_instrin1_in : std_logic_vector (16 downto 0);
      signal zll_main_loop158_in : std_logic_vector (78 downto 0);
      signal zll_main_loop167_in : std_logic_vector (78 downto 0);
      signal zll_main_loop18_in : std_logic_vector (142 downto 0);
      signal zll_main_loop_in : std_logic_vector (142 downto 0);
      signal zll_main_loop211_in : std_logic_vector (78 downto 0);
      signal zll_main_loop73_in : std_logic_vector (139 downto 0);
      signal zll_main_loop121_in : std_logic_vector (139 downto 0);
      signal zll_main_loop58_in : std_logic_vector (151 downto 0);
      signal zll_main_loop223_in : std_logic_vector (151 downto 0);
      signal zll_main_loop215_in : std_logic_vector (148 downto 0);
      signal zll_main_loop199_in : std_logic_vector (78 downto 0);
      signal zll_main_loop50_in : std_logic_vector (75 downto 0);
      signal zll_main_loop52_in : std_logic_vector (75 downto 0);
      signal main_getreg1_in : std_logic_vector (69 downto 0);
      signal main_getreg1_out : std_logic_vector (77 downto 0);
      signal zll_main_loop222_in : std_logic_vector (83 downto 0);
      signal zll_main_bnz2_in : std_logic_vector (83 downto 0);
      signal binop_in : std_logic_vector (15 downto 0);
      signal zll_main_bnz4_in : std_logic_vector (84 downto 0);
      signal \binop_inR1\ : std_logic_vector (15 downto 0);
      signal zll_main_bnz3_in : std_logic_vector (76 downto 0);
      signal zll_main_bnz6_in : std_logic_vector (76 downto 0);
      signal zll_main_bnz1_in : std_logic_vector (75 downto 0);
      signal zll_main_putpc14_in : std_logic_vector (75 downto 0);
      signal zll_main_putpc14_out : std_logic_vector (69 downto 0);
      signal zll_main_nand9_in : std_logic_vector (70 downto 0);
      signal main_incrpc_in : std_logic_vector (69 downto 0);
      signal main_incrpc_out : std_logic_vector (69 downto 0);
      signal zll_main_loop225_in : std_logic_vector (69 downto 0);
      signal zll_main_loop225_out : std_logic_vector (142 downto 0);
      signal zll_main_loop179_in : std_logic_vector (142 downto 0);
      signal zll_main_loop116_in : std_logic_vector (142 downto 0);
      signal zll_main_loop35_in : std_logic_vector (69 downto 0);
      signal main_getpc_in : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zll_main_loop112_in : std_logic_vector (75 downto 0);
      signal zll_main_loop112_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR1\ : std_logic_vector (142 downto 0);
      signal zll_main_loop126_in : std_logic_vector (142 downto 0);
      signal zll_main_loop155_in : std_logic_vector (142 downto 0);
      signal zll_main_loop79_in : std_logic_vector (69 downto 0);
      signal main_getout_in : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_loop140_in : std_logic_vector (84 downto 0);
      signal zll_main_loop140_out : std_logic_vector (142 downto 0);
      signal zll_main_loop91_in : std_logic_vector (142 downto 0);
      signal zll_main_loop160_in : std_logic_vector (142 downto 0);
      signal zll_main_loop89_in : std_logic_vector (84 downto 0);
      signal zll_main_loop187_in : std_logic_vector (78 downto 0);
      signal zll_main_loop32_in : std_logic_vector (75 downto 0);
      signal zll_main_loop101_in : std_logic_vector (75 downto 0);
      signal zll_main_loop99_in : std_logic_vector (75 downto 0);
      signal main_getreg_in : std_logic_vector (71 downto 0);
      signal main_getreg_out : std_logic_vector (77 downto 0);
      signal zll_main_loop127_in : std_logic_vector (81 downto 0);
      signal zll_main_loop68_in : std_logic_vector (81 downto 0);
      signal zll_main_loop87_in : std_logic_vector (81 downto 0);
      signal zll_main_nand4_in : std_logic_vector (81 downto 0);
      signal \main_getreg_inR1\ : std_logic_vector (71 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (77 downto 0);
      signal zll_main_nand5_in : std_logic_vector (87 downto 0);
      signal zll_main_nand3_in : std_logic_vector (87 downto 0);
      signal \binop_inR2\ : std_logic_vector (15 downto 0);
      signal unop_in : std_logic_vector (7 downto 0);
      signal main_putreg_in : std_logic_vector (79 downto 0);
      signal zll_main_putreg27_in : std_logic_vector (89 downto 0);
      signal zll_main_putreg27_out : std_logic_vector (69 downto 0);
      signal \main_incrpc_inR1\ : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR2\ : std_logic_vector (142 downto 0);
      signal zll_main_loop88_in : std_logic_vector (142 downto 0);
      signal zll_main_loop84_in : std_logic_vector (142 downto 0);
      signal zll_main_loop15_in : std_logic_vector (69 downto 0);
      signal \main_getpc_inR1\ : std_logic_vector (69 downto 0);
      signal \main_getpc_outR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_inR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR1\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR3\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR3\ : std_logic_vector (142 downto 0);
      signal zll_main_loop118_in : std_logic_vector (142 downto 0);
      signal zll_main_loop204_in : std_logic_vector (142 downto 0);
      signal zll_main_loop178_in : std_logic_vector (69 downto 0);
      signal \main_getout_inR1\ : std_logic_vector (69 downto 0);
      signal \main_getout_outR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_inR1\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR1\ : std_logic_vector (142 downto 0);
      signal zll_main_loop30_in : std_logic_vector (142 downto 0);
      signal zll_main_loop201_in : std_logic_vector (142 downto 0);
      signal zll_main_loop209_in : std_logic_vector (84 downto 0);
      signal zll_main_loop71_in : std_logic_vector (78 downto 0);
      signal zll_main_loop95_in : std_logic_vector (75 downto 0);
      signal zll_main_loop82_in : std_logic_vector (75 downto 0);
      signal \main_getreg1_inR1\ : std_logic_vector (69 downto 0);
      signal \main_getreg1_outR1\ : std_logic_vector (77 downto 0);
      signal zll_main_loop132_in : std_logic_vector (83 downto 0);
      signal zll_main_st_in : std_logic_vector (83 downto 0);
      signal zll_main_putaddrout1_in : std_logic_vector (75 downto 0);
      signal zll_main_putaddrout1_out : std_logic_vector (69 downto 0);
      signal zll_main_st3_in : std_logic_vector (77 downto 0);
      signal zll_main_putdataout3_in : std_logic_vector (77 downto 0);
      signal \main_getout_inR2\ : std_logic_vector (69 downto 0);
      signal \main_getout_outR2\ : std_logic_vector (84 downto 0);
      signal zll_main_putdataout4_in : std_logic_vector (92 downto 0);
      signal zll_main_putdataout7_in : std_logic_vector (92 downto 0);
      signal zll_main_putdataout6_in : std_logic_vector (22 downto 0);
      signal zll_main_putdataout11_in : std_logic_vector (22 downto 0);
      signal zll_main_putdataout_in : std_logic_vector (22 downto 0);
      signal zll_main_putout5_in : std_logic_vector (84 downto 0);
      signal zll_main_putout5_out : std_logic_vector (69 downto 0);
      signal main_putweout1_in : std_logic_vector (69 downto 0);
      signal zll_main_putweout2_in : std_logic_vector (70 downto 0);
      signal zll_main_putweout2_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR4\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR4\ : std_logic_vector (142 downto 0);
      signal zll_main_loop159_in : std_logic_vector (142 downto 0);
      signal zll_main_loop38_in : std_logic_vector (142 downto 0);
      signal zll_main_loop14_in : std_logic_vector (69 downto 0);
      signal \main_getout_inR3\ : std_logic_vector (69 downto 0);
      signal \main_getout_outR3\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_inR2\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR2\ : std_logic_vector (142 downto 0);
      signal zll_main_loop104_in : std_logic_vector (142 downto 0);
      signal zll_main_loop78_in : std_logic_vector (142 downto 0);
      signal zll_main_loop41_in : std_logic_vector (84 downto 0);
      signal zll_main_loop100_in : std_logic_vector (78 downto 0);
      signal zll_main_loop180_in : std_logic_vector (75 downto 0);
      signal zll_main_loop142_in : std_logic_vector (75 downto 0);
      signal \zll_main_putaddrout1_inR1\ : std_logic_vector (75 downto 0);
      signal \zll_main_putaddrout1_outR1\ : std_logic_vector (69 downto 0);
      signal main_putweout_in : std_logic_vector (69 downto 0);
      signal main_putweout_out : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR5\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR5\ : std_logic_vector (142 downto 0);
      signal zll_main_loop173_in : std_logic_vector (142 downto 0);
      signal zll_main_loop65_in : std_logic_vector (142 downto 0);
      signal zll_main_loop123_in : std_logic_vector (69 downto 0);
      signal \main_getout_inR4\ : std_logic_vector (69 downto 0);
      signal \main_getout_outR4\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_inR3\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR3\ : std_logic_vector (142 downto 0);
      signal zll_main_loop128_in : std_logic_vector (142 downto 0);
      signal zll_main_loop10_in : std_logic_vector (142 downto 0);
      signal zll_main_loop21_in : std_logic_vector (84 downto 0);
      signal zll_main_loop9_in : std_logic_vector (78 downto 0);
      signal zll_main_loop76_in : std_logic_vector (69 downto 0);
      signal \main_incrpc_inR2\ : std_logic_vector (69 downto 0);
      signal \main_incrpc_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR6\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR6\ : std_logic_vector (142 downto 0);
      signal zll_main_loop147_in : std_logic_vector (142 downto 0);
      signal zll_main_loop106_in : std_logic_vector (142 downto 0);
      signal zll_main_loop124_in : std_logic_vector (69 downto 0);
      signal \main_getpc_inR2\ : std_logic_vector (69 downto 0);
      signal \main_getpc_outR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_inR2\ : std_logic_vector (75 downto 0);
      signal \zll_main_loop112_outR2\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_inR7\ : std_logic_vector (69 downto 0);
      signal \zll_main_loop225_outR7\ : std_logic_vector (142 downto 0);
      signal zll_main_loop217_in : std_logic_vector (142 downto 0);
      signal zll_main_loop138_in : std_logic_vector (142 downto 0);
      signal zll_main_loop139_in : std_logic_vector (69 downto 0);
      signal \main_getout_inR5\ : std_logic_vector (69 downto 0);
      signal \main_getout_outR5\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_inR4\ : std_logic_vector (84 downto 0);
      signal \zll_main_loop140_outR4\ : std_logic_vector (142 downto 0);
      signal zll_main_loop13_in : std_logic_vector (142 downto 0);
      signal zll_main_loop198_in : std_logic_vector (142 downto 0);
      signal zll_main_loop200_in : std_logic_vector (84 downto 0);
begin
zll_main_loop214_in <= arg0;
      main_loop_in <= zll_main_loop214_in(69 downto 0);
      main_getinstr_in <= main_loop_in(69 downto 0);
      main_getins_in <= main_getinstr_in(69 downto 0);
      inst : \Main_getIns\ port map (main_getins_in(69 downto 0), main_getins_out);
      zll_main_getinstr_in <= main_getins_out;
      zll_main_getinstr1_in <= zll_main_getinstr_in(86 downto 0);
      zll_main_instrin_in <= zll_main_getinstr1_in(86 downto 70);
      zll_main_instrin1_in <= zll_main_instrin_in(16 downto 0);
      zll_main_loop158_in <= (zll_main_instrin1_in(16 downto 8) & zll_main_getinstr1_in(69 downto 0));
      zll_main_loop167_in <= zll_main_loop158_in(78 downto 0);
      zll_main_loop18_in <= rw_resize((rw_repl(64, std_logic_vector'(B"0")) & zll_main_loop167_in(78 downto 70) & zll_main_loop167_in(69 downto 0)), 143);
      zll_main_loop_in <= zll_main_loop18_in(142 downto 0);
      zll_main_loop211_in <= (zll_main_loop_in(78 downto 70) & zll_main_loop_in(69 downto 0));
      zll_main_loop73_in <= (zll_main_loop211_in(69 downto 0) & zll_main_loop211_in(69 downto 0));
      zll_main_loop121_in <= zll_main_loop73_in(139 downto 0);
      zll_main_loop58_in <= (zll_main_loop211_in(78 downto 70) & (std_logic_vector'(B"011") & zll_main_loop121_in(139 downto 70) & zll_main_loop121_in(69 downto 0)));
      zll_main_loop223_in <= (zll_main_loop58_in(151 downto 143) & zll_main_loop58_in(142 downto 0));
      zll_main_loop215_in <= (zll_main_loop223_in(151 downto 143) & zll_main_loop223_in(139 downto 70) & zll_main_loop223_in(69 downto 0));
      zll_main_loop199_in <= (zll_main_loop215_in(69 downto 0) & zll_main_loop215_in(148 downto 140));
      zll_main_loop50_in <= (zll_main_loop199_in(78 downto 9) & zll_main_loop199_in(5 downto 0));
      zll_main_loop52_in <= (zll_main_loop50_in(5 downto 0) & zll_main_loop50_in(75 downto 6));
      main_getreg1_in <= zll_main_loop52_in(69 downto 0);
      \instR1\ : \Main_getReg1\ port map (main_getreg1_in(69 downto 0), main_getreg1_out);
      zll_main_loop222_in <= (zll_main_loop52_in(75 downto 70) & main_getreg1_out);
      zll_main_bnz2_in <= (zll_main_loop222_in(83 downto 78) & zll_main_loop222_in(77 downto 0));
      binop_in <= (zll_main_bnz2_in(77 downto 70) & std_logic_vector'(B"00000000"));
      zll_main_bnz4_in <= rw_resize((zll_main_bnz2_in(77 downto 70) & zll_main_bnz2_in(83 downto 78) & rw_eq(binop_in(15 downto 8), binop_in(7 downto 0)) & zll_main_bnz2_in(69 downto 0)), 85);
      \binop_inR1\ <= (zll_main_bnz4_in(84 downto 77) & std_logic_vector'(B"00000000"));
      zll_main_bnz3_in <= rw_resize((zll_main_bnz4_in(76 downto 71) & rw_eq(\binop_inR1\(15 downto 8), \binop_inR1\(7 downto 0)) & zll_main_bnz4_in(69 downto 0)), 77);
      zll_main_bnz6_in <= (zll_main_bnz3_in(69 downto 0) & zll_main_bnz3_in(76 downto 71) & zll_main_bnz3_in(70 downto 70));
      zll_main_bnz1_in <= (zll_main_bnz6_in(76 downto 7) & zll_main_bnz6_in(6 downto 1));
      zll_main_putpc14_in <= (zll_main_bnz1_in(5 downto 0) & zll_main_bnz1_in(75 downto 6));
      \instR2\ : \ZLL_Main_putPC14\ port map (zll_main_putpc14_in(75 downto 70), zll_main_putpc14_in(69 downto 0), zll_main_putpc14_out);
      zll_main_nand9_in <= (zll_main_bnz4_in(69 downto 0) & zll_main_bnz4_in(70 downto 70));
      main_incrpc_in <= zll_main_nand9_in(70 downto 1);
      \instR3\ : \Main_incrPC\ port map (main_incrpc_in(69 downto 0), main_incrpc_out);
      zll_main_loop225_in <= rw_resize(rw_cond(rw_eq(zll_main_nand9_in(0 downto 0), std_logic_vector'(B"1")), main_incrpc_out, zll_main_putpc14_out), 70);
      \instR4\ : \ZLL_Main_loop225\ port map (zll_main_loop225_in(69 downto 0), zll_main_loop225_out);
      zll_main_loop179_in <= zll_main_loop225_out;
      zll_main_loop116_in <= zll_main_loop179_in(142 downto 0);
      zll_main_loop35_in <= zll_main_loop116_in(69 downto 0);
      main_getpc_in <= zll_main_loop35_in(69 downto 0);
      \instR5\ : \Main_getPC\ port map (main_getpc_in(69 downto 0), main_getpc_out);
      zll_main_loop112_in <= main_getpc_out;
      \instR6\ : \ZLL_Main_loop112\ port map (zll_main_loop112_in(75 downto 0), zll_main_loop112_out);
      \zll_main_loop225_inR1\ <= zll_main_loop112_out;
      \instR7\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR1\(69 downto 0), \zll_main_loop225_outR1\);
      zll_main_loop126_in <= \zll_main_loop225_outR1\;
      zll_main_loop155_in <= zll_main_loop126_in(142 downto 0);
      zll_main_loop79_in <= zll_main_loop155_in(69 downto 0);
      main_getout_in <= zll_main_loop79_in(69 downto 0);
      \instR8\ : \Main_getOut\ port map (main_getout_in(69 downto 0), main_getout_out);
      zll_main_loop140_in <= main_getout_out;
      \instR9\ : \ZLL_Main_loop140\ port map (zll_main_loop140_in(84 downto 0), zll_main_loop140_out);
      zll_main_loop91_in <= zll_main_loop140_out;
      zll_main_loop160_in <= zll_main_loop91_in(142 downto 0);
      zll_main_loop89_in <= (zll_main_loop160_in(84 downto 70) & zll_main_loop160_in(69 downto 0));
      zll_main_loop187_in <= (zll_main_loop215_in(69 downto 0) & zll_main_loop215_in(148 downto 140));
      zll_main_loop32_in <= (zll_main_loop187_in(78 downto 9) & zll_main_loop187_in(5 downto 4) & zll_main_loop187_in(3 downto 2) & zll_main_loop187_in(1 downto 0));
      zll_main_loop101_in <= (zll_main_loop32_in(5 downto 4) & zll_main_loop32_in(75 downto 6) & zll_main_loop32_in(3 downto 2) & zll_main_loop32_in(1 downto 0));
      zll_main_loop99_in <= (zll_main_loop101_in(75 downto 74) & zll_main_loop101_in(3 downto 2) & zll_main_loop101_in(1 downto 0) & zll_main_loop101_in(73 downto 4));
      main_getreg_in <= (zll_main_loop99_in(73 downto 72) & zll_main_loop99_in(69 downto 0));
      \instR10\ : \Main_getReg\ port map (main_getreg_in(71 downto 70), main_getreg_in(69 downto 0), main_getreg_out);
      zll_main_loop127_in <= (zll_main_loop99_in(71 downto 70) & zll_main_loop99_in(75 downto 74) & main_getreg_out);
      zll_main_loop68_in <= (zll_main_loop127_in(81 downto 80) & zll_main_loop127_in(79 downto 78) & zll_main_loop127_in(77 downto 0));
      zll_main_loop87_in <= (zll_main_loop68_in(79 downto 78) & zll_main_loop68_in(81 downto 80) & zll_main_loop68_in(77 downto 70) & zll_main_loop68_in(69 downto 0));
      zll_main_nand4_in <= (zll_main_loop87_in(79 downto 78) & zll_main_loop87_in(81 downto 80) & zll_main_loop87_in(77 downto 70) & zll_main_loop87_in(69 downto 0));
      \main_getreg_inR1\ <= (zll_main_nand4_in(81 downto 80) & zll_main_nand4_in(69 downto 0));
      \instR11\ : \Main_getReg\ port map (\main_getreg_inR1\(71 downto 70), \main_getreg_inR1\(69 downto 0), \main_getreg_outR1\);
      zll_main_nand5_in <= (zll_main_nand4_in(77 downto 70) & zll_main_nand4_in(79 downto 78) & \main_getreg_outR1\);
      zll_main_nand3_in <= (zll_main_nand5_in(87 downto 80) & zll_main_nand5_in(79 downto 78) & zll_main_nand5_in(77 downto 0));
      \binop_inR2\ <= (zll_main_nand3_in(87 downto 80) & zll_main_nand3_in(77 downto 70));
      unop_in <= rw_resize(rw_and(\binop_inR2\(15 downto 8), \binop_inR2\(7 downto 0)), 8);
      main_putreg_in <= rw_resize((zll_main_nand3_in(79 downto 78) & rw_not(unop_in(7 downto 0)) & zll_main_nand3_in(69 downto 0)), 80);
      zll_main_putreg27_in <= (main_putreg_in(77 downto 70) & main_putreg_in(79 downto 78) & main_putreg_in(79 downto 78) & main_putreg_in(77 downto 70) & main_putreg_in(69 downto 0));
      \instR12\ : \ZLL_Main_putReg27\ port map (zll_main_putreg27_in(89 downto 82), zll_main_putreg27_in(81 downto 80), zll_main_putreg27_in(79 downto 70), zll_main_putreg27_in(69 downto 0), zll_main_putreg27_out);
      \main_incrpc_inR1\ <= zll_main_putreg27_out;
      \instR13\ : \Main_incrPC\ port map (\main_incrpc_inR1\(69 downto 0), \main_incrpc_outR1\);
      \zll_main_loop225_inR2\ <= \main_incrpc_outR1\;
      \instR14\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR2\(69 downto 0), \zll_main_loop225_outR2\);
      zll_main_loop88_in <= \zll_main_loop225_outR2\;
      zll_main_loop84_in <= zll_main_loop88_in(142 downto 0);
      zll_main_loop15_in <= zll_main_loop84_in(69 downto 0);
      \main_getpc_inR1\ <= zll_main_loop15_in(69 downto 0);
      \instR15\ : \Main_getPC\ port map (\main_getpc_inR1\(69 downto 0), \main_getpc_outR1\);
      \zll_main_loop112_inR1\ <= \main_getpc_outR1\;
      \instR16\ : \ZLL_Main_loop112\ port map (\zll_main_loop112_inR1\(75 downto 0), \zll_main_loop112_outR1\);
      \zll_main_loop225_inR3\ <= \zll_main_loop112_outR1\;
      \instR17\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR3\(69 downto 0), \zll_main_loop225_outR3\);
      zll_main_loop118_in <= \zll_main_loop225_outR3\;
      zll_main_loop204_in <= zll_main_loop118_in(142 downto 0);
      zll_main_loop178_in <= zll_main_loop204_in(69 downto 0);
      \main_getout_inR1\ <= zll_main_loop178_in(69 downto 0);
      \instR18\ : \Main_getOut\ port map (\main_getout_inR1\(69 downto 0), \main_getout_outR1\);
      \zll_main_loop140_inR1\ <= \main_getout_outR1\;
      \instR19\ : \ZLL_Main_loop140\ port map (\zll_main_loop140_inR1\(84 downto 0), \zll_main_loop140_outR1\);
      zll_main_loop30_in <= \zll_main_loop140_outR1\;
      zll_main_loop201_in <= zll_main_loop30_in(142 downto 0);
      zll_main_loop209_in <= (zll_main_loop201_in(84 downto 70) & zll_main_loop201_in(69 downto 0));
      zll_main_loop71_in <= (zll_main_loop215_in(69 downto 0) & zll_main_loop215_in(148 downto 140));
      zll_main_loop95_in <= (zll_main_loop71_in(78 downto 9) & zll_main_loop71_in(5 downto 0));
      zll_main_loop82_in <= (zll_main_loop95_in(5 downto 0) & zll_main_loop95_in(75 downto 6));
      \main_getreg1_inR1\ <= zll_main_loop82_in(69 downto 0);
      \instR20\ : \Main_getReg1\ port map (\main_getreg1_inR1\(69 downto 0), \main_getreg1_outR1\);
      zll_main_loop132_in <= (zll_main_loop82_in(75 downto 70) & \main_getreg1_outR1\);
      zll_main_st_in <= (zll_main_loop132_in(83 downto 78) & zll_main_loop132_in(77 downto 0));
      zll_main_putaddrout1_in <= (zll_main_st_in(83 downto 78) & zll_main_st_in(69 downto 0));
      \instR21\ : \ZLL_Main_putAddrOut1\ port map (zll_main_putaddrout1_in(75 downto 70), zll_main_putaddrout1_in(69 downto 0), zll_main_putaddrout1_out);
      zll_main_st3_in <= (zll_main_st_in(77 downto 70) & zll_main_putaddrout1_out);
      zll_main_putdataout3_in <= (zll_main_st3_in(77 downto 70) & zll_main_st3_in(69 downto 0));
      \main_getout_inR2\ <= zll_main_putdataout3_in(69 downto 0);
      \instR22\ : \Main_getOut\ port map (\main_getout_inR2\(69 downto 0), \main_getout_outR2\);
      zll_main_putdataout4_in <= (zll_main_putdataout3_in(77 downto 70) & \main_getout_outR2\);
      zll_main_putdataout7_in <= (zll_main_putdataout4_in(92 downto 85) & zll_main_putdataout4_in(84 downto 0));
      zll_main_putdataout6_in <= (zll_main_putdataout7_in(92 downto 85) & zll_main_putdataout7_in(84 downto 70));
      zll_main_putdataout11_in <= (zll_main_putdataout6_in(22 downto 15) & zll_main_putdataout6_in(14 downto 0));
      zll_main_putdataout_in <= (zll_main_putdataout11_in(22 downto 15) & zll_main_putdataout11_in(13 downto 8) & zll_main_putdataout11_in(14 downto 14) & zll_main_putdataout11_in(7 downto 0));
      zll_main_putout5_in <= ((zll_main_putdataout_in(8 downto 8) & zll_main_putdataout_in(14 downto 9) & zll_main_putdataout_in(22 downto 15)) & zll_main_putdataout7_in(69 downto 0));
      \instR23\ : \ZLL_Main_putOut5\ port map (zll_main_putout5_in(84 downto 70), zll_main_putout5_in(69 downto 0), zll_main_putout5_out);
      main_putweout1_in <= zll_main_putout5_out;
      zll_main_putweout2_in <= (std_logic_vector'(B"1") & main_putweout1_in(69 downto 0));
      \instR24\ : \ZLL_Main_putWeOut2\ port map (zll_main_putweout2_in(70 downto 70), zll_main_putweout2_in(69 downto 0), zll_main_putweout2_out);
      \zll_main_loop225_inR4\ <= zll_main_putweout2_out;
      \instR25\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR4\(69 downto 0), \zll_main_loop225_outR4\);
      zll_main_loop159_in <= \zll_main_loop225_outR4\;
      zll_main_loop38_in <= zll_main_loop159_in(142 downto 0);
      zll_main_loop14_in <= zll_main_loop38_in(69 downto 0);
      \main_getout_inR3\ <= zll_main_loop14_in(69 downto 0);
      \instR26\ : \Main_getOut\ port map (\main_getout_inR3\(69 downto 0), \main_getout_outR3\);
      \zll_main_loop140_inR2\ <= \main_getout_outR3\;
      \instR27\ : \ZLL_Main_loop140\ port map (\zll_main_loop140_inR2\(84 downto 0), \zll_main_loop140_outR2\);
      zll_main_loop104_in <= \zll_main_loop140_outR2\;
      zll_main_loop78_in <= zll_main_loop104_in(142 downto 0);
      zll_main_loop41_in <= (zll_main_loop78_in(84 downto 70) & zll_main_loop78_in(69 downto 0));
      zll_main_loop100_in <= (zll_main_loop215_in(69 downto 0) & zll_main_loop215_in(148 downto 140));
      zll_main_loop180_in <= (zll_main_loop100_in(78 downto 9) & zll_main_loop100_in(5 downto 0));
      zll_main_loop142_in <= (zll_main_loop180_in(5 downto 0) & zll_main_loop180_in(75 downto 6));
      \zll_main_putaddrout1_inR1\ <= (zll_main_loop142_in(75 downto 70) & zll_main_loop142_in(69 downto 0));
      \instR28\ : \ZLL_Main_putAddrOut1\ port map (\zll_main_putaddrout1_inR1\(75 downto 70), \zll_main_putaddrout1_inR1\(69 downto 0), \zll_main_putaddrout1_outR1\);
      main_putweout_in <= \zll_main_putaddrout1_outR1\;
      \instR29\ : \Main_putWeOut\ port map (main_putweout_in(69 downto 0), main_putweout_out);
      \zll_main_loop225_inR5\ <= main_putweout_out;
      \instR30\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR5\(69 downto 0), \zll_main_loop225_outR5\);
      zll_main_loop173_in <= \zll_main_loop225_outR5\;
      zll_main_loop65_in <= zll_main_loop173_in(142 downto 0);
      zll_main_loop123_in <= zll_main_loop65_in(69 downto 0);
      \main_getout_inR4\ <= zll_main_loop123_in(69 downto 0);
      \instR31\ : \Main_getOut\ port map (\main_getout_inR4\(69 downto 0), \main_getout_outR4\);
      \zll_main_loop140_inR3\ <= \main_getout_outR4\;
      \instR32\ : \ZLL_Main_loop140\ port map (\zll_main_loop140_inR3\(84 downto 0), \zll_main_loop140_outR3\);
      zll_main_loop128_in <= \zll_main_loop140_outR3\;
      zll_main_loop10_in <= zll_main_loop128_in(142 downto 0);
      zll_main_loop21_in <= (zll_main_loop10_in(84 downto 70) & zll_main_loop10_in(69 downto 0));
      zll_main_loop9_in <= (zll_main_loop215_in(69 downto 0) & zll_main_loop215_in(148 downto 140));
      zll_main_loop76_in <= zll_main_loop9_in(78 downto 9);
      \main_incrpc_inR2\ <= zll_main_loop76_in(69 downto 0);
      \instR33\ : \Main_incrPC\ port map (\main_incrpc_inR2\(69 downto 0), \main_incrpc_outR2\);
      \zll_main_loop225_inR6\ <= \main_incrpc_outR2\;
      \instR34\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR6\(69 downto 0), \zll_main_loop225_outR6\);
      zll_main_loop147_in <= \zll_main_loop225_outR6\;
      zll_main_loop106_in <= zll_main_loop147_in(142 downto 0);
      zll_main_loop124_in <= zll_main_loop106_in(69 downto 0);
      \main_getpc_inR2\ <= zll_main_loop124_in(69 downto 0);
      \instR35\ : \Main_getPC\ port map (\main_getpc_inR2\(69 downto 0), \main_getpc_outR2\);
      \zll_main_loop112_inR2\ <= \main_getpc_outR2\;
      \instR36\ : \ZLL_Main_loop112\ port map (\zll_main_loop112_inR2\(75 downto 0), \zll_main_loop112_outR2\);
      \zll_main_loop225_inR7\ <= \zll_main_loop112_outR2\;
      \instR37\ : \ZLL_Main_loop225\ port map (\zll_main_loop225_inR7\(69 downto 0), \zll_main_loop225_outR7\);
      zll_main_loop217_in <= \zll_main_loop225_outR7\;
      zll_main_loop138_in <= zll_main_loop217_in(142 downto 0);
      zll_main_loop139_in <= zll_main_loop138_in(69 downto 0);
      \main_getout_inR5\ <= zll_main_loop139_in(69 downto 0);
      \instR38\ : \Main_getOut\ port map (\main_getout_inR5\(69 downto 0), \main_getout_outR5\);
      \zll_main_loop140_inR4\ <= \main_getout_outR5\;
      \instR39\ : \ZLL_Main_loop140\ port map (\zll_main_loop140_inR4\(84 downto 0), \zll_main_loop140_outR4\);
      zll_main_loop13_in <= \zll_main_loop140_outR4\;
      zll_main_loop198_in <= zll_main_loop13_in(142 downto 0);
      zll_main_loop200_in <= (zll_main_loop198_in(84 downto 70) & zll_main_loop198_in(69 downto 0));
      res <= rw_resize(rw_cond(rw_eq(zll_main_loop9_in(8 downto 6), std_logic_vector'(B"000")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_loop200_in(84 downto 70) & std_logic_vector'(B"0000") & zll_main_loop200_in(69 downto 0)), rw_cond(rw_eq(zll_main_loop100_in(8 downto 6), std_logic_vector'(B"001")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_loop21_in(84 downto 70) & std_logic_vector'(B"0111") & zll_main_loop21_in(69 downto 0)), rw_cond(rw_eq(zll_main_loop71_in(8 downto 6), std_logic_vector'(B"010")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_loop41_in(84 downto 70) & std_logic_vector'(B"0010") & zll_main_loop41_in(69 downto 0)), rw_cond(rw_eq(zll_main_loop187_in(8 downto 6), std_logic_vector'(B"011")), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_loop209_in(84 downto 70) & std_logic_vector'(B"0100") & zll_main_loop209_in(69 downto 0)), ((std_logic_vector'(B"1") & rw_repl(53, std_logic_vector'(B"0"))) & zll_main_loop89_in(84 downto 70) & std_logic_vector'(B"0011") & zll_main_loop89_in(69 downto 0)))))), 143);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putPC14\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putPC14\ is
signal zll_main_putpc11_in : std_logic_vector (145 downto 0);
      signal zll_main_putpc10_in : std_logic_vector (145 downto 0);
      signal zll_main_putpc13_in : std_logic_vector (75 downto 0);
      signal zll_main_putpc8_in : std_logic_vector (75 downto 0);
      signal zll_main_putpc6_in : std_logic_vector (75 downto 0);
      signal zll_main_putpc12_in : std_logic_vector (75 downto 0);
      signal zll_main_putpc7_in : std_logic_vector (69 downto 0);
begin
zll_main_putpc11_in <= (arg0 & arg1 & arg1);
      zll_main_putpc10_in <= (zll_main_putpc11_in(145 downto 140) & zll_main_putpc11_in(139 downto 0));
      zll_main_putpc13_in <= (zll_main_putpc10_in(145 downto 140) & zll_main_putpc10_in(139 downto 70));
      zll_main_putpc8_in <= (zll_main_putpc13_in(75 downto 70) & zll_main_putpc13_in(69 downto 0));
      zll_main_putpc6_in <= (zll_main_putpc8_in(75 downto 70) & zll_main_putpc8_in(53 downto 46) & zll_main_putpc8_in(69 downto 62) & zll_main_putpc8_in(61 downto 54) & zll_main_putpc8_in(45 downto 38) & zll_main_putpc8_in(37 downto 32) & zll_main_putpc8_in(31 downto 15) & zll_main_putpc8_in(14 downto 0));
      zll_main_putpc12_in <= (zll_main_putpc6_in(75 downto 70) & zll_main_putpc6_in(45 downto 38) & zll_main_putpc6_in(69 downto 62) & zll_main_putpc6_in(61 downto 54) & zll_main_putpc6_in(53 downto 46) & zll_main_putpc6_in(37 downto 32) & zll_main_putpc6_in(31 downto 15) & zll_main_putpc6_in(14 downto 0));
      zll_main_putpc7_in <= (zll_main_putpc12_in(75 downto 70) & zll_main_putpc12_in(69 downto 62) & zll_main_putpc12_in(61 downto 54) & zll_main_putpc12_in(53 downto 46) & zll_main_putpc12_in(45 downto 38) & zll_main_putpc12_in(31 downto 15) & zll_main_putpc12_in(14 downto 0));
      res <= (zll_main_putpc7_in(47 downto 40) & zll_main_putpc7_in(39 downto 32) & zll_main_putpc7_in(55 downto 48) & zll_main_putpc7_in(63 downto 56) & zll_main_putpc7_in(69 downto 64) & zll_main_putpc7_in(31 downto 15) & zll_main_putpc7_in(14 downto 0));
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
      signal zll_main_getreg6_in : std_logic_vector (73 downto 0);
      signal zll_main_getreg6_out : std_logic_vector (77 downto 0);
begin
zll_main_getreg6_in <= (std_logic_vector'(B"0000") & arg0);
      inst : \ZLL_Main_getReg6\ port map (zll_main_getreg6_in(73 downto 72), zll_main_getreg6_in(71 downto 70), zll_main_getreg6_in(69 downto 0), zll_main_getreg6_out);
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
component \ZLL_Main_loop221\ is
      port (arg0 : in std_logic_vector (142 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_loop225\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (142 downto 0));
      end component;
      component \ZLL_Main_putIns11\ is
      port (arg0 : in std_logic_vector (16 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zll_main_loop220_in : std_logic_vector (86 downto 0);
      signal zll_main_putins11_in : std_logic_vector (86 downto 0);
      signal zll_main_putins11_out : std_logic_vector (69 downto 0);
      signal zll_main_loop225_in : std_logic_vector (69 downto 0);
      signal zll_main_loop225_out : std_logic_vector (142 downto 0);
      signal zll_main_loop221_in : std_logic_vector (142 downto 0);
      signal zll_main_loop221_out : std_logic_vector (142 downto 0);
begin
zll_main_loop220_in <= (arg0 & arg1);
      zll_main_putins11_in <= (zll_main_loop220_in(86 downto 70) & zll_main_loop220_in(69 downto 0));
      inst : \ZLL_Main_putIns11\ port map (zll_main_putins11_in(86 downto 70), zll_main_putins11_in(69 downto 0), zll_main_putins11_out);
      zll_main_loop225_in <= zll_main_putins11_out;
      \instR1\ : \ZLL_Main_loop225\ port map (zll_main_loop225_in(69 downto 0), zll_main_loop225_out);
      zll_main_loop221_in <= zll_main_loop225_out;
      \instR2\ : \ZLL_Main_loop221\ port map (zll_main_loop221_in(142 downto 0), zll_main_loop221_out);
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
      signal zll_main_getreg6_in : std_logic_vector (73 downto 0);
      signal zll_main_getreg6_out : std_logic_vector (77 downto 0);
begin
zll_main_getreg6_in <= (arg0 & arg0 & arg1);
      inst : \ZLL_Main_getReg6\ port map (zll_main_getreg6_in(73 downto 72), zll_main_getreg6_in(71 downto 70), zll_main_getreg6_in(69 downto 0), zll_main_getreg6_out);
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
signal zll_main_r25_in : std_logic_vector (39 downto 0);
      signal zll_main_r26_in : std_logic_vector (22 downto 0);
begin
zll_main_r25_in <= (arg0 & arg2 & arg3);
      zll_main_r26_in <= (zll_main_r25_in(39 downto 32) & zll_main_r25_in(14 downto 0));
      res <= zll_main_r26_in(22 downto 15);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putIns11\ is
port (arg0 : in std_logic_vector (16 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putIns11\ is
signal zll_main_putins12_in : std_logic_vector (156 downto 0);
      signal zll_main_putins1_in : std_logic_vector (156 downto 0);
      signal zll_main_putins2_in : std_logic_vector (86 downto 0);
      signal zll_main_putins3_in : std_logic_vector (86 downto 0);
      signal zll_main_putins4_in : std_logic_vector (86 downto 0);
      signal zll_main_putins9_in : std_logic_vector (86 downto 0);
      signal zll_main_putins14_in : std_logic_vector (86 downto 0);
begin
zll_main_putins12_in <= (arg0 & arg1 & arg1);
      zll_main_putins1_in <= (zll_main_putins12_in(156 downto 140) & zll_main_putins12_in(139 downto 0));
      zll_main_putins2_in <= (zll_main_putins1_in(156 downto 140) & zll_main_putins1_in(139 downto 70));
      zll_main_putins3_in <= (zll_main_putins2_in(86 downto 70) & zll_main_putins2_in(69 downto 0));
      zll_main_putins4_in <= (zll_main_putins3_in(86 downto 70) & zll_main_putins3_in(53 downto 46) & zll_main_putins3_in(69 downto 62) & zll_main_putins3_in(61 downto 54) & zll_main_putins3_in(45 downto 38) & zll_main_putins3_in(37 downto 32) & zll_main_putins3_in(31 downto 15) & zll_main_putins3_in(14 downto 0));
      zll_main_putins9_in <= (zll_main_putins4_in(86 downto 70) & zll_main_putins4_in(45 downto 38) & zll_main_putins4_in(69 downto 62) & zll_main_putins4_in(61 downto 54) & zll_main_putins4_in(53 downto 46) & zll_main_putins4_in(37 downto 32) & zll_main_putins4_in(31 downto 15) & zll_main_putins4_in(14 downto 0));
      zll_main_putins14_in <= (zll_main_putins9_in(86 downto 70) & zll_main_putins9_in(69 downto 62) & zll_main_putins9_in(37 downto 32) & zll_main_putins9_in(61 downto 54) & zll_main_putins9_in(53 downto 46) & zll_main_putins9_in(45 downto 38) & zll_main_putins9_in(31 downto 15) & zll_main_putins9_in(14 downto 0));
      res <= (zll_main_putins14_in(47 downto 40) & zll_main_putins14_in(39 downto 32) & zll_main_putins14_in(55 downto 48) & zll_main_putins14_in(69 downto 62) & zll_main_putins14_in(61 downto 56) & zll_main_putins14_in(86 downto 70) & zll_main_putins14_in(14 downto 0));
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
signal zll_main_loop207_in : std_logic_vector (84 downto 0);
begin
zll_main_loop207_in <= arg0;
      res <= rw_resize(((std_logic_vector'(B"001") & rw_repl(55, std_logic_vector'(B"0"))) & zll_main_loop207_in(84 downto 70) & zll_main_loop207_in(69 downto 0)), 143);
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
signal zll_main_getins2_in : std_logic_vector (139 downto 0);
      signal zll_main_getins1_in : std_logic_vector (139 downto 0);
      signal zll_main_inputs5_in : std_logic_vector (69 downto 0);
      signal zll_main_inputs7_in : std_logic_vector (69 downto 0);
      signal zll_main_inputs6_in : std_logic_vector (61 downto 0);
      signal zll_main_inputs4_in : std_logic_vector (53 downto 0);
      signal zll_main_inputs_in : std_logic_vector (45 downto 0);
      signal zll_main_inputs1_in : std_logic_vector (37 downto 0);
      signal zll_main_inputs2_in : std_logic_vector (31 downto 0);
begin
zll_main_getins2_in <= (arg0 & arg0);
      zll_main_getins1_in <= zll_main_getins2_in(139 downto 0);
      zll_main_inputs5_in <= zll_main_getins1_in(139 downto 70);
      zll_main_inputs7_in <= zll_main_inputs5_in(69 downto 0);
      zll_main_inputs6_in <= (zll_main_inputs7_in(61 downto 54) & zll_main_inputs7_in(53 downto 46) & zll_main_inputs7_in(45 downto 38) & zll_main_inputs7_in(37 downto 32) & zll_main_inputs7_in(31 downto 15) & zll_main_inputs7_in(14 downto 0));
      zll_main_inputs4_in <= (zll_main_inputs6_in(53 downto 46) & zll_main_inputs6_in(45 downto 38) & zll_main_inputs6_in(37 downto 32) & zll_main_inputs6_in(31 downto 15) & zll_main_inputs6_in(14 downto 0));
      zll_main_inputs_in <= (zll_main_inputs4_in(45 downto 38) & zll_main_inputs4_in(37 downto 32) & zll_main_inputs4_in(31 downto 15) & zll_main_inputs4_in(14 downto 0));
      zll_main_inputs1_in <= (zll_main_inputs_in(37 downto 32) & zll_main_inputs_in(31 downto 15) & zll_main_inputs_in(14 downto 0));
      zll_main_inputs2_in <= (zll_main_inputs1_in(31 downto 15) & zll_main_inputs1_in(14 downto 0));
      res <= (zll_main_inputs2_in(31 downto 15) & zll_main_getins1_in(69 downto 0));
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
component \Main_putWeOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_Main_putAddrOut1\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zll_main_finishinstr1_in : std_logic_vector (75 downto 0);
      signal zll_main_putaddrout1_in : std_logic_vector (75 downto 0);
      signal zll_main_putaddrout1_out : std_logic_vector (69 downto 0);
      signal main_putweout_in : std_logic_vector (69 downto 0);
      signal main_putweout_out : std_logic_vector (69 downto 0);
begin
zll_main_finishinstr1_in <= arg0;
      zll_main_putaddrout1_in <= (zll_main_finishinstr1_in(75 downto 70) & zll_main_finishinstr1_in(69 downto 0));
      inst : \ZLL_Main_putAddrOut1\ port map (zll_main_putaddrout1_in(75 downto 70), zll_main_putaddrout1_in(69 downto 0), zll_main_putaddrout1_out);
      main_putweout_in <= zll_main_putaddrout1_out;
      \instR1\ : \Main_putWeOut\ port map (main_putweout_in(69 downto 0), main_putweout_out);
      res <= main_putweout_out;
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
signal zll_main_getout_in : std_logic_vector (139 downto 0);
      signal zll_main_getout1_in : std_logic_vector (139 downto 0);
      signal zll_main_outputs6_in : std_logic_vector (69 downto 0);
      signal zll_main_outputs2_in : std_logic_vector (69 downto 0);
      signal zll_main_outputs3_in : std_logic_vector (61 downto 0);
      signal zll_main_outputs4_in : std_logic_vector (53 downto 0);
      signal zll_main_outputs5_in : std_logic_vector (45 downto 0);
      signal zll_main_outputs_in : std_logic_vector (37 downto 0);
      signal zll_main_outputs7_in : std_logic_vector (31 downto 0);
begin
zll_main_getout_in <= (arg0 & arg0);
      zll_main_getout1_in <= zll_main_getout_in(139 downto 0);
      zll_main_outputs6_in <= zll_main_getout1_in(139 downto 70);
      zll_main_outputs2_in <= zll_main_outputs6_in(69 downto 0);
      zll_main_outputs3_in <= (zll_main_outputs2_in(61 downto 54) & zll_main_outputs2_in(53 downto 46) & zll_main_outputs2_in(45 downto 38) & zll_main_outputs2_in(37 downto 32) & zll_main_outputs2_in(31 downto 15) & zll_main_outputs2_in(14 downto 0));
      zll_main_outputs4_in <= (zll_main_outputs3_in(53 downto 46) & zll_main_outputs3_in(45 downto 38) & zll_main_outputs3_in(37 downto 32) & zll_main_outputs3_in(31 downto 15) & zll_main_outputs3_in(14 downto 0));
      zll_main_outputs5_in <= (zll_main_outputs4_in(45 downto 38) & zll_main_outputs4_in(37 downto 32) & zll_main_outputs4_in(31 downto 15) & zll_main_outputs4_in(14 downto 0));
      zll_main_outputs_in <= (zll_main_outputs5_in(37 downto 32) & zll_main_outputs5_in(31 downto 15) & zll_main_outputs5_in(14 downto 0));
      zll_main_outputs7_in <= (zll_main_outputs_in(31 downto 15) & zll_main_outputs_in(14 downto 0));
      res <= (zll_main_outputs7_in(14 downto 0) & zll_main_getout1_in(69 downto 0));
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
      signal zll_main_r27_in : std_logic_vector (53 downto 0);
      signal zll_main_r27_out : std_logic_vector (7 downto 0);
begin
zll_main_r27_in <= (arg0 & arg2 & arg3 & arg4 & arg5);
      inst : \ZLL_Main_r27\ port map (zll_main_r27_in(53 downto 46), zll_main_r27_in(45 downto 38), zll_main_r27_in(37 downto 32), zll_main_r27_in(31 downto 15), zll_main_r27_in(14 downto 0), zll_main_r27_out);
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
signal zll_main_putreg57_in : std_logic_vector (89 downto 0);
      signal zll_main_putreg25_in : std_logic_vector (89 downto 0);
      signal zll_main_putreg40_in : std_logic_vector (79 downto 0);
      signal zll_main_putreg18_in : std_logic_vector (79 downto 0);
      signal zll_main_putreg55_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg16_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg53_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg48_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg37_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg13_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg64_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg35_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg4_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg36_in : std_logic_vector (79 downto 0);
      signal zll_main_putreg10_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg33_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg32_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg43_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg28_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg67_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg14_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg20_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg22_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg52_in : std_logic_vector (79 downto 0);
      signal zll_main_putreg7_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg58_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg46_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg51_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg5_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg42_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg29_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg62_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg17_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg24_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg11_in : std_logic_vector (79 downto 0);
      signal zll_main_putreg61_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg63_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg21_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg59_in : std_logic_vector (147 downto 0);
      signal zll_main_putreg2_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg66_in : std_logic_vector (77 downto 0);
      signal zll_main_putreg47_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg19_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg49_in : std_logic_vector (69 downto 0);
      signal zll_main_putreg_in : std_logic_vector (69 downto 0);
begin
zll_main_putreg57_in <= (arg0 & arg1 & arg1 & arg0 & arg3);
      zll_main_putreg25_in <= (zll_main_putreg57_in(89 downto 82) & zll_main_putreg57_in(81 downto 80) & zll_main_putreg57_in(81 downto 80) & zll_main_putreg57_in(89 downto 82) & zll_main_putreg57_in(69 downto 0));
      zll_main_putreg40_in <= (zll_main_putreg25_in(81 downto 80) & zll_main_putreg25_in(89 downto 82) & zll_main_putreg25_in(69 downto 0));
      zll_main_putreg18_in <= (zll_main_putreg40_in(69 downto 0) & zll_main_putreg40_in(79 downto 70));
      zll_main_putreg55_in <= (zll_main_putreg18_in(79 downto 10) & zll_main_putreg18_in(7 downto 0));
      zll_main_putreg16_in <= (zll_main_putreg55_in(7 downto 0) & zll_main_putreg55_in(77 downto 8));
      zll_main_putreg53_in <= (zll_main_putreg16_in(77 downto 70) & zll_main_putreg16_in(69 downto 0) & zll_main_putreg16_in(69 downto 0));
      zll_main_putreg48_in <= (zll_main_putreg53_in(147 downto 140) & zll_main_putreg53_in(139 downto 0));
      zll_main_putreg37_in <= (zll_main_putreg48_in(147 downto 140) & zll_main_putreg48_in(139 downto 70));
      zll_main_putreg13_in <= (zll_main_putreg37_in(77 downto 70) & zll_main_putreg37_in(69 downto 0));
      zll_main_putreg64_in <= (zll_main_putreg13_in(61 downto 54) & zll_main_putreg13_in(77 downto 70) & zll_main_putreg13_in(69 downto 62) & zll_main_putreg13_in(53 downto 46) & zll_main_putreg13_in(45 downto 38) & zll_main_putreg13_in(37 downto 32) & zll_main_putreg13_in(31 downto 15) & zll_main_putreg13_in(14 downto 0));
      zll_main_putreg35_in <= (zll_main_putreg64_in(53 downto 46) & zll_main_putreg64_in(77 downto 70) & zll_main_putreg64_in(69 downto 62) & zll_main_putreg64_in(61 downto 54) & zll_main_putreg64_in(45 downto 38) & zll_main_putreg64_in(37 downto 32) & zll_main_putreg64_in(31 downto 15) & zll_main_putreg64_in(14 downto 0));
      zll_main_putreg4_in <= (zll_main_putreg35_in(77 downto 70) & zll_main_putreg35_in(69 downto 62) & zll_main_putreg35_in(61 downto 54) & zll_main_putreg35_in(53 downto 46) & zll_main_putreg35_in(37 downto 32) & zll_main_putreg35_in(31 downto 15) & zll_main_putreg35_in(14 downto 0));
      zll_main_putreg36_in <= (zll_main_putreg25_in(69 downto 0) & zll_main_putreg25_in(79 downto 70));
      zll_main_putreg10_in <= (zll_main_putreg36_in(79 downto 10) & zll_main_putreg36_in(7 downto 0));
      zll_main_putreg33_in <= (zll_main_putreg10_in(7 downto 0) & zll_main_putreg10_in(77 downto 8));
      zll_main_putreg32_in <= (zll_main_putreg33_in(77 downto 70) & zll_main_putreg33_in(69 downto 0) & zll_main_putreg33_in(69 downto 0));
      zll_main_putreg43_in <= (zll_main_putreg32_in(147 downto 140) & zll_main_putreg32_in(139 downto 0));
      zll_main_putreg28_in <= (zll_main_putreg43_in(147 downto 140) & zll_main_putreg43_in(139 downto 70));
      zll_main_putreg67_in <= (zll_main_putreg28_in(77 downto 70) & zll_main_putreg28_in(69 downto 0));
      zll_main_putreg14_in <= (zll_main_putreg67_in(69 downto 62) & zll_main_putreg67_in(77 downto 70) & zll_main_putreg67_in(61 downto 54) & zll_main_putreg67_in(53 downto 46) & zll_main_putreg67_in(45 downto 38) & zll_main_putreg67_in(37 downto 32) & zll_main_putreg67_in(31 downto 15) & zll_main_putreg67_in(14 downto 0));
      zll_main_putreg20_in <= (zll_main_putreg14_in(77 downto 70) & zll_main_putreg14_in(69 downto 62) & zll_main_putreg14_in(61 downto 54) & zll_main_putreg14_in(45 downto 38) & zll_main_putreg14_in(37 downto 32) & zll_main_putreg14_in(31 downto 15) & zll_main_putreg14_in(14 downto 0));
      zll_main_putreg22_in <= (zll_main_putreg20_in(69 downto 62) & zll_main_putreg20_in(45 downto 38) & zll_main_putreg20_in(61 downto 54) & zll_main_putreg20_in(53 downto 46) & zll_main_putreg20_in(37 downto 32) & zll_main_putreg20_in(31 downto 15) & zll_main_putreg20_in(14 downto 0));
      zll_main_putreg52_in <= (zll_main_putreg57_in(69 downto 0) & zll_main_putreg57_in(79 downto 70));
      zll_main_putreg7_in <= (zll_main_putreg52_in(79 downto 10) & zll_main_putreg52_in(7 downto 0));
      zll_main_putreg58_in <= (zll_main_putreg7_in(7 downto 0) & zll_main_putreg7_in(77 downto 8));
      zll_main_putreg46_in <= (zll_main_putreg58_in(77 downto 70) & zll_main_putreg58_in(69 downto 0) & zll_main_putreg58_in(69 downto 0));
      zll_main_putreg51_in <= (zll_main_putreg46_in(147 downto 140) & zll_main_putreg46_in(139 downto 0));
      zll_main_putreg5_in <= (zll_main_putreg51_in(147 downto 140) & zll_main_putreg51_in(139 downto 70));
      zll_main_putreg42_in <= (zll_main_putreg5_in(77 downto 70) & zll_main_putreg5_in(69 downto 0));
      zll_main_putreg29_in <= (zll_main_putreg42_in(69 downto 62) & zll_main_putreg42_in(77 downto 70) & zll_main_putreg42_in(61 downto 54) & zll_main_putreg42_in(53 downto 46) & zll_main_putreg42_in(45 downto 38) & zll_main_putreg42_in(37 downto 32) & zll_main_putreg42_in(31 downto 15) & zll_main_putreg42_in(14 downto 0));
      zll_main_putreg62_in <= (zll_main_putreg29_in(77 downto 70) & zll_main_putreg29_in(69 downto 62) & zll_main_putreg29_in(53 downto 46) & zll_main_putreg29_in(45 downto 38) & zll_main_putreg29_in(37 downto 32) & zll_main_putreg29_in(31 downto 15) & zll_main_putreg29_in(14 downto 0));
      zll_main_putreg17_in <= (zll_main_putreg62_in(69 downto 62) & zll_main_putreg62_in(61 downto 54) & zll_main_putreg62_in(45 downto 38) & zll_main_putreg62_in(53 downto 46) & zll_main_putreg62_in(37 downto 32) & zll_main_putreg62_in(31 downto 15) & zll_main_putreg62_in(14 downto 0));
      zll_main_putreg24_in <= (zll_main_putreg17_in(69 downto 62) & zll_main_putreg17_in(37 downto 32) & zll_main_putreg17_in(61 downto 54) & zll_main_putreg17_in(53 downto 46) & zll_main_putreg17_in(45 downto 38) & zll_main_putreg17_in(31 downto 15) & zll_main_putreg17_in(14 downto 0));
      zll_main_putreg11_in <= (arg3 & arg2);
      zll_main_putreg61_in <= (zll_main_putreg11_in(79 downto 10) & zll_main_putreg11_in(7 downto 0));
      zll_main_putreg63_in <= (zll_main_putreg61_in(7 downto 0) & zll_main_putreg61_in(77 downto 8));
      zll_main_putreg21_in <= (zll_main_putreg63_in(77 downto 70) & zll_main_putreg63_in(69 downto 0) & zll_main_putreg63_in(69 downto 0));
      zll_main_putreg59_in <= (zll_main_putreg21_in(147 downto 140) & zll_main_putreg21_in(139 downto 0));
      zll_main_putreg2_in <= (zll_main_putreg59_in(147 downto 140) & zll_main_putreg59_in(139 downto 70));
      zll_main_putreg66_in <= (zll_main_putreg2_in(77 downto 70) & zll_main_putreg2_in(69 downto 0));
      zll_main_putreg47_in <= (zll_main_putreg66_in(77 downto 70) & zll_main_putreg66_in(61 downto 54) & zll_main_putreg66_in(53 downto 46) & zll_main_putreg66_in(45 downto 38) & zll_main_putreg66_in(37 downto 32) & zll_main_putreg66_in(31 downto 15) & zll_main_putreg66_in(14 downto 0));
      zll_main_putreg19_in <= (zll_main_putreg47_in(61 downto 54) & zll_main_putreg47_in(69 downto 62) & zll_main_putreg47_in(53 downto 46) & zll_main_putreg47_in(45 downto 38) & zll_main_putreg47_in(37 downto 32) & zll_main_putreg47_in(31 downto 15) & zll_main_putreg47_in(14 downto 0));
      zll_main_putreg49_in <= (zll_main_putreg19_in(69 downto 62) & zll_main_putreg19_in(53 downto 46) & zll_main_putreg19_in(61 downto 54) & zll_main_putreg19_in(45 downto 38) & zll_main_putreg19_in(37 downto 32) & zll_main_putreg19_in(31 downto 15) & zll_main_putreg19_in(14 downto 0));
      zll_main_putreg_in <= (zll_main_putreg49_in(69 downto 62) & zll_main_putreg49_in(61 downto 54) & zll_main_putreg49_in(37 downto 32) & zll_main_putreg49_in(53 downto 46) & zll_main_putreg49_in(45 downto 38) & zll_main_putreg49_in(31 downto 15) & zll_main_putreg49_in(14 downto 0));
      res <= rw_resize(rw_cond(rw_eq(zll_main_putreg11_in(9 downto 8), std_logic_vector'(B"00")), (zll_main_putreg_in(47 downto 40) & zll_main_putreg_in(69 downto 62) & zll_main_putreg_in(61 downto 54) & zll_main_putreg_in(39 downto 32) & zll_main_putreg_in(53 downto 48) & zll_main_putreg_in(31 downto 15) & zll_main_putreg_in(14 downto 0)), rw_cond(rw_eq(zll_main_putreg52_in(9 downto 8), std_logic_vector'(B"01")), (zll_main_putreg24_in(69 downto 62) & zll_main_putreg24_in(55 downto 48) & zll_main_putreg24_in(39 downto 32) & zll_main_putreg24_in(47 downto 40) & zll_main_putreg24_in(61 downto 56) & zll_main_putreg24_in(31 downto 15) & zll_main_putreg24_in(14 downto 0)), rw_cond(rw_eq(zll_main_putreg36_in(9 downto 8), std_logic_vector'(B"10")), (zll_main_putreg22_in(69 downto 62) & zll_main_putreg22_in(45 downto 38) & zll_main_putreg22_in(53 downto 46) & zll_main_putreg22_in(61 downto 54) & zll_main_putreg22_in(37 downto 32) & zll_main_putreg22_in(31 downto 15) & zll_main_putreg22_in(14 downto 0)), (zll_main_putreg4_in(45 downto 38) & zll_main_putreg4_in(61 downto 54) & zll_main_putreg4_in(69 downto 62) & zll_main_putreg4_in(53 downto 46) & zll_main_putreg4_in(37 downto 32) & zll_main_putreg4_in(31 downto 15) & zll_main_putreg4_in(14 downto 0))))), 70);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putOut5\ is
port (arg0 : in std_logic_vector (14 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putOut5\ is
signal zll_main_putout14_in : std_logic_vector (154 downto 0);
      signal zll_main_putout10_in : std_logic_vector (154 downto 0);
      signal zll_main_putout1_in : std_logic_vector (84 downto 0);
      signal zll_main_putout12_in : std_logic_vector (84 downto 0);
      signal zll_main_putout11_in : std_logic_vector (84 downto 0);
      signal zll_main_putout4_in : std_logic_vector (84 downto 0);
      signal zll_main_putout9_in : std_logic_vector (84 downto 0);
      signal zll_main_putout7_in : std_logic_vector (84 downto 0);
begin
zll_main_putout14_in <= (arg0 & arg1 & arg1);
      zll_main_putout10_in <= (zll_main_putout14_in(154 downto 140) & zll_main_putout14_in(139 downto 0));
      zll_main_putout1_in <= (zll_main_putout10_in(154 downto 140) & zll_main_putout10_in(139 downto 70));
      zll_main_putout12_in <= (zll_main_putout1_in(84 downto 70) & zll_main_putout1_in(69 downto 0));
      zll_main_putout11_in <= (zll_main_putout12_in(53 downto 46) & zll_main_putout12_in(84 downto 70) & zll_main_putout12_in(69 downto 62) & zll_main_putout12_in(61 downto 54) & zll_main_putout12_in(45 downto 38) & zll_main_putout12_in(37 downto 32) & zll_main_putout12_in(31 downto 15) & zll_main_putout12_in(14 downto 0));
      zll_main_putout4_in <= (zll_main_putout11_in(84 downto 77) & zll_main_putout11_in(45 downto 38) & zll_main_putout11_in(76 downto 62) & zll_main_putout11_in(61 downto 54) & zll_main_putout11_in(53 downto 46) & zll_main_putout11_in(37 downto 32) & zll_main_putout11_in(31 downto 15) & zll_main_putout11_in(14 downto 0));
      zll_main_putout9_in <= (zll_main_putout4_in(84 downto 77) & zll_main_putout4_in(37 downto 32) & zll_main_putout4_in(76 downto 69) & zll_main_putout4_in(68 downto 54) & zll_main_putout4_in(53 downto 46) & zll_main_putout4_in(45 downto 38) & zll_main_putout4_in(31 downto 15) & zll_main_putout4_in(14 downto 0));
      zll_main_putout7_in <= (zll_main_putout9_in(84 downto 77) & zll_main_putout9_in(76 downto 71) & zll_main_putout9_in(31 downto 15) & zll_main_putout9_in(70 downto 63) & zll_main_putout9_in(62 downto 48) & zll_main_putout9_in(47 downto 40) & zll_main_putout9_in(39 downto 32) & zll_main_putout9_in(14 downto 0));
      res <= (zll_main_putout7_in(30 downto 23) & zll_main_putout7_in(22 downto 15) & zll_main_putout7_in(84 downto 77) & zll_main_putout7_in(53 downto 46) & zll_main_putout7_in(76 downto 71) & zll_main_putout7_in(70 downto 54) & zll_main_putout7_in(45 downto 31));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_putWeOut\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \Main_putWeOut\ is
component \ZLL_Main_putWeOut2\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal zll_main_putweout2_in : std_logic_vector (70 downto 0);
      signal zll_main_putweout2_out : std_logic_vector (69 downto 0);
begin
zll_main_putweout2_in <= (std_logic_vector'(B"0") & arg0);
      inst : \ZLL_Main_putWeOut2\ port map (zll_main_putweout2_in(70 downto 70), zll_main_putweout2_in(69 downto 0), zll_main_putweout2_out);
      res <= zll_main_putweout2_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putAddrOut1\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putAddrOut1\ is
component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \ZLL_Main_putOut5\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getout_in : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_putaddrout10_in : std_logic_vector (90 downto 0);
      signal zll_main_putaddrout9_in : std_logic_vector (90 downto 0);
      signal zll_main_putaddrout3_in : std_logic_vector (20 downto 0);
      signal zll_main_putaddrout6_in : std_logic_vector (20 downto 0);
      signal zll_main_putout5_in : std_logic_vector (84 downto 0);
      signal zll_main_putout5_out : std_logic_vector (69 downto 0);
begin
main_getout_in <= arg1;
      inst : \Main_getOut\ port map (main_getout_in(69 downto 0), main_getout_out);
      zll_main_putaddrout10_in <= (arg0 & main_getout_out);
      zll_main_putaddrout9_in <= (zll_main_putaddrout10_in(90 downto 85) & zll_main_putaddrout10_in(84 downto 0));
      zll_main_putaddrout3_in <= (zll_main_putaddrout9_in(90 downto 85) & zll_main_putaddrout9_in(84 downto 70));
      zll_main_putaddrout6_in <= (zll_main_putaddrout3_in(20 downto 15) & zll_main_putaddrout3_in(14 downto 0));
      zll_main_putout5_in <= ((zll_main_putaddrout6_in(14 downto 14) & zll_main_putaddrout6_in(20 downto 15) & zll_main_putaddrout6_in(7 downto 0)) & zll_main_putaddrout9_in(69 downto 0));
      \instR1\ : \ZLL_Main_putOut5\ port map (zll_main_putout5_in(84 downto 70), zll_main_putout5_in(69 downto 0), zll_main_putout5_out);
      res <= zll_main_putout5_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_putWeOut2\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_Main_putWeOut2\ is
component \Main_getOut\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (84 downto 0));
      end component;
      component \ZLL_Main_putOut5\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getout_in : std_logic_vector (69 downto 0);
      signal main_getout_out : std_logic_vector (84 downto 0);
      signal zll_main_putweout10_in : std_logic_vector (85 downto 0);
      signal zll_main_putweout3_in : std_logic_vector (85 downto 0);
      signal zll_main_putweout8_in : std_logic_vector (15 downto 0);
      signal zll_main_putweout_in : std_logic_vector (15 downto 0);
      signal zll_main_putout5_in : std_logic_vector (84 downto 0);
      signal zll_main_putout5_out : std_logic_vector (69 downto 0);
begin
main_getout_in <= arg1;
      inst : \Main_getOut\ port map (main_getout_in(69 downto 0), main_getout_out);
      zll_main_putweout10_in <= (arg0 & main_getout_out);
      zll_main_putweout3_in <= (zll_main_putweout10_in(85 downto 85) & zll_main_putweout10_in(84 downto 0));
      zll_main_putweout8_in <= (zll_main_putweout3_in(85 downto 85) & zll_main_putweout3_in(84 downto 70));
      zll_main_putweout_in <= (zll_main_putweout8_in(15 downto 15) & zll_main_putweout8_in(14 downto 0));
      zll_main_putout5_in <= ((zll_main_putweout_in(15 downto 15) & zll_main_putweout_in(13 downto 8) & zll_main_putweout_in(7 downto 0)) & zll_main_putweout3_in(69 downto 0));
      \instR1\ : \ZLL_Main_putOut5\ port map (zll_main_putout5_in(84 downto 70), zll_main_putout5_in(69 downto 0), zll_main_putout5_out);
      res <= zll_main_putout5_out;
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
      component \ZLL_Main_putPC14\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_getpc_in : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (75 downto 0);
      signal zll_main_incrpc1_in : std_logic_vector (75 downto 0);
      signal zll_main_incrpc_in : std_logic_vector (75 downto 0);
      signal binop_in : std_logic_vector (11 downto 0);
      signal zll_main_putpc14_in : std_logic_vector (75 downto 0);
      signal zll_main_putpc14_out : std_logic_vector (69 downto 0);
begin
main_getpc_in <= arg0;
      inst : \Main_getPC\ port map (main_getpc_in(69 downto 0), main_getpc_out);
      zll_main_incrpc1_in <= main_getpc_out;
      zll_main_incrpc_in <= zll_main_incrpc1_in(75 downto 0);
      binop_in <= (zll_main_incrpc_in(75 downto 70) & std_logic_vector'(B"000001"));
      zll_main_putpc14_in <= rw_resize((rw_add(binop_in(11 downto 6), binop_in(5 downto 0)) & zll_main_incrpc_in(69 downto 0)), 76);
      \instR1\ : \ZLL_Main_putPC14\ port map (zll_main_putpc14_in(75 downto 70), zll_main_putpc14_in(69 downto 0), zll_main_putpc14_out);
      res <= zll_main_putpc14_out;
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
      signal zll_main_getreg13_in : std_logic_vector (73 downto 0);
      signal zll_main_getreg3_in : std_logic_vector (73 downto 0);
      signal zll_main_getreg12_in : std_logic_vector (71 downto 0);
      signal zll_main_getreg_in : std_logic_vector (71 downto 0);
      signal zll_main_getreg21_in : std_logic_vector (69 downto 0);
      signal zll_main_getreg11_in : std_logic_vector (139 downto 0);
      signal zll_main_getreg14_in : std_logic_vector (139 downto 0);
      signal zll_main_r34_in : std_logic_vector (69 downto 0);
      signal zll_main_r33_in : std_logic_vector (69 downto 0);
      signal zll_main_r3_in : std_logic_vector (61 downto 0);
      signal zll_main_r37_in : std_logic_vector (53 downto 0);
      signal zll_main_r35_in : std_logic_vector (45 downto 0);
      signal zll_main_r35_out : std_logic_vector (7 downto 0);
      signal zll_main_getreg9_in : std_logic_vector (71 downto 0);
      signal zll_main_getreg4_in : std_logic_vector (69 downto 0);
      signal zll_main_getreg16_in : std_logic_vector (139 downto 0);
      signal zll_main_getreg20_in : std_logic_vector (139 downto 0);
      signal zll_main_r24_in : std_logic_vector (69 downto 0);
      signal zll_main_r22_in : std_logic_vector (69 downto 0);
      signal zll_main_r23_in : std_logic_vector (61 downto 0);
      signal zll_main_r27_in : std_logic_vector (53 downto 0);
      signal zll_main_r27_out : std_logic_vector (7 downto 0);
      signal zll_main_getreg7_in : std_logic_vector (71 downto 0);
      signal zll_main_getreg10_in : std_logic_vector (69 downto 0);
      signal zll_main_getreg2_in : std_logic_vector (139 downto 0);
      signal zll_main_getreg18_in : std_logic_vector (139 downto 0);
      signal zll_main_r15_in : std_logic_vector (69 downto 0);
      signal zll_main_r14_in : std_logic_vector (69 downto 0);
      signal zll_main_r13_in : std_logic_vector (61 downto 0);
      signal zll_main_r13_out : std_logic_vector (7 downto 0);
      signal zll_main_getreg1_in : std_logic_vector (71 downto 0);
      signal zll_main_getreg22_in : std_logic_vector (69 downto 0);
      signal zll_main_getreg17_in : std_logic_vector (139 downto 0);
      signal zll_main_getreg15_in : std_logic_vector (139 downto 0);
      signal zll_main_r0_in : std_logic_vector (69 downto 0);
      signal zll_main_r05_in : std_logic_vector (69 downto 0);
      signal \zll_main_r13_inR1\ : std_logic_vector (61 downto 0);
      signal \zll_main_r13_outR1\ : std_logic_vector (7 downto 0);
begin
zll_main_getreg13_in <= (arg0 & arg0 & arg2);
      zll_main_getreg3_in <= (zll_main_getreg13_in(73 downto 72) & zll_main_getreg13_in(73 downto 72) & zll_main_getreg13_in(69 downto 0));
      zll_main_getreg12_in <= (zll_main_getreg3_in(73 downto 72) & zll_main_getreg3_in(69 downto 0));
      zll_main_getreg_in <= (zll_main_getreg12_in(69 downto 0) & zll_main_getreg12_in(71 downto 70));
      zll_main_getreg21_in <= zll_main_getreg_in(71 downto 2);
      zll_main_getreg11_in <= (zll_main_getreg21_in(69 downto 0) & zll_main_getreg21_in(69 downto 0));
      zll_main_getreg14_in <= zll_main_getreg11_in(139 downto 0);
      zll_main_r34_in <= zll_main_getreg14_in(139 downto 70);
      zll_main_r33_in <= zll_main_r34_in(69 downto 0);
      zll_main_r3_in <= (zll_main_r33_in(61 downto 54) & zll_main_r33_in(53 downto 46) & zll_main_r33_in(45 downto 38) & zll_main_r33_in(37 downto 32) & zll_main_r33_in(31 downto 15) & zll_main_r33_in(14 downto 0));
      zll_main_r37_in <= (zll_main_r3_in(53 downto 46) & zll_main_r3_in(45 downto 38) & zll_main_r3_in(37 downto 32) & zll_main_r3_in(31 downto 15) & zll_main_r3_in(14 downto 0));
      zll_main_r35_in <= (zll_main_r37_in(45 downto 38) & zll_main_r37_in(37 downto 32) & zll_main_r37_in(31 downto 15) & zll_main_r37_in(14 downto 0));
      inst : \ZLL_Main_r35\ port map (zll_main_r35_in(45 downto 38), zll_main_r35_in(37 downto 32), zll_main_r35_in(31 downto 15), zll_main_r35_in(14 downto 0), zll_main_r35_out);
      zll_main_getreg9_in <= (zll_main_getreg3_in(69 downto 0) & zll_main_getreg3_in(71 downto 70));
      zll_main_getreg4_in <= zll_main_getreg9_in(71 downto 2);
      zll_main_getreg16_in <= (zll_main_getreg4_in(69 downto 0) & zll_main_getreg4_in(69 downto 0));
      zll_main_getreg20_in <= zll_main_getreg16_in(139 downto 0);
      zll_main_r24_in <= zll_main_getreg20_in(139 downto 70);
      zll_main_r22_in <= zll_main_r24_in(69 downto 0);
      zll_main_r23_in <= (zll_main_r22_in(61 downto 54) & zll_main_r22_in(53 downto 46) & zll_main_r22_in(45 downto 38) & zll_main_r22_in(37 downto 32) & zll_main_r22_in(31 downto 15) & zll_main_r22_in(14 downto 0));
      zll_main_r27_in <= (zll_main_r23_in(53 downto 46) & zll_main_r23_in(45 downto 38) & zll_main_r23_in(37 downto 32) & zll_main_r23_in(31 downto 15) & zll_main_r23_in(14 downto 0));
      \instR1\ : \ZLL_Main_r27\ port map (zll_main_r27_in(53 downto 46), zll_main_r27_in(45 downto 38), zll_main_r27_in(37 downto 32), zll_main_r27_in(31 downto 15), zll_main_r27_in(14 downto 0), zll_main_r27_out);
      zll_main_getreg7_in <= (zll_main_getreg13_in(69 downto 0) & zll_main_getreg13_in(71 downto 70));
      zll_main_getreg10_in <= zll_main_getreg7_in(71 downto 2);
      zll_main_getreg2_in <= (zll_main_getreg10_in(69 downto 0) & zll_main_getreg10_in(69 downto 0));
      zll_main_getreg18_in <= zll_main_getreg2_in(139 downto 0);
      zll_main_r15_in <= zll_main_getreg18_in(139 downto 70);
      zll_main_r14_in <= zll_main_r15_in(69 downto 0);
      zll_main_r13_in <= (zll_main_r14_in(61 downto 54) & zll_main_r14_in(53 downto 46) & zll_main_r14_in(45 downto 38) & zll_main_r14_in(37 downto 32) & zll_main_r14_in(31 downto 15) & zll_main_r14_in(14 downto 0));
      \instR2\ : \ZLL_Main_r13\ port map (zll_main_r13_in(61 downto 54), zll_main_r13_in(53 downto 46), zll_main_r13_in(45 downto 38), zll_main_r13_in(37 downto 32), zll_main_r13_in(31 downto 15), zll_main_r13_in(14 downto 0), zll_main_r13_out);
      zll_main_getreg1_in <= (arg2 & arg1);
      zll_main_getreg22_in <= zll_main_getreg1_in(71 downto 2);
      zll_main_getreg17_in <= (zll_main_getreg22_in(69 downto 0) & zll_main_getreg22_in(69 downto 0));
      zll_main_getreg15_in <= zll_main_getreg17_in(139 downto 0);
      zll_main_r0_in <= zll_main_getreg15_in(139 downto 70);
      zll_main_r05_in <= zll_main_r0_in(69 downto 0);
      \zll_main_r13_inR1\ <= (zll_main_r05_in(69 downto 62) & zll_main_r05_in(53 downto 46) & zll_main_r05_in(45 downto 38) & zll_main_r05_in(37 downto 32) & zll_main_r05_in(31 downto 15) & zll_main_r05_in(14 downto 0));
      \instR3\ : \ZLL_Main_r13\ port map (\zll_main_r13_inR1\(61 downto 54), \zll_main_r13_inR1\(53 downto 46), \zll_main_r13_inR1\(45 downto 38), \zll_main_r13_inR1\(37 downto 32), \zll_main_r13_inR1\(31 downto 15), \zll_main_r13_inR1\(14 downto 0), \zll_main_r13_outR1\);
      res <= rw_resize(rw_cond(rw_eq(zll_main_getreg1_in(1 downto 0), std_logic_vector'(B"00")), (\zll_main_r13_outR1\ & zll_main_getreg15_in(69 downto 0)), rw_cond(rw_eq(zll_main_getreg7_in(1 downto 0), std_logic_vector'(B"01")), (zll_main_r13_out & zll_main_getreg18_in(69 downto 0)), rw_cond(rw_eq(zll_main_getreg9_in(1 downto 0), std_logic_vector'(B"10")), (zll_main_r27_out & zll_main_getreg20_in(69 downto 0)), (zll_main_r35_out & zll_main_getreg14_in(69 downto 0))))), 78);
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
signal zll_main_getpc1_in : std_logic_vector (139 downto 0);
      signal zll_main_getpc_in : std_logic_vector (139 downto 0);
      signal zll_main_pc6_in : std_logic_vector (69 downto 0);
      signal zll_main_pc3_in : std_logic_vector (69 downto 0);
      signal zll_main_pc4_in : std_logic_vector (61 downto 0);
      signal zll_main_pc2_in : std_logic_vector (53 downto 0);
      signal zll_main_pc_in : std_logic_vector (45 downto 0);
      signal zll_main_pc5_in : std_logic_vector (37 downto 0);
      signal zll_main_pc1_in : std_logic_vector (20 downto 0);
begin
zll_main_getpc1_in <= (arg0 & arg0);
      zll_main_getpc_in <= zll_main_getpc1_in(139 downto 0);
      zll_main_pc6_in <= zll_main_getpc_in(139 downto 70);
      zll_main_pc3_in <= zll_main_pc6_in(69 downto 0);
      zll_main_pc4_in <= (zll_main_pc3_in(61 downto 54) & zll_main_pc3_in(53 downto 46) & zll_main_pc3_in(45 downto 38) & zll_main_pc3_in(37 downto 32) & zll_main_pc3_in(31 downto 15) & zll_main_pc3_in(14 downto 0));
      zll_main_pc2_in <= (zll_main_pc4_in(53 downto 46) & zll_main_pc4_in(45 downto 38) & zll_main_pc4_in(37 downto 32) & zll_main_pc4_in(31 downto 15) & zll_main_pc4_in(14 downto 0));
      zll_main_pc_in <= (zll_main_pc2_in(45 downto 38) & zll_main_pc2_in(37 downto 32) & zll_main_pc2_in(31 downto 15) & zll_main_pc2_in(14 downto 0));
      zll_main_pc5_in <= (zll_main_pc_in(37 downto 32) & zll_main_pc_in(31 downto 15) & zll_main_pc_in(14 downto 0));
      zll_main_pc1_in <= (zll_main_pc5_in(37 downto 32) & zll_main_pc5_in(14 downto 0));
      res <= (zll_main_pc1_in(20 downto 15) & zll_main_getpc_in(69 downto 0));
end architecture;