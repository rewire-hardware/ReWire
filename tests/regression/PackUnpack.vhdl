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
port (\__in0\ : in std_logic_vector (7 downto 0);
      \__out0\ : out std_logic_vector (7 downto 0);
      \__out1\ : out std_logic_vector (7 downto 0);
      \__out2\ : out std_logic_vector (7 downto 0);
      \__out3\ : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of top_level is
component \ZLL_Main_dev158\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev171\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev204\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev261\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev262\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev276\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (7 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev72\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zll_main_dev69_in : std_logic_vector (7 downto 0);
      signal zll_main_x2_in : std_logic_vector (7 downto 0);
      signal binop_in : std_logic_vector (15 downto 0);
      signal zll_main_dev207_in : std_logic_vector (15 downto 0);
      signal zll_main_dev253_in : std_logic_vector (15 downto 0);
      signal zll_main_dev42_in : std_logic_vector (15 downto 0);
      signal zll_main_dev8_in : std_logic_vector (15 downto 0);
      signal zll_main_dev124_in : std_logic_vector (15 downto 0);
      signal zll_main_dev73_in : std_logic_vector (18 downto 0);
      signal zll_main_dev_in : std_logic_vector (21 downto 0);
      signal zll_main_dev72_in : std_logic_vector (24 downto 0);
      signal zll_main_dev72_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev72_inR1\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev72_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev72_inR2\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev72_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev72_inR3\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev72_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev72_inR4\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev72_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev72_inR5\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev72_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev72_inR6\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev72_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev72_inR7\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev72_outR7\ : std_logic_vector (0 downto 0);
      signal zll_main_dev89_in : std_logic_vector (15 downto 0);
      signal zll_main_dev31_in : std_logic_vector (15 downto 0);
      signal zll_main_dev23_in : std_logic_vector (15 downto 0);
      signal zll_main_dev95_in : std_logic_vector (18 downto 0);
      signal zll_main_dev234_in : std_logic_vector (21 downto 0);
      signal zll_main_dev261_in : std_logic_vector (5 downto 0);
      signal zll_main_dev261_out : std_logic_vector (2 downto 0);
      signal zll_main_dev286_in : std_logic_vector (24 downto 0);
      signal zll_main_dev45_in : std_logic_vector (14 downto 0);
      signal zll_main_dev158_in : std_logic_vector (6 downto 0);
      signal zll_main_dev158_out : std_logic_vector (2 downto 0);
      signal id_in : std_logic_vector (3 downto 0);
      signal zll_main_dev151_in : std_logic_vector (24 downto 0);
      signal zll_main_dev204_in : std_logic_vector (27 downto 0);
      signal zll_main_dev204_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev204_inR1\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev204_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev204_inR2\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev204_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev204_inR3\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev204_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev204_inR4\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev204_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev204_inR5\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev204_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev204_inR6\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev204_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev204_inR7\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev204_outR7\ : std_logic_vector (0 downto 0);
      signal zll_main_dev32_in : std_logic_vector (15 downto 0);
      signal zll_main_dev86_in : std_logic_vector (15 downto 0);
      signal zll_main_dev115_in : std_logic_vector (15 downto 0);
      signal zll_main_dev59_in : std_logic_vector (15 downto 0);
      signal zll_main_dev187_in : std_logic_vector (18 downto 0);
      signal zll_main_dev97_in : std_logic_vector (18 downto 0);
      signal zll_main_dev214_in : std_logic_vector (21 downto 0);
      signal \zll_main_dev261_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev261_outR1\ : std_logic_vector (2 downto 0);
      signal zll_main_dev29_in : std_logic_vector (24 downto 0);
      signal zll_main_dev260_in : std_logic_vector (14 downto 0);
      signal \zll_main_dev158_inR1\ : std_logic_vector (6 downto 0);
      signal \zll_main_dev158_outR1\ : std_logic_vector (2 downto 0);
      signal \id_inR1\ : std_logic_vector (3 downto 0);
      signal zll_main_dev268_in : std_logic_vector (21 downto 0);
      signal zll_main_dev171_in : std_logic_vector (24 downto 0);
      signal zll_main_dev171_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev171_inR1\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev171_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev171_inR2\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev171_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev171_inR3\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev171_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev171_inR4\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev171_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev171_inR5\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev171_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev171_inR6\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev171_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev171_inR7\ : std_logic_vector (24 downto 0);
      signal \zll_main_dev171_outR7\ : std_logic_vector (0 downto 0);
      signal zll_main_dev273_in : std_logic_vector (15 downto 0);
      signal zll_main_dev185_in : std_logic_vector (15 downto 0);
      signal zll_main_dev121_in : std_logic_vector (15 downto 0);
      signal zll_main_dev2_in : std_logic_vector (18 downto 0);
      signal zll_main_dev84_in : std_logic_vector (18 downto 0);
      signal zll_main_dev230_in : std_logic_vector (21 downto 0);
      signal \zll_main_dev261_inR2\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev261_outR2\ : std_logic_vector (2 downto 0);
      signal zll_main_dev249_in : std_logic_vector (24 downto 0);
      signal zll_main_dev63_in : std_logic_vector (14 downto 0);
      signal zll_main_dev170_in : std_logic_vector (6 downto 0);
      signal zll_main_dev213_in : std_logic_vector (6 downto 0);
      signal zll_main_dev262_in : std_logic_vector (5 downto 0);
      signal zll_main_dev262_out : std_logic_vector (2 downto 0);
      signal \id_inR2\ : std_logic_vector (3 downto 0);
      signal zll_main_dev254_in : std_logic_vector (24 downto 0);
      signal zll_main_dev276_in : std_logic_vector (27 downto 0);
      signal zll_main_dev276_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev276_inR1\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev276_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev276_inR2\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev276_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev276_inR3\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev276_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev276_inR4\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev276_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev276_inR5\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev276_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev276_inR6\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev276_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev276_inR7\ : std_logic_vector (27 downto 0);
      signal \zll_main_dev276_outR7\ : std_logic_vector (0 downto 0);
      signal pause : std_logic_vector (31 downto 0);
begin
zll_main_dev69_in <= \__in0\;
      zll_main_x2_in <= zll_main_dev69_in(7 downto 0);
      binop_in <= (zll_main_x2_in(7 downto 0) & std_logic_vector'(B"00000010"));
      zll_main_dev207_in <= (zll_main_dev69_in(7 downto 0) & rw_mul(binop_in(15 downto 8), binop_in(7 downto 0)));
      zll_main_dev253_in <= (zll_main_dev207_in(15 downto 8) & zll_main_dev207_in(7 downto 0));
      zll_main_dev42_in <= (zll_main_dev253_in(15 downto 8) & zll_main_dev253_in(7 downto 0));
      zll_main_dev8_in <= zll_main_dev42_in(15 downto 0);
      zll_main_dev124_in <= (zll_main_dev8_in(7 downto 0) & zll_main_dev8_in(15 downto 8));
      zll_main_dev73_in <= (zll_main_dev124_in(15 downto 8) & zll_main_dev124_in(7 downto 0) & std_logic_vector'(B"001"));
      zll_main_dev_in <= (zll_main_dev73_in(18 downto 11) & zll_main_dev73_in(10 downto 3) & zll_main_dev73_in(2 downto 0) & std_logic_vector'(B"010"));
      zll_main_dev72_in <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"000"));
      inst : \ZLL_Main_dev72\ port map (zll_main_dev72_in(24 downto 17), zll_main_dev72_in(16 downto 9), zll_main_dev72_in(8 downto 6), zll_main_dev72_in(5 downto 3), zll_main_dev72_in(2 downto 0), zll_main_dev72_out);
      \zll_main_dev72_inR1\ <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"001"));
      \instR1\ : \ZLL_Main_dev72\ port map (\zll_main_dev72_inR1\(24 downto 17), \zll_main_dev72_inR1\(16 downto 9), \zll_main_dev72_inR1\(8 downto 6), \zll_main_dev72_inR1\(5 downto 3), \zll_main_dev72_inR1\(2 downto 0), \zll_main_dev72_outR1\);
      \zll_main_dev72_inR2\ <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"010"));
      \instR2\ : \ZLL_Main_dev72\ port map (\zll_main_dev72_inR2\(24 downto 17), \zll_main_dev72_inR2\(16 downto 9), \zll_main_dev72_inR2\(8 downto 6), \zll_main_dev72_inR2\(5 downto 3), \zll_main_dev72_inR2\(2 downto 0), \zll_main_dev72_outR2\);
      \zll_main_dev72_inR3\ <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"011"));
      \instR3\ : \ZLL_Main_dev72\ port map (\zll_main_dev72_inR3\(24 downto 17), \zll_main_dev72_inR3\(16 downto 9), \zll_main_dev72_inR3\(8 downto 6), \zll_main_dev72_inR3\(5 downto 3), \zll_main_dev72_inR3\(2 downto 0), \zll_main_dev72_outR3\);
      \zll_main_dev72_inR4\ <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"100"));
      \instR4\ : \ZLL_Main_dev72\ port map (\zll_main_dev72_inR4\(24 downto 17), \zll_main_dev72_inR4\(16 downto 9), \zll_main_dev72_inR4\(8 downto 6), \zll_main_dev72_inR4\(5 downto 3), \zll_main_dev72_inR4\(2 downto 0), \zll_main_dev72_outR4\);
      \zll_main_dev72_inR5\ <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"101"));
      \instR5\ : \ZLL_Main_dev72\ port map (\zll_main_dev72_inR5\(24 downto 17), \zll_main_dev72_inR5\(16 downto 9), \zll_main_dev72_inR5\(8 downto 6), \zll_main_dev72_inR5\(5 downto 3), \zll_main_dev72_inR5\(2 downto 0), \zll_main_dev72_outR5\);
      \zll_main_dev72_inR6\ <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"110"));
      \instR6\ : \ZLL_Main_dev72\ port map (\zll_main_dev72_inR6\(24 downto 17), \zll_main_dev72_inR6\(16 downto 9), \zll_main_dev72_inR6\(8 downto 6), \zll_main_dev72_inR6\(5 downto 3), \zll_main_dev72_inR6\(2 downto 0), \zll_main_dev72_outR6\);
      \zll_main_dev72_inR7\ <= (zll_main_dev_in(21 downto 14) & zll_main_dev_in(13 downto 6) & zll_main_dev_in(5 downto 3) & zll_main_dev_in(2 downto 0) & std_logic_vector'(B"111"));
      \instR7\ : \ZLL_Main_dev72\ port map (\zll_main_dev72_inR7\(24 downto 17), \zll_main_dev72_inR7\(16 downto 9), \zll_main_dev72_inR7\(8 downto 6), \zll_main_dev72_inR7\(5 downto 3), \zll_main_dev72_inR7\(2 downto 0), \zll_main_dev72_outR7\);
      zll_main_dev89_in <= (zll_main_dev207_in(15 downto 8) & zll_main_dev207_in(7 downto 0));
      zll_main_dev31_in <= (zll_main_dev89_in(15 downto 8) & zll_main_dev89_in(7 downto 0));
      zll_main_dev23_in <= zll_main_dev31_in(15 downto 0);
      zll_main_dev95_in <= (zll_main_dev23_in(15 downto 8) & zll_main_dev23_in(7 downto 0) & std_logic_vector'(B"001"));
      zll_main_dev234_in <= (zll_main_dev95_in(18 downto 11) & zll_main_dev95_in(10 downto 3) & zll_main_dev95_in(2 downto 0) & std_logic_vector'(B"010"));
      zll_main_dev261_in <= (std_logic_vector'(B"111") & zll_main_dev234_in(2 downto 0));
      \instR8\ : \ZLL_Main_dev261\ port map (zll_main_dev261_in(5 downto 3), zll_main_dev261_in(2 downto 0), zll_main_dev261_out);
      zll_main_dev286_in <= (zll_main_dev234_in(21 downto 14) & zll_main_dev234_in(2 downto 0) & zll_main_dev234_in(13 downto 6) & zll_main_dev234_in(5 downto 3) & zll_main_dev261_out);
      zll_main_dev45_in <= (zll_main_dev286_in(24 downto 17) & zll_main_dev286_in(5 downto 3) & zll_main_dev286_in(2 downto 0) & std_logic_vector'(B"0"));
      zll_main_dev158_in <= (zll_main_dev45_in(6 downto 4) & zll_main_dev45_in(3 downto 1) & std_logic_vector'(B"0"));
      \instR9\ : \ZLL_Main_dev158\ port map (zll_main_dev158_in(6 downto 4), zll_main_dev158_in(3 downto 1), zll_main_dev158_in(0 downto 0), zll_main_dev158_out);
      id_in <= (zll_main_dev45_in(3 downto 1) & zll_main_dev45_in(0 downto 0));
      zll_main_dev151_in <= (zll_main_dev286_in(24 downto 17) & zll_main_dev286_in(16 downto 14) & zll_main_dev286_in(13 downto 6) & zll_main_dev286_in(5 downto 3) & rw_cond(rw_eq(id_in(0 downto 0), std_logic_vector'(B"1")), id_in(3 downto 1), zll_main_dev158_out));
      zll_main_dev204_in <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"000"));
      \instR10\ : \ZLL_Main_dev204\ port map (zll_main_dev204_in(27 downto 20), zll_main_dev204_in(19 downto 17), zll_main_dev204_in(16 downto 9), zll_main_dev204_in(8 downto 6), zll_main_dev204_in(5 downto 3), zll_main_dev204_in(2 downto 0), zll_main_dev204_out);
      \zll_main_dev204_inR1\ <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"001"));
      \instR11\ : \ZLL_Main_dev204\ port map (\zll_main_dev204_inR1\(27 downto 20), \zll_main_dev204_inR1\(19 downto 17), \zll_main_dev204_inR1\(16 downto 9), \zll_main_dev204_inR1\(8 downto 6), \zll_main_dev204_inR1\(5 downto 3), \zll_main_dev204_inR1\(2 downto 0), \zll_main_dev204_outR1\);
      \zll_main_dev204_inR2\ <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"010"));
      \instR12\ : \ZLL_Main_dev204\ port map (\zll_main_dev204_inR2\(27 downto 20), \zll_main_dev204_inR2\(19 downto 17), \zll_main_dev204_inR2\(16 downto 9), \zll_main_dev204_inR2\(8 downto 6), \zll_main_dev204_inR2\(5 downto 3), \zll_main_dev204_inR2\(2 downto 0), \zll_main_dev204_outR2\);
      \zll_main_dev204_inR3\ <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"011"));
      \instR13\ : \ZLL_Main_dev204\ port map (\zll_main_dev204_inR3\(27 downto 20), \zll_main_dev204_inR3\(19 downto 17), \zll_main_dev204_inR3\(16 downto 9), \zll_main_dev204_inR3\(8 downto 6), \zll_main_dev204_inR3\(5 downto 3), \zll_main_dev204_inR3\(2 downto 0), \zll_main_dev204_outR3\);
      \zll_main_dev204_inR4\ <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"100"));
      \instR14\ : \ZLL_Main_dev204\ port map (\zll_main_dev204_inR4\(27 downto 20), \zll_main_dev204_inR4\(19 downto 17), \zll_main_dev204_inR4\(16 downto 9), \zll_main_dev204_inR4\(8 downto 6), \zll_main_dev204_inR4\(5 downto 3), \zll_main_dev204_inR4\(2 downto 0), \zll_main_dev204_outR4\);
      \zll_main_dev204_inR5\ <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"101"));
      \instR15\ : \ZLL_Main_dev204\ port map (\zll_main_dev204_inR5\(27 downto 20), \zll_main_dev204_inR5\(19 downto 17), \zll_main_dev204_inR5\(16 downto 9), \zll_main_dev204_inR5\(8 downto 6), \zll_main_dev204_inR5\(5 downto 3), \zll_main_dev204_inR5\(2 downto 0), \zll_main_dev204_outR5\);
      \zll_main_dev204_inR6\ <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"110"));
      \instR16\ : \ZLL_Main_dev204\ port map (\zll_main_dev204_inR6\(27 downto 20), \zll_main_dev204_inR6\(19 downto 17), \zll_main_dev204_inR6\(16 downto 9), \zll_main_dev204_inR6\(8 downto 6), \zll_main_dev204_inR6\(5 downto 3), \zll_main_dev204_inR6\(2 downto 0), \zll_main_dev204_outR6\);
      \zll_main_dev204_inR7\ <= (zll_main_dev151_in(24 downto 17) & zll_main_dev151_in(16 downto 14) & zll_main_dev151_in(13 downto 6) & zll_main_dev151_in(5 downto 3) & zll_main_dev151_in(2 downto 0) & std_logic_vector'(B"111"));
      \instR17\ : \ZLL_Main_dev204\ port map (\zll_main_dev204_inR7\(27 downto 20), \zll_main_dev204_inR7\(19 downto 17), \zll_main_dev204_inR7\(16 downto 9), \zll_main_dev204_inR7\(8 downto 6), \zll_main_dev204_inR7\(5 downto 3), \zll_main_dev204_inR7\(2 downto 0), \zll_main_dev204_outR7\);
      zll_main_dev32_in <= (zll_main_dev207_in(15 downto 8) & zll_main_dev207_in(7 downto 0));
      zll_main_dev86_in <= (zll_main_dev32_in(15 downto 8) & zll_main_dev32_in(7 downto 0));
      zll_main_dev115_in <= zll_main_dev86_in(15 downto 0);
      zll_main_dev59_in <= (zll_main_dev115_in(7 downto 0) & zll_main_dev115_in(15 downto 8));
      zll_main_dev187_in <= (zll_main_dev59_in(15 downto 8) & zll_main_dev59_in(7 downto 0) & std_logic_vector'(B"001"));
      zll_main_dev97_in <= (zll_main_dev187_in(2 downto 0) & zll_main_dev187_in(18 downto 11) & zll_main_dev187_in(10 downto 3));
      zll_main_dev214_in <= (zll_main_dev97_in(18 downto 16) & zll_main_dev97_in(15 downto 8) & zll_main_dev97_in(7 downto 0) & std_logic_vector'(B"010"));
      \zll_main_dev261_inR1\ <= (std_logic_vector'(B"111") & zll_main_dev214_in(2 downto 0));
      \instR18\ : \ZLL_Main_dev261\ port map (\zll_main_dev261_inR1\(5 downto 3), \zll_main_dev261_inR1\(2 downto 0), \zll_main_dev261_outR1\);
      zll_main_dev29_in <= (zll_main_dev214_in(21 downto 19) & zll_main_dev214_in(18 downto 11) & zll_main_dev214_in(2 downto 0) & zll_main_dev214_in(10 downto 3) & \zll_main_dev261_outR1\);
      zll_main_dev260_in <= (zll_main_dev29_in(24 downto 22) & zll_main_dev29_in(2 downto 0) & zll_main_dev29_in(10 downto 3) & std_logic_vector'(B"0"));
      \zll_main_dev158_inR1\ <= (zll_main_dev260_in(14 downto 12) & zll_main_dev260_in(11 downto 9) & std_logic_vector'(B"0"));
      \instR19\ : \ZLL_Main_dev158\ port map (\zll_main_dev158_inR1\(6 downto 4), \zll_main_dev158_inR1\(3 downto 1), \zll_main_dev158_inR1\(0 downto 0), \zll_main_dev158_outR1\);
      \id_inR1\ <= (zll_main_dev260_in(11 downto 9) & zll_main_dev260_in(0 downto 0));
      zll_main_dev268_in <= (zll_main_dev29_in(21 downto 14) & zll_main_dev29_in(13 downto 11) & zll_main_dev29_in(10 downto 3) & rw_cond(rw_eq(\id_inR1\(0 downto 0), std_logic_vector'(B"1")), \id_inR1\(3 downto 1), \zll_main_dev158_outR1\));
      zll_main_dev171_in <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"000"));
      \instR20\ : \ZLL_Main_dev171\ port map (zll_main_dev171_in(24 downto 17), zll_main_dev171_in(16 downto 14), zll_main_dev171_in(13 downto 11), zll_main_dev171_in(10 downto 3), zll_main_dev171_in(2 downto 0), zll_main_dev171_out);
      \zll_main_dev171_inR1\ <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"001"));
      \instR21\ : \ZLL_Main_dev171\ port map (\zll_main_dev171_inR1\(24 downto 17), \zll_main_dev171_inR1\(16 downto 14), \zll_main_dev171_inR1\(13 downto 11), \zll_main_dev171_inR1\(10 downto 3), \zll_main_dev171_inR1\(2 downto 0), \zll_main_dev171_outR1\);
      \zll_main_dev171_inR2\ <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"010"));
      \instR22\ : \ZLL_Main_dev171\ port map (\zll_main_dev171_inR2\(24 downto 17), \zll_main_dev171_inR2\(16 downto 14), \zll_main_dev171_inR2\(13 downto 11), \zll_main_dev171_inR2\(10 downto 3), \zll_main_dev171_inR2\(2 downto 0), \zll_main_dev171_outR2\);
      \zll_main_dev171_inR3\ <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"011"));
      \instR23\ : \ZLL_Main_dev171\ port map (\zll_main_dev171_inR3\(24 downto 17), \zll_main_dev171_inR3\(16 downto 14), \zll_main_dev171_inR3\(13 downto 11), \zll_main_dev171_inR3\(10 downto 3), \zll_main_dev171_inR3\(2 downto 0), \zll_main_dev171_outR3\);
      \zll_main_dev171_inR4\ <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"100"));
      \instR24\ : \ZLL_Main_dev171\ port map (\zll_main_dev171_inR4\(24 downto 17), \zll_main_dev171_inR4\(16 downto 14), \zll_main_dev171_inR4\(13 downto 11), \zll_main_dev171_inR4\(10 downto 3), \zll_main_dev171_inR4\(2 downto 0), \zll_main_dev171_outR4\);
      \zll_main_dev171_inR5\ <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"101"));
      \instR25\ : \ZLL_Main_dev171\ port map (\zll_main_dev171_inR5\(24 downto 17), \zll_main_dev171_inR5\(16 downto 14), \zll_main_dev171_inR5\(13 downto 11), \zll_main_dev171_inR5\(10 downto 3), \zll_main_dev171_inR5\(2 downto 0), \zll_main_dev171_outR5\);
      \zll_main_dev171_inR6\ <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"110"));
      \instR26\ : \ZLL_Main_dev171\ port map (\zll_main_dev171_inR6\(24 downto 17), \zll_main_dev171_inR6\(16 downto 14), \zll_main_dev171_inR6\(13 downto 11), \zll_main_dev171_inR6\(10 downto 3), \zll_main_dev171_inR6\(2 downto 0), \zll_main_dev171_outR6\);
      \zll_main_dev171_inR7\ <= (zll_main_dev268_in(21 downto 14) & zll_main_dev268_in(13 downto 11) & zll_main_dev268_in(2 downto 0) & zll_main_dev268_in(10 downto 3) & std_logic_vector'(B"111"));
      \instR27\ : \ZLL_Main_dev171\ port map (\zll_main_dev171_inR7\(24 downto 17), \zll_main_dev171_inR7\(16 downto 14), \zll_main_dev171_inR7\(13 downto 11), \zll_main_dev171_inR7\(10 downto 3), \zll_main_dev171_inR7\(2 downto 0), \zll_main_dev171_outR7\);
      zll_main_dev273_in <= (zll_main_dev207_in(15 downto 8) & zll_main_dev207_in(7 downto 0));
      zll_main_dev185_in <= (zll_main_dev273_in(15 downto 8) & zll_main_dev273_in(7 downto 0));
      zll_main_dev121_in <= zll_main_dev185_in(15 downto 0);
      zll_main_dev2_in <= (zll_main_dev121_in(15 downto 8) & zll_main_dev121_in(7 downto 0) & std_logic_vector'(B"001"));
      zll_main_dev84_in <= (zll_main_dev2_in(18 downto 11) & zll_main_dev2_in(2 downto 0) & zll_main_dev2_in(10 downto 3));
      zll_main_dev230_in <= (zll_main_dev84_in(18 downto 11) & zll_main_dev84_in(10 downto 8) & zll_main_dev84_in(7 downto 0) & std_logic_vector'(B"010"));
      \zll_main_dev261_inR2\ <= (std_logic_vector'(B"111") & zll_main_dev230_in(2 downto 0));
      \instR28\ : \ZLL_Main_dev261\ port map (\zll_main_dev261_inR2\(5 downto 3), \zll_main_dev261_inR2\(2 downto 0), \zll_main_dev261_outR2\);
      zll_main_dev249_in <= (zll_main_dev230_in(21 downto 14) & zll_main_dev230_in(2 downto 0) & zll_main_dev230_in(13 downto 11) & zll_main_dev230_in(10 downto 3) & \zll_main_dev261_outR2\);
      zll_main_dev63_in <= (zll_main_dev249_in(24 downto 17) & zll_main_dev249_in(2 downto 0) & zll_main_dev249_in(13 downto 11) & std_logic_vector'(B"0"));
      zll_main_dev170_in <= (zll_main_dev63_in(6 downto 4) & zll_main_dev63_in(3 downto 1) & std_logic_vector'(B"0"));
      zll_main_dev213_in <= (zll_main_dev170_in(6 downto 4) & zll_main_dev170_in(3 downto 1) & zll_main_dev170_in(0 downto 0));
      zll_main_dev262_in <= (zll_main_dev213_in(6 downto 4) & zll_main_dev213_in(3 downto 1));
      \instR29\ : \ZLL_Main_dev262\ port map (zll_main_dev262_in(5 downto 3), zll_main_dev262_in(2 downto 0), zll_main_dev262_out);
      \id_inR2\ <= (zll_main_dev63_in(6 downto 4) & zll_main_dev63_in(0 downto 0));
      zll_main_dev254_in <= (zll_main_dev249_in(24 downto 17) & zll_main_dev249_in(16 downto 14) & zll_main_dev249_in(13 downto 11) & zll_main_dev249_in(10 downto 3) & rw_cond(rw_eq(\id_inR2\(0 downto 0), std_logic_vector'(B"1")), \id_inR2\(3 downto 1), zll_main_dev262_out));
      zll_main_dev276_in <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"000"));
      \instR30\ : \ZLL_Main_dev276\ port map (zll_main_dev276_in(27 downto 20), zll_main_dev276_in(19 downto 17), zll_main_dev276_in(16 downto 14), zll_main_dev276_in(13 downto 11), zll_main_dev276_in(10 downto 3), zll_main_dev276_in(2 downto 0), zll_main_dev276_out);
      \zll_main_dev276_inR1\ <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"001"));
      \instR31\ : \ZLL_Main_dev276\ port map (\zll_main_dev276_inR1\(27 downto 20), \zll_main_dev276_inR1\(19 downto 17), \zll_main_dev276_inR1\(16 downto 14), \zll_main_dev276_inR1\(13 downto 11), \zll_main_dev276_inR1\(10 downto 3), \zll_main_dev276_inR1\(2 downto 0), \zll_main_dev276_outR1\);
      \zll_main_dev276_inR2\ <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"010"));
      \instR32\ : \ZLL_Main_dev276\ port map (\zll_main_dev276_inR2\(27 downto 20), \zll_main_dev276_inR2\(19 downto 17), \zll_main_dev276_inR2\(16 downto 14), \zll_main_dev276_inR2\(13 downto 11), \zll_main_dev276_inR2\(10 downto 3), \zll_main_dev276_inR2\(2 downto 0), \zll_main_dev276_outR2\);
      \zll_main_dev276_inR3\ <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"011"));
      \instR33\ : \ZLL_Main_dev276\ port map (\zll_main_dev276_inR3\(27 downto 20), \zll_main_dev276_inR3\(19 downto 17), \zll_main_dev276_inR3\(16 downto 14), \zll_main_dev276_inR3\(13 downto 11), \zll_main_dev276_inR3\(10 downto 3), \zll_main_dev276_inR3\(2 downto 0), \zll_main_dev276_outR3\);
      \zll_main_dev276_inR4\ <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"100"));
      \instR34\ : \ZLL_Main_dev276\ port map (\zll_main_dev276_inR4\(27 downto 20), \zll_main_dev276_inR4\(19 downto 17), \zll_main_dev276_inR4\(16 downto 14), \zll_main_dev276_inR4\(13 downto 11), \zll_main_dev276_inR4\(10 downto 3), \zll_main_dev276_inR4\(2 downto 0), \zll_main_dev276_outR4\);
      \zll_main_dev276_inR5\ <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"101"));
      \instR35\ : \ZLL_Main_dev276\ port map (\zll_main_dev276_inR5\(27 downto 20), \zll_main_dev276_inR5\(19 downto 17), \zll_main_dev276_inR5\(16 downto 14), \zll_main_dev276_inR5\(13 downto 11), \zll_main_dev276_inR5\(10 downto 3), \zll_main_dev276_inR5\(2 downto 0), \zll_main_dev276_outR5\);
      \zll_main_dev276_inR6\ <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"110"));
      \instR36\ : \ZLL_Main_dev276\ port map (\zll_main_dev276_inR6\(27 downto 20), \zll_main_dev276_inR6\(19 downto 17), \zll_main_dev276_inR6\(16 downto 14), \zll_main_dev276_inR6\(13 downto 11), \zll_main_dev276_inR6\(10 downto 3), \zll_main_dev276_inR6\(2 downto 0), \zll_main_dev276_outR6\);
      \zll_main_dev276_inR7\ <= (zll_main_dev254_in(24 downto 17) & zll_main_dev254_in(2 downto 0) & zll_main_dev254_in(16 downto 14) & zll_main_dev254_in(13 downto 11) & zll_main_dev254_in(10 downto 3) & std_logic_vector'(B"111"));
      \instR37\ : \ZLL_Main_dev276\ port map (\zll_main_dev276_inR7\(27 downto 20), \zll_main_dev276_inR7\(19 downto 17), \zll_main_dev276_inR7\(16 downto 14), \zll_main_dev276_inR7\(13 downto 11), \zll_main_dev276_inR7\(10 downto 3), \zll_main_dev276_inR7\(2 downto 0), \zll_main_dev276_outR7\);
      pause <= ((zll_main_dev72_out & \zll_main_dev72_outR1\ & \zll_main_dev72_outR2\ & \zll_main_dev72_outR3\ & \zll_main_dev72_outR4\ & \zll_main_dev72_outR5\ & \zll_main_dev72_outR6\ & \zll_main_dev72_outR7\) & (zll_main_dev204_out & \zll_main_dev204_outR1\ & \zll_main_dev204_outR2\ & \zll_main_dev204_outR3\ & \zll_main_dev204_outR4\ & \zll_main_dev204_outR5\ & \zll_main_dev204_outR6\ & \zll_main_dev204_outR7\) & (zll_main_dev171_out & \zll_main_dev171_outR1\ & \zll_main_dev171_outR2\ & \zll_main_dev171_outR3\ & \zll_main_dev171_outR4\ & \zll_main_dev171_outR5\ & \zll_main_dev171_outR6\ & \zll_main_dev171_outR7\) & (zll_main_dev276_out & \zll_main_dev276_outR1\ & \zll_main_dev276_outR2\ & \zll_main_dev276_outR3\ & \zll_main_dev276_outR4\ & \zll_main_dev276_outR5\ & \zll_main_dev276_outR6\ & \zll_main_dev276_outR7\));
      \__out0\ <= pause(31 downto 24);
      \__out1\ <= pause(23 downto 16);
      \__out2\ <= pause(15 downto 8);
      \__out3\ <= pause(7 downto 0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev276\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (7 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev276\ is
component \ZLL_Main_dev179\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev255\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev256\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev262\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_dev255_in : std_logic_vector (5 downto 0);
      signal zll_main_dev255_out : std_logic_vector (0 downto 0);
      signal zll_main_dev5_in : std_logic_vector (28 downto 0);
      signal \zll_main_dev255_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev255_outR1\ : std_logic_vector (0 downto 0);
      signal zll_main_dev114_in : std_logic_vector (20 downto 0);
      signal zll_main_dev165_in : std_logic_vector (20 downto 0);
      signal resize_in : std_logic_vector (7 downto 0);
      signal zll_main_dev179_in : std_logic_vector (5 downto 0);
      signal zll_main_dev179_out : std_logic_vector (2 downto 0);
      signal zll_main_dev256_in : std_logic_vector (5 downto 0);
      signal zll_main_dev256_out : std_logic_vector (2 downto 0);
      signal zll_main_dev262_in : std_logic_vector (5 downto 0);
      signal zll_main_dev262_out : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \binop_inR2\ : std_logic_vector (255 downto 0);
      signal \binop_inR3\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
      signal zll_main_dev81_in : std_logic_vector (17 downto 0);
      signal \resize_inR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_dev256_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev256_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev262_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev262_outR1\ : std_logic_vector (2 downto 0);
      signal \resize_inR4\ : std_logic_vector (2 downto 0);
      signal \binop_inR4\ : std_logic_vector (255 downto 0);
      signal \binop_inR5\ : std_logic_vector (255 downto 0);
      signal \binop_inR6\ : std_logic_vector (255 downto 0);
      signal \binop_inR7\ : std_logic_vector (255 downto 0);
      signal \resize_inR5\ : std_logic_vector (127 downto 0);
begin
zll_main_dev255_in <= (arg5 & arg1);
      inst : \ZLL_Main_dev255\ port map (zll_main_dev255_in(5 downto 3), zll_main_dev255_in(2 downto 0), zll_main_dev255_out);
      zll_main_dev5_in <= (arg0 & arg5 & arg1 & arg2 & arg3 & arg4 & zll_main_dev255_out);
      \zll_main_dev255_inR1\ <= (zll_main_dev5_in(20 downto 18) & zll_main_dev5_in(17 downto 15));
      \instR1\ : \ZLL_Main_dev255\ port map (\zll_main_dev255_inR1\(5 downto 3), \zll_main_dev255_inR1\(2 downto 0), \zll_main_dev255_outR1\);
      zll_main_dev114_in <= (zll_main_dev5_in(20 downto 18) & zll_main_dev5_in(17 downto 15) & zll_main_dev5_in(14 downto 12) & zll_main_dev5_in(11 downto 9) & zll_main_dev5_in(8 downto 1) & \zll_main_dev255_outR1\);
      zll_main_dev165_in <= (zll_main_dev114_in(20 downto 18) & zll_main_dev114_in(17 downto 15) & zll_main_dev114_in(14 downto 12) & zll_main_dev114_in(11 downto 9) & zll_main_dev114_in(8 downto 1) & zll_main_dev114_in(0 downto 0));
      resize_in <= zll_main_dev165_in(8 downto 1);
      zll_main_dev179_in <= (zll_main_dev165_in(20 downto 18) & zll_main_dev165_in(17 downto 15));
      \instR2\ : \ZLL_Main_dev179\ port map (zll_main_dev179_in(5 downto 3), zll_main_dev179_in(2 downto 0), zll_main_dev179_out);
      zll_main_dev256_in <= (zll_main_dev179_out & zll_main_dev165_in(14 downto 12));
      \instR3\ : \ZLL_Main_dev256\ port map (zll_main_dev256_in(5 downto 3), zll_main_dev256_in(2 downto 0), zll_main_dev256_out);
      zll_main_dev262_in <= (zll_main_dev256_out & zll_main_dev165_in(11 downto 9));
      \instR4\ : \ZLL_Main_dev262\ port map (zll_main_dev262_in(5 downto 3), zll_main_dev262_in(2 downto 0), zll_main_dev262_out);
      \resize_inR1\ <= zll_main_dev262_out;
      binop_in <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_sub(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR2\ <= (rw_sub(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR3\ <= (rw_resize(resize_in(7 downto 0), 128) & rw_mul(\binop_inR2\(255 downto 128), \binop_inR2\(127 downto 0)));
      \resize_inR2\ <= rw_shiftr(\binop_inR3\(255 downto 128), \binop_inR3\(127 downto 0));
      zll_main_dev81_in <= (zll_main_dev5_in(28 downto 21) & zll_main_dev5_in(20 downto 18) & zll_main_dev5_in(14 downto 12) & zll_main_dev5_in(11 downto 9) & zll_main_dev5_in(0 downto 0));
      \resize_inR3\ <= zll_main_dev81_in(17 downto 10);
      \zll_main_dev256_inR1\ <= (zll_main_dev81_in(9 downto 7) & zll_main_dev81_in(6 downto 4));
      \instR5\ : \ZLL_Main_dev256\ port map (\zll_main_dev256_inR1\(5 downto 3), \zll_main_dev256_inR1\(2 downto 0), \zll_main_dev256_outR1\);
      \zll_main_dev262_inR1\ <= (\zll_main_dev256_outR1\ & zll_main_dev81_in(3 downto 1));
      \instR6\ : \ZLL_Main_dev262\ port map (\zll_main_dev262_inR1\(5 downto 3), \zll_main_dev262_inR1\(2 downto 0), \zll_main_dev262_outR1\);
      \resize_inR4\ <= \zll_main_dev262_outR1\;
      \binop_inR4\ <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR4\(2 downto 0), 128));
      \binop_inR5\ <= (rw_sub(\binop_inR4\(255 downto 128), \binop_inR4\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR6\ <= (rw_sub(\binop_inR5\(255 downto 128), \binop_inR5\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR7\ <= (rw_resize(\resize_inR3\(7 downto 0), 128) & rw_mul(\binop_inR6\(255 downto 128), \binop_inR6\(127 downto 0)));
      \resize_inR5\ <= rw_shiftr(\binop_inR7\(255 downto 128), \binop_inR7\(127 downto 0));
      res <= rw_cond(rw_eq(zll_main_dev81_in(0 downto 0), std_logic_vector'(B"1")), rw_resize(\resize_inR5\(127 downto 0), 1), rw_resize(\resize_inR2\(127 downto 0), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev266\ is
port (arg0 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev266\ is
signal resize_in : std_logic_vector (2 downto 0);
      signal zll_main_dev265_in : std_logic_vector (127 downto 0);
      signal \resize_inR1\ : std_logic_vector (127 downto 0);
      signal msbit_in : std_logic_vector (0 downto 0);
      signal rewire_prelude_not_in : std_logic_vector (0 downto 0);
      signal zll_rewire_prelude_not_in : std_logic_vector (1 downto 0);
      signal lit_in : std_logic_vector (0 downto 0);
begin
resize_in <= arg0;
      zll_main_dev265_in <= rw_resize(resize_in(2 downto 0), 128);
      \resize_inR1\ <= zll_main_dev265_in(127 downto 0);
      msbit_in <= rw_resize(\resize_inR1\(127 downto 0), 1);
      rewire_prelude_not_in <= msbit_in(0 downto 0);
      zll_rewire_prelude_not_in <= (rewire_prelude_not_in(0 downto 0) & rewire_prelude_not_in(0 downto 0));
      lit_in <= zll_rewire_prelude_not_in(0 downto 0);
      res <= rw_cond(rw_eq(lit_in(0 downto 0), std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev262\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev262\ is
signal zll_main_dev278_in : std_logic_vector (5 downto 0);
      signal zll_main_dev235_in : std_logic_vector (5 downto 0);
      signal resize_in : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
begin
zll_main_dev278_in <= (arg0 & arg1);
      zll_main_dev235_in <= zll_main_dev278_in(5 downto 0);
      resize_in <= zll_main_dev235_in(5 downto 3);
      \resize_inR1\ <= zll_main_dev235_in(2 downto 0);
      binop_in <= (rw_resize(resize_in(2 downto 0), 128) & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_add(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"));
      \resize_inR2\ <= rw_mod(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0));
      res <= rw_resize(\resize_inR2\(127 downto 0), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev261\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev261\ is
signal zll_main_dev244_in : std_logic_vector (5 downto 0);
      signal zll_main_dev282_in : std_logic_vector (5 downto 0);
      signal resize_in : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
begin
zll_main_dev244_in <= (arg0 & arg1);
      zll_main_dev282_in <= zll_main_dev244_in(5 downto 0);
      resize_in <= zll_main_dev282_in(5 downto 3);
      \resize_inR1\ <= zll_main_dev282_in(2 downto 0);
      binop_in <= (rw_resize(resize_in(2 downto 0), 128) & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_div(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"));
      \resize_inR2\ <= rw_mod(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0));
      res <= rw_resize(\resize_inR2\(127 downto 0), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev256\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev256\ is
signal zll_main_dev159_in : std_logic_vector (5 downto 0);
      signal zll_main_dev200_in : std_logic_vector (5 downto 0);
      signal resize_in : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
begin
zll_main_dev159_in <= (arg0 & arg1);
      zll_main_dev200_in <= zll_main_dev159_in(5 downto 0);
      resize_in <= zll_main_dev200_in(5 downto 3);
      \resize_inR1\ <= zll_main_dev200_in(2 downto 0);
      binop_in <= (rw_resize(resize_in(2 downto 0), 128) & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_mul(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"));
      \resize_inR2\ <= rw_mod(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0));
      res <= rw_resize(\resize_inR2\(127 downto 0), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev255\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev255\ is
signal zll_main_dev267_in : std_logic_vector (5 downto 0);
      signal zll_main_dev270_in : std_logic_vector (5 downto 0);
      signal resize_in : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
begin
zll_main_dev267_in <= (arg0 & arg1);
      zll_main_dev270_in <= zll_main_dev267_in(5 downto 0);
      resize_in <= zll_main_dev270_in(5 downto 3);
      \resize_inR1\ <= zll_main_dev270_in(2 downto 0);
      binop_in <= (rw_resize(resize_in(2 downto 0), 128) & rw_resize(\resize_inR1\(2 downto 0), 128));
      res <= rw_lt(binop_in(255 downto 128), binop_in(127 downto 0));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev204\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev204\ is
component \ZLL_Main_dev179\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev261\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev262\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev266\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zll_main_dev266_in : std_logic_vector (2 downto 0);
      signal zll_main_dev266_out : std_logic_vector (0 downto 0);
      signal zll_main_dev88_in : std_logic_vector (28 downto 0);
      signal \zll_main_dev266_inR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev266_outR1\ : std_logic_vector (0 downto 0);
      signal zll_main_dev74_in : std_logic_vector (20 downto 0);
      signal zll_main_dev242_in : std_logic_vector (20 downto 0);
      signal resize_in : std_logic_vector (7 downto 0);
      signal zll_main_dev179_in : std_logic_vector (5 downto 0);
      signal zll_main_dev179_out : std_logic_vector (2 downto 0);
      signal zll_main_dev261_in : std_logic_vector (5 downto 0);
      signal zll_main_dev261_out : std_logic_vector (2 downto 0);
      signal zll_main_dev262_in : std_logic_vector (5 downto 0);
      signal zll_main_dev262_out : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \binop_inR2\ : std_logic_vector (255 downto 0);
      signal \binop_inR3\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
      signal zll_main_dev100_in : std_logic_vector (17 downto 0);
      signal \resize_inR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_dev261_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev261_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev262_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev262_outR1\ : std_logic_vector (2 downto 0);
      signal \resize_inR4\ : std_logic_vector (2 downto 0);
      signal \binop_inR4\ : std_logic_vector (255 downto 0);
      signal \binop_inR5\ : std_logic_vector (255 downto 0);
      signal \binop_inR6\ : std_logic_vector (255 downto 0);
      signal \binop_inR7\ : std_logic_vector (255 downto 0);
      signal \resize_inR5\ : std_logic_vector (127 downto 0);
begin
zll_main_dev266_in <= arg5;
      inst : \ZLL_Main_dev266\ port map (zll_main_dev266_in(2 downto 0), zll_main_dev266_out);
      zll_main_dev88_in <= (arg0 & arg5 & arg1 & arg2 & arg3 & arg4 & zll_main_dev266_out);
      \zll_main_dev266_inR1\ <= zll_main_dev88_in(20 downto 18);
      \instR1\ : \ZLL_Main_dev266\ port map (\zll_main_dev266_inR1\(2 downto 0), \zll_main_dev266_outR1\);
      zll_main_dev74_in <= (zll_main_dev88_in(20 downto 18) & zll_main_dev88_in(17 downto 15) & zll_main_dev88_in(14 downto 7) & zll_main_dev88_in(6 downto 4) & zll_main_dev88_in(3 downto 1) & \zll_main_dev266_outR1\);
      zll_main_dev242_in <= (zll_main_dev74_in(20 downto 18) & zll_main_dev74_in(17 downto 15) & zll_main_dev74_in(14 downto 7) & zll_main_dev74_in(6 downto 4) & zll_main_dev74_in(3 downto 1) & zll_main_dev74_in(0 downto 0));
      resize_in <= zll_main_dev242_in(14 downto 7);
      zll_main_dev179_in <= (zll_main_dev242_in(20 downto 18) & zll_main_dev242_in(6 downto 4));
      \instR2\ : \ZLL_Main_dev179\ port map (zll_main_dev179_in(5 downto 3), zll_main_dev179_in(2 downto 0), zll_main_dev179_out);
      zll_main_dev261_in <= (zll_main_dev179_out & zll_main_dev242_in(17 downto 15));
      \instR3\ : \ZLL_Main_dev261\ port map (zll_main_dev261_in(5 downto 3), zll_main_dev261_in(2 downto 0), zll_main_dev261_out);
      zll_main_dev262_in <= (zll_main_dev242_in(3 downto 1) & zll_main_dev261_out);
      \instR4\ : \ZLL_Main_dev262\ port map (zll_main_dev262_in(5 downto 3), zll_main_dev262_in(2 downto 0), zll_main_dev262_out);
      \resize_inR1\ <= zll_main_dev262_out;
      binop_in <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_sub(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR2\ <= (rw_sub(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR3\ <= (rw_resize(resize_in(7 downto 0), 128) & rw_mul(\binop_inR2\(255 downto 128), \binop_inR2\(127 downto 0)));
      \resize_inR2\ <= rw_shiftr(\binop_inR3\(255 downto 128), \binop_inR3\(127 downto 0));
      zll_main_dev100_in <= (zll_main_dev88_in(28 downto 21) & zll_main_dev88_in(20 downto 18) & zll_main_dev88_in(17 downto 15) & zll_main_dev88_in(3 downto 1) & zll_main_dev88_in(0 downto 0));
      \resize_inR3\ <= zll_main_dev100_in(17 downto 10);
      \zll_main_dev261_inR1\ <= (zll_main_dev100_in(9 downto 7) & zll_main_dev100_in(6 downto 4));
      \instR5\ : \ZLL_Main_dev261\ port map (\zll_main_dev261_inR1\(5 downto 3), \zll_main_dev261_inR1\(2 downto 0), \zll_main_dev261_outR1\);
      \zll_main_dev262_inR1\ <= (zll_main_dev100_in(3 downto 1) & \zll_main_dev261_outR1\);
      \instR6\ : \ZLL_Main_dev262\ port map (\zll_main_dev262_inR1\(5 downto 3), \zll_main_dev262_inR1\(2 downto 0), \zll_main_dev262_outR1\);
      \resize_inR4\ <= \zll_main_dev262_outR1\;
      \binop_inR4\ <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR4\(2 downto 0), 128));
      \binop_inR5\ <= (rw_sub(\binop_inR4\(255 downto 128), \binop_inR4\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR6\ <= (rw_sub(\binop_inR5\(255 downto 128), \binop_inR5\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR7\ <= (rw_resize(\resize_inR3\(7 downto 0), 128) & rw_mul(\binop_inR6\(255 downto 128), \binop_inR6\(127 downto 0)));
      \resize_inR5\ <= rw_shiftr(\binop_inR7\(255 downto 128), \binop_inR7\(127 downto 0));
      res <= rw_cond(rw_eq(zll_main_dev100_in(0 downto 0), std_logic_vector'(B"1")), rw_resize(\resize_inR5\(127 downto 0), 1), rw_resize(\resize_inR2\(127 downto 0), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev179\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev179\ is
signal zll_main_dev152_in : std_logic_vector (5 downto 0);
      signal zll_main_dev283_in : std_logic_vector (5 downto 0);
      signal resize_in : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
begin
zll_main_dev152_in <= (arg0 & arg1);
      zll_main_dev283_in <= zll_main_dev152_in(5 downto 0);
      resize_in <= zll_main_dev283_in(5 downto 3);
      \resize_inR1\ <= zll_main_dev283_in(2 downto 0);
      binop_in <= (rw_resize(resize_in(2 downto 0), 128) & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_sub(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"));
      \resize_inR2\ <= rw_mod(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0));
      res <= rw_resize(\resize_inR2\(127 downto 0), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev171\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev171\ is
component \ZLL_Main_dev179\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev255\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev256\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_dev255_in : std_logic_vector (5 downto 0);
      signal zll_main_dev255_out : std_logic_vector (0 downto 0);
      signal zll_main_dev67_in : std_logic_vector (25 downto 0);
      signal \zll_main_dev255_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev255_outR1\ : std_logic_vector (0 downto 0);
      signal zll_main_dev221_in : std_logic_vector (17 downto 0);
      signal zll_main_dev175_in : std_logic_vector (17 downto 0);
      signal resize_in : std_logic_vector (7 downto 0);
      signal zll_main_dev179_in : std_logic_vector (5 downto 0);
      signal zll_main_dev179_out : std_logic_vector (2 downto 0);
      signal zll_main_dev256_in : std_logic_vector (5 downto 0);
      signal zll_main_dev256_out : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \binop_inR2\ : std_logic_vector (255 downto 0);
      signal \binop_inR3\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
      signal zll_main_dev93_in : std_logic_vector (14 downto 0);
      signal \resize_inR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_dev256_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev256_outR1\ : std_logic_vector (2 downto 0);
      signal \resize_inR4\ : std_logic_vector (2 downto 0);
      signal \binop_inR4\ : std_logic_vector (255 downto 0);
      signal \binop_inR5\ : std_logic_vector (255 downto 0);
      signal \binop_inR6\ : std_logic_vector (255 downto 0);
      signal \binop_inR7\ : std_logic_vector (255 downto 0);
      signal \resize_inR5\ : std_logic_vector (127 downto 0);
begin
zll_main_dev255_in <= (arg4 & arg2);
      inst : \ZLL_Main_dev255\ port map (zll_main_dev255_in(5 downto 3), zll_main_dev255_in(2 downto 0), zll_main_dev255_out);
      zll_main_dev67_in <= (arg0 & arg1 & arg2 & arg3 & arg4 & zll_main_dev255_out);
      \zll_main_dev255_inR1\ <= (zll_main_dev67_in(3 downto 1) & zll_main_dev67_in(14 downto 12));
      \instR1\ : \ZLL_Main_dev255\ port map (\zll_main_dev255_inR1\(5 downto 3), \zll_main_dev255_inR1\(2 downto 0), \zll_main_dev255_outR1\);
      zll_main_dev221_in <= (zll_main_dev67_in(25 downto 18) & zll_main_dev67_in(17 downto 15) & zll_main_dev67_in(14 downto 12) & zll_main_dev67_in(3 downto 1) & \zll_main_dev255_outR1\);
      zll_main_dev175_in <= (zll_main_dev221_in(17 downto 10) & zll_main_dev221_in(9 downto 7) & zll_main_dev221_in(6 downto 4) & zll_main_dev221_in(3 downto 1) & zll_main_dev221_in(0 downto 0));
      resize_in <= zll_main_dev175_in(17 downto 10);
      zll_main_dev179_in <= (zll_main_dev175_in(3 downto 1) & zll_main_dev175_in(6 downto 4));
      \instR2\ : \ZLL_Main_dev179\ port map (zll_main_dev179_in(5 downto 3), zll_main_dev179_in(2 downto 0), zll_main_dev179_out);
      zll_main_dev256_in <= (zll_main_dev179_out & zll_main_dev175_in(9 downto 7));
      \instR3\ : \ZLL_Main_dev256\ port map (zll_main_dev256_in(5 downto 3), zll_main_dev256_in(2 downto 0), zll_main_dev256_out);
      \resize_inR1\ <= zll_main_dev256_out;
      binop_in <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_sub(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR2\ <= (rw_sub(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR3\ <= (rw_resize(resize_in(7 downto 0), 128) & rw_mul(\binop_inR2\(255 downto 128), \binop_inR2\(127 downto 0)));
      \resize_inR2\ <= rw_shiftr(\binop_inR3\(255 downto 128), \binop_inR3\(127 downto 0));
      zll_main_dev93_in <= (zll_main_dev67_in(17 downto 15) & zll_main_dev67_in(11 downto 4) & zll_main_dev67_in(3 downto 1) & zll_main_dev67_in(0 downto 0));
      \resize_inR3\ <= zll_main_dev93_in(11 downto 4);
      \zll_main_dev256_inR1\ <= (zll_main_dev93_in(3 downto 1) & zll_main_dev93_in(14 downto 12));
      \instR4\ : \ZLL_Main_dev256\ port map (\zll_main_dev256_inR1\(5 downto 3), \zll_main_dev256_inR1\(2 downto 0), \zll_main_dev256_outR1\);
      \resize_inR4\ <= \zll_main_dev256_outR1\;
      \binop_inR4\ <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR4\(2 downto 0), 128));
      \binop_inR5\ <= (rw_sub(\binop_inR4\(255 downto 128), \binop_inR4\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR6\ <= (rw_sub(\binop_inR5\(255 downto 128), \binop_inR5\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR7\ <= (rw_resize(\resize_inR3\(7 downto 0), 128) & rw_mul(\binop_inR6\(255 downto 128), \binop_inR6\(127 downto 0)));
      \resize_inR5\ <= rw_shiftr(\binop_inR7\(255 downto 128), \binop_inR7\(127 downto 0));
      res <= rw_cond(rw_eq(zll_main_dev93_in(0 downto 0), std_logic_vector'(B"1")), rw_resize(\resize_inR5\(127 downto 0), 1), rw_resize(\resize_inR2\(127 downto 0), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev158\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev158\ is
component \ZLL_Main_dev262\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_dev231_in : std_logic_vector (6 downto 0);
      signal zll_main_dev262_in : std_logic_vector (5 downto 0);
      signal zll_main_dev262_out : std_logic_vector (2 downto 0);
begin
zll_main_dev231_in <= (arg0 & arg1 & arg2);
      zll_main_dev262_in <= (zll_main_dev231_in(3 downto 1) & zll_main_dev231_in(6 downto 4));
      inst : \ZLL_Main_dev262\ port map (zll_main_dev262_in(5 downto 3), zll_main_dev262_in(2 downto 0), zll_main_dev262_out);
      res <= zll_main_dev262_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev72\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev72\ is
component \ZLL_Main_dev179\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev261\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev266\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zll_main_dev266_in : std_logic_vector (2 downto 0);
      signal zll_main_dev266_out : std_logic_vector (0 downto 0);
      signal zll_main_dev167_in : std_logic_vector (25 downto 0);
      signal \zll_main_dev266_inR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev266_outR1\ : std_logic_vector (0 downto 0);
      signal zll_main_dev24_in : std_logic_vector (17 downto 0);
      signal zll_main_dev197_in : std_logic_vector (17 downto 0);
      signal resize_in : std_logic_vector (7 downto 0);
      signal zll_main_dev179_in : std_logic_vector (5 downto 0);
      signal zll_main_dev179_out : std_logic_vector (2 downto 0);
      signal zll_main_dev261_in : std_logic_vector (5 downto 0);
      signal zll_main_dev261_out : std_logic_vector (2 downto 0);
      signal \resize_inR1\ : std_logic_vector (2 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \binop_inR1\ : std_logic_vector (255 downto 0);
      signal \binop_inR2\ : std_logic_vector (255 downto 0);
      signal \binop_inR3\ : std_logic_vector (255 downto 0);
      signal \resize_inR2\ : std_logic_vector (127 downto 0);
      signal zll_main_dev277_in : std_logic_vector (14 downto 0);
      signal \resize_inR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_dev261_inR1\ : std_logic_vector (5 downto 0);
      signal \zll_main_dev261_outR1\ : std_logic_vector (2 downto 0);
      signal \resize_inR4\ : std_logic_vector (2 downto 0);
      signal \binop_inR4\ : std_logic_vector (255 downto 0);
      signal \binop_inR5\ : std_logic_vector (255 downto 0);
      signal \binop_inR6\ : std_logic_vector (255 downto 0);
      signal \binop_inR7\ : std_logic_vector (255 downto 0);
      signal \resize_inR5\ : std_logic_vector (127 downto 0);
begin
zll_main_dev266_in <= arg4;
      inst : \ZLL_Main_dev266\ port map (zll_main_dev266_in(2 downto 0), zll_main_dev266_out);
      zll_main_dev167_in <= (arg0 & arg1 & arg2 & arg4 & arg3 & zll_main_dev266_out);
      \zll_main_dev266_inR1\ <= zll_main_dev167_in(6 downto 4);
      \instR1\ : \ZLL_Main_dev266\ port map (\zll_main_dev266_inR1\(2 downto 0), \zll_main_dev266_outR1\);
      zll_main_dev24_in <= (zll_main_dev167_in(25 downto 18) & zll_main_dev167_in(9 downto 7) & zll_main_dev167_in(6 downto 4) & zll_main_dev167_in(3 downto 1) & \zll_main_dev266_outR1\);
      zll_main_dev197_in <= (zll_main_dev24_in(17 downto 10) & zll_main_dev24_in(9 downto 7) & zll_main_dev24_in(6 downto 4) & zll_main_dev24_in(3 downto 1) & zll_main_dev24_in(0 downto 0));
      resize_in <= zll_main_dev197_in(17 downto 10);
      zll_main_dev179_in <= (zll_main_dev197_in(6 downto 4) & zll_main_dev197_in(9 downto 7));
      \instR2\ : \ZLL_Main_dev179\ port map (zll_main_dev179_in(5 downto 3), zll_main_dev179_in(2 downto 0), zll_main_dev179_out);
      zll_main_dev261_in <= (zll_main_dev179_out & zll_main_dev197_in(3 downto 1));
      \instR3\ : \ZLL_Main_dev261\ port map (zll_main_dev261_in(5 downto 3), zll_main_dev261_in(2 downto 0), zll_main_dev261_out);
      \resize_inR1\ <= zll_main_dev261_out;
      binop_in <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR1\(2 downto 0), 128));
      \binop_inR1\ <= (rw_sub(binop_in(255 downto 128), binop_in(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR2\ <= (rw_sub(\binop_inR1\(255 downto 128), \binop_inR1\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR3\ <= (rw_resize(resize_in(7 downto 0), 128) & rw_mul(\binop_inR2\(255 downto 128), \binop_inR2\(127 downto 0)));
      \resize_inR2\ <= rw_shiftr(\binop_inR3\(255 downto 128), \binop_inR3\(127 downto 0));
      zll_main_dev277_in <= (zll_main_dev167_in(17 downto 10) & zll_main_dev167_in(6 downto 4) & zll_main_dev167_in(3 downto 1) & zll_main_dev167_in(0 downto 0));
      \resize_inR3\ <= zll_main_dev277_in(14 downto 7);
      \zll_main_dev261_inR1\ <= (zll_main_dev277_in(6 downto 4) & zll_main_dev277_in(3 downto 1));
      \instR4\ : \ZLL_Main_dev261\ port map (\zll_main_dev261_inR1\(5 downto 3), \zll_main_dev261_inR1\(2 downto 0), \zll_main_dev261_outR1\);
      \resize_inR4\ <= \zll_main_dev261_outR1\;
      \binop_inR4\ <= (std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000") & rw_resize(\resize_inR4\(2 downto 0), 128));
      \binop_inR5\ <= (rw_sub(\binop_inR4\(255 downto 128), \binop_inR4\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR6\ <= (rw_sub(\binop_inR5\(255 downto 128), \binop_inR5\(127 downto 0)) & std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"));
      \binop_inR7\ <= (rw_resize(\resize_inR3\(7 downto 0), 128) & rw_mul(\binop_inR6\(255 downto 128), \binop_inR6\(127 downto 0)));
      \resize_inR5\ <= rw_shiftr(\binop_inR7\(255 downto 128), \binop_inR7\(127 downto 0));
      res <= rw_cond(rw_eq(zll_main_dev277_in(0 downto 0), std_logic_vector'(B"1")), rw_resize(\resize_inR5\(127 downto 0), 1), rw_resize(\resize_inR2\(127 downto 0), 1));
end architecture;