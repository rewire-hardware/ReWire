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
port (\__in0\ : in std_logic_vector (1023 downto 0);
      \__in1\ : in std_logic_vector (31 downto 0);
      \__out0\ : out std_logic_vector (63 downto 0));
end entity;

architecture rtl of top_level is
component \ZLL_Main_compute\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (63 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zll_main_loop4_in : std_logic_vector (1055 downto 0);
      signal zll_main_loop3_in : std_logic_vector (1055 downto 0);
      signal main_compute_in : std_logic_vector (1055 downto 0);
      signal zll_main_compute1_in : std_logic_vector (1055 downto 0);
      signal zll_main_compute3_in : std_logic_vector (1055 downto 0);
      signal id_in : std_logic_vector (1023 downto 0);
      signal \id_inR1\ : std_logic_vector (511 downto 0);
      signal zll_main_compute_in : std_logic_vector (95 downto 0);
      signal zll_main_compute_out : std_logic_vector (7 downto 0);
      signal \id_inR2\ : std_logic_vector (1023 downto 0);
      signal \id_inR3\ : std_logic_vector (511 downto 0);
      signal \zll_main_compute_inR1\ : std_logic_vector (95 downto 0);
      signal \zll_main_compute_outR1\ : std_logic_vector (7 downto 0);
      signal \id_inR4\ : std_logic_vector (1023 downto 0);
      signal \id_inR5\ : std_logic_vector (511 downto 0);
      signal \zll_main_compute_inR2\ : std_logic_vector (95 downto 0);
      signal \zll_main_compute_outR2\ : std_logic_vector (7 downto 0);
      signal \id_inR6\ : std_logic_vector (1023 downto 0);
      signal \id_inR7\ : std_logic_vector (511 downto 0);
      signal \zll_main_compute_inR3\ : std_logic_vector (95 downto 0);
      signal \zll_main_compute_outR3\ : std_logic_vector (7 downto 0);
      signal \id_inR8\ : std_logic_vector (1023 downto 0);
      signal \id_inR9\ : std_logic_vector (511 downto 0);
      signal \zll_main_compute_inR4\ : std_logic_vector (95 downto 0);
      signal \zll_main_compute_outR4\ : std_logic_vector (7 downto 0);
      signal \id_inR10\ : std_logic_vector (1023 downto 0);
      signal \id_inR11\ : std_logic_vector (511 downto 0);
      signal \zll_main_compute_inR5\ : std_logic_vector (95 downto 0);
      signal \zll_main_compute_outR5\ : std_logic_vector (7 downto 0);
      signal \id_inR12\ : std_logic_vector (1023 downto 0);
      signal \id_inR13\ : std_logic_vector (511 downto 0);
      signal \zll_main_compute_inR6\ : std_logic_vector (95 downto 0);
      signal \zll_main_compute_outR6\ : std_logic_vector (7 downto 0);
      signal \id_inR14\ : std_logic_vector (1023 downto 0);
      signal \id_inR15\ : std_logic_vector (511 downto 0);
      signal \zll_main_compute_inR7\ : std_logic_vector (95 downto 0);
      signal \zll_main_compute_outR7\ : std_logic_vector (7 downto 0);
      signal zll_main_loop1_in : std_logic_vector (64 downto 0);
      signal zll_main_loop5_in : std_logic_vector (64 downto 0);
      signal \__padding\ : std_logic_vector (0 downto 0);
      signal rwtmp0 : std_logic_vector (64 downto 0);
begin
zll_main_loop4_in <= (\__in0\ & \__in1\);
      zll_main_loop3_in <= zll_main_loop4_in(1055 downto 0);
      main_compute_in <= (zll_main_loop3_in(1055 downto 32) & zll_main_loop3_in(31 downto 0));
      zll_main_compute1_in <= (main_compute_in(1055 downto 32) & main_compute_in(31 downto 0));
      zll_main_compute3_in <= zll_main_compute1_in(1055 downto 0);
      id_in <= zll_main_compute3_in(1055 downto 32);
      \id_inR1\ <= id_in(1023 downto 512);
      zll_main_compute_in <= (zll_main_compute3_in(31 downto 0) & \id_inR1\(511 downto 448));
      inst : \ZLL_Main_compute\ port map (zll_main_compute_in(95 downto 64), zll_main_compute_in(63 downto 0), zll_main_compute_out);
      \id_inR2\ <= zll_main_compute3_in(1055 downto 32);
      \id_inR3\ <= \id_inR2\(1023 downto 512);
      \zll_main_compute_inR1\ <= (zll_main_compute3_in(31 downto 0) & \id_inR3\(447 downto 384));
      \instR1\ : \ZLL_Main_compute\ port map (\zll_main_compute_inR1\(95 downto 64), \zll_main_compute_inR1\(63 downto 0), \zll_main_compute_outR1\);
      \id_inR4\ <= zll_main_compute3_in(1055 downto 32);
      \id_inR5\ <= \id_inR4\(1023 downto 512);
      \zll_main_compute_inR2\ <= (zll_main_compute3_in(31 downto 0) & \id_inR5\(383 downto 320));
      \instR2\ : \ZLL_Main_compute\ port map (\zll_main_compute_inR2\(95 downto 64), \zll_main_compute_inR2\(63 downto 0), \zll_main_compute_outR2\);
      \id_inR6\ <= zll_main_compute3_in(1055 downto 32);
      \id_inR7\ <= \id_inR6\(1023 downto 512);
      \zll_main_compute_inR3\ <= (zll_main_compute3_in(31 downto 0) & \id_inR7\(319 downto 256));
      \instR3\ : \ZLL_Main_compute\ port map (\zll_main_compute_inR3\(95 downto 64), \zll_main_compute_inR3\(63 downto 0), \zll_main_compute_outR3\);
      \id_inR8\ <= zll_main_compute3_in(1055 downto 32);
      \id_inR9\ <= \id_inR8\(1023 downto 512);
      \zll_main_compute_inR4\ <= (zll_main_compute3_in(31 downto 0) & \id_inR9\(255 downto 192));
      \instR4\ : \ZLL_Main_compute\ port map (\zll_main_compute_inR4\(95 downto 64), \zll_main_compute_inR4\(63 downto 0), \zll_main_compute_outR4\);
      \id_inR10\ <= zll_main_compute3_in(1055 downto 32);
      \id_inR11\ <= \id_inR10\(1023 downto 512);
      \zll_main_compute_inR5\ <= (zll_main_compute3_in(31 downto 0) & \id_inR11\(191 downto 128));
      \instR5\ : \ZLL_Main_compute\ port map (\zll_main_compute_inR5\(95 downto 64), \zll_main_compute_inR5\(63 downto 0), \zll_main_compute_outR5\);
      \id_inR12\ <= zll_main_compute3_in(1055 downto 32);
      \id_inR13\ <= \id_inR12\(1023 downto 512);
      \zll_main_compute_inR6\ <= (zll_main_compute3_in(31 downto 0) & \id_inR13\(127 downto 64));
      \instR6\ : \ZLL_Main_compute\ port map (\zll_main_compute_inR6\(95 downto 64), \zll_main_compute_inR6\(63 downto 0), \zll_main_compute_outR6\);
      \id_inR14\ <= zll_main_compute3_in(1055 downto 32);
      \id_inR15\ <= \id_inR14\(1023 downto 512);
      \zll_main_compute_inR7\ <= (zll_main_compute3_in(31 downto 0) & \id_inR15\(63 downto 0));
      \instR7\ : \ZLL_Main_compute\ port map (\zll_main_compute_inR7\(95 downto 64), \zll_main_compute_inR7\(63 downto 0), \zll_main_compute_outR7\);
      zll_main_loop1_in <= (std_logic_vector'(B"0") & (zll_main_compute_out & \zll_main_compute_outR1\ & \zll_main_compute_outR2\ & \zll_main_compute_outR3\ & \zll_main_compute_outR4\ & \zll_main_compute_outR5\ & \zll_main_compute_outR6\ & \zll_main_compute_outR7\));
      zll_main_loop5_in <= zll_main_loop1_in(64 downto 0);
      rwtmp0 <= (std_logic_vector'(B"1") & zll_main_loop5_in(63 downto 0));
      \__padding\ <= rwtmp0(64 downto 64);
      \__out0\ <= rwtmp0(63 downto 0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (63 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute\ is
signal id_in : std_logic_vector (31 downto 0);
      signal \id_inR1\ : std_logic_vector (63 downto 0);
      signal binop_in : std_logic_vector (15 downto 0);
begin
id_in <= arg0;
      \id_inR1\ <= arg1;
      binop_in <= (id_in(7 downto 0) & \id_inR1\(39 downto 32));
      res <= rw_resize(rw_add(binop_in(15 downto 8), binop_in(7 downto 0)), 8);
end architecture;