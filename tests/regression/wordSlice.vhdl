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
port (\__in0\ : in std_logic_vector (15 downto 0);
      \__out0\ : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of top_level is
component \ReWirezuPreludezuzaza\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal \__padding\ : std_logic_vector (0 downto 0);
      signal zll_main_loop1_in : std_logic_vector (15 downto 0);
      signal zll_main_compute3_in : std_logic_vector (15 downto 0);
      signal msbit_in : std_logic_vector (15 downto 0);
      signal id_in : std_logic_vector (15 downto 0);
      signal rewirezupreludezuzazazuin : std_logic_vector (1 downto 0);
      signal rewirezupreludezuzaza_out : std_logic_vector (0 downto 0);
      signal zll_main_compute4_in : std_logic_vector (16 downto 0);
      signal \msbit_inR1\ : std_logic_vector (15 downto 0);
      signal \id_inR1\ : std_logic_vector (15 downto 0);
      signal \rewirezupreludezuzazazuinR1\ : std_logic_vector (1 downto 0);
      signal \rewirezupreludezuzaza_outR1\ : std_logic_vector (0 downto 0);
      signal zll_main_compute2_in : std_logic_vector (16 downto 0);
      signal zll_main_compute5_in : std_logic_vector (16 downto 0);
      signal \id_inR2\ : std_logic_vector (15 downto 0);
      signal zll_main_compute_in : std_logic_vector (16 downto 0);
      signal \id_inR3\ : std_logic_vector (15 downto 0);
      signal zll_main_loop_in : std_logic_vector (8 downto 0);
      signal zll_main_loop2_in : std_logic_vector (8 downto 0);
      signal pause : std_logic_vector (8 downto 0);
begin
zll_main_loop1_in <= \__in0\;
      zll_main_compute3_in <= zll_main_loop1_in(15 downto 0);
      msbit_in <= zll_main_compute3_in(15 downto 0);
      id_in <= zll_main_compute3_in(15 downto 0);
      rewirezupreludezuzazazuin <= (msbit_in(15 downto 15) & id_in(8 downto 8));
      inst : \ReWirezuPreludezuzaza\ port map (rewirezupreludezuzazazuin(1 downto 1), rewirezupreludezuzazazuin(0 downto 0), rewirezupreludezuzaza_out);
      zll_main_compute4_in <= (zll_main_compute3_in(15 downto 0) & rewirezupreludezuzaza_out);
      \msbit_inR1\ <= zll_main_compute4_in(16 downto 1);
      \id_inR1\ <= zll_main_compute4_in(16 downto 1);
      \rewirezupreludezuzazazuinR1\ <= (\msbit_inR1\(15 downto 15) & \id_inR1\(8 downto 8));
      \instR1\ : \ReWirezuPreludezuzaza\ port map (\rewirezupreludezuzazazuinR1\(1 downto 1), \rewirezupreludezuzazazuinR1\(0 downto 0), \rewirezupreludezuzaza_outR1\);
      zll_main_compute2_in <= (zll_main_compute4_in(16 downto 1) & \rewirezupreludezuzaza_outR1\);
      zll_main_compute5_in <= (zll_main_compute2_in(16 downto 1) & zll_main_compute2_in(0 downto 0));
      \id_inR2\ <= zll_main_compute5_in(16 downto 1);
      zll_main_compute_in <= (zll_main_compute4_in(16 downto 1) & zll_main_compute4_in(0 downto 0));
      \id_inR3\ <= zll_main_compute_in(16 downto 1);
      zll_main_loop_in <= (std_logic_vector'(B"0") & rw_cond(rw_eq(zll_main_compute_in(0 downto 0), std_logic_vector'(B"1")), \id_inR3\(7 downto 0), \id_inR2\(15 downto 8)));
      zll_main_loop2_in <= zll_main_loop_in(8 downto 0);
      pause <= (std_logic_vector'(B"1") & zll_main_loop2_in(7 downto 0));
      \__padding\ <= pause(8 downto 8);
      \__out0\ <= pause(7 downto 0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ReWirezuPreludezuzaza\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ReWirezuPreludezuzaza\ is
signal zzllzurewirezupreludezuzaza1zuin : std_logic_vector (3 downto 0);
      signal zzllzurewirezupreludezuzazazuin : std_logic_vector (1 downto 0);
      signal lit_in : std_logic_vector (1 downto 0);
      signal id_in : std_logic_vector (1 downto 0);
begin
zzllzurewirezupreludezuzaza1zuin <= (arg0 & arg1 & arg0 & arg1);
      zzllzurewirezupreludezuzazazuin <= (zzllzurewirezupreludezuzaza1zuin(3 downto 3) & zzllzurewirezupreludezuzaza1zuin(2 downto 2));
      lit_in <= zzllzurewirezupreludezuzazazuin(1 downto 0);
      id_in <= zzllzurewirezupreludezuzaza1zuin(1 downto 0);
      res <= rw_cond(rw_eq(id_in(1 downto 1), std_logic_vector'(B"1")), id_in(0 downto 0), std_logic_vector'(B"0"));
end architecture;