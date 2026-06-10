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
      \__in0\ : in std_logic_vector (0 downto 0);
      \__out0\ : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of top_level is
component \ReWire_Prelude_not\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_sig15\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      component \ZLL_Main_sig24\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      component \ZLL_Main_sig35\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal zll_main_sig34_in : std_logic_vector (2 downto 0);
      signal rewire_prelude_not_in : std_logic_vector (0 downto 0);
      signal rewire_prelude_not_out : std_logic_vector (0 downto 0);
      signal \rewire_prelude_not_inR1\ : std_logic_vector (0 downto 0);
      signal \rewire_prelude_not_outR1\ : std_logic_vector (0 downto 0);
      signal zll_main_sig29_in : std_logic_vector (2 downto 0);
      signal zll_main_sig23_in : std_logic_vector (1 downto 0);
      signal zll_main_sig24_in : std_logic_vector (4 downto 0);
      signal zll_main_sig24_out : std_logic_vector (4 downto 0);
      signal zll_main_sig39_in : std_logic_vector (2 downto 0);
      signal zll_main_sig46_in : std_logic_vector (1 downto 0);
      signal zll_main_sig8_in : std_logic_vector (2 downto 0);
      signal zll_main_sig45_in : std_logic_vector (2 downto 0);
      signal zll_main_sig16_in : std_logic_vector (4 downto 0);
      signal zll_main_sig36_in : std_logic_vector (4 downto 0);
      signal zll_main_sig41_in : std_logic_vector (2 downto 0);
      signal zll_main_sig35_in : std_logic_vector (2 downto 0);
      signal zll_main_sig35_out : std_logic_vector (4 downto 0);
      signal zll_main_sig14_in : std_logic_vector (5 downto 0);
      signal zll_main_sig11_in : std_logic_vector (5 downto 0);
      signal zll_main_sig38_in : std_logic_vector (3 downto 0);
      signal zll_main_sig26_in : std_logic_vector (3 downto 0);
      signal zll_main_sig47_in : std_logic_vector (3 downto 0);
      signal zll_main_sig15_in : std_logic_vector (1 downto 0);
      signal zll_main_sig15_out : std_logic_vector (4 downto 0);
      signal zll_main_sig49_in : std_logic_vector (6 downto 0);
      signal zll_main_sig32_in : std_logic_vector (6 downto 0);
      signal zll_main_sig5_in : std_logic_vector (3 downto 0);
      signal zll_main_sig3_in : std_logic_vector (3 downto 0);
      signal zll_main_incr3_in : std_logic_vector (1 downto 0);
      signal zll_main_incr2_in : std_logic_vector (1 downto 0);
      signal zll_main_incr1_in : std_logic_vector (1 downto 0);
      signal binop_in : std_logic_vector (1 downto 0);
      signal msbit_in : std_logic_vector (0 downto 0);
      signal \zll_main_sig15_inR1\ : std_logic_vector (1 downto 0);
      signal \zll_main_sig15_outR1\ : std_logic_vector (4 downto 0);
      signal \zll_main_sig24_inR1\ : std_logic_vector (4 downto 0);
      signal \zll_main_sig24_outR1\ : std_logic_vector (4 downto 0);
      signal \__padding\ : std_logic_vector (1 downto 0);
      signal \__st0\ : std_logic_vector (0 downto 0) := std_logic_vector'(B"1");
      signal \__st1\ : std_logic_vector (0 downto 0) := std_logic_vector'(B"1");
      signal \__st0_next\ : std_logic_vector (0 downto 0);
      signal \__st1_next\ : std_logic_vector (0 downto 0);
      signal rwtmp0 : std_logic_vector (4 downto 0);
begin
zll_main_sig34_in <= (\__in0\ & (\__st0\ & \__st1\));
      rewire_prelude_not_in <= zll_main_sig34_in(2 downto 2);
      inst : \ReWire_Prelude_not\ port map (rewire_prelude_not_in(0 downto 0), rewire_prelude_not_out);
      \rewire_prelude_not_inR1\ <= zll_main_sig34_in(2 downto 2);
      \instR1\ : \ReWire_Prelude_not\ port map (\rewire_prelude_not_inR1\(0 downto 0), \rewire_prelude_not_outR1\);
      zll_main_sig29_in <= (zll_main_sig34_in(0 downto 0) & zll_main_sig34_in(1 downto 1) & \rewire_prelude_not_outR1\);
      zll_main_sig23_in <= (zll_main_sig29_in(1 downto 1) & zll_main_sig29_in(2 downto 2));
      zll_main_sig24_in <= (std_logic_vector'(B"010") & zll_main_sig23_in(1 downto 1) & zll_main_sig23_in(0 downto 0));
      \instR2\ : \ZLL_Main_sig24\ port map (zll_main_sig24_in(4 downto 0), zll_main_sig24_out);
      zll_main_sig39_in <= (zll_main_sig34_in(0 downto 0) & zll_main_sig34_in(1 downto 1) & rewire_prelude_not_out);
      zll_main_sig46_in <= (zll_main_sig39_in(1 downto 1) & zll_main_sig39_in(2 downto 2));
      zll_main_sig8_in <= (zll_main_sig46_in(1 downto 1) & zll_main_sig46_in(1 downto 1) & zll_main_sig46_in(0 downto 0));
      zll_main_sig45_in <= zll_main_sig8_in(2 downto 0);
      zll_main_sig16_in <= (std_logic_vector'(B"00") & zll_main_sig45_in(2 downto 2) & zll_main_sig45_in(1 downto 1) & zll_main_sig45_in(0 downto 0));
      zll_main_sig36_in <= zll_main_sig16_in(4 downto 0);
      zll_main_sig41_in <= (zll_main_sig36_in(2 downto 2) & zll_main_sig36_in(1 downto 1) & zll_main_sig36_in(0 downto 0));
      zll_main_sig35_in <= (zll_main_sig41_in(0 downto 0) & zll_main_sig41_in(1 downto 1) & zll_main_sig41_in(0 downto 0));
      \instR3\ : \ZLL_Main_sig35\ port map (zll_main_sig35_in(2 downto 0), zll_main_sig35_out);
      zll_main_sig14_in <= (zll_main_sig41_in(2 downto 2) & zll_main_sig35_out);
      zll_main_sig11_in <= (zll_main_sig14_in(5 downto 5) & zll_main_sig14_in(4 downto 0));
      zll_main_sig38_in <= (zll_main_sig11_in(5 downto 5) & zll_main_sig11_in(2 downto 2) & zll_main_sig11_in(1 downto 1) & zll_main_sig11_in(0 downto 0));
      zll_main_sig26_in <= (zll_main_sig38_in(2 downto 2) & zll_main_sig38_in(3 downto 3) & zll_main_sig38_in(1 downto 1) & zll_main_sig38_in(0 downto 0));
      zll_main_sig47_in <= (zll_main_sig26_in(2 downto 2) & zll_main_sig26_in(3 downto 3) & zll_main_sig26_in(1 downto 1) & zll_main_sig26_in(0 downto 0));
      zll_main_sig15_in <= (zll_main_sig47_in(2 downto 2) & zll_main_sig47_in(0 downto 0));
      \instR4\ : \ZLL_Main_sig15\ port map (zll_main_sig15_in(1 downto 0), zll_main_sig15_out);
      zll_main_sig49_in <= (zll_main_sig47_in(2 downto 2) & zll_main_sig47_in(3 downto 3) & zll_main_sig15_out);
      zll_main_sig32_in <= (zll_main_sig49_in(6 downto 6) & zll_main_sig49_in(5 downto 5) & zll_main_sig49_in(4 downto 0));
      zll_main_sig5_in <= (zll_main_sig32_in(6 downto 6) & zll_main_sig32_in(5 downto 5) & zll_main_sig32_in(1 downto 1) & zll_main_sig32_in(0 downto 0));
      zll_main_sig3_in <= (zll_main_sig5_in(2 downto 2) & zll_main_sig5_in(3 downto 3) & zll_main_sig5_in(1 downto 1) & zll_main_sig5_in(0 downto 0));
      zll_main_incr3_in <= (zll_main_sig3_in(3 downto 3) & zll_main_sig3_in(2 downto 2));
      zll_main_incr2_in <= (zll_main_incr3_in(1 downto 1) & zll_main_incr3_in(0 downto 0));
      zll_main_incr1_in <= zll_main_incr2_in(1 downto 0);
      binop_in <= (zll_main_incr1_in(1 downto 1) & zll_main_incr1_in(0 downto 0));
      msbit_in <= rw_resize(rw_xor(binop_in(1 downto 1), binop_in(0 downto 0)), 1);
      \zll_main_sig15_inR1\ <= (zll_main_sig3_in(1 downto 1) & msbit_in(0 downto 0));
      \instR5\ : \ZLL_Main_sig15\ port map (\zll_main_sig15_inR1\(1 downto 0), \zll_main_sig15_outR1\);
      \zll_main_sig24_inR1\ <= \zll_main_sig15_outR1\;
      \instR6\ : \ZLL_Main_sig24\ port map (\zll_main_sig24_inR1\(4 downto 0), \zll_main_sig24_outR1\);
      rwtmp0 <= rw_resize(rw_cond(rw_eq(zll_main_sig39_in(0 downto 0), std_logic_vector'(B"1")), \zll_main_sig24_outR1\, zll_main_sig24_out), 5);
      \__padding\ <= rwtmp0(4 downto 3);
      \__out0\ <= rwtmp0(2 downto 2);
      \__st0_next\ <= rwtmp0(1 downto 1);
      \__st1_next\ <= rwtmp0(0 downto 0);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"1");
                  \__st1\ <= std_logic_vector'(B"1");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ReWire_Prelude_not\ is
port (arg0 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ReWire_Prelude_not\ is
signal zll_rewire_prelude_not1_in : std_logic_vector (1 downto 0);
      signal lit_in : std_logic_vector (0 downto 0);
begin
zll_rewire_prelude_not1_in <= (arg0 & arg0);
      lit_in <= zll_rewire_prelude_not1_in(0 downto 0);
      res <= rw_resize(rw_cond(rw_eq(lit_in(0 downto 0), std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1")), 1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_sig35\ is
port (arg0 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \ZLL_Main_sig35\ is
signal zll_main_sig30_in : std_logic_vector (2 downto 0);
      signal zll_main_sig50_in : std_logic_vector (2 downto 0);
begin
zll_main_sig30_in <= arg0;
      zll_main_sig50_in <= (zll_main_sig30_in(1 downto 1) & zll_main_sig30_in(2 downto 2) & zll_main_sig30_in(0 downto 0));
      res <= (std_logic_vector'(B"00") & zll_main_sig50_in(1 downto 1) & zll_main_sig50_in(2 downto 2) & zll_main_sig50_in(0 downto 0));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_sig24\ is
port (arg0 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \ZLL_Main_sig24\ is
component \ZLL_Main_sig35\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal zll_main_sig43_in : std_logic_vector (4 downto 0);
      signal zll_main_sig1_in : std_logic_vector (1 downto 0);
      signal zll_main_sig35_in : std_logic_vector (2 downto 0);
      signal zll_main_sig35_out : std_logic_vector (4 downto 0);
      signal zll_main_sig7_in : std_logic_vector (4 downto 0);
      signal zll_main_sig17_in : std_logic_vector (4 downto 0);
      signal zll_main_sig40_in : std_logic_vector (2 downto 0);
begin
zll_main_sig43_in <= arg0;
      zll_main_sig1_in <= (zll_main_sig43_in(1 downto 1) & zll_main_sig43_in(0 downto 0));
      zll_main_sig35_in <= (zll_main_sig1_in(1 downto 1) & zll_main_sig1_in(1 downto 1) & zll_main_sig1_in(0 downto 0));
      inst : \ZLL_Main_sig35\ port map (zll_main_sig35_in(2 downto 0), zll_main_sig35_out);
      zll_main_sig7_in <= zll_main_sig35_out;
      zll_main_sig17_in <= zll_main_sig7_in(4 downto 0);
      zll_main_sig40_in <= (zll_main_sig17_in(2 downto 2) & zll_main_sig17_in(1 downto 1) & zll_main_sig17_in(0 downto 0));
      res <= (std_logic_vector'(B"10") & zll_main_sig40_in(2 downto 2) & zll_main_sig40_in(1 downto 1) & zll_main_sig40_in(0 downto 0));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_sig15\ is
port (arg0 : in std_logic_vector (1 downto 0);
      res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \ZLL_Main_sig15\ is
signal zll_main_sig27_in : std_logic_vector (1 downto 0);
begin
zll_main_sig27_in <= arg0;
      res <= (std_logic_vector'(B"010") & zll_main_sig27_in(1 downto 1) & zll_main_sig27_in(0 downto 0));
end architecture;