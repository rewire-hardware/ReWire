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
      \__out0\ : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of top_level is
component \Main_first\ is
      port (arg0 : in std_logic_vector (15 downto 0);
            res : out std_logic_vector (23 downto 0));
      end component;
      component \ZLL_Main_incr17\ is
      port (arg0 : in std_logic_vector (15 downto 0);
            res : out std_logic_vector (25 downto 0));
      end component;
      component \ZLL_Main_sig10\ is
      port (arg0 : in std_logic_vector (23 downto 0);
            res : out std_logic_vector (25 downto 0));
      end component;
      signal zll_main_sig9_in : std_logic_vector (16 downto 0);
      signal zll_main_sig6_in : std_logic_vector (17 downto 0);
      signal zll_main_sig4_in : std_logic_vector (16 downto 0);
      signal zll_main_sig3_in : std_logic_vector (16 downto 0);
      signal main_incr_in : std_logic_vector (15 downto 0);
      signal main_first_in : std_logic_vector (15 downto 0);
      signal main_first_out : std_logic_vector (23 downto 0);
      signal zll_main_sig10_in : std_logic_vector (23 downto 0);
      signal zll_main_sig10_out : std_logic_vector (25 downto 0);
      signal zll_main_incr2_in : std_logic_vector (25 downto 0);
      signal zll_main_incr9_in : std_logic_vector (25 downto 0);
      signal zll_main_incr3_in : std_logic_vector (23 downto 0);
      signal main_second_in : std_logic_vector (15 downto 0);
      signal zll_main_second2_in : std_logic_vector (31 downto 0);
      signal zll_main_second6_in : std_logic_vector (31 downto 0);
      signal zll_main_second_in : std_logic_vector (15 downto 0);
      signal resize_in : std_logic_vector (15 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \resize_inR1\ : std_logic_vector (127 downto 0);
      signal \zll_main_sig10_inR1\ : std_logic_vector (23 downto 0);
      signal \zll_main_sig10_outR1\ : std_logic_vector (25 downto 0);
      signal zll_main_incr19_in : std_logic_vector (33 downto 0);
      signal zll_main_incr20_in : std_logic_vector (33 downto 0);
      signal zll_main_incr14_in : std_logic_vector (31 downto 0);
      signal \binop_inR1\ : std_logic_vector (15 downto 0);
      signal zll_main_begin6_in : std_logic_vector (15 downto 0);
      signal zll_main_incr10_in : std_logic_vector (25 downto 0);
      signal zll_main_incr17_in : std_logic_vector (25 downto 0);
      signal zll_main_incr17_out : std_logic_vector (25 downto 0);
      signal \zll_main_incr17_inR1\ : std_logic_vector (16 downto 0);
      signal \zll_main_incr17_outR1\ : std_logic_vector (25 downto 0);
      signal \__padding\ : std_logic_vector (1 downto 0);
      signal \__st0\ : std_logic_vector (15 downto 0) := std_logic_vector'(B"0000000000000001");
      signal \__st0_next\ : std_logic_vector (15 downto 0);
      signal rwtmp0 : std_logic_vector (25 downto 0);
begin
zll_main_sig9_in <= (\__in0\ & \__st0\);
      zll_main_sig6_in <= (zll_main_sig9_in(16 downto 16) & zll_main_sig9_in(16 downto 16) & zll_main_sig9_in(15 downto 0));
      zll_main_sig4_in <= (zll_main_sig6_in(17 downto 17) & zll_main_sig6_in(15 downto 0));
      zll_main_sig3_in <= (zll_main_sig4_in(15 downto 0) & zll_main_sig4_in(16 downto 16));
      main_incr_in <= zll_main_sig3_in(16 downto 1);
      main_first_in <= main_incr_in(15 downto 0);
      inst : \Main_first\ port map (main_first_in(15 downto 0), main_first_out);
      zll_main_sig10_in <= main_first_out;
      \instR1\ : \ZLL_Main_sig10\ port map (zll_main_sig10_in(23 downto 0), zll_main_sig10_out);
      zll_main_incr2_in <= zll_main_sig10_out;
      zll_main_incr9_in <= zll_main_incr2_in(25 downto 0);
      zll_main_incr3_in <= (zll_main_incr9_in(23 downto 16) & zll_main_incr9_in(15 downto 0));
      main_second_in <= zll_main_incr3_in(15 downto 0);
      zll_main_second2_in <= (main_second_in(15 downto 0) & main_second_in(15 downto 0));
      zll_main_second6_in <= zll_main_second2_in(31 downto 0);
      zll_main_second_in <= zll_main_second6_in(31 downto 16);
      resize_in <= zll_main_second_in(15 downto 0);
      binop_in <= rw_resize((rw_resize(resize_in(15 downto 0), 128) & rw_repl(128, std_logic_vector'(B"0"))), 256);
      \resize_inR1\ <= rw_resize(rw_shiftr(binop_in(255 downto 128), binop_in(127 downto 0)), 128);
      \zll_main_sig10_inR1\ <= (\resize_inR1\(7 downto 0) & zll_main_second6_in(15 downto 0));
      \instR2\ : \ZLL_Main_sig10\ port map (\zll_main_sig10_inR1\(23 downto 0), \zll_main_sig10_outR1\);
      zll_main_incr19_in <= (zll_main_incr3_in(23 downto 16) & \zll_main_sig10_outR1\);
      zll_main_incr20_in <= (zll_main_incr19_in(33 downto 26) & zll_main_incr19_in(25 downto 0));
      zll_main_incr14_in <= (zll_main_incr20_in(33 downto 26) & zll_main_incr20_in(23 downto 16) & zll_main_incr20_in(15 downto 0));
      \binop_inR1\ <= (zll_main_incr14_in(31 downto 24) & zll_main_incr14_in(23 downto 16));
      zll_main_begin6_in <= rw_resize((zll_main_incr14_in(23 downto 16) & rw_add(\binop_inR1\(15 downto 8), \binop_inR1\(7 downto 0))), 16);
      zll_main_incr10_in <= (std_logic_vector'(B"0100000000") & zll_main_begin6_in(15 downto 0));
      zll_main_incr17_in <= zll_main_incr10_in(25 downto 0);
      \instR3\ : \ZLL_Main_incr17\ port map (zll_main_incr17_in(15 downto 0), zll_main_incr17_out);
      \zll_main_incr17_inR1\ <= (zll_main_sig6_in(15 downto 0) & zll_main_sig6_in(16 downto 16));
      \instR4\ : \ZLL_Main_incr17\ port map (\zll_main_incr17_inR1\(16 downto 1), \zll_main_incr17_outR1\);
      rwtmp0 <= rw_resize(rw_cond(rw_eq(\zll_main_incr17_inR1\(0 downto 0), std_logic_vector'(B"1")), \zll_main_incr17_outR1\, zll_main_incr17_out), 26);
      \__padding\ <= rwtmp0(25 downto 24);
      \__out0\ <= rwtmp0(23 downto 16);
      \__st0_next\ <= rwtmp0(15 downto 0);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"0000000000000001");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_incr17\ is
port (arg0 : in std_logic_vector (15 downto 0);
      res : out std_logic_vector (25 downto 0));
end entity;

architecture rtl of \ZLL_Main_incr17\ is
component \Main_first\ is
      port (arg0 : in std_logic_vector (15 downto 0);
            res : out std_logic_vector (23 downto 0));
      end component;
      component \ZLL_Main_sig10\ is
      port (arg0 : in std_logic_vector (23 downto 0);
            res : out std_logic_vector (25 downto 0));
      end component;
      signal main_sig_in : std_logic_vector (15 downto 0);
      signal main_first_in : std_logic_vector (15 downto 0);
      signal main_first_out : std_logic_vector (23 downto 0);
      signal zll_main_sig10_in : std_logic_vector (23 downto 0);
      signal zll_main_sig10_out : std_logic_vector (25 downto 0);
      signal zll_main_sig2_in : std_logic_vector (25 downto 0);
      signal zll_main_sig7_in : std_logic_vector (25 downto 0);
      signal zll_main_sig8_in : std_logic_vector (23 downto 0);
begin
main_sig_in <= arg0;
      main_first_in <= main_sig_in(15 downto 0);
      inst : \Main_first\ port map (main_first_in(15 downto 0), main_first_out);
      zll_main_sig10_in <= main_first_out;
      \instR1\ : \ZLL_Main_sig10\ port map (zll_main_sig10_in(23 downto 0), zll_main_sig10_out);
      zll_main_sig2_in <= zll_main_sig10_out;
      zll_main_sig7_in <= zll_main_sig2_in(25 downto 0);
      zll_main_sig8_in <= (zll_main_sig7_in(23 downto 16) & zll_main_sig7_in(15 downto 0));
      res <= (std_logic_vector'(B"10") & zll_main_sig8_in(23 downto 16) & zll_main_sig8_in(15 downto 0));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_sig10\ is
port (arg0 : in std_logic_vector (23 downto 0);
      res : out std_logic_vector (25 downto 0));
end entity;

architecture rtl of \ZLL_Main_sig10\ is
signal zll_main_sig5_in : std_logic_vector (23 downto 0);
begin
zll_main_sig5_in <= arg0;
      res <= (std_logic_vector'(B"00") & zll_main_sig5_in(23 downto 16) & zll_main_sig5_in(15 downto 0));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_first\ is
port (arg0 : in std_logic_vector (15 downto 0);
      res : out std_logic_vector (23 downto 0));
end entity;

architecture rtl of \Main_first\ is
signal zll_main_first_in : std_logic_vector (31 downto 0);
      signal zll_main_first1_in : std_logic_vector (31 downto 0);
      signal zll_main_first4_in : std_logic_vector (15 downto 0);
      signal id_in : std_logic_vector (15 downto 0);
begin
zll_main_first_in <= (arg0 & arg0);
      zll_main_first1_in <= zll_main_first_in(31 downto 0);
      zll_main_first4_in <= zll_main_first1_in(31 downto 16);
      id_in <= zll_main_first4_in(15 downto 0);
      res <= (id_in(15 downto 8) & zll_main_first1_in(15 downto 0));
end architecture;