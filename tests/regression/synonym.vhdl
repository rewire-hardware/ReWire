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
      \__in0\ : in std_logic_vector (0 downto 0);
      \__out0\ : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of top_level is
component \Main_sig\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (25 downto 0));
      end component;
      component \ZLL_Main_incr32\ is
      port (arg0 : in std_logic_vector (23 downto 0);
            res : out std_logic_vector (25 downto 0));
      end component;
      component \ZLL_Main_incr36\ is
      port (arg0 : in std_logic_vector (15 downto 0);
            res : out std_logic_vector (25 downto 0));
      end component;
      signal \__st0\ : std_logic_vector (7 downto 0) := std_logic_vector'(B"00000000");
      signal \__st0_next\ : std_logic_vector (7 downto 0);
      signal \__st1\ : std_logic_vector (7 downto 0) := std_logic_vector'(B"00000001");
      signal \__st1_next\ : std_logic_vector (7 downto 0);
      signal zin : std_logic_vector (16 downto 0);
      signal zi0 : std_logic_vector (15 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (16 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (16 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (23 downto 0);
      signal zll_main_incr32_out : std_logic_vector (25 downto 0);
      signal zi9 : std_logic_vector (25 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal \connR1\ : std_logic_vector (23 downto 0);
      signal \zll_main_incr32_outR1\ : std_logic_vector (25 downto 0);
      signal zi13 : std_logic_vector (25 downto 0);
      signal zi14 : std_logic_vector (33 downto 0);
      signal zi15 : std_logic_vector (7 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal \connR2\ : std_logic_vector (15 downto 0);
      signal zll_main_incr36_out : std_logic_vector (25 downto 0);
      signal zi19 : std_logic_vector (25 downto 0);
      signal zi20 : std_logic_vector (41 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (7 downto 0);
      signal zi24 : std_logic_vector (7 downto 0);
      signal \connR3\ : std_logic_vector (15 downto 0);
      signal \zll_main_incr36_outR1\ : std_logic_vector (25 downto 0);
      signal zi25 : std_logic_vector (25 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (7 downto 0);
      signal main_sig_out : std_logic_vector (25 downto 0);
      signal zi28 : std_logic_vector (16 downto 0);
      signal zi29 : std_logic_vector (7 downto 0);
      signal zi30 : std_logic_vector (7 downto 0);
      signal \main_sig_outR1\ : std_logic_vector (25 downto 0);
      signal zres : std_logic_vector (25 downto 0);
begin
zin <= (\__st0\ & \__st1\ & \__in0\);
      zi0 <= zin(16 downto 1);
      zi1 <= zin(0 downto 0);
      zi2 <= (zi1 & zi0);
      zi3 <= zi2(16 downto 16);
      zi4 <= zi2(15 downto 8);
      zi5 <= zi2(7 downto 0);
      zi6 <= (zi4 & zi5 & zi3);
      zi7 <= zi6(16 downto 9);
      zi8 <= zi6(8 downto 1);
      conn <= (zi7 & zi7 & zi8);
      inst : \ZLL_Main_incr32\ port map (conn, zll_main_incr32_out);
      zi9 <= zll_main_incr32_out;
      zi10 <= zi9(23 downto 16);
      zi11 <= zi9(15 downto 8);
      zi12 <= zi9(7 downto 0);
      \connR1\ <= (zi12 & zi11 & zi12);
      \instR1\ : \ZLL_Main_incr32\ port map (\connR1\, \zll_main_incr32_outR1\);
      zi13 <= \zll_main_incr32_outR1\;
      zi14 <= (zi10 & zi13);
      zi15 <= zi14(33 downto 26);
      zi16 <= zi14(23 downto 16);
      zi17 <= zi14(15 downto 8);
      zi18 <= zi14(7 downto 0);
      \connR2\ <= (zi16 & zi18);
      \instR2\ : \ZLL_Main_incr36\ port map (\connR2\, zll_main_incr36_out);
      zi19 <= zll_main_incr36_out;
      zi20 <= (zi15 & zi16 & zi19);
      zi21 <= zi20(41 downto 34);
      zi22 <= zi20(33 downto 26);
      zi23 <= zi20(15 downto 8);
      zi24 <= zi20(7 downto 0);
      \connR3\ <= (zi23 & rw_add(zi21, zi22));
      \instR3\ : \ZLL_Main_incr36\ port map (\connR3\, \zll_main_incr36_outR1\);
      zi25 <= \zll_main_incr36_outR1\;
      zi26 <= zi25(15 downto 8);
      zi27 <= zi25(7 downto 0);
      \instR4\ : \Main_sig\ port map (zi26, zi27, main_sig_out);
      zi28 <= (zi5 & zi4 & zi3);
      zi29 <= zi28(16 downto 9);
      zi30 <= zi28(8 downto 1);
      \instR5\ : \Main_sig\ port map (zi30, zi29, \main_sig_outR1\);
      zres <= rw_cond(rw_eq(zi6(0 downto 0), std_logic_vector'(B"1")), main_sig_out, \main_sig_outR1\);
      \__st0_next\ <= zres(15 downto 8);
      \__st1_next\ <= zres(7 downto 0);
      \__out0\ <= zres(23 downto 16);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"00000000");
                  \__st1\ <= std_logic_vector'(B"00000001");
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
entity \ZLL_Main_incr36\ is
port (arg0 : in std_logic_vector (15 downto 0);
      res : out std_logic_vector (25 downto 0));
end entity;

architecture rtl of \ZLL_Main_incr36\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
begin
zi0 <= arg0(15 downto 8);
      zi1 <= arg0(7 downto 0);
      res <= (std_logic_vector'(B"0100000000") & zi0 & zi1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_incr32\ is
port (arg0 : in std_logic_vector (23 downto 0);
      res : out std_logic_vector (25 downto 0));
end entity;

architecture rtl of \ZLL_Main_incr32\ is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
begin
zi0 <= arg0(23 downto 16);
      zi1 <= arg0(15 downto 8);
      zi2 <= arg0(7 downto 0);
      res <= (std_logic_vector'(B"00") & zi0 & zi1 & zi2);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_sig\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (25 downto 0));
end entity;

architecture rtl of \Main_sig\ is
signal zi0 : std_logic_vector (23 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (25 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
begin
zi0 <= (arg0 & arg0 & arg1);
      zi1 <= zi0(23 downto 16);
      zi2 <= zi0(15 downto 8);
      zi3 <= zi0(7 downto 0);
      zi4 <= (std_logic_vector'(B"00") & zi1 & zi2 & zi3);
      zi5 <= zi4(23 downto 16);
      zi6 <= zi4(15 downto 8);
      zi7 <= zi4(7 downto 0);
      res <= (std_logic_vector'(B"10") & zi5 & zi6 & zi7);
end architecture;