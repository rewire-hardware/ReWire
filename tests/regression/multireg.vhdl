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
      \__in0\ : in std_logic_vector (3 downto 0);
      \__out0\ : out std_logic_vector (15 downto 0));
end entity;

architecture rtl of top_level is
signal \__st0\ : std_logic_vector (3 downto 0) := std_logic_vector'(B"0000");
      signal \__st0_next\ : std_logic_vector (3 downto 0);
      signal \__st1\ : std_logic_vector (7 downto 0) := std_logic_vector'(B"00000011");
      signal \__st1_next\ : std_logic_vector (7 downto 0);
      signal \__st2\ : std_logic_vector (15 downto 0) := std_logic_vector'(B"0000000000000101");
      signal \__st2_next\ : std_logic_vector (15 downto 0);
      signal zin : std_logic_vector (31 downto 0);
      signal zi0 : std_logic_vector (27 downto 0);
      signal zi1 : std_logic_vector (3 downto 0);
      signal zi2 : std_logic_vector (31 downto 0);
      signal zi3 : std_logic_vector (3 downto 0);
      signal zi4 : std_logic_vector (3 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (15 downto 0);
      signal zi7 : std_logic_vector (31 downto 0);
      signal zi8 : std_logic_vector (3 downto 0);
      signal zi9 : std_logic_vector (3 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (15 downto 0);
      signal zi12 : std_logic_vector (46 downto 0);
      signal zi13 : std_logic_vector (50 downto 0);
      signal zi14 : std_logic_vector (3 downto 0);
      signal zi15 : std_logic_vector (3 downto 0);
      signal zi16 : std_logic_vector (3 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (15 downto 0);
      signal zi19 : std_logic_vector (35 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (3 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (15 downto 0);
      signal zi24 : std_logic_vector (46 downto 0);
      signal zi25 : std_logic_vector (54 downto 0);
      signal zi26 : std_logic_vector (3 downto 0);
      signal zi27 : std_logic_vector (3 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal zi29 : std_logic_vector (3 downto 0);
      signal zi30 : std_logic_vector (7 downto 0);
      signal zi31 : std_logic_vector (15 downto 0);
      signal zi32 : std_logic_vector (43 downto 0);
      signal zi33 : std_logic_vector (15 downto 0);
      signal zi34 : std_logic_vector (3 downto 0);
      signal zi35 : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (15 downto 0);
      signal zi37 : std_logic_vector (46 downto 0);
      signal zi38 : std_logic_vector (62 downto 0);
      signal zi39 : std_logic_vector (7 downto 0);
      signal zi40 : std_logic_vector (3 downto 0);
      signal zi41 : std_logic_vector (3 downto 0);
      signal zi42 : std_logic_vector (15 downto 0);
      signal zi43 : std_logic_vector (3 downto 0);
      signal zi44 : std_logic_vector (7 downto 0);
      signal zi45 : std_logic_vector (15 downto 0);
      signal zi46 : std_logic_vector (27 downto 0);
      signal zi47 : std_logic_vector (3 downto 0);
      signal zi48 : std_logic_vector (7 downto 0);
      signal zi49 : std_logic_vector (15 downto 0);
      signal zi50 : std_logic_vector (46 downto 0);
      signal zi51 : std_logic_vector (74 downto 0);
      signal zi52 : std_logic_vector (15 downto 0);
      signal zi53 : std_logic_vector (7 downto 0);
      signal zi54 : std_logic_vector (3 downto 0);
      signal zi55 : std_logic_vector (3 downto 0);
      signal zi56 : std_logic_vector (7 downto 0);
      signal zi57 : std_logic_vector (15 downto 0);
      signal zi58 : std_logic_vector (27 downto 0);
      signal zi59 : std_logic_vector (3 downto 0);
      signal zi60 : std_logic_vector (7 downto 0);
      signal zi61 : std_logic_vector (15 downto 0);
      signal zi62 : std_logic_vector (46 downto 0);
      signal zi63 : std_logic_vector (74 downto 0);
      signal zi64 : std_logic_vector (15 downto 0);
      signal zi65 : std_logic_vector (3 downto 0);
      signal zi66 : std_logic_vector (7 downto 0);
      signal zi67 : std_logic_vector (3 downto 0);
      signal zi68 : std_logic_vector (7 downto 0);
      signal zi69 : std_logic_vector (15 downto 0);
      signal zi70 : std_logic_vector (27 downto 0);
      signal zi71 : std_logic_vector (3 downto 0);
      signal zi72 : std_logic_vector (7 downto 0);
      signal zi73 : std_logic_vector (15 downto 0);
      signal zi74 : std_logic_vector (46 downto 0);
      signal zi75 : std_logic_vector (66 downto 0);
      signal zi76 : std_logic_vector (3 downto 0);
      signal zi77 : std_logic_vector (15 downto 0);
      signal zi78 : std_logic_vector (3 downto 0);
      signal zi79 : std_logic_vector (7 downto 0);
      signal zi80 : std_logic_vector (15 downto 0);
      signal zres : std_logic_vector (46 downto 0);
begin
zin <= (\__st0\ & \__st1\ & \__st2\ & \__in0\);
      zi0 <= zin(31 downto 4);
      zi1 <= zin(3 downto 0);
      zi2 <= (zi1 & zi0);
      zi3 <= zi2(31 downto 28);
      zi4 <= zi2(27 downto 24);
      zi5 <= zi2(23 downto 16);
      zi6 <= zi2(15 downto 0);
      zi7 <= (zi4 & zi4 & zi5 & zi6);
      zi8 <= zi7(31 downto 28);
      zi9 <= zi7(27 downto 24);
      zi10 <= zi7(23 downto 16);
      zi11 <= zi7(15 downto 0);
      zi12 <= (std_logic_vector'(B"001000000000000") & zi8 & zi9 & zi10 & zi11);
      zi13 <= (zi3 & zi12);
      zi14 <= zi13(50 downto 47);
      zi15 <= zi13(31 downto 28);
      zi16 <= zi13(27 downto 24);
      zi17 <= zi13(23 downto 16);
      zi18 <= zi13(15 downto 0);
      zi19 <= (zi17 & zi16 & zi17 & zi18);
      zi20 <= zi19(35 downto 28);
      zi21 <= zi19(27 downto 24);
      zi22 <= zi19(23 downto 16);
      zi23 <= zi19(15 downto 0);
      zi24 <= (std_logic_vector'(B"01000000000") & zi20 & zi21 & zi22 & zi23);
      zi25 <= (zi15 & zi14 & zi24);
      zi26 <= zi25(54 downto 51);
      zi27 <= zi25(50 downto 47);
      zi28 <= zi25(35 downto 28);
      zi29 <= zi25(27 downto 24);
      zi30 <= zi25(23 downto 16);
      zi31 <= zi25(15 downto 0);
      zi32 <= (zi31 & zi29 & zi30 & zi31);
      zi33 <= zi32(43 downto 28);
      zi34 <= zi32(27 downto 24);
      zi35 <= zi32(23 downto 16);
      zi36 <= zi32(15 downto 0);
      zi37 <= (std_logic_vector'(B"000") & zi33 & zi34 & zi35 & zi36);
      zi38 <= (zi28 & zi27 & zi26 & zi37);
      zi39 <= zi38(62 downto 55);
      zi40 <= zi38(54 downto 51);
      zi41 <= zi38(50 downto 47);
      zi42 <= zi38(43 downto 28);
      zi43 <= zi38(27 downto 24);
      zi44 <= zi38(23 downto 16);
      zi45 <= zi38(15 downto 0);
      zi46 <= (zi40 & zi44 & zi45);
      zi47 <= zi46(27 downto 24);
      zi48 <= zi46(23 downto 16);
      zi49 <= zi46(15 downto 0);
      zi50 <= (std_logic_vector'(B"0110000000000000000") & zi47 & zi48 & zi49);
      zi51 <= (zi42 & zi39 & zi41 & zi50);
      zi52 <= zi51(74 downto 59);
      zi53 <= zi51(58 downto 51);
      zi54 <= zi51(50 downto 47);
      zi55 <= zi51(27 downto 24);
      zi56 <= zi51(23 downto 16);
      zi57 <= zi51(15 downto 0);
      zi58 <= (zi55 & rw_add(rw_resize(zi54, 8), zi53) & zi57);
      zi59 <= zi58(27 downto 24);
      zi60 <= zi58(23 downto 16);
      zi61 <= zi58(15 downto 0);
      zi62 <= (std_logic_vector'(B"0110000000000000000") & zi59 & zi60 & zi61);
      zi63 <= (zi52 & zi54 & zi53 & zi62);
      zi64 <= zi63(74 downto 59);
      zi65 <= zi63(58 downto 55);
      zi66 <= zi63(54 downto 47);
      zi67 <= zi63(27 downto 24);
      zi68 <= zi63(23 downto 16);
      zi69 <= zi63(15 downto 0);
      zi70 <= (zi67 & zi68 & rw_add(rw_resize(zi66, 16), zi64));
      zi71 <= zi70(27 downto 24);
      zi72 <= zi70(23 downto 16);
      zi73 <= zi70(15 downto 0);
      zi74 <= (std_logic_vector'(B"0110000000000000000") & zi71 & zi72 & zi73);
      zi75 <= (zi65 & zi64 & zi74);
      zi76 <= zi75(66 downto 63);
      zi77 <= zi75(62 downto 47);
      zi78 <= zi75(27 downto 24);
      zi79 <= zi75(23 downto 16);
      zi80 <= zi75(15 downto 0);
      zres <= (std_logic_vector'(B"100") & rw_add(rw_resize(zi76, 16), zi77) & zi78 & zi79 & zi80);
      \__st0_next\ <= zres(27 downto 24);
      \__st1_next\ <= zres(23 downto 16);
      \__st2_next\ <= zres(15 downto 0);
      \__out0\ <= zres(43 downto 28);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"0000");
                  \__st1\ <= std_logic_vector'(B"00000011");
                  \__st2\ <= std_logic_vector'(B"0000000000000101");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
                  \__st2\ <= \__st2_next\;
            end if;
      end process;
end architecture;