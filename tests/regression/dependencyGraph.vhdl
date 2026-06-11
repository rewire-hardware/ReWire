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
      \__in0\ : in std_logic_vector (9 downto 0);
      \__out0\ : out std_logic_vector (15 downto 0));
end entity;

architecture rtl of top_level is
signal \__st0\ : std_logic_vector (29 downto 0) := std_logic_vector'(B"000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (29 downto 0);
      signal zin : std_logic_vector (39 downto 0);
      signal zi0 : std_logic_vector (29 downto 0);
      signal zi1 : std_logic_vector (9 downto 0);
      signal zi2 : std_logic_vector (39 downto 0);
      signal zi3 : std_logic_vector (9 downto 0);
      signal zi4 : std_logic_vector (29 downto 0);
      signal zi5 : std_logic_vector (59 downto 0);
      signal zi6 : std_logic_vector (69 downto 0);
      signal zi7 : std_logic_vector (9 downto 0);
      signal zi8 : std_logic_vector (29 downto 0);
      signal zi9 : std_logic_vector (29 downto 0);
      signal zi10 : std_logic_vector (39 downto 0);
      signal zi11 : std_logic_vector (9 downto 0);
      signal zi12 : std_logic_vector (29 downto 0);
      signal zi13 : std_logic_vector (14 downto 0);
      signal zi14 : std_logic_vector (24 downto 0);
      signal zi15 : std_logic_vector (3 downto 0);
      signal zi16 : std_logic_vector (0 downto 0);
      signal zi17 : std_logic_vector (4 downto 0);
      signal zi18 : std_logic_vector (8 downto 0);
      signal zi19 : std_logic_vector (24 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (0 downto 0);
      signal zi22 : std_logic_vector (4 downto 0);
      signal zi23 : std_logic_vector (8 downto 0);
      signal zi24 : std_logic_vector (24 downto 0);
      signal zi25 : std_logic_vector (0 downto 0);
      signal zi26 : std_logic_vector (3 downto 0);
      signal zi27 : std_logic_vector (8 downto 0);
      signal zi28 : std_logic_vector (24 downto 0);
      signal zi29 : std_logic_vector (0 downto 0);
      signal zi30 : std_logic_vector (8 downto 0);
      signal zi31 : std_logic_vector (24 downto 0);
      signal zi32 : std_logic_vector (0 downto 0);
      signal zi33 : std_logic_vector (4 downto 0);
      signal zi34 : std_logic_vector (8 downto 0);
      signal zi35 : std_logic_vector (44 downto 0);
      signal zi36 : std_logic_vector (14 downto 0);
      signal zi37 : std_logic_vector (29 downto 0);
      signal zi38 : std_logic_vector (14 downto 0);
      signal zi39 : std_logic_vector (29 downto 0);
      signal zi40 : std_logic_vector (14 downto 0);
      signal zi41 : std_logic_vector (14 downto 0);
      signal zi42 : std_logic_vector (44 downto 0);
      signal zi43 : std_logic_vector (29 downto 0);
      signal zi44 : std_logic_vector (14 downto 0);
      signal zi45 : std_logic_vector (4 downto 0);
      signal zi46 : std_logic_vector (3 downto 0);
      signal zi47 : std_logic_vector (4 downto 0);
      signal zi48 : std_logic_vector (7 downto 0);
      signal zi49 : std_logic_vector (4 downto 0);
      signal zi50 : std_logic_vector (8 downto 0);
      signal zi51 : std_logic_vector (75 downto 0);
      signal zi52 : std_logic_vector (45 downto 0);
      signal zi53 : std_logic_vector (29 downto 0);
      signal zi54 : std_logic_vector (77 downto 0);
      signal zi55 : std_logic_vector (45 downto 0);
      signal zi56 : std_logic_vector (29 downto 0);
      signal zi57 : std_logic_vector (75 downto 0);
      signal zi58 : std_logic_vector (29 downto 0);
      signal zi59 : std_logic_vector (15 downto 0);
      signal zi60 : std_logic_vector (29 downto 0);
      signal zi61 : std_logic_vector (77 downto 0);
      signal zi62 : std_logic_vector (93 downto 0);
      signal zi63 : std_logic_vector (15 downto 0);
      signal zi64 : std_logic_vector (29 downto 0);
      signal zres : std_logic_vector (77 downto 0);
begin
zin <= (\__st0\ & \__in0\);
      zi0 <= zin(39 downto 10);
      zi1 <= zin(9 downto 0);
      zi2 <= (zi1 & zi0);
      zi3 <= zi2(39 downto 30);
      zi4 <= zi2(29 downto 0);
      zi5 <= (zi4 & zi4);
      zi6 <= (zi3 & zi5);
      zi7 <= zi6(69 downto 60);
      zi8 <= zi6(59 downto 30);
      zi9 <= zi6(29 downto 0);
      zi10 <= (zi7 & zi8);
      zi11 <= zi10(39 downto 30);
      zi12 <= zi10(29 downto 0);
      zi13 <= zi12(29 downto 15);
      zi14 <= (zi11 & zi13);
      zi15 <= zi14(18 downto 15);
      zi16 <= zi14(14 downto 14);
      zi17 <= zi14(13 downto 9);
      zi18 <= zi14(8 downto 0);
      zi19 <= (zi11 & zi13);
      zi20 <= zi19(22 downto 15);
      zi21 <= zi19(14 downto 14);
      zi22 <= zi19(13 downto 9);
      zi23 <= zi19(8 downto 0);
      zi24 <= (zi11 & zi13);
      zi25 <= zi24(14 downto 14);
      zi26 <= zi24(12 downto 9);
      zi27 <= zi24(8 downto 0);
      zi28 <= (zi11 & zi13);
      zi29 <= zi28(14 downto 14);
      zi30 <= zi28(8 downto 0);
      zi31 <= (zi11 & zi13);
      zi32 <= zi31(14 downto 14);
      zi33 <= zi31(13 downto 9);
      zi34 <= zi31(8 downto 0);
      zi35 <= (rw_cond(rw_eq(zi14(24 downto 23), std_logic_vector'(B"00")), (zi16 & std_logic_vector'(B"1") & zi15 & std_logic_vector'(B"00000") & zi15), rw_cond(rw_eq(zi19(24 downto 23), std_logic_vector'(B"01")), (zi21 & zi22 & std_logic_vector'(B"1") & zi20), rw_cond(rw_and(rw_eq(zi24(24 downto 23), std_logic_vector'(B"10")), rw_and(rw_eq(zi24(15 downto 15), std_logic_vector'(B"1")), rw_eq(zi24(13 downto 13), std_logic_vector'(B"1")))), (std_logic_vector'(B"10000000000") & zi26), rw_cond(rw_and(rw_eq(zi28(24 downto 23), std_logic_vector'(B"10")), rw_and(rw_eq(zi28(15 downto 15), std_logic_vector'(B"1")), rw_eq(zi28(13 downto 13), std_logic_vector'(B"0")))), (std_logic_vector'(B"100000") & zi30), (std_logic_vector'(B"000000") & zi34))))) & zi12);
      zi36 <= zi35(44 downto 30);
      zi37 <= zi35(29 downto 0);
      zi38 <= zi37(29 downto 15);
      zi39 <= (zi36 & zi38);
      zi40 <= zi39(29 downto 15);
      zi41 <= zi39(14 downto 0);
      zi42 <= ((zi40 & zi41) & rw_resize(rw_shiftr(zi37, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010"), rw_resize(std_logic_vector'(B"1"), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111"))), 15));
      zi43 <= zi42(44 downto 15);
      zi44 <= zi42(14 downto 0);
      zi45 <= zi44(13 downto 9);
      zi46 <= zi44(3 downto 0);
      zi47 <= zi44(13 downto 9);
      zi48 <= zi44(7 downto 0);
      zi49 <= zi44(13 downto 9);
      zi50 <= zi44(8 downto 0);
      zi51 <= ((rw_cond(rw_and(rw_eq(zi44(14 downto 14), std_logic_vector'(B"1")), rw_eq(zi44(8 downto 8), std_logic_vector'(B"0"))), (std_logic_vector'(B"11") & zi45 & std_logic_vector'(B"00000") & zi46), rw_cond(rw_and(rw_eq(zi44(14 downto 14), std_logic_vector'(B"1")), rw_eq(zi44(8 downto 8), std_logic_vector'(B"1"))), (std_logic_vector'(B"01") & zi47 & std_logic_vector'(B"1") & zi48), (std_logic_vector'(B"0000000") & zi50))) & zi43) & zi9);
      zi52 <= zi51(75 downto 30);
      zi53 <= zi51(29 downto 0);
      zi54 <= (std_logic_vector'(B"00") & zi52 & zi53);
      zi55 <= zi54(75 downto 30);
      zi56 <= zi54(29 downto 0);
      zi57 <= (zi56 & zi55);
      zi58 <= zi57(75 downto 46);
      zi59 <= zi57(45 downto 30);
      zi60 <= zi57(29 downto 0);
      zi61 <= ((std_logic_vector'(B"01") & rw_repl(46, std_logic_vector'(B"0"))) & zi60);
      zi62 <= (zi59 & zi61);
      zi63 <= zi62(93 downto 78);
      zi64 <= zi62(29 downto 0);
      zres <= ((std_logic_vector'(B"1") & rw_repl(31, std_logic_vector'(B"0"))) & zi63 & zi64);
      \__st0_next\ <= zres(29 downto 0);
      \__out0\ <= zres(45 downto 30);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;