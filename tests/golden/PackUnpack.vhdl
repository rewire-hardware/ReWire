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
  function rw_not (a : std_logic_vector) return std_logic_vector;
  function rw_shiftl (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_shiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_ashiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_rand (a : std_logic_vector) return std_logic_vector;
  function rw_ror (a : std_logic_vector) return std_logic_vector;
  function rw_rxor (a : std_logic_vector) return std_logic_vector;
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
  function rw_rand (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((and a) = '1');
  end;
  function rw_ror (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((or a) = '1');
  end;
  function rw_rxor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((xor a) = '1');
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
port (\__in0\ : in std_logic_vector (7 downto 0);
      \__out0\ : out std_logic_vector (7 downto 0);
      \__out1\ : out std_logic_vector (7 downto 0);
      \__out2\ : out std_logic_vector (7 downto 0);
      \__out3\ : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of top_level is
signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal zi10 : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal zi12 : std_logic_vector (0 downto 0);
      signal zi13 : std_logic_vector (0 downto 0);
      signal zi14 : std_logic_vector (0 downto 0);
      signal zi15 : std_logic_vector (0 downto 0);
      signal zi16 : std_logic_vector (0 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (0 downto 0);
      signal zi19 : std_logic_vector (0 downto 0);
      signal zi20 : std_logic_vector (0 downto 0);
      signal zi21 : std_logic_vector (0 downto 0);
      signal zi22 : std_logic_vector (0 downto 0);
      signal zi23 : std_logic_vector (0 downto 0);
      signal zi24 : std_logic_vector (0 downto 0);
      signal zi25 : std_logic_vector (0 downto 0);
      signal zi26 : std_logic_vector (0 downto 0);
      signal zi27 : std_logic_vector (0 downto 0);
      signal zi28 : std_logic_vector (0 downto 0);
      signal zi29 : std_logic_vector (0 downto 0);
      signal zi30 : std_logic_vector (0 downto 0);
      signal zi31 : std_logic_vector (0 downto 0);
      signal zi32 : std_logic_vector (0 downto 0);
      signal zi33 : std_logic_vector (0 downto 0);
      signal zi34 : std_logic_vector (7 downto 0);
      signal zi35 : std_logic_vector (0 downto 0);
      signal zi36 : std_logic_vector (0 downto 0);
      signal zi37 : std_logic_vector (0 downto 0);
      signal zi38 : std_logic_vector (0 downto 0);
      signal zi39 : std_logic_vector (0 downto 0);
      signal zi40 : std_logic_vector (0 downto 0);
      signal zi41 : std_logic_vector (0 downto 0);
      signal zi42 : std_logic_vector (0 downto 0);
      signal zi43 : std_logic_vector (0 downto 0);
      signal zi44 : std_logic_vector (0 downto 0);
      signal zi45 : std_logic_vector (0 downto 0);
      signal zi46 : std_logic_vector (0 downto 0);
      signal zi47 : std_logic_vector (0 downto 0);
      signal zi48 : std_logic_vector (0 downto 0);
      signal zi49 : std_logic_vector (0 downto 0);
      signal zi50 : std_logic_vector (0 downto 0);
      signal zi51 : std_logic_vector (7 downto 0);
      signal zi52 : std_logic_vector (0 downto 0);
      signal zi53 : std_logic_vector (0 downto 0);
      signal zi54 : std_logic_vector (0 downto 0);
      signal zi55 : std_logic_vector (0 downto 0);
      signal zi56 : std_logic_vector (0 downto 0);
      signal zi57 : std_logic_vector (0 downto 0);
      signal zi58 : std_logic_vector (0 downto 0);
      signal zi59 : std_logic_vector (0 downto 0);
      signal zi60 : std_logic_vector (0 downto 0);
      signal zi61 : std_logic_vector (0 downto 0);
      signal zi62 : std_logic_vector (0 downto 0);
      signal zi63 : std_logic_vector (0 downto 0);
      signal zi64 : std_logic_vector (0 downto 0);
      signal zi65 : std_logic_vector (0 downto 0);
      signal zi66 : std_logic_vector (0 downto 0);
      signal zi67 : std_logic_vector (0 downto 0);
      signal zi68 : std_logic_vector (7 downto 0);
      signal zi69 : std_logic_vector (31 downto 0);
      signal zres : std_logic_vector (31 downto 0);
begin
zi0 <= rw_mul(\__in0\, std_logic_vector'(B"00000010"));
      zi1 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111")), 1);
      zi2 <= zi1;
      zi3 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111")), 1);
      zi4 <= zi3;
      zi5 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110")), 1);
      zi6 <= zi5;
      zi7 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110")), 1);
      zi8 <= zi7;
      zi9 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101")), 1);
      zi10 <= zi9;
      zi11 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101")), 1);
      zi12 <= zi11;
      zi13 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100")), 1);
      zi14 <= zi13;
      zi15 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100")), 1);
      zi16 <= zi15;
      zi17 <= (zi2 & zi4 & zi6 & zi8 & zi10 & zi12 & zi14 & zi16);
      zi18 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011")), 1);
      zi19 <= zi18;
      zi20 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011")), 1);
      zi21 <= zi20;
      zi22 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010")), 1);
      zi23 <= zi22;
      zi24 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010")), 1);
      zi25 <= zi24;
      zi26 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), 1);
      zi27 <= zi26;
      zi28 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), 1);
      zi29 <= zi28;
      zi30 <= rw_resize(rw_shiftr(\__in0\, rw_repl(128, std_logic_vector'(B"0"))), 1);
      zi31 <= zi30;
      zi32 <= rw_resize(rw_shiftr(zi0, rw_repl(128, std_logic_vector'(B"0"))), 1);
      zi33 <= zi32;
      zi34 <= (zi19 & zi21 & zi23 & zi25 & zi27 & zi29 & zi31 & zi33);
      zi35 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111")), 1);
      zi36 <= zi35;
      zi37 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101")), 1);
      zi38 <= zi37;
      zi39 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011")), 1);
      zi40 <= zi39;
      zi41 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), 1);
      zi42 <= zi41;
      zi43 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111")), 1);
      zi44 <= zi43;
      zi45 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101")), 1);
      zi46 <= zi45;
      zi47 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011")), 1);
      zi48 <= zi47;
      zi49 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), 1);
      zi50 <= zi49;
      zi51 <= (zi36 & zi38 & zi40 & zi42 & zi44 & zi46 & zi48 & zi50);
      zi52 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110")), 1);
      zi53 <= zi52;
      zi54 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100")), 1);
      zi55 <= zi54;
      zi56 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010")), 1);
      zi57 <= zi56;
      zi58 <= rw_resize(rw_shiftr(\__in0\, rw_repl(128, std_logic_vector'(B"0"))), 1);
      zi59 <= zi58;
      zi60 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110")), 1);
      zi61 <= zi60;
      zi62 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100")), 1);
      zi63 <= zi62;
      zi64 <= rw_resize(rw_shiftr(zi0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010")), 1);
      zi65 <= zi64;
      zi66 <= rw_resize(rw_shiftr(zi0, rw_repl(128, std_logic_vector'(B"0"))), 1);
      zi67 <= zi66;
      zi68 <= (zi53 & zi55 & zi57 & zi59 & zi61 & zi63 & zi65 & zi67);
      zi69 <= (zi17 & zi34 & zi51 & zi68);
      zres <= zi69;
      \__out0\ <= zres(31 downto 24);
      \__out1\ <= zres(23 downto 16);
      \__out2\ <= zres(15 downto 8);
      \__out3\ <= zres(7 downto 0);
end architecture;