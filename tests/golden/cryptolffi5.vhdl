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
      \__in1\ : in std_logic_vector (7 downto 0);
      \__out0\ : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of top_level is
signal zi2 : std_logic_vector (63 downto 0);
      signal zi3 : std_logic_vector (15 downto 0);
      signal zi4 : std_logic_vector (15 downto 0);
      signal zi5 : std_logic_vector (15 downto 0);
      signal zi6 : std_logic_vector (15 downto 0);
      signal zi7 : std_logic_vector (15 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal zi15 : std_logic_vector (23 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (31 downto 0);
      signal zi22 : std_logic_vector (1 downto 0);
      signal zi23 : std_logic_vector (7 downto 0);
      signal zi24 : std_logic_vector (31 downto 0);
      signal zi25 : std_logic_vector (7 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (7 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal slice_in : std_logic_vector (39 downto 0);
      signal zi29 : std_logic_vector (7 downto 0);
      signal zi30 : std_logic_vector (7 downto 0);
      signal zi31 : std_logic_vector (7 downto 0);
      signal zi32 : std_logic_vector (7 downto 0);
      signal \slice_inR1\ : std_logic_vector (39 downto 0);
      signal zi33 : std_logic_vector (7 downto 0);
      signal zi34 : std_logic_vector (7 downto 0);
      signal zi35 : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (7 downto 0);
      signal \slice_inR2\ : std_logic_vector (39 downto 0);
      signal zi37 : std_logic_vector (7 downto 0);
      signal zi38 : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (7 downto 0);
      signal zi40 : std_logic_vector (7 downto 0);
      signal \slice_inR3\ : std_logic_vector (39 downto 0);
      signal zi41 : std_logic_vector (7 downto 0);
      signal zi42 : std_logic_vector (7 downto 0);
      signal zi43 : std_logic_vector (7 downto 0);
      signal zi44 : std_logic_vector (7 downto 0);
      signal \slice_inR4\ : std_logic_vector (39 downto 0);
      signal zi45 : std_logic_vector (7 downto 0);
      signal \slice_inR5\ : std_logic_vector (14 downto 0);
      signal \slice_inR6\ : std_logic_vector (14 downto 0);
      signal \slice_inR7\ : std_logic_vector (14 downto 0);
      signal \slice_inR8\ : std_logic_vector (14 downto 0);
      signal \slice_inR9\ : std_logic_vector (14 downto 0);
      signal \slice_inR10\ : std_logic_vector (14 downto 0);
      signal \slice_inR11\ : std_logic_vector (14 downto 0);
      signal \slice_inR12\ : std_logic_vector (14 downto 0);
      signal \slice_inR13\ : std_logic_vector (14 downto 0);
      signal \slice_inR14\ : std_logic_vector (14 downto 0);
      signal \slice_inR15\ : std_logic_vector (14 downto 0);
      signal \slice_inR16\ : std_logic_vector (14 downto 0);
      signal \slice_inR17\ : std_logic_vector (14 downto 0);
      signal \slice_inR18\ : std_logic_vector (14 downto 0);
      signal \slice_inR19\ : std_logic_vector (14 downto 0);
      signal \slice_inR20\ : std_logic_vector (14 downto 0);
      signal \slice_inR21\ : std_logic_vector (14 downto 0);
      signal \slice_inR22\ : std_logic_vector (14 downto 0);
      signal \slice_inR23\ : std_logic_vector (14 downto 0);
      signal \slice_inR24\ : std_logic_vector (14 downto 0);
      signal \slice_inR25\ : std_logic_vector (14 downto 0);
      signal \slice_inR26\ : std_logic_vector (14 downto 0);
      signal \slice_inR27\ : std_logic_vector (14 downto 0);
      signal \slice_inR28\ : std_logic_vector (14 downto 0);
      signal \slice_inR29\ : std_logic_vector (14 downto 0);
      signal \slice_inR30\ : std_logic_vector (14 downto 0);
      signal \slice_inR31\ : std_logic_vector (14 downto 0);
      signal \slice_inR32\ : std_logic_vector (14 downto 0);
      signal \slice_inR33\ : std_logic_vector (14 downto 0);
      signal \slice_inR34\ : std_logic_vector (14 downto 0);
      signal \slice_inR35\ : std_logic_vector (14 downto 0);
      signal \slice_inR36\ : std_logic_vector (14 downto 0);
      signal \slice_inR37\ : std_logic_vector (14 downto 0);
      signal \slice_inR38\ : std_logic_vector (14 downto 0);
      signal \slice_inR39\ : std_logic_vector (14 downto 0);
      signal \slice_inR40\ : std_logic_vector (14 downto 0);
      signal \slice_inR41\ : std_logic_vector (14 downto 0);
      signal \slice_inR42\ : std_logic_vector (14 downto 0);
      signal \slice_inR43\ : std_logic_vector (14 downto 0);
      signal \slice_inR44\ : std_logic_vector (14 downto 0);
      signal \slice_inR45\ : std_logic_vector (14 downto 0);
      signal \slice_inR46\ : std_logic_vector (14 downto 0);
      signal \slice_inR47\ : std_logic_vector (14 downto 0);
      signal \slice_inR48\ : std_logic_vector (14 downto 0);
      signal \slice_inR49\ : std_logic_vector (14 downto 0);
      signal \slice_inR50\ : std_logic_vector (14 downto 0);
      signal \slice_inR51\ : std_logic_vector (14 downto 0);
      signal \slice_inR52\ : std_logic_vector (14 downto 0);
      signal \slice_inR53\ : std_logic_vector (14 downto 0);
      signal \slice_inR54\ : std_logic_vector (14 downto 0);
      signal \slice_inR55\ : std_logic_vector (14 downto 0);
      signal zi46 : std_logic_vector (7 downto 0);
      signal zres : std_logic_vector (7 downto 0);
begin
zi2 <= (\__in0\ & \__in1\ & \__in1\ & \__in0\ & rw_add(\__in0\, std_logic_vector'(B"00000001")) & \__in1\ & \__in0\ & rw_add(\__in1\, std_logic_vector'(B"00000001")));
      zi3 <= zi2(15 downto 0);
      zi4 <= rw_xor((zi2(44 downto 32) & zi2(47 downto 45)), rw_add(rw_add((zi2(1 downto 0) & zi2(15 downto 2)), rw_shiftr(zi3, std_logic_vector'(B"1"))), std_logic_vector'(B"0000000000000101")));
      zi5 <= rw_xor((zi2(12 downto 0) & zi2(15 downto 13)), rw_add(rw_add((zi4(1 downto 0) & zi4(15 downto 2)), rw_shiftr(zi4, std_logic_vector'(B"1"))), std_logic_vector'(B"0000000000000111")));
      zi6 <= rw_xor((zi4(12 downto 0) & zi4(15 downto 13)), rw_add(rw_add((zi5(1 downto 0) & zi5(15 downto 2)), rw_shiftr(zi5, std_logic_vector'(B"1"))), std_logic_vector'(B"0000000000001001")));
      zi7 <= rw_xor((zi5(12 downto 0) & zi5(15 downto 13)), rw_add(rw_add((zi6(1 downto 0) & zi6(15 downto 2)), rw_shiftr(zi6, std_logic_vector'(B"1"))), std_logic_vector'(B"0000000000001011")));
      zi11 <= rw_xor(\__in0\, rw_add(\__in1\, std_logic_vector'(B"00000010")));
      zi12 <= rw_xor(\__in1\, rw_add(zi11, std_logic_vector'(B"00000011")));
      zi13 <= rw_xor(zi11, rw_add(zi12, std_logic_vector'(B"00000100")));
      zi14 <= rw_xor(zi12, rw_add(zi13, std_logic_vector'(B"00000101")));
      zi15 <= (\__in0\ & \__in1\ & rw_add(\__in0\, \__in1\));
      zi16 <= rw_xor(\__in0\, \__in1\);
      zi17 <= rw_add(rw_xor(zi15(23 downto 16), zi16), std_logic_vector'(B"00000001"));
      zi18 <= rw_add(rw_xor(zi15(15 downto 8), zi17), std_logic_vector'(B"00000001"));
      zi19 <= rw_add(rw_xor(zi15(7 downto 0), zi18), std_logic_vector'(B"00000001"));
      zi21 <= (\__in0\ & \__in1\ & rw_add(\__in0\, \__in1\) & rw_sub(\__in0\, \__in1\));
      zi22 <= \__in1\(1 downto 0);
      zi23 <= rw_xor(\__in0\, \__in1\);
      zi24 <= (\__in0\ & \__in1\ & rw_add(\__in0\, \__in1\) & rw_xor(\__in0\, \__in1\));
      zi25 <= rw_add(std_logic_vector'(B"00000001"), zi24(31 downto 24));
      zi26 <= rw_add(zi25, zi24(23 downto 16));
      zi27 <= rw_add(zi26, zi24(15 downto 8));
      zi28 <= rw_add(zi27, zi24(7 downto 0));
      slice_in <= (std_logic_vector'(B"00000001") & zi25 & zi26 & zi27 & zi28);
      zi29 <= rw_add(std_logic_vector'(B"00000001"), zi24(31 downto 24));
      zi30 <= rw_add(zi29, zi24(23 downto 16));
      zi31 <= rw_add(zi30, zi24(15 downto 8));
      zi32 <= rw_add(zi31, zi24(7 downto 0));
      \slice_inR1\ <= (std_logic_vector'(B"00000001") & zi29 & zi30 & zi31 & zi32);
      zi33 <= rw_add(std_logic_vector'(B"00000001"), zi24(31 downto 24));
      zi34 <= rw_add(zi33, zi24(23 downto 16));
      zi35 <= rw_add(zi34, zi24(15 downto 8));
      zi36 <= rw_add(zi35, zi24(7 downto 0));
      \slice_inR2\ <= (std_logic_vector'(B"00000001") & zi33 & zi34 & zi35 & zi36);
      zi37 <= rw_add(std_logic_vector'(B"00000001"), zi24(31 downto 24));
      zi38 <= rw_add(zi37, zi24(23 downto 16));
      zi39 <= rw_add(zi38, zi24(15 downto 8));
      zi40 <= rw_add(zi39, zi24(7 downto 0));
      \slice_inR3\ <= (std_logic_vector'(B"00000001") & zi37 & zi38 & zi39 & zi40);
      zi41 <= rw_add(std_logic_vector'(B"00000001"), zi24(31 downto 24));
      zi42 <= rw_add(zi41, zi24(23 downto 16));
      zi43 <= rw_add(zi42, zi24(15 downto 8));
      zi44 <= rw_add(zi43, zi24(7 downto 0));
      \slice_inR4\ <= (std_logic_vector'(B"00000001") & zi41 & zi42 & zi43 & zi44);
      zi45 <= rw_cond(rw_eq(\__in1\, std_logic_vector'(B"00000000")), std_logic_vector'(B"00000001"), \__in1\);
      \slice_inR5\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR6\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR7\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR8\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR9\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR10\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR11\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR12\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR13\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR14\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR15\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR16\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR17\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR18\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR19\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR20\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR21\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR22\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR23\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR24\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR25\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR26\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR27\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR28\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR29\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR30\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR31\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR32\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR33\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR34\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR35\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR36\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR37\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR38\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR39\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR40\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR41\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR42\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR43\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR44\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR45\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR46\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR47\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR48\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR49\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR50\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR51\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR52\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR53\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR54\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      \slice_inR55\ <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(B"0000") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(B"0000")), std_logic_vector'(B"000000000000000"))))));
      zi46 <= rw_xor(rw_add(rw_add(rw_add(rw_add(rw_add(rw_add(zi7(7 downto 0), zi14), zi19), rw_xor(rw_xor(rw_xor(rw_xor(std_logic_vector'(B"00000000"), rw_cond(rw_eq(zi22, std_logic_vector'(B"00")), zi23, zi21(31 downto 24))), rw_cond(rw_eq(zi22, std_logic_vector'(B"01")), zi23, zi21(23 downto 16))), rw_add(zi23, std_logic_vector'(B"00000001"))), rw_cond(rw_eq(zi22, std_logic_vector'(B"11")), zi23, zi21(7 downto 0)))), rw_xor(rw_xor(rw_xor(rw_xor(rw_xor(std_logic_vector'(B"00000000"), slice_in(39 downto 32)), \slice_inR1\(31 downto 24)), \slice_inR2\(23 downto 16)), \slice_inR3\(15 downto 8)), \slice_inR4\(7 downto 0))), rw_add(rw_add(rw_cond(rw_xor(rw_lts(\__in0\, std_logic_vector'(B"00000000")), rw_lts(zi45, std_logic_vector'(B"00000000"))), rw_sub(std_logic_vector'(B"00000000"), rw_div(rw_cond(rw_lts(\__in0\, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), \__in0\), \__in0\), rw_cond(rw_lts(zi45, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), zi45), zi45))), rw_div(rw_cond(rw_lts(\__in0\, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), \__in0\), \__in0\), rw_cond(rw_lts(zi45, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), zi45), zi45))), rw_cond(rw_lts(\__in0\, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), rw_mod(rw_cond(rw_lts(\__in0\, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), \__in0\), \__in0\), rw_cond(rw_lts(zi45, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), zi45), zi45))), rw_mod(rw_cond(rw_lts(\__in0\, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), \__in0\), \__in0\), rw_cond(rw_lts(zi45, std_logic_vector'(B"00000000")), rw_sub(std_logic_vector'(B"00000000"), zi45), zi45)))), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"00000001")), std_logic_vector'(B"00000000"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"00000010")), std_logic_vector'(B"00000001"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"00000100")), std_logic_vector'(B"00000010"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"00001000")), std_logic_vector'(B"00000011"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"00010000")), std_logic_vector'(B"00000100"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"00100000")), std_logic_vector'(B"00000101"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"01000000")), std_logic_vector'(B"00000110"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(B"10000000")), std_logic_vector'(B"00000111"), std_logic_vector'(B"00001000"))))))))))), rw_xor((rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR5\(7 downto 7), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR6\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR7\(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR8\(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))) & rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR9\(6 downto 6), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR10\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR11\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR12\(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR13\(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR14\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR15\(5 downto 5), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR16\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR17\(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR18\(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR19\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR20\(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR21\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR22\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR23\(4 downto 4), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR24\(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR25\(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR26\(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR27\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR28\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR29\(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR30\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR31\(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(rw_xor(\slice_inR32\(3 downto 3), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR33\(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR34\(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR35\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR36\(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR37\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR38\(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(\slice_inR39\(2 downto 2), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR40\(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR41\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR42\(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR43\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR44\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(\slice_inR45\(1 downto 1), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR46\(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR47\(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR48\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR49\(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR50\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR51\(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(\slice_inR52\(0 downto 0), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR53\(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR54\(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(\slice_inR55\(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))), (std_logic_vector'(B"00") & rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")) & rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))) & rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))) & rw_xor(rw_xor(\__in0\(3 downto 3), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))), rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))))) & rw_xor(rw_xor(\__in0\(2 downto 2), rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))))), rw_xor(rw_xor(\__in0\(3 downto 3), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))), rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))))))))), rw_add((\__in1\(7 downto 7) & \__in1\(3 downto 3) & \__in1\(6 downto 6) & \__in1\(2 downto 2) & \__in1\(5 downto 5) & \__in1\(1 downto 1) & \__in1\(4 downto 4) & \__in1\(0 downto 0)), std_logic_vector'(B"00111000")));
      zres <= zi46;
      \__out0\ <= zres;
end architecture;