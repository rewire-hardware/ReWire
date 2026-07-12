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
signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal zi15 : std_logic_vector (7 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (31 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zres : std_logic_vector (7 downto 0);
begin
zi2 <= rw_add(rw_mul(\__in0\, std_logic_vector'(B"00000011")), std_logic_vector'(B"00000001"));
      zi3 <= rw_add(rw_mul(zi2, std_logic_vector'(B"00000011")), std_logic_vector'(B"00000001"));
      zi4 <= rw_add(rw_mul(zi3, std_logic_vector'(B"00000011")), std_logic_vector'(B"00000001"));
      zi5 <= rw_add(rw_mul(zi4, std_logic_vector'(B"00000011")), std_logic_vector'(B"00000001"));
      zi6 <= rw_xor(rw_shiftl(\__in1\, std_logic_vector'(B"1")), rw_cond(\__in1\(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi7 <= rw_xor(rw_shiftl(\__in1\, std_logic_vector'(B"1")), rw_cond(\__in1\(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi8 <= rw_xor(rw_shiftl(zi7, std_logic_vector'(B"1")), rw_cond(zi7(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi9 <= rw_xor(rw_shiftl(\__in1\, std_logic_vector'(B"1")), rw_cond(\__in1\(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi10 <= rw_xor(rw_shiftl(zi9, std_logic_vector'(B"1")), rw_cond(zi9(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi11 <= rw_xor(rw_shiftl(zi10, std_logic_vector'(B"1")), rw_cond(zi10(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi12 <= rw_xor(rw_shiftl(\__in1\, std_logic_vector'(B"1")), rw_cond(\__in1\(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi13 <= rw_xor(rw_shiftl(zi12, std_logic_vector'(B"1")), rw_cond(zi12(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi14 <= rw_xor(rw_shiftl(zi13, std_logic_vector'(B"1")), rw_cond(zi13(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi15 <= rw_xor(rw_shiftl(zi14, std_logic_vector'(B"1")), rw_cond(zi14(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi16 <= rw_xor(rw_shiftl(\__in1\, std_logic_vector'(B"1")), rw_cond(\__in1\(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi17 <= rw_xor(rw_shiftl(zi16, std_logic_vector'(B"1")), rw_cond(zi16(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi18 <= rw_xor(rw_shiftl(zi17, std_logic_vector'(B"1")), rw_cond(zi17(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi19 <= rw_xor(rw_shiftl(zi18, std_logic_vector'(B"1")), rw_cond(zi18(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi20 <= rw_xor(rw_shiftl(zi19, std_logic_vector'(B"1")), rw_cond(zi19(7 downto 7), std_logic_vector'(B"00011101"), std_logic_vector'(B"00000000")));
      zi21 <= rw_resize(\__in0\, 32);
      zi22 <= rw_add(rw_add(rw_add(rw_add(rw_add(rw_resize((\__in0\ & rw_add(\__in0\, std_logic_vector'(B"00000001")) & rw_add(\__in0\, std_logic_vector'(B"00000010")) & rw_add(\__in0\, std_logic_vector'(B"00000011")) & rw_add(\__in0\, std_logic_vector'(B"00000100"))), 8), zi5), rw_resize((rw_add(\__in1\, rw_mul(std_logic_vector'(B"00000010"), rw_sub(rw_add(\__in1\, std_logic_vector'(B"00000010")), \__in1\))) & rw_add(\__in1\, rw_mul(std_logic_vector'(B"00000011"), rw_sub(rw_add(\__in1\, std_logic_vector'(B"00000010")), \__in1\))) & rw_add(\__in1\, rw_mul(std_logic_vector'(B"00000100"), rw_sub(rw_add(\__in1\, std_logic_vector'(B"00000010")), \__in1\)))), 8)), rw_resize((\__in1\ & zi6 & zi8 & zi11 & zi15 & zi20), 8)), rw_resize((rw_add(zi21(31 downto 24), std_logic_vector'(B"00000000")) & rw_add(zi21(23 downto 16), std_logic_vector'(B"00000001")) & rw_add(zi21(15 downto 8), std_logic_vector'(B"00000010")) & rw_add(zi21(7 downto 0), std_logic_vector'(B"00000011"))), 8)), rw_resize((\__in1\ & \__in1\ & \__in1\ & \__in1\), 8));
      zres <= zi22;
      \__out0\ <= zres;
end architecture;