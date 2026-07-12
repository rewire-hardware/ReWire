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
      signal b : std_logic_vector (7 downto 0);
      signal za2 : std_logic_vector (0 downto 0);
      signal za2_r1 : std_logic_vector (0 downto 0);
      signal za2_r2 : std_logic_vector (0 downto 0);
      signal za2_r3 : std_logic_vector (0 downto 0);
      signal za2_r4 : std_logic_vector (0 downto 0);
      signal za2_r5 : std_logic_vector (0 downto 0);
      signal za2_r6 : std_logic_vector (0 downto 0);
      signal za2_r7 : std_logic_vector (0 downto 0);
      signal za : std_logic_vector (7 downto 0);
      signal za5 : std_logic_vector (0 downto 0);
      signal za5_r1 : std_logic_vector (0 downto 0);
      signal za5_r2 : std_logic_vector (0 downto 0);
      signal za5_r3 : std_logic_vector (0 downto 0);
      signal za5_r4 : std_logic_vector (0 downto 0);
      signal za5_r5 : std_logic_vector (0 downto 0);
      signal za5_r6 : std_logic_vector (0 downto 0);
      signal za5_r7 : std_logic_vector (0 downto 0);
      signal za3 : std_logic_vector (7 downto 0);
      signal za6 : std_logic_vector (7 downto 0);
      signal za9 : std_logic_vector (7 downto 0);
      signal za10 : std_logic_vector (31 downto 0);
begin
      -- combinational logic
      b <= rw_mul(\__in0\, std_logic_vector'(X"02"));
      za2 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(X"00000000000000000000000000000007")), 1);
      za2_r1 <= rw_resize(rw_shiftr(b, std_logic_vector'(X"00000000000000000000000000000007")), 1);
      za2_r2 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(X"00000000000000000000000000000006")), 1);
      za2_r3 <= rw_resize(rw_shiftr(b, std_logic_vector'(X"00000000000000000000000000000006")), 1);
      za2_r4 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(X"00000000000000000000000000000005")), 1);
      za2_r5 <= rw_resize(rw_shiftr(b, std_logic_vector'(X"00000000000000000000000000000005")), 1);
      za2_r6 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(X"00000000000000000000000000000004")), 1);
      za2_r7 <= rw_resize(rw_shiftr(b, std_logic_vector'(X"00000000000000000000000000000004")), 1);
      za <= (za2 & za2_r1 & za2_r2 & za2_r3 & za2_r4 & za2_r5 & za2_r6 & za2_r7);
      za5 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(X"00000000000000000000000000000003")), 1);
      za5_r1 <= rw_resize(rw_shiftr(b, std_logic_vector'(X"00000000000000000000000000000003")), 1);
      za5_r2 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(X"00000000000000000000000000000002")), 1);
      za5_r3 <= rw_resize(rw_shiftr(b, std_logic_vector'(X"00000000000000000000000000000002")), 1);
      za5_r4 <= rw_resize(rw_shiftr(\__in0\, std_logic_vector'(X"00000000000000000000000000000001")), 1);
      za5_r5 <= rw_resize(rw_shiftr(b, std_logic_vector'(X"00000000000000000000000000000001")), 1);
      za5_r6 <= rw_resize(rw_shiftr(\__in0\, rw_repl(128, std_logic_vector'(B"0"))), 1);
      za5_r7 <= rw_resize(rw_shiftr(b, rw_repl(128, std_logic_vector'(B"0"))), 1);
      za3 <= (za5 & za5_r1 & za5_r2 & za5_r3 & za5_r4 & za5_r5 & za5_r6 & za5_r7);
      za6 <= (za2 & za2_r4 & za5 & za5_r4 & za2_r1 & za2_r5 & za5_r1 & za5_r5);
      za9 <= (za2_r2 & za2_r6 & za5_r2 & za5_r6 & za2_r3 & za2_r7 & za5_r3 & za5_r7);
      za10 <= (za & za3 & za6 & za9);
      -- outputs
      \__out0\ <= za10(31 downto 24);
      \__out1\ <= za10(23 downto 16);
      \__out2\ <= za10(15 downto 8);
      \__out3\ <= za10(7 downto 0);
end architecture;