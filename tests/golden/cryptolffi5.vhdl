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
      signal \m$0\ : std_logic_vector (63 downto 0);
      signal \ws$4734$3\ : std_logic_vector (15 downto 0);
      signal \ws$4734$5\ : std_logic_vector (15 downto 0);
      signal \ws$4734$7\ : std_logic_vector (15 downto 0);
      signal \ws$4734$9\ : std_logic_vector (15 downto 0);
      signal \ws$4734$11\ : std_logic_vector (15 downto 0);
      signal \ws$4733$2\ : std_logic_vector (7 downto 0);
      signal \ws$4733$3\ : std_logic_vector (7 downto 0);
      signal \ws$4733$4\ : std_logic_vector (7 downto 0);
      signal \ws$4733$5\ : std_logic_vector (7 downto 0);
      signal \pts$0\ : std_logic_vector (23 downto 0);
      signal \iv$1\ : std_logic_vector (7 downto 0);
      signal \cts$4735$1\ : std_logic_vector (7 downto 0);
      signal \cts$4735$2\ : std_logic_vector (7 downto 0);
      signal \cts$4735$3\ : std_logic_vector (7 downto 0);
      signal \xs$0\ : std_logic_vector (31 downto 0);
      signal \i$1\ : std_logic_vector (1 downto 0);
      signal \xs$0_r1\ : std_logic_vector (31 downto 0);
      signal \fold$0$1\ : std_logic_vector (7 downto 0);
      signal \fold$0$2\ : std_logic_vector (7 downto 0);
      signal \fold$0$3\ : std_logic_vector (7 downto 0);
      signal \fold$0$4\ : std_logic_vector (7 downto 0);
      signal slice_in : std_logic_vector (39 downto 0);
      signal slice_in_r1 : std_logic_vector (39 downto 0);
      signal slice_in_r2 : std_logic_vector (39 downto 0);
      signal slice_in_r3 : std_logic_vector (39 downto 0);
      signal slice_in_r4 : std_logic_vector (39 downto 0);
      signal \nz$4733\ : std_logic_vector (7 downto 0);
      signal slice_in_r5 : std_logic_vector (14 downto 0);
      signal slice_in_r6 : std_logic_vector (14 downto 0);
      signal slice_in_r7 : std_logic_vector (14 downto 0);
      signal slice_in_r8 : std_logic_vector (14 downto 0);
      signal slice_in_r9 : std_logic_vector (14 downto 0);
      signal slice_in_r10 : std_logic_vector (14 downto 0);
      signal slice_in_r11 : std_logic_vector (14 downto 0);
      signal slice_in_r12 : std_logic_vector (14 downto 0);
      signal slice_in_r13 : std_logic_vector (14 downto 0);
      signal slice_in_r14 : std_logic_vector (14 downto 0);
      signal slice_in_r15 : std_logic_vector (14 downto 0);
      signal slice_in_r16 : std_logic_vector (14 downto 0);
      signal slice_in_r17 : std_logic_vector (14 downto 0);
      signal slice_in_r18 : std_logic_vector (14 downto 0);
      signal slice_in_r19 : std_logic_vector (14 downto 0);
      signal slice_in_r20 : std_logic_vector (14 downto 0);
      signal slice_in_r21 : std_logic_vector (14 downto 0);
      signal slice_in_r22 : std_logic_vector (14 downto 0);
      signal slice_in_r23 : std_logic_vector (14 downto 0);
      signal slice_in_r24 : std_logic_vector (14 downto 0);
      signal slice_in_r25 : std_logic_vector (14 downto 0);
      signal slice_in_r26 : std_logic_vector (14 downto 0);
      signal slice_in_r27 : std_logic_vector (14 downto 0);
      signal slice_in_r28 : std_logic_vector (14 downto 0);
      signal slice_in_r29 : std_logic_vector (14 downto 0);
      signal slice_in_r30 : std_logic_vector (14 downto 0);
      signal slice_in_r31 : std_logic_vector (14 downto 0);
      signal slice_in_r32 : std_logic_vector (14 downto 0);
      signal slice_in_r33 : std_logic_vector (14 downto 0);
      signal slice_in_r34 : std_logic_vector (14 downto 0);
      signal slice_in_r35 : std_logic_vector (14 downto 0);
      signal slice_in_r36 : std_logic_vector (14 downto 0);
      signal slice_in_r37 : std_logic_vector (14 downto 0);
      signal slice_in_r38 : std_logic_vector (14 downto 0);
      signal slice_in_r39 : std_logic_vector (14 downto 0);
      signal slice_in_r40 : std_logic_vector (14 downto 0);
      signal slice_in_r41 : std_logic_vector (14 downto 0);
      signal slice_in_r42 : std_logic_vector (14 downto 0);
      signal slice_in_r43 : std_logic_vector (14 downto 0);
      signal slice_in_r44 : std_logic_vector (14 downto 0);
      signal slice_in_r45 : std_logic_vector (14 downto 0);
      signal slice_in_r46 : std_logic_vector (14 downto 0);
      signal slice_in_r47 : std_logic_vector (14 downto 0);
      signal slice_in_r48 : std_logic_vector (14 downto 0);
      signal slice_in_r49 : std_logic_vector (14 downto 0);
      signal slice_in_r50 : std_logic_vector (14 downto 0);
      signal slice_in_r51 : std_logic_vector (14 downto 0);
      signal slice_in_r52 : std_logic_vector (14 downto 0);
      signal slice_in_r53 : std_logic_vector (14 downto 0);
      signal slice_in_r54 : std_logic_vector (14 downto 0);
      signal slice_in_r55 : std_logic_vector (14 downto 0);
      signal za : std_logic_vector (7 downto 0);
begin
      -- combinational logic
      \m$0\ <= (\__in0\ & \__in1\ & \__in1\ & \__in0\ & rw_add(\__in0\, std_logic_vector'(X"01")) & \__in1\ & \__in0\ & rw_add(\__in1\, std_logic_vector'(X"01")));
      \ws$4734$3\ <= \m$0\(15 downto 0);
      \ws$4734$5\ <= rw_xor((\m$0\(44 downto 32) & \m$0\(47 downto 45)), rw_add(rw_add((\m$0\(1 downto 0) & \m$0\(15 downto 2)), rw_shiftr(\ws$4734$3\, std_logic_vector'(B"1"))), std_logic_vector'(X"0005")));
      \ws$4734$7\ <= rw_xor((\m$0\(12 downto 0) & \m$0\(15 downto 13)), rw_add(rw_add((\ws$4734$5\(1 downto 0) & \ws$4734$5\(15 downto 2)), rw_shiftr(\ws$4734$5\, std_logic_vector'(B"1"))), std_logic_vector'(X"0007")));
      \ws$4734$9\ <= rw_xor((\ws$4734$5\(12 downto 0) & \ws$4734$5\(15 downto 13)), rw_add(rw_add((\ws$4734$7\(1 downto 0) & \ws$4734$7\(15 downto 2)), rw_shiftr(\ws$4734$7\, std_logic_vector'(B"1"))), std_logic_vector'(X"0009")));
      \ws$4734$11\ <= rw_xor((\ws$4734$7\(12 downto 0) & \ws$4734$7\(15 downto 13)), rw_add(rw_add((\ws$4734$9\(1 downto 0) & \ws$4734$9\(15 downto 2)), rw_shiftr(\ws$4734$9\, std_logic_vector'(B"1"))), std_logic_vector'(X"000b")));
      \ws$4733$2\ <= rw_xor(\__in0\, rw_add(\__in1\, std_logic_vector'(X"02")));
      \ws$4733$3\ <= rw_xor(\__in1\, rw_add(\ws$4733$2\, std_logic_vector'(X"03")));
      \ws$4733$4\ <= rw_xor(\ws$4733$2\, rw_add(\ws$4733$3\, std_logic_vector'(X"04")));
      \ws$4733$5\ <= rw_xor(\ws$4733$3\, rw_add(\ws$4733$4\, std_logic_vector'(X"05")));
      \pts$0\ <= (\__in0\ & \__in1\ & rw_add(\__in0\, \__in1\));
      \iv$1\ <= rw_xor(\__in0\, \__in1\);
      \cts$4735$1\ <= rw_add(rw_xor(\pts$0\(23 downto 16), \iv$1\), std_logic_vector'(X"01"));
      \cts$4735$2\ <= rw_add(rw_xor(\pts$0\(15 downto 8), \cts$4735$1\), std_logic_vector'(X"01"));
      \cts$4735$3\ <= rw_add(rw_xor(\pts$0\(7 downto 0), \cts$4735$2\), std_logic_vector'(X"01"));
      \xs$0\ <= (\__in0\ & \__in1\ & rw_add(\__in0\, \__in1\) & rw_sub(\__in0\, \__in1\));
      \i$1\ <= \__in1\(1 downto 0);
      \xs$0_r1\ <= (\__in0\ & \__in1\ & rw_add(\__in0\, \__in1\) & rw_xor(\__in0\, \__in1\));
      \fold$0$1\ <= rw_add(std_logic_vector'(X"01"), \xs$0_r1\(31 downto 24));
      \fold$0$2\ <= rw_add(\fold$0$1\, \xs$0_r1\(23 downto 16));
      \fold$0$3\ <= rw_add(\fold$0$2\, \xs$0_r1\(15 downto 8));
      \fold$0$4\ <= rw_add(\fold$0$3\, \xs$0_r1\(7 downto 0));
      slice_in <= (std_logic_vector'(X"01") & \fold$0$1\ & \fold$0$2\ & \fold$0$3\ & \fold$0$4\);
      slice_in_r1 <= (std_logic_vector'(X"01") & \fold$0$1\ & \fold$0$2\ & \fold$0$3\ & \fold$0$4\);
      slice_in_r2 <= (std_logic_vector'(X"01") & \fold$0$1\ & \fold$0$2\ & \fold$0$3\ & \fold$0$4\);
      slice_in_r3 <= (std_logic_vector'(X"01") & \fold$0$1\ & \fold$0$2\ & \fold$0$3\ & \fold$0$4\);
      slice_in_r4 <= (std_logic_vector'(X"01") & \fold$0$1\ & \fold$0$2\ & \fold$0$3\ & \fold$0$4\);
      \nz$4733\ <= rw_cond(rw_eq(\__in1\, std_logic_vector'(X"00")), std_logic_vector'(X"01"), \__in1\);
      slice_in_r5 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r6 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r7 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r8 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r9 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r10 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r11 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r12 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r13 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r14 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r15 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r16 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r17 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r18 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r19 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r20 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r21 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r22 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r23 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r24 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r25 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r26 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r27 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r28 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r29 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r30 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r31 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r32 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r33 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r34 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r35 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r36 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r37 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r38 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r39 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r40 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r41 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r42 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r43 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r44 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r45 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r46 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r47 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r48 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r49 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r50 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r51 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r52 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r53 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r54 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      slice_in_r55 <= rw_xor((std_logic_vector'(B"0000000") & \__in0\), rw_xor((std_logic_vector'(B"000000") & \__in0\ & std_logic_vector'(B"0")), rw_xor(std_logic_vector'(B"000000000000000"), rw_xor((std_logic_vector'(X"0") & \__in0\ & std_logic_vector'(B"000")), rw_xor((std_logic_vector'(B"000") & \__in0\ & std_logic_vector'(X"0")), std_logic_vector'(B"000000000000000"))))));
      za <= rw_xor(rw_add(rw_add(rw_add(rw_add(rw_add(rw_add(\ws$4734$11\(7 downto 0), \ws$4733$5\), \cts$4735$3\), rw_xor(rw_xor(rw_xor(rw_xor(std_logic_vector'(X"00"), rw_cond(rw_eq(\i$1\, std_logic_vector'(B"00")), \iv$1\, \xs$0\(31 downto 24))), rw_cond(rw_eq(\i$1\, std_logic_vector'(B"01")), \iv$1\, \xs$0\(23 downto 16))), rw_add(\iv$1\, std_logic_vector'(X"01"))), rw_cond(rw_eq(\i$1\, std_logic_vector'(B"11")), \iv$1\, \xs$0\(7 downto 0)))), rw_xor(rw_xor(rw_xor(rw_xor(rw_xor(std_logic_vector'(X"00"), slice_in(39 downto 32)), slice_in_r1(31 downto 24)), slice_in_r2(23 downto 16)), slice_in_r3(15 downto 8)), slice_in_r4(7 downto 0))), rw_add(rw_add(rw_cond(rw_xor(rw_lts(\__in0\, std_logic_vector'(X"00")), rw_lts(\nz$4733\, std_logic_vector'(X"00"))), rw_sub(std_logic_vector'(X"00"), rw_div(rw_cond(rw_lts(\__in0\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \__in0\), \__in0\), rw_cond(rw_lts(\nz$4733\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \nz$4733\), \nz$4733\))), rw_div(rw_cond(rw_lts(\__in0\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \__in0\), \__in0\), rw_cond(rw_lts(\nz$4733\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \nz$4733\), \nz$4733\))), rw_cond(rw_lts(\__in0\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), rw_mod(rw_cond(rw_lts(\__in0\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \__in0\), \__in0\), rw_cond(rw_lts(\nz$4733\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \nz$4733\), \nz$4733\))), rw_mod(rw_cond(rw_lts(\__in0\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \__in0\), \__in0\), rw_cond(rw_lts(\nz$4733\, std_logic_vector'(X"00")), rw_sub(std_logic_vector'(X"00"), \nz$4733\), \nz$4733\)))), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"01")), std_logic_vector'(X"00"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"02")), std_logic_vector'(X"01"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"04")), std_logic_vector'(X"02"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"08")), std_logic_vector'(X"03"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"10")), std_logic_vector'(X"04"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"20")), std_logic_vector'(X"05"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"40")), std_logic_vector'(X"06"), rw_cond(rw_lteq(\__in1\, std_logic_vector'(X"80")), std_logic_vector'(X"07"), std_logic_vector'(X"08"))))))))))), rw_xor((rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r5(7 downto 7), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r6(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r7(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r8(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))) & rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r9(6 downto 6), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r10(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r11(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r12(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r13(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r14(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r15(5 downto 5), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r16(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r17(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r18(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r19(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r20(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r21(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r22(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r23(4 downto 4), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r24(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r25(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r26(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r27(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r28(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r29(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r30(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r31(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(rw_xor(slice_in_r32(3 downto 3), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r33(11 downto 11), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r34(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r35(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r36(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r37(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r38(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(slice_in_r39(2 downto 2), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r40(10 downto 10), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r41(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r42(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r43(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r44(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(slice_in_r45(1 downto 1), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r46(9 downto 9), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r47(14 downto 14), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r48(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r49(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r50(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r51(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(slice_in_r52(0 downto 0), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r53(8 downto 8), std_logic_vector'(B"0")), std_logic_vector'(B"0")), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r54(13 downto 13), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(rw_xor(rw_xor(slice_in_r55(12 downto 12), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))), (std_logic_vector'(B"00") & rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")) & rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))) & rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))) & rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))) & rw_xor(rw_xor(\__in0\(3 downto 3), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))), rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))))) & rw_xor(rw_xor(\__in0\(2 downto 2), rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))))), rw_xor(rw_xor(\__in0\(3 downto 3), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))))), rw_xor(rw_xor(\__in0\(4 downto 4), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))), rw_xor(rw_xor(\__in0\(5 downto 5), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0"))), rw_xor(rw_xor(\__in0\(6 downto 6), std_logic_vector'(B"0")), rw_xor(rw_xor(\__in0\(7 downto 7), std_logic_vector'(B"0")), std_logic_vector'(B"0")))))))))), rw_add((\__in1\(7 downto 7) & \__in1\(3 downto 3) & \__in1\(6 downto 6) & \__in1\(2 downto 2) & \__in1\(5 downto 5) & \__in1\(1 downto 1) & \__in1\(4 downto 4) & \__in1\(0 downto 0)), std_logic_vector'(X"38")));
      -- outputs
      \__out0\ <= za;
end architecture;