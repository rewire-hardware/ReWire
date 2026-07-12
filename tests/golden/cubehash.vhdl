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
      port (clk : in std_logic_vector (0 downto 0);
            rst : in std_logic_vector (0 downto 0);
            \__in0\ : in std_logic_vector (0 downto 0);
            \__out0\ : out std_logic_vector (1023 downto 0));
end entity;

architecture rtl of top_level is
      component \Main_add\ is
            port (s : in std_logic_vector (1023 downto 0);
                  res : out std_logic_vector (1023 downto 0));
      end component;
      component \Main_rot\ is
            port (rc : in std_logic_vector (31 downto 0);
                  s : in std_logic_vector (1023 downto 0);
                  f32 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix1\ is
            port (s : in std_logic_vector (1023 downto 0);
                  f32 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix2\ is
            port (s : in std_logic_vector (1023 downto 0);
                  f32 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix3\ is
            port (s : in std_logic_vector (1023 downto 0);
                  f32 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix4\ is
            port (s : in std_logic_vector (1023 downto 0);
                  f32 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_xor\ is
            port (s : in std_logic_vector (1023 downto 0);
                  res : out std_logic_vector (1023 downto 0));
      end component;
      component main_arm2 is
            port (s0 : in std_logic_vector (1023 downto 0);
                  res : out std_logic_vector (2047 downto 0));
      end component;
      -- state registers
      -- __st0: 1024 bits, init 0x2000000001000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
      signal \__st0\ : std_logic_vector (1023 downto 0) := std_logic_vector'(X"0000002000000001000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (1023 downto 0);
      signal main_add_out : std_logic_vector (1023 downto 0);
      signal main_rot_out : std_logic_vector (31 downto 0);
      signal main_rot_out_r1 : std_logic_vector (31 downto 0);
      signal main_rot_out_r2 : std_logic_vector (31 downto 0);
      signal main_rot_out_r3 : std_logic_vector (31 downto 0);
      signal main_rot_out_r4 : std_logic_vector (31 downto 0);
      signal main_rot_out_r5 : std_logic_vector (31 downto 0);
      signal main_rot_out_r6 : std_logic_vector (31 downto 0);
      signal main_rot_out_r7 : std_logic_vector (31 downto 0);
      signal main_rot_out_r8 : std_logic_vector (31 downto 0);
      signal main_rot_out_r9 : std_logic_vector (31 downto 0);
      signal main_rot_out_r10 : std_logic_vector (31 downto 0);
      signal main_rot_out_r11 : std_logic_vector (31 downto 0);
      signal main_rot_out_r12 : std_logic_vector (31 downto 0);
      signal main_rot_out_r13 : std_logic_vector (31 downto 0);
      signal main_rot_out_r14 : std_logic_vector (31 downto 0);
      signal main_rot_out_r15 : std_logic_vector (31 downto 0);
      signal main_rot_out_r16 : std_logic_vector (31 downto 0);
      signal main_rot_out_r17 : std_logic_vector (31 downto 0);
      signal main_rot_out_r18 : std_logic_vector (31 downto 0);
      signal main_rot_out_r19 : std_logic_vector (31 downto 0);
      signal main_rot_out_r20 : std_logic_vector (31 downto 0);
      signal main_rot_out_r21 : std_logic_vector (31 downto 0);
      signal main_rot_out_r22 : std_logic_vector (31 downto 0);
      signal main_rot_out_r23 : std_logic_vector (31 downto 0);
      signal main_rot_out_r24 : std_logic_vector (31 downto 0);
      signal main_rot_out_r25 : std_logic_vector (31 downto 0);
      signal main_rot_out_r26 : std_logic_vector (31 downto 0);
      signal main_rot_out_r27 : std_logic_vector (31 downto 0);
      signal main_rot_out_r28 : std_logic_vector (31 downto 0);
      signal main_rot_out_r29 : std_logic_vector (31 downto 0);
      signal main_rot_out_r30 : std_logic_vector (31 downto 0);
      signal main_rot_out_r31 : std_logic_vector (31 downto 0);
      signal s : std_logic_vector (1023 downto 0);
      signal main_swapix1_out : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r1 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r2 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r3 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r4 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r5 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r6 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r7 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r8 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r9 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r10 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r11 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r12 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r13 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r14 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r15 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r16 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r17 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r18 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r19 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r20 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r21 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r22 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r23 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r24 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r25 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r26 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r27 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r28 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r29 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r30 : std_logic_vector (31 downto 0);
      signal main_swapix1_out_r31 : std_logic_vector (31 downto 0);
      signal conn : std_logic_vector (1023 downto 0);
      signal main_xor_out : std_logic_vector (1023 downto 0);
      signal main_swapix2_out : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r1 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r2 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r3 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r4 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r5 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r6 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r7 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r8 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r9 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r10 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r11 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r12 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r13 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r14 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r15 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r16 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r17 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r18 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r19 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r20 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r21 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r22 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r23 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r24 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r25 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r26 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r27 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r28 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r29 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r30 : std_logic_vector (31 downto 0);
      signal main_swapix2_out_r31 : std_logic_vector (31 downto 0);
      signal conn_r1 : std_logic_vector (1023 downto 0);
      signal main_add_out_r1 : std_logic_vector (1023 downto 0);
      signal main_rot_out_r32 : std_logic_vector (31 downto 0);
      signal main_rot_out_r33 : std_logic_vector (31 downto 0);
      signal main_rot_out_r34 : std_logic_vector (31 downto 0);
      signal main_rot_out_r35 : std_logic_vector (31 downto 0);
      signal main_rot_out_r36 : std_logic_vector (31 downto 0);
      signal main_rot_out_r37 : std_logic_vector (31 downto 0);
      signal main_rot_out_r38 : std_logic_vector (31 downto 0);
      signal main_rot_out_r39 : std_logic_vector (31 downto 0);
      signal main_rot_out_r40 : std_logic_vector (31 downto 0);
      signal main_rot_out_r41 : std_logic_vector (31 downto 0);
      signal main_rot_out_r42 : std_logic_vector (31 downto 0);
      signal main_rot_out_r43 : std_logic_vector (31 downto 0);
      signal main_rot_out_r44 : std_logic_vector (31 downto 0);
      signal main_rot_out_r45 : std_logic_vector (31 downto 0);
      signal main_rot_out_r46 : std_logic_vector (31 downto 0);
      signal main_rot_out_r47 : std_logic_vector (31 downto 0);
      signal main_rot_out_r48 : std_logic_vector (31 downto 0);
      signal main_rot_out_r49 : std_logic_vector (31 downto 0);
      signal main_rot_out_r50 : std_logic_vector (31 downto 0);
      signal main_rot_out_r51 : std_logic_vector (31 downto 0);
      signal main_rot_out_r52 : std_logic_vector (31 downto 0);
      signal main_rot_out_r53 : std_logic_vector (31 downto 0);
      signal main_rot_out_r54 : std_logic_vector (31 downto 0);
      signal main_rot_out_r55 : std_logic_vector (31 downto 0);
      signal main_rot_out_r56 : std_logic_vector (31 downto 0);
      signal main_rot_out_r57 : std_logic_vector (31 downto 0);
      signal main_rot_out_r58 : std_logic_vector (31 downto 0);
      signal main_rot_out_r59 : std_logic_vector (31 downto 0);
      signal main_rot_out_r60 : std_logic_vector (31 downto 0);
      signal main_rot_out_r61 : std_logic_vector (31 downto 0);
      signal main_rot_out_r62 : std_logic_vector (31 downto 0);
      signal main_rot_out_r63 : std_logic_vector (31 downto 0);
      signal s_r1 : std_logic_vector (1023 downto 0);
      signal main_swapix3_out : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r1 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r2 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r3 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r4 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r5 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r6 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r7 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r8 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r9 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r10 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r11 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r12 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r13 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r14 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r15 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r16 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r17 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r18 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r19 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r20 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r21 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r22 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r23 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r24 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r25 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r26 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r27 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r28 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r29 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r30 : std_logic_vector (31 downto 0);
      signal main_swapix3_out_r31 : std_logic_vector (31 downto 0);
      signal conn_r2 : std_logic_vector (1023 downto 0);
      signal main_xor_out_r1 : std_logic_vector (1023 downto 0);
      signal main_swapix4_out : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r1 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r2 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r3 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r4 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r5 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r6 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r7 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r8 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r9 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r10 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r11 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r12 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r13 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r14 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r15 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r16 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r17 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r18 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r19 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r20 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r21 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r22 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r23 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r24 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r25 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r26 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r27 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r28 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r29 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r30 : std_logic_vector (31 downto 0);
      signal main_swapix4_out_r31 : std_logic_vector (31 downto 0);
      signal za : std_logic_vector (1023 downto 0);
      signal main_arm2_out : std_logic_vector (2047 downto 0);
      signal main_arm2_out_r1 : std_logic_vector (2047 downto 0);
      signal zres : std_logic_vector (2047 downto 0);
begin
      -- combinational logic
      add_i : \Main_add\ port map (\__st0\, main_add_out);
      rot_i : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00000"), main_rot_out);
      rot_i_r1 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00001"), main_rot_out_r1);
      rot_i_r2 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00010"), main_rot_out_r2);
      rot_i_r3 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00011"), main_rot_out_r3);
      rot_i_r4 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00100"), main_rot_out_r4);
      rot_i_r5 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00101"), main_rot_out_r5);
      rot_i_r6 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00110"), main_rot_out_r6);
      rot_i_r7 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"00111"), main_rot_out_r7);
      rot_i_r8 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01000"), main_rot_out_r8);
      rot_i_r9 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01001"), main_rot_out_r9);
      rot_i_r10 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01010"), main_rot_out_r10);
      rot_i_r11 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01011"), main_rot_out_r11);
      rot_i_r12 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01100"), main_rot_out_r12);
      rot_i_r13 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01101"), main_rot_out_r13);
      rot_i_r14 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01110"), main_rot_out_r14);
      rot_i_r15 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"01111"), main_rot_out_r15);
      rot_i_r16 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10000"), main_rot_out_r16);
      rot_i_r17 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10001"), main_rot_out_r17);
      rot_i_r18 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10010"), main_rot_out_r18);
      rot_i_r19 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10011"), main_rot_out_r19);
      rot_i_r20 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10100"), main_rot_out_r20);
      rot_i_r21 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10101"), main_rot_out_r21);
      rot_i_r22 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10110"), main_rot_out_r22);
      rot_i_r23 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"10111"), main_rot_out_r23);
      rot_i_r24 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11000"), main_rot_out_r24);
      rot_i_r25 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11001"), main_rot_out_r25);
      rot_i_r26 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11010"), main_rot_out_r26);
      rot_i_r27 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11011"), main_rot_out_r27);
      rot_i_r28 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11100"), main_rot_out_r28);
      rot_i_r29 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11101"), main_rot_out_r29);
      rot_i_r30 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11110"), main_rot_out_r30);
      rot_i_r31 : \Main_rot\ port map (std_logic_vector'(X"00000007"), main_add_out, std_logic_vector'(B"11111"), main_rot_out_r31);
      s <= (main_rot_out & main_rot_out_r1 & main_rot_out_r2 & main_rot_out_r3 & main_rot_out_r4 & main_rot_out_r5 & main_rot_out_r6 & main_rot_out_r7 & main_rot_out_r8 & main_rot_out_r9 & main_rot_out_r10 & main_rot_out_r11 & main_rot_out_r12 & main_rot_out_r13 & main_rot_out_r14 & main_rot_out_r15 & main_rot_out_r16 & main_rot_out_r17 & main_rot_out_r18 & main_rot_out_r19 & main_rot_out_r20 & main_rot_out_r21 & main_rot_out_r22 & main_rot_out_r23 & main_rot_out_r24 & main_rot_out_r25 & main_rot_out_r26 & main_rot_out_r27 & main_rot_out_r28 & main_rot_out_r29 & main_rot_out_r30 & main_rot_out_r31);
      swapix1_i : \Main_swapix1\ port map (s, std_logic_vector'(B"00000"), main_swapix1_out);
      swapix1_i_r1 : \Main_swapix1\ port map (s, std_logic_vector'(B"00001"), main_swapix1_out_r1);
      swapix1_i_r2 : \Main_swapix1\ port map (s, std_logic_vector'(B"00010"), main_swapix1_out_r2);
      swapix1_i_r3 : \Main_swapix1\ port map (s, std_logic_vector'(B"00011"), main_swapix1_out_r3);
      swapix1_i_r4 : \Main_swapix1\ port map (s, std_logic_vector'(B"00100"), main_swapix1_out_r4);
      swapix1_i_r5 : \Main_swapix1\ port map (s, std_logic_vector'(B"00101"), main_swapix1_out_r5);
      swapix1_i_r6 : \Main_swapix1\ port map (s, std_logic_vector'(B"00110"), main_swapix1_out_r6);
      swapix1_i_r7 : \Main_swapix1\ port map (s, std_logic_vector'(B"00111"), main_swapix1_out_r7);
      swapix1_i_r8 : \Main_swapix1\ port map (s, std_logic_vector'(B"01000"), main_swapix1_out_r8);
      swapix1_i_r9 : \Main_swapix1\ port map (s, std_logic_vector'(B"01001"), main_swapix1_out_r9);
      swapix1_i_r10 : \Main_swapix1\ port map (s, std_logic_vector'(B"01010"), main_swapix1_out_r10);
      swapix1_i_r11 : \Main_swapix1\ port map (s, std_logic_vector'(B"01011"), main_swapix1_out_r11);
      swapix1_i_r12 : \Main_swapix1\ port map (s, std_logic_vector'(B"01100"), main_swapix1_out_r12);
      swapix1_i_r13 : \Main_swapix1\ port map (s, std_logic_vector'(B"01101"), main_swapix1_out_r13);
      swapix1_i_r14 : \Main_swapix1\ port map (s, std_logic_vector'(B"01110"), main_swapix1_out_r14);
      swapix1_i_r15 : \Main_swapix1\ port map (s, std_logic_vector'(B"01111"), main_swapix1_out_r15);
      swapix1_i_r16 : \Main_swapix1\ port map (s, std_logic_vector'(B"10000"), main_swapix1_out_r16);
      swapix1_i_r17 : \Main_swapix1\ port map (s, std_logic_vector'(B"10001"), main_swapix1_out_r17);
      swapix1_i_r18 : \Main_swapix1\ port map (s, std_logic_vector'(B"10010"), main_swapix1_out_r18);
      swapix1_i_r19 : \Main_swapix1\ port map (s, std_logic_vector'(B"10011"), main_swapix1_out_r19);
      swapix1_i_r20 : \Main_swapix1\ port map (s, std_logic_vector'(B"10100"), main_swapix1_out_r20);
      swapix1_i_r21 : \Main_swapix1\ port map (s, std_logic_vector'(B"10101"), main_swapix1_out_r21);
      swapix1_i_r22 : \Main_swapix1\ port map (s, std_logic_vector'(B"10110"), main_swapix1_out_r22);
      swapix1_i_r23 : \Main_swapix1\ port map (s, std_logic_vector'(B"10111"), main_swapix1_out_r23);
      swapix1_i_r24 : \Main_swapix1\ port map (s, std_logic_vector'(B"11000"), main_swapix1_out_r24);
      swapix1_i_r25 : \Main_swapix1\ port map (s, std_logic_vector'(B"11001"), main_swapix1_out_r25);
      swapix1_i_r26 : \Main_swapix1\ port map (s, std_logic_vector'(B"11010"), main_swapix1_out_r26);
      swapix1_i_r27 : \Main_swapix1\ port map (s, std_logic_vector'(B"11011"), main_swapix1_out_r27);
      swapix1_i_r28 : \Main_swapix1\ port map (s, std_logic_vector'(B"11100"), main_swapix1_out_r28);
      swapix1_i_r29 : \Main_swapix1\ port map (s, std_logic_vector'(B"11101"), main_swapix1_out_r29);
      swapix1_i_r30 : \Main_swapix1\ port map (s, std_logic_vector'(B"11110"), main_swapix1_out_r30);
      swapix1_i_r31 : \Main_swapix1\ port map (s, std_logic_vector'(B"11111"), main_swapix1_out_r31);
      conn <= (main_swapix1_out & main_swapix1_out_r1 & main_swapix1_out_r2 & main_swapix1_out_r3 & main_swapix1_out_r4 & main_swapix1_out_r5 & main_swapix1_out_r6 & main_swapix1_out_r7 & main_swapix1_out_r8 & main_swapix1_out_r9 & main_swapix1_out_r10 & main_swapix1_out_r11 & main_swapix1_out_r12 & main_swapix1_out_r13 & main_swapix1_out_r14 & main_swapix1_out_r15 & main_swapix1_out_r16 & main_swapix1_out_r17 & main_swapix1_out_r18 & main_swapix1_out_r19 & main_swapix1_out_r20 & main_swapix1_out_r21 & main_swapix1_out_r22 & main_swapix1_out_r23 & main_swapix1_out_r24 & main_swapix1_out_r25 & main_swapix1_out_r26 & main_swapix1_out_r27 & main_swapix1_out_r28 & main_swapix1_out_r29 & main_swapix1_out_r30 & main_swapix1_out_r31);
      xor_i : \Main_xor\ port map (conn, main_xor_out);
      swapix2_i : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00000"), main_swapix2_out);
      swapix2_i_r1 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00001"), main_swapix2_out_r1);
      swapix2_i_r2 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00010"), main_swapix2_out_r2);
      swapix2_i_r3 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00011"), main_swapix2_out_r3);
      swapix2_i_r4 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00100"), main_swapix2_out_r4);
      swapix2_i_r5 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00101"), main_swapix2_out_r5);
      swapix2_i_r6 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00110"), main_swapix2_out_r6);
      swapix2_i_r7 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"00111"), main_swapix2_out_r7);
      swapix2_i_r8 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01000"), main_swapix2_out_r8);
      swapix2_i_r9 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01001"), main_swapix2_out_r9);
      swapix2_i_r10 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01010"), main_swapix2_out_r10);
      swapix2_i_r11 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01011"), main_swapix2_out_r11);
      swapix2_i_r12 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01100"), main_swapix2_out_r12);
      swapix2_i_r13 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01101"), main_swapix2_out_r13);
      swapix2_i_r14 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01110"), main_swapix2_out_r14);
      swapix2_i_r15 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"01111"), main_swapix2_out_r15);
      swapix2_i_r16 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10000"), main_swapix2_out_r16);
      swapix2_i_r17 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10001"), main_swapix2_out_r17);
      swapix2_i_r18 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10010"), main_swapix2_out_r18);
      swapix2_i_r19 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10011"), main_swapix2_out_r19);
      swapix2_i_r20 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10100"), main_swapix2_out_r20);
      swapix2_i_r21 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10101"), main_swapix2_out_r21);
      swapix2_i_r22 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10110"), main_swapix2_out_r22);
      swapix2_i_r23 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"10111"), main_swapix2_out_r23);
      swapix2_i_r24 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11000"), main_swapix2_out_r24);
      swapix2_i_r25 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11001"), main_swapix2_out_r25);
      swapix2_i_r26 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11010"), main_swapix2_out_r26);
      swapix2_i_r27 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11011"), main_swapix2_out_r27);
      swapix2_i_r28 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11100"), main_swapix2_out_r28);
      swapix2_i_r29 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11101"), main_swapix2_out_r29);
      swapix2_i_r30 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11110"), main_swapix2_out_r30);
      swapix2_i_r31 : \Main_swapix2\ port map (main_xor_out, std_logic_vector'(B"11111"), main_swapix2_out_r31);
      conn_r1 <= (main_swapix2_out & main_swapix2_out_r1 & main_swapix2_out_r2 & main_swapix2_out_r3 & main_swapix2_out_r4 & main_swapix2_out_r5 & main_swapix2_out_r6 & main_swapix2_out_r7 & main_swapix2_out_r8 & main_swapix2_out_r9 & main_swapix2_out_r10 & main_swapix2_out_r11 & main_swapix2_out_r12 & main_swapix2_out_r13 & main_swapix2_out_r14 & main_swapix2_out_r15 & main_swapix2_out_r16 & main_swapix2_out_r17 & main_swapix2_out_r18 & main_swapix2_out_r19 & main_swapix2_out_r20 & main_swapix2_out_r21 & main_swapix2_out_r22 & main_swapix2_out_r23 & main_swapix2_out_r24 & main_swapix2_out_r25 & main_swapix2_out_r26 & main_swapix2_out_r27 & main_swapix2_out_r28 & main_swapix2_out_r29 & main_swapix2_out_r30 & main_swapix2_out_r31);
      add_i_r1 : \Main_add\ port map (conn_r1, main_add_out_r1);
      rot_i_r32 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00000"), main_rot_out_r32);
      rot_i_r33 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00001"), main_rot_out_r33);
      rot_i_r34 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00010"), main_rot_out_r34);
      rot_i_r35 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00011"), main_rot_out_r35);
      rot_i_r36 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00100"), main_rot_out_r36);
      rot_i_r37 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00101"), main_rot_out_r37);
      rot_i_r38 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00110"), main_rot_out_r38);
      rot_i_r39 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"00111"), main_rot_out_r39);
      rot_i_r40 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01000"), main_rot_out_r40);
      rot_i_r41 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01001"), main_rot_out_r41);
      rot_i_r42 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01010"), main_rot_out_r42);
      rot_i_r43 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01011"), main_rot_out_r43);
      rot_i_r44 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01100"), main_rot_out_r44);
      rot_i_r45 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01101"), main_rot_out_r45);
      rot_i_r46 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01110"), main_rot_out_r46);
      rot_i_r47 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"01111"), main_rot_out_r47);
      rot_i_r48 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10000"), main_rot_out_r48);
      rot_i_r49 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10001"), main_rot_out_r49);
      rot_i_r50 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10010"), main_rot_out_r50);
      rot_i_r51 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10011"), main_rot_out_r51);
      rot_i_r52 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10100"), main_rot_out_r52);
      rot_i_r53 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10101"), main_rot_out_r53);
      rot_i_r54 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10110"), main_rot_out_r54);
      rot_i_r55 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"10111"), main_rot_out_r55);
      rot_i_r56 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11000"), main_rot_out_r56);
      rot_i_r57 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11001"), main_rot_out_r57);
      rot_i_r58 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11010"), main_rot_out_r58);
      rot_i_r59 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11011"), main_rot_out_r59);
      rot_i_r60 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11100"), main_rot_out_r60);
      rot_i_r61 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11101"), main_rot_out_r61);
      rot_i_r62 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11110"), main_rot_out_r62);
      rot_i_r63 : \Main_rot\ port map (std_logic_vector'(X"0000000b"), main_add_out_r1, std_logic_vector'(B"11111"), main_rot_out_r63);
      s_r1 <= (main_rot_out_r32 & main_rot_out_r33 & main_rot_out_r34 & main_rot_out_r35 & main_rot_out_r36 & main_rot_out_r37 & main_rot_out_r38 & main_rot_out_r39 & main_rot_out_r40 & main_rot_out_r41 & main_rot_out_r42 & main_rot_out_r43 & main_rot_out_r44 & main_rot_out_r45 & main_rot_out_r46 & main_rot_out_r47 & main_rot_out_r48 & main_rot_out_r49 & main_rot_out_r50 & main_rot_out_r51 & main_rot_out_r52 & main_rot_out_r53 & main_rot_out_r54 & main_rot_out_r55 & main_rot_out_r56 & main_rot_out_r57 & main_rot_out_r58 & main_rot_out_r59 & main_rot_out_r60 & main_rot_out_r61 & main_rot_out_r62 & main_rot_out_r63);
      swapix3_i : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00000"), main_swapix3_out);
      swapix3_i_r1 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00001"), main_swapix3_out_r1);
      swapix3_i_r2 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00010"), main_swapix3_out_r2);
      swapix3_i_r3 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00011"), main_swapix3_out_r3);
      swapix3_i_r4 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00100"), main_swapix3_out_r4);
      swapix3_i_r5 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00101"), main_swapix3_out_r5);
      swapix3_i_r6 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00110"), main_swapix3_out_r6);
      swapix3_i_r7 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"00111"), main_swapix3_out_r7);
      swapix3_i_r8 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01000"), main_swapix3_out_r8);
      swapix3_i_r9 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01001"), main_swapix3_out_r9);
      swapix3_i_r10 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01010"), main_swapix3_out_r10);
      swapix3_i_r11 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01011"), main_swapix3_out_r11);
      swapix3_i_r12 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01100"), main_swapix3_out_r12);
      swapix3_i_r13 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01101"), main_swapix3_out_r13);
      swapix3_i_r14 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01110"), main_swapix3_out_r14);
      swapix3_i_r15 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"01111"), main_swapix3_out_r15);
      swapix3_i_r16 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10000"), main_swapix3_out_r16);
      swapix3_i_r17 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10001"), main_swapix3_out_r17);
      swapix3_i_r18 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10010"), main_swapix3_out_r18);
      swapix3_i_r19 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10011"), main_swapix3_out_r19);
      swapix3_i_r20 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10100"), main_swapix3_out_r20);
      swapix3_i_r21 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10101"), main_swapix3_out_r21);
      swapix3_i_r22 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10110"), main_swapix3_out_r22);
      swapix3_i_r23 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"10111"), main_swapix3_out_r23);
      swapix3_i_r24 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11000"), main_swapix3_out_r24);
      swapix3_i_r25 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11001"), main_swapix3_out_r25);
      swapix3_i_r26 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11010"), main_swapix3_out_r26);
      swapix3_i_r27 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11011"), main_swapix3_out_r27);
      swapix3_i_r28 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11100"), main_swapix3_out_r28);
      swapix3_i_r29 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11101"), main_swapix3_out_r29);
      swapix3_i_r30 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11110"), main_swapix3_out_r30);
      swapix3_i_r31 : \Main_swapix3\ port map (s_r1, std_logic_vector'(B"11111"), main_swapix3_out_r31);
      conn_r2 <= (main_swapix3_out & main_swapix3_out_r1 & main_swapix3_out_r2 & main_swapix3_out_r3 & main_swapix3_out_r4 & main_swapix3_out_r5 & main_swapix3_out_r6 & main_swapix3_out_r7 & main_swapix3_out_r8 & main_swapix3_out_r9 & main_swapix3_out_r10 & main_swapix3_out_r11 & main_swapix3_out_r12 & main_swapix3_out_r13 & main_swapix3_out_r14 & main_swapix3_out_r15 & main_swapix3_out_r16 & main_swapix3_out_r17 & main_swapix3_out_r18 & main_swapix3_out_r19 & main_swapix3_out_r20 & main_swapix3_out_r21 & main_swapix3_out_r22 & main_swapix3_out_r23 & main_swapix3_out_r24 & main_swapix3_out_r25 & main_swapix3_out_r26 & main_swapix3_out_r27 & main_swapix3_out_r28 & main_swapix3_out_r29 & main_swapix3_out_r30 & main_swapix3_out_r31);
      xor_i_r1 : \Main_xor\ port map (conn_r2, main_xor_out_r1);
      swapix4_i : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00000"), main_swapix4_out);
      swapix4_i_r1 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00001"), main_swapix4_out_r1);
      swapix4_i_r2 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00010"), main_swapix4_out_r2);
      swapix4_i_r3 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00011"), main_swapix4_out_r3);
      swapix4_i_r4 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00100"), main_swapix4_out_r4);
      swapix4_i_r5 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00101"), main_swapix4_out_r5);
      swapix4_i_r6 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00110"), main_swapix4_out_r6);
      swapix4_i_r7 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"00111"), main_swapix4_out_r7);
      swapix4_i_r8 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01000"), main_swapix4_out_r8);
      swapix4_i_r9 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01001"), main_swapix4_out_r9);
      swapix4_i_r10 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01010"), main_swapix4_out_r10);
      swapix4_i_r11 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01011"), main_swapix4_out_r11);
      swapix4_i_r12 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01100"), main_swapix4_out_r12);
      swapix4_i_r13 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01101"), main_swapix4_out_r13);
      swapix4_i_r14 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01110"), main_swapix4_out_r14);
      swapix4_i_r15 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"01111"), main_swapix4_out_r15);
      swapix4_i_r16 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10000"), main_swapix4_out_r16);
      swapix4_i_r17 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10001"), main_swapix4_out_r17);
      swapix4_i_r18 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10010"), main_swapix4_out_r18);
      swapix4_i_r19 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10011"), main_swapix4_out_r19);
      swapix4_i_r20 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10100"), main_swapix4_out_r20);
      swapix4_i_r21 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10101"), main_swapix4_out_r21);
      swapix4_i_r22 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10110"), main_swapix4_out_r22);
      swapix4_i_r23 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"10111"), main_swapix4_out_r23);
      swapix4_i_r24 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11000"), main_swapix4_out_r24);
      swapix4_i_r25 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11001"), main_swapix4_out_r25);
      swapix4_i_r26 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11010"), main_swapix4_out_r26);
      swapix4_i_r27 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11011"), main_swapix4_out_r27);
      swapix4_i_r28 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11100"), main_swapix4_out_r28);
      swapix4_i_r29 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11101"), main_swapix4_out_r29);
      swapix4_i_r30 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11110"), main_swapix4_out_r30);
      swapix4_i_r31 : \Main_swapix4\ port map (main_xor_out_r1, std_logic_vector'(B"11111"), main_swapix4_out_r31);
      za <= (main_swapix4_out & main_swapix4_out_r1 & main_swapix4_out_r2 & main_swapix4_out_r3 & main_swapix4_out_r4 & main_swapix4_out_r5 & main_swapix4_out_r6 & main_swapix4_out_r7 & main_swapix4_out_r8 & main_swapix4_out_r9 & main_swapix4_out_r10 & main_swapix4_out_r11 & main_swapix4_out_r12 & main_swapix4_out_r13 & main_swapix4_out_r14 & main_swapix4_out_r15 & main_swapix4_out_r16 & main_swapix4_out_r17 & main_swapix4_out_r18 & main_swapix4_out_r19 & main_swapix4_out_r20 & main_swapix4_out_r21 & main_swapix4_out_r22 & main_swapix4_out_r23 & main_swapix4_out_r24 & main_swapix4_out_r25 & main_swapix4_out_r26 & main_swapix4_out_r27 & main_swapix4_out_r28 & main_swapix4_out_r29 & main_swapix4_out_r30 & main_swapix4_out_r31);
      arm2_i : main_arm2 port map (za, main_arm2_out);
      arm2_i_r1 : main_arm2 port map (\__st0\, main_arm2_out_r1);
      zres <= rw_cond(rw_not(\__in0\), main_arm2_out, main_arm2_out_r1);
      \__st0_next\ <= zres(1023 downto 0);
      -- outputs
      \__out0\ <= zres(2047 downto 1024);
      -- state register update
      process (clk, rst)
      begin
            if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(X"0000002000000001000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;

-- main.arm2
-- block '$L.arm2' of process main
-- also: main._unused
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm2 is
      port (s0 : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (2047 downto 0));
end entity;

architecture rtl of main_arm2 is
begin
      res <= (s0 & s0);
end architecture;

-- Main.explode5
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_explode5\ is
      port (w5 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \Main_explode5\ is
begin
      res <= (rw_resize(rw_shiftr(w5, std_logic_vector'(X"00000000000000000000000000000004")), 1) & rw_resize(rw_shiftr(w5, std_logic_vector'(X"00000000000000000000000000000003")), 1) & rw_resize(rw_shiftr(w5, std_logic_vector'(X"00000000000000000000000000000002")), 1) & rw_resize(rw_shiftr(w5, std_logic_vector'(X"00000000000000000000000000000001")), 1) & rw_resize(rw_shiftr(w5, rw_repl(128, std_logic_vector'(B"0"))), 1));
end architecture;

-- Main.rot
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_rot\ is
      port (rc : in std_logic_vector (31 downto 0);
            s : in std_logic_vector (1023 downto 0);
            f32 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_rot\ is
      component \Main_explode5\ is
            port (w5 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal h : std_logic_vector (0 downto 0);
      signal zds : std_logic_vector (32 downto 0);
      signal w0 : std_logic_vector (31 downto 0);
      signal h_r1 : std_logic_vector (0 downto 0);
begin
      explode5_i : \Main_explode5\ port map (f32, main_explode5_out);
      h <= main_explode5_out(4 downto 4);
      zds <= (h & rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize(f32, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32));
      w0 <= zds(31 downto 0);
      h_r1 <= zds(32 downto 32);
      res <= rw_cond(rw_not(h_r1), rw_or(rw_shiftl(w0, rc), rw_shiftr(w0, rw_sub(std_logic_vector'(X"00000020"), rc))), w0);
end architecture;

-- Main.addix
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_addix\ is
      port (s : in std_logic_vector (1023 downto 0);
            f32 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_addix\ is
      component \Main_explode5\ is
            port (w5 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal h : std_logic_vector (0 downto 0);
      signal zds : std_logic_vector (64 downto 0);
      signal v0 : std_logic_vector (31 downto 0);
      signal h_r1 : std_logic_vector (0 downto 0);
      signal v1 : std_logic_vector (31 downto 0);
begin
      explode5_i : \Main_explode5\ port map (f32, main_explode5_out);
      j <= main_explode5_out(3 downto 3);
      k <= main_explode5_out(2 downto 2);
      l <= main_explode5_out(1 downto 1);
      m <= main_explode5_out(0 downto 0);
      h <= main_explode5_out(4 downto 4);
      zds <= (h & rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"0") & j & k & l & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32) & rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"1") & j & k & l & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32));
      v0 <= zds(63 downto 32);
      h_r1 <= zds(64 downto 64);
      v1 <= zds(31 downto 0);
      res <= rw_cond(rw_not(h_r1), v0, rw_add(v0, v1));
end architecture;

-- Main.add
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_add\ is
      port (s : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (1023 downto 0));
end entity;

architecture rtl of \Main_add\ is
      component \Main_addix\ is
            port (s : in std_logic_vector (1023 downto 0);
                  f32 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      signal main_addix_out : std_logic_vector (31 downto 0);
      signal main_addix_out_r1 : std_logic_vector (31 downto 0);
      signal main_addix_out_r2 : std_logic_vector (31 downto 0);
      signal main_addix_out_r3 : std_logic_vector (31 downto 0);
      signal main_addix_out_r4 : std_logic_vector (31 downto 0);
      signal main_addix_out_r5 : std_logic_vector (31 downto 0);
      signal main_addix_out_r6 : std_logic_vector (31 downto 0);
      signal main_addix_out_r7 : std_logic_vector (31 downto 0);
      signal main_addix_out_r8 : std_logic_vector (31 downto 0);
      signal main_addix_out_r9 : std_logic_vector (31 downto 0);
      signal main_addix_out_r10 : std_logic_vector (31 downto 0);
      signal main_addix_out_r11 : std_logic_vector (31 downto 0);
      signal main_addix_out_r12 : std_logic_vector (31 downto 0);
      signal main_addix_out_r13 : std_logic_vector (31 downto 0);
      signal main_addix_out_r14 : std_logic_vector (31 downto 0);
      signal main_addix_out_r15 : std_logic_vector (31 downto 0);
      signal main_addix_out_r16 : std_logic_vector (31 downto 0);
      signal main_addix_out_r17 : std_logic_vector (31 downto 0);
      signal main_addix_out_r18 : std_logic_vector (31 downto 0);
      signal main_addix_out_r19 : std_logic_vector (31 downto 0);
      signal main_addix_out_r20 : std_logic_vector (31 downto 0);
      signal main_addix_out_r21 : std_logic_vector (31 downto 0);
      signal main_addix_out_r22 : std_logic_vector (31 downto 0);
      signal main_addix_out_r23 : std_logic_vector (31 downto 0);
      signal main_addix_out_r24 : std_logic_vector (31 downto 0);
      signal main_addix_out_r25 : std_logic_vector (31 downto 0);
      signal main_addix_out_r26 : std_logic_vector (31 downto 0);
      signal main_addix_out_r27 : std_logic_vector (31 downto 0);
      signal main_addix_out_r28 : std_logic_vector (31 downto 0);
      signal main_addix_out_r29 : std_logic_vector (31 downto 0);
      signal main_addix_out_r30 : std_logic_vector (31 downto 0);
      signal main_addix_out_r31 : std_logic_vector (31 downto 0);
begin
      addix_i : \Main_addix\ port map (s, std_logic_vector'(B"00000"), main_addix_out);
      addix_i_r1 : \Main_addix\ port map (s, std_logic_vector'(B"00001"), main_addix_out_r1);
      addix_i_r2 : \Main_addix\ port map (s, std_logic_vector'(B"00010"), main_addix_out_r2);
      addix_i_r3 : \Main_addix\ port map (s, std_logic_vector'(B"00011"), main_addix_out_r3);
      addix_i_r4 : \Main_addix\ port map (s, std_logic_vector'(B"00100"), main_addix_out_r4);
      addix_i_r5 : \Main_addix\ port map (s, std_logic_vector'(B"00101"), main_addix_out_r5);
      addix_i_r6 : \Main_addix\ port map (s, std_logic_vector'(B"00110"), main_addix_out_r6);
      addix_i_r7 : \Main_addix\ port map (s, std_logic_vector'(B"00111"), main_addix_out_r7);
      addix_i_r8 : \Main_addix\ port map (s, std_logic_vector'(B"01000"), main_addix_out_r8);
      addix_i_r9 : \Main_addix\ port map (s, std_logic_vector'(B"01001"), main_addix_out_r9);
      addix_i_r10 : \Main_addix\ port map (s, std_logic_vector'(B"01010"), main_addix_out_r10);
      addix_i_r11 : \Main_addix\ port map (s, std_logic_vector'(B"01011"), main_addix_out_r11);
      addix_i_r12 : \Main_addix\ port map (s, std_logic_vector'(B"01100"), main_addix_out_r12);
      addix_i_r13 : \Main_addix\ port map (s, std_logic_vector'(B"01101"), main_addix_out_r13);
      addix_i_r14 : \Main_addix\ port map (s, std_logic_vector'(B"01110"), main_addix_out_r14);
      addix_i_r15 : \Main_addix\ port map (s, std_logic_vector'(B"01111"), main_addix_out_r15);
      addix_i_r16 : \Main_addix\ port map (s, std_logic_vector'(B"10000"), main_addix_out_r16);
      addix_i_r17 : \Main_addix\ port map (s, std_logic_vector'(B"10001"), main_addix_out_r17);
      addix_i_r18 : \Main_addix\ port map (s, std_logic_vector'(B"10010"), main_addix_out_r18);
      addix_i_r19 : \Main_addix\ port map (s, std_logic_vector'(B"10011"), main_addix_out_r19);
      addix_i_r20 : \Main_addix\ port map (s, std_logic_vector'(B"10100"), main_addix_out_r20);
      addix_i_r21 : \Main_addix\ port map (s, std_logic_vector'(B"10101"), main_addix_out_r21);
      addix_i_r22 : \Main_addix\ port map (s, std_logic_vector'(B"10110"), main_addix_out_r22);
      addix_i_r23 : \Main_addix\ port map (s, std_logic_vector'(B"10111"), main_addix_out_r23);
      addix_i_r24 : \Main_addix\ port map (s, std_logic_vector'(B"11000"), main_addix_out_r24);
      addix_i_r25 : \Main_addix\ port map (s, std_logic_vector'(B"11001"), main_addix_out_r25);
      addix_i_r26 : \Main_addix\ port map (s, std_logic_vector'(B"11010"), main_addix_out_r26);
      addix_i_r27 : \Main_addix\ port map (s, std_logic_vector'(B"11011"), main_addix_out_r27);
      addix_i_r28 : \Main_addix\ port map (s, std_logic_vector'(B"11100"), main_addix_out_r28);
      addix_i_r29 : \Main_addix\ port map (s, std_logic_vector'(B"11101"), main_addix_out_r29);
      addix_i_r30 : \Main_addix\ port map (s, std_logic_vector'(B"11110"), main_addix_out_r30);
      addix_i_r31 : \Main_addix\ port map (s, std_logic_vector'(B"11111"), main_addix_out_r31);
      res <= (main_addix_out & main_addix_out_r1 & main_addix_out_r2 & main_addix_out_r3 & main_addix_out_r4 & main_addix_out_r5 & main_addix_out_r6 & main_addix_out_r7 & main_addix_out_r8 & main_addix_out_r9 & main_addix_out_r10 & main_addix_out_r11 & main_addix_out_r12 & main_addix_out_r13 & main_addix_out_r14 & main_addix_out_r15 & main_addix_out_r16 & main_addix_out_r17 & main_addix_out_r18 & main_addix_out_r19 & main_addix_out_r20 & main_addix_out_r21 & main_addix_out_r22 & main_addix_out_r23 & main_addix_out_r24 & main_addix_out_r25 & main_addix_out_r26 & main_addix_out_r27 & main_addix_out_r28 & main_addix_out_r29 & main_addix_out_r30 & main_addix_out_r31);
end architecture;

-- Main.swapix1
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix1\ is
      port (s : in std_logic_vector (1023 downto 0);
            f32 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix1\ is
      component \Main_explode5\ is
            port (w5 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
      signal zt3 : std_logic_vector (0 downto 0);
begin
      explode5_i : \Main_explode5\ port map (f32, main_explode5_out);
      b1 <= main_explode5_out(4 downto 4);
      b2 <= main_explode5_out(3 downto 3);
      k <= main_explode5_out(2 downto 2);
      l <= main_explode5_out(1 downto 1);
      m <= main_explode5_out(0 downto 0);
      zt0 <= rw_not(b1);
      zt1 <= rw_cond(zt0, b2, std_logic_vector'(B"0"));
      zt3 <= rw_cond(zt0, rw_not(b2), std_logic_vector'(B"0"));
      res <= rw_cond(rw_not(zt1), rw_cond(rw_not(zt3), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize(f32, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"01") & k & l & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32)), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"00") & k & l & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32));
end architecture;

-- Main.swapix2
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix2\ is
      port (s : in std_logic_vector (1023 downto 0);
            f32 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix2\ is
      component \Main_explode5\ is
            port (w5 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
begin
      explode5_i : \Main_explode5\ port map (f32, main_explode5_out);
      b1 <= main_explode5_out(4 downto 4);
      j <= main_explode5_out(3 downto 3);
      k <= main_explode5_out(2 downto 2);
      b2 <= main_explode5_out(1 downto 1);
      m <= main_explode5_out(0 downto 0);
      zt0 <= rw_cond(b1, rw_not(b2), std_logic_vector'(B"0"));
      zt1 <= rw_cond(b1, b2, std_logic_vector'(B"0"));
      res <= rw_cond(rw_not(zt0), rw_cond(rw_not(zt1), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize(f32, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"1") & j & k & std_logic_vector'(B"0") & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32)), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"1") & j & k & std_logic_vector'(B"1") & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32));
end architecture;

-- Main.swapix3
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix3\ is
      port (s : in std_logic_vector (1023 downto 0);
            f32 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix3\ is
      component \Main_explode5\ is
            port (w5 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
      signal zt3 : std_logic_vector (0 downto 0);
begin
      explode5_i : \Main_explode5\ port map (f32, main_explode5_out);
      b1 <= main_explode5_out(4 downto 4);
      j <= main_explode5_out(3 downto 3);
      b2 <= main_explode5_out(2 downto 2);
      l <= main_explode5_out(1 downto 1);
      m <= main_explode5_out(0 downto 0);
      zt0 <= rw_not(b1);
      zt1 <= rw_cond(zt0, rw_not(b2), std_logic_vector'(B"0"));
      zt3 <= rw_cond(zt0, b2, std_logic_vector'(B"0"));
      res <= rw_cond(rw_not(zt1), rw_cond(rw_not(zt3), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize(f32, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"0") & j & std_logic_vector'(B"0") & l & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32)), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"0") & j & std_logic_vector'(B"1") & l & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32));
end architecture;

-- Main.swapix4
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix4\ is
      port (s : in std_logic_vector (1023 downto 0);
            f32 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix4\ is
      component \Main_explode5\ is
            port (w5 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
begin
      explode5_i : \Main_explode5\ port map (f32, main_explode5_out);
      b1 <= main_explode5_out(4 downto 4);
      j <= main_explode5_out(3 downto 3);
      k <= main_explode5_out(2 downto 2);
      l <= main_explode5_out(1 downto 1);
      b2 <= main_explode5_out(0 downto 0);
      zt0 <= rw_cond(b1, rw_not(b2), std_logic_vector'(B"0"));
      zt1 <= rw_cond(b1, b2, std_logic_vector'(B"0"));
      res <= rw_cond(rw_not(zt0), rw_cond(rw_not(zt1), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize(f32, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"1") & j & k & l & std_logic_vector'(B"0")), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32)), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"1") & j & k & l & std_logic_vector'(B"1")), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32));
end architecture;

-- Main.xorix
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_xorix\ is
      port (s : in std_logic_vector (1023 downto 0);
            f32 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_xorix\ is
      component \Main_explode5\ is
            port (w5 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal i1 : std_logic_vector (4 downto 0);
      signal h : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
begin
      explode5_i : \Main_explode5\ port map (f32, main_explode5_out);
      j <= main_explode5_out(3 downto 3);
      k <= main_explode5_out(2 downto 2);
      l <= main_explode5_out(1 downto 1);
      m <= main_explode5_out(0 downto 0);
      i1 <= (std_logic_vector'(B"1") & j & k & l & m);
      h <= main_explode5_out(4 downto 4);
      zt1 <= rw_not(h);
      res <= rw_cond(rw_not(zt1), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize(i1, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32), rw_xor(rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize((std_logic_vector'(B"0") & j & k & l & m), 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32), rw_resize(rw_shiftr(s, rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000020"), rw_resize(i1, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32)));
end architecture;

-- Main.xor
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_xor\ is
      port (s : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (1023 downto 0));
end entity;

architecture rtl of \Main_xor\ is
      component \Main_xorix\ is
            port (s : in std_logic_vector (1023 downto 0);
                  f32 : in std_logic_vector (4 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      signal main_xorix_out : std_logic_vector (31 downto 0);
      signal main_xorix_out_r1 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r2 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r3 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r4 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r5 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r6 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r7 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r8 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r9 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r10 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r11 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r12 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r13 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r14 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r15 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r16 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r17 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r18 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r19 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r20 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r21 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r22 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r23 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r24 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r25 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r26 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r27 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r28 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r29 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r30 : std_logic_vector (31 downto 0);
      signal main_xorix_out_r31 : std_logic_vector (31 downto 0);
begin
      xorix_i : \Main_xorix\ port map (s, std_logic_vector'(B"00000"), main_xorix_out);
      xorix_i_r1 : \Main_xorix\ port map (s, std_logic_vector'(B"00001"), main_xorix_out_r1);
      xorix_i_r2 : \Main_xorix\ port map (s, std_logic_vector'(B"00010"), main_xorix_out_r2);
      xorix_i_r3 : \Main_xorix\ port map (s, std_logic_vector'(B"00011"), main_xorix_out_r3);
      xorix_i_r4 : \Main_xorix\ port map (s, std_logic_vector'(B"00100"), main_xorix_out_r4);
      xorix_i_r5 : \Main_xorix\ port map (s, std_logic_vector'(B"00101"), main_xorix_out_r5);
      xorix_i_r6 : \Main_xorix\ port map (s, std_logic_vector'(B"00110"), main_xorix_out_r6);
      xorix_i_r7 : \Main_xorix\ port map (s, std_logic_vector'(B"00111"), main_xorix_out_r7);
      xorix_i_r8 : \Main_xorix\ port map (s, std_logic_vector'(B"01000"), main_xorix_out_r8);
      xorix_i_r9 : \Main_xorix\ port map (s, std_logic_vector'(B"01001"), main_xorix_out_r9);
      xorix_i_r10 : \Main_xorix\ port map (s, std_logic_vector'(B"01010"), main_xorix_out_r10);
      xorix_i_r11 : \Main_xorix\ port map (s, std_logic_vector'(B"01011"), main_xorix_out_r11);
      xorix_i_r12 : \Main_xorix\ port map (s, std_logic_vector'(B"01100"), main_xorix_out_r12);
      xorix_i_r13 : \Main_xorix\ port map (s, std_logic_vector'(B"01101"), main_xorix_out_r13);
      xorix_i_r14 : \Main_xorix\ port map (s, std_logic_vector'(B"01110"), main_xorix_out_r14);
      xorix_i_r15 : \Main_xorix\ port map (s, std_logic_vector'(B"01111"), main_xorix_out_r15);
      xorix_i_r16 : \Main_xorix\ port map (s, std_logic_vector'(B"10000"), main_xorix_out_r16);
      xorix_i_r17 : \Main_xorix\ port map (s, std_logic_vector'(B"10001"), main_xorix_out_r17);
      xorix_i_r18 : \Main_xorix\ port map (s, std_logic_vector'(B"10010"), main_xorix_out_r18);
      xorix_i_r19 : \Main_xorix\ port map (s, std_logic_vector'(B"10011"), main_xorix_out_r19);
      xorix_i_r20 : \Main_xorix\ port map (s, std_logic_vector'(B"10100"), main_xorix_out_r20);
      xorix_i_r21 : \Main_xorix\ port map (s, std_logic_vector'(B"10101"), main_xorix_out_r21);
      xorix_i_r22 : \Main_xorix\ port map (s, std_logic_vector'(B"10110"), main_xorix_out_r22);
      xorix_i_r23 : \Main_xorix\ port map (s, std_logic_vector'(B"10111"), main_xorix_out_r23);
      xorix_i_r24 : \Main_xorix\ port map (s, std_logic_vector'(B"11000"), main_xorix_out_r24);
      xorix_i_r25 : \Main_xorix\ port map (s, std_logic_vector'(B"11001"), main_xorix_out_r25);
      xorix_i_r26 : \Main_xorix\ port map (s, std_logic_vector'(B"11010"), main_xorix_out_r26);
      xorix_i_r27 : \Main_xorix\ port map (s, std_logic_vector'(B"11011"), main_xorix_out_r27);
      xorix_i_r28 : \Main_xorix\ port map (s, std_logic_vector'(B"11100"), main_xorix_out_r28);
      xorix_i_r29 : \Main_xorix\ port map (s, std_logic_vector'(B"11101"), main_xorix_out_r29);
      xorix_i_r30 : \Main_xorix\ port map (s, std_logic_vector'(B"11110"), main_xorix_out_r30);
      xorix_i_r31 : \Main_xorix\ port map (s, std_logic_vector'(B"11111"), main_xorix_out_r31);
      res <= (main_xorix_out & main_xorix_out_r1 & main_xorix_out_r2 & main_xorix_out_r3 & main_xorix_out_r4 & main_xorix_out_r5 & main_xorix_out_r6 & main_xorix_out_r7 & main_xorix_out_r8 & main_xorix_out_r9 & main_xorix_out_r10 & main_xorix_out_r11 & main_xorix_out_r12 & main_xorix_out_r13 & main_xorix_out_r14 & main_xorix_out_r15 & main_xorix_out_r16 & main_xorix_out_r17 & main_xorix_out_r18 & main_xorix_out_r19 & main_xorix_out_r20 & main_xorix_out_r21 & main_xorix_out_r22 & main_xorix_out_r23 & main_xorix_out_r24 & main_xorix_out_r25 & main_xorix_out_r26 & main_xorix_out_r27 & main_xorix_out_r28 & main_xorix_out_r29 & main_xorix_out_r30 & main_xorix_out_r31);
end architecture;