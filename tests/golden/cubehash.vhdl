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
      port (arg0 : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (1023 downto 0));
      end component;
      component \Main_rot\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (1023 downto 0);
            arg2 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix1\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix2\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix3\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_swapix4\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_xor\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (1023 downto 0));
      end component;
      component \main_$L___unused5$838\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (2047 downto 0));
      end component;
      signal \__st0\ : std_logic_vector (1023 downto 0) := std_logic_vector'(B"0000000000000000000000000010000000000000000000000000000000000001000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (1023 downto 0);
      signal main_add_out : std_logic_vector (1023 downto 0);
      signal zi0 : std_logic_vector (1023 downto 0);
      signal main_rot_out : std_logic_vector (31 downto 0);
      signal \main_rot_outR1\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR2\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR3\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR4\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR5\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR6\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR7\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR8\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR9\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR10\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR11\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR12\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR13\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR14\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR15\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR16\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR17\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR18\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR19\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR20\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR21\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR22\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR23\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR24\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR25\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR26\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR27\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR28\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR29\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR30\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR31\ : std_logic_vector (31 downto 0);
      signal zi1 : std_logic_vector (1023 downto 0);
      signal main_swapix1_out : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR1\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR2\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR3\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR4\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR5\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR6\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR7\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR8\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR9\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR10\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR11\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR12\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR13\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR14\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR15\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR16\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR17\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR18\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR19\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR20\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR21\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR22\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR23\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR24\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR25\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR26\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR27\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR28\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR29\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR30\ : std_logic_vector (31 downto 0);
      signal \main_swapix1_outR31\ : std_logic_vector (31 downto 0);
      signal conn : std_logic_vector (1023 downto 0);
      signal main_xor_out : std_logic_vector (1023 downto 0);
      signal zi2 : std_logic_vector (1023 downto 0);
      signal main_swapix2_out : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR1\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR2\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR3\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR4\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR5\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR6\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR7\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR8\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR9\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR10\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR11\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR12\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR13\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR14\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR15\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR16\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR17\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR18\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR19\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR20\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR21\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR22\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR23\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR24\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR25\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR26\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR27\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR28\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR29\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR30\ : std_logic_vector (31 downto 0);
      signal \main_swapix2_outR31\ : std_logic_vector (31 downto 0);
      signal \connR1\ : std_logic_vector (1023 downto 0);
      signal \main_add_outR1\ : std_logic_vector (1023 downto 0);
      signal zi3 : std_logic_vector (1023 downto 0);
      signal \main_rot_outR32\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR33\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR34\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR35\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR36\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR37\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR38\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR39\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR40\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR41\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR42\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR43\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR44\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR45\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR46\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR47\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR48\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR49\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR50\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR51\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR52\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR53\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR54\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR55\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR56\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR57\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR58\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR59\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR60\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR61\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR62\ : std_logic_vector (31 downto 0);
      signal \main_rot_outR63\ : std_logic_vector (31 downto 0);
      signal zi4 : std_logic_vector (1023 downto 0);
      signal main_swapix3_out : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR1\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR2\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR3\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR4\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR5\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR6\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR7\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR8\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR9\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR10\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR11\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR12\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR13\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR14\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR15\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR16\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR17\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR18\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR19\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR20\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR21\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR22\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR23\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR24\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR25\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR26\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR27\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR28\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR29\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR30\ : std_logic_vector (31 downto 0);
      signal \main_swapix3_outR31\ : std_logic_vector (31 downto 0);
      signal \connR2\ : std_logic_vector (1023 downto 0);
      signal \main_xor_outR1\ : std_logic_vector (1023 downto 0);
      signal zi5 : std_logic_vector (1023 downto 0);
      signal main_swapix4_out : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR1\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR2\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR3\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR4\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR5\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR6\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR7\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR8\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR9\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR10\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR11\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR12\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR13\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR14\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR15\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR16\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR17\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR18\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR19\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR20\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR21\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR22\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR23\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR24\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR25\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR26\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR27\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR28\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR29\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR30\ : std_logic_vector (31 downto 0);
      signal \main_swapix4_outR31\ : std_logic_vector (31 downto 0);
      signal zi6 : std_logic_vector (1023 downto 0);
      signal \main_$l__unused5$838_out\ : std_logic_vector (2047 downto 0);
      signal \main_$l__unused5$838_outR1\ : std_logic_vector (2047 downto 0);
      signal zres : std_logic_vector (2047 downto 0);
begin
inst : \Main_add\ port map (\__st0\, main_add_out);
      zi0 <= main_add_out;
      \instR1\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00000"), main_rot_out);
      \instR2\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00001"), \main_rot_outR1\);
      \instR3\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00010"), \main_rot_outR2\);
      \instR4\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00011"), \main_rot_outR3\);
      \instR5\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00100"), \main_rot_outR4\);
      \instR6\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00101"), \main_rot_outR5\);
      \instR7\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00110"), \main_rot_outR6\);
      \instR8\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"00111"), \main_rot_outR7\);
      \instR9\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01000"), \main_rot_outR8\);
      \instR10\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01001"), \main_rot_outR9\);
      \instR11\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01010"), \main_rot_outR10\);
      \instR12\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01011"), \main_rot_outR11\);
      \instR13\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01100"), \main_rot_outR12\);
      \instR14\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01101"), \main_rot_outR13\);
      \instR15\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01110"), \main_rot_outR14\);
      \instR16\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"01111"), \main_rot_outR15\);
      \instR17\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10000"), \main_rot_outR16\);
      \instR18\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10001"), \main_rot_outR17\);
      \instR19\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10010"), \main_rot_outR18\);
      \instR20\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10011"), \main_rot_outR19\);
      \instR21\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10100"), \main_rot_outR20\);
      \instR22\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10101"), \main_rot_outR21\);
      \instR23\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10110"), \main_rot_outR22\);
      \instR24\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"10111"), \main_rot_outR23\);
      \instR25\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11000"), \main_rot_outR24\);
      \instR26\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11001"), \main_rot_outR25\);
      \instR27\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11010"), \main_rot_outR26\);
      \instR28\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11011"), \main_rot_outR27\);
      \instR29\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11100"), \main_rot_outR28\);
      \instR30\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11101"), \main_rot_outR29\);
      \instR31\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11110"), \main_rot_outR30\);
      \instR32\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000000111"), zi0, std_logic_vector'(B"11111"), \main_rot_outR31\);
      zi1 <= (main_rot_out & \main_rot_outR1\ & \main_rot_outR2\ & \main_rot_outR3\ & \main_rot_outR4\ & \main_rot_outR5\ & \main_rot_outR6\ & \main_rot_outR7\ & \main_rot_outR8\ & \main_rot_outR9\ & \main_rot_outR10\ & \main_rot_outR11\ & \main_rot_outR12\ & \main_rot_outR13\ & \main_rot_outR14\ & \main_rot_outR15\ & \main_rot_outR16\ & \main_rot_outR17\ & \main_rot_outR18\ & \main_rot_outR19\ & \main_rot_outR20\ & \main_rot_outR21\ & \main_rot_outR22\ & \main_rot_outR23\ & \main_rot_outR24\ & \main_rot_outR25\ & \main_rot_outR26\ & \main_rot_outR27\ & \main_rot_outR28\ & \main_rot_outR29\ & \main_rot_outR30\ & \main_rot_outR31\);
      \instR33\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00000"), main_swapix1_out);
      \instR34\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00001"), \main_swapix1_outR1\);
      \instR35\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00010"), \main_swapix1_outR2\);
      \instR36\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00011"), \main_swapix1_outR3\);
      \instR37\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00100"), \main_swapix1_outR4\);
      \instR38\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00101"), \main_swapix1_outR5\);
      \instR39\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00110"), \main_swapix1_outR6\);
      \instR40\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00111"), \main_swapix1_outR7\);
      \instR41\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01000"), \main_swapix1_outR8\);
      \instR42\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01001"), \main_swapix1_outR9\);
      \instR43\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01010"), \main_swapix1_outR10\);
      \instR44\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01011"), \main_swapix1_outR11\);
      \instR45\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01100"), \main_swapix1_outR12\);
      \instR46\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01101"), \main_swapix1_outR13\);
      \instR47\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01110"), \main_swapix1_outR14\);
      \instR48\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01111"), \main_swapix1_outR15\);
      \instR49\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10000"), \main_swapix1_outR16\);
      \instR50\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10001"), \main_swapix1_outR17\);
      \instR51\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10010"), \main_swapix1_outR18\);
      \instR52\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10011"), \main_swapix1_outR19\);
      \instR53\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10100"), \main_swapix1_outR20\);
      \instR54\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10101"), \main_swapix1_outR21\);
      \instR55\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10110"), \main_swapix1_outR22\);
      \instR56\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10111"), \main_swapix1_outR23\);
      \instR57\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11000"), \main_swapix1_outR24\);
      \instR58\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11001"), \main_swapix1_outR25\);
      \instR59\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11010"), \main_swapix1_outR26\);
      \instR60\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11011"), \main_swapix1_outR27\);
      \instR61\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11100"), \main_swapix1_outR28\);
      \instR62\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11101"), \main_swapix1_outR29\);
      \instR63\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11110"), \main_swapix1_outR30\);
      \instR64\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11111"), \main_swapix1_outR31\);
      conn <= (main_swapix1_out & \main_swapix1_outR1\ & \main_swapix1_outR2\ & \main_swapix1_outR3\ & \main_swapix1_outR4\ & \main_swapix1_outR5\ & \main_swapix1_outR6\ & \main_swapix1_outR7\ & \main_swapix1_outR8\ & \main_swapix1_outR9\ & \main_swapix1_outR10\ & \main_swapix1_outR11\ & \main_swapix1_outR12\ & \main_swapix1_outR13\ & \main_swapix1_outR14\ & \main_swapix1_outR15\ & \main_swapix1_outR16\ & \main_swapix1_outR17\ & \main_swapix1_outR18\ & \main_swapix1_outR19\ & \main_swapix1_outR20\ & \main_swapix1_outR21\ & \main_swapix1_outR22\ & \main_swapix1_outR23\ & \main_swapix1_outR24\ & \main_swapix1_outR25\ & \main_swapix1_outR26\ & \main_swapix1_outR27\ & \main_swapix1_outR28\ & \main_swapix1_outR29\ & \main_swapix1_outR30\ & \main_swapix1_outR31\);
      \instR65\ : \Main_xor\ port map (conn, main_xor_out);
      zi2 <= main_xor_out;
      \instR66\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00000"), main_swapix2_out);
      \instR67\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00001"), \main_swapix2_outR1\);
      \instR68\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00010"), \main_swapix2_outR2\);
      \instR69\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00011"), \main_swapix2_outR3\);
      \instR70\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00100"), \main_swapix2_outR4\);
      \instR71\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00101"), \main_swapix2_outR5\);
      \instR72\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00110"), \main_swapix2_outR6\);
      \instR73\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00111"), \main_swapix2_outR7\);
      \instR74\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01000"), \main_swapix2_outR8\);
      \instR75\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01001"), \main_swapix2_outR9\);
      \instR76\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01010"), \main_swapix2_outR10\);
      \instR77\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01011"), \main_swapix2_outR11\);
      \instR78\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01100"), \main_swapix2_outR12\);
      \instR79\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01101"), \main_swapix2_outR13\);
      \instR80\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01110"), \main_swapix2_outR14\);
      \instR81\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01111"), \main_swapix2_outR15\);
      \instR82\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10000"), \main_swapix2_outR16\);
      \instR83\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10001"), \main_swapix2_outR17\);
      \instR84\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10010"), \main_swapix2_outR18\);
      \instR85\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10011"), \main_swapix2_outR19\);
      \instR86\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10100"), \main_swapix2_outR20\);
      \instR87\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10101"), \main_swapix2_outR21\);
      \instR88\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10110"), \main_swapix2_outR22\);
      \instR89\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10111"), \main_swapix2_outR23\);
      \instR90\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11000"), \main_swapix2_outR24\);
      \instR91\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11001"), \main_swapix2_outR25\);
      \instR92\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11010"), \main_swapix2_outR26\);
      \instR93\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11011"), \main_swapix2_outR27\);
      \instR94\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11100"), \main_swapix2_outR28\);
      \instR95\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11101"), \main_swapix2_outR29\);
      \instR96\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11110"), \main_swapix2_outR30\);
      \instR97\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11111"), \main_swapix2_outR31\);
      \connR1\ <= (main_swapix2_out & \main_swapix2_outR1\ & \main_swapix2_outR2\ & \main_swapix2_outR3\ & \main_swapix2_outR4\ & \main_swapix2_outR5\ & \main_swapix2_outR6\ & \main_swapix2_outR7\ & \main_swapix2_outR8\ & \main_swapix2_outR9\ & \main_swapix2_outR10\ & \main_swapix2_outR11\ & \main_swapix2_outR12\ & \main_swapix2_outR13\ & \main_swapix2_outR14\ & \main_swapix2_outR15\ & \main_swapix2_outR16\ & \main_swapix2_outR17\ & \main_swapix2_outR18\ & \main_swapix2_outR19\ & \main_swapix2_outR20\ & \main_swapix2_outR21\ & \main_swapix2_outR22\ & \main_swapix2_outR23\ & \main_swapix2_outR24\ & \main_swapix2_outR25\ & \main_swapix2_outR26\ & \main_swapix2_outR27\ & \main_swapix2_outR28\ & \main_swapix2_outR29\ & \main_swapix2_outR30\ & \main_swapix2_outR31\);
      \instR98\ : \Main_add\ port map (\connR1\, \main_add_outR1\);
      zi3 <= \main_add_outR1\;
      \instR99\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00000"), \main_rot_outR32\);
      \instR100\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00001"), \main_rot_outR33\);
      \instR101\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00010"), \main_rot_outR34\);
      \instR102\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00011"), \main_rot_outR35\);
      \instR103\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00100"), \main_rot_outR36\);
      \instR104\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00101"), \main_rot_outR37\);
      \instR105\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00110"), \main_rot_outR38\);
      \instR106\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"00111"), \main_rot_outR39\);
      \instR107\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01000"), \main_rot_outR40\);
      \instR108\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01001"), \main_rot_outR41\);
      \instR109\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01010"), \main_rot_outR42\);
      \instR110\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01011"), \main_rot_outR43\);
      \instR111\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01100"), \main_rot_outR44\);
      \instR112\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01101"), \main_rot_outR45\);
      \instR113\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01110"), \main_rot_outR46\);
      \instR114\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"01111"), \main_rot_outR47\);
      \instR115\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10000"), \main_rot_outR48\);
      \instR116\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10001"), \main_rot_outR49\);
      \instR117\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10010"), \main_rot_outR50\);
      \instR118\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10011"), \main_rot_outR51\);
      \instR119\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10100"), \main_rot_outR52\);
      \instR120\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10101"), \main_rot_outR53\);
      \instR121\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10110"), \main_rot_outR54\);
      \instR122\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"10111"), \main_rot_outR55\);
      \instR123\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11000"), \main_rot_outR56\);
      \instR124\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11001"), \main_rot_outR57\);
      \instR125\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11010"), \main_rot_outR58\);
      \instR126\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11011"), \main_rot_outR59\);
      \instR127\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11100"), \main_rot_outR60\);
      \instR128\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11101"), \main_rot_outR61\);
      \instR129\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11110"), \main_rot_outR62\);
      \instR130\ : \Main_rot\ port map (std_logic_vector'(B"00000000000000000000000000001011"), zi3, std_logic_vector'(B"11111"), \main_rot_outR63\);
      zi4 <= (\main_rot_outR32\ & \main_rot_outR33\ & \main_rot_outR34\ & \main_rot_outR35\ & \main_rot_outR36\ & \main_rot_outR37\ & \main_rot_outR38\ & \main_rot_outR39\ & \main_rot_outR40\ & \main_rot_outR41\ & \main_rot_outR42\ & \main_rot_outR43\ & \main_rot_outR44\ & \main_rot_outR45\ & \main_rot_outR46\ & \main_rot_outR47\ & \main_rot_outR48\ & \main_rot_outR49\ & \main_rot_outR50\ & \main_rot_outR51\ & \main_rot_outR52\ & \main_rot_outR53\ & \main_rot_outR54\ & \main_rot_outR55\ & \main_rot_outR56\ & \main_rot_outR57\ & \main_rot_outR58\ & \main_rot_outR59\ & \main_rot_outR60\ & \main_rot_outR61\ & \main_rot_outR62\ & \main_rot_outR63\);
      \instR131\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00000"), main_swapix3_out);
      \instR132\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00001"), \main_swapix3_outR1\);
      \instR133\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00010"), \main_swapix3_outR2\);
      \instR134\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00011"), \main_swapix3_outR3\);
      \instR135\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00100"), \main_swapix3_outR4\);
      \instR136\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00101"), \main_swapix3_outR5\);
      \instR137\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00110"), \main_swapix3_outR6\);
      \instR138\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00111"), \main_swapix3_outR7\);
      \instR139\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01000"), \main_swapix3_outR8\);
      \instR140\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01001"), \main_swapix3_outR9\);
      \instR141\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01010"), \main_swapix3_outR10\);
      \instR142\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01011"), \main_swapix3_outR11\);
      \instR143\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01100"), \main_swapix3_outR12\);
      \instR144\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01101"), \main_swapix3_outR13\);
      \instR145\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01110"), \main_swapix3_outR14\);
      \instR146\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01111"), \main_swapix3_outR15\);
      \instR147\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10000"), \main_swapix3_outR16\);
      \instR148\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10001"), \main_swapix3_outR17\);
      \instR149\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10010"), \main_swapix3_outR18\);
      \instR150\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10011"), \main_swapix3_outR19\);
      \instR151\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10100"), \main_swapix3_outR20\);
      \instR152\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10101"), \main_swapix3_outR21\);
      \instR153\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10110"), \main_swapix3_outR22\);
      \instR154\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10111"), \main_swapix3_outR23\);
      \instR155\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11000"), \main_swapix3_outR24\);
      \instR156\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11001"), \main_swapix3_outR25\);
      \instR157\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11010"), \main_swapix3_outR26\);
      \instR158\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11011"), \main_swapix3_outR27\);
      \instR159\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11100"), \main_swapix3_outR28\);
      \instR160\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11101"), \main_swapix3_outR29\);
      \instR161\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11110"), \main_swapix3_outR30\);
      \instR162\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11111"), \main_swapix3_outR31\);
      \connR2\ <= (main_swapix3_out & \main_swapix3_outR1\ & \main_swapix3_outR2\ & \main_swapix3_outR3\ & \main_swapix3_outR4\ & \main_swapix3_outR5\ & \main_swapix3_outR6\ & \main_swapix3_outR7\ & \main_swapix3_outR8\ & \main_swapix3_outR9\ & \main_swapix3_outR10\ & \main_swapix3_outR11\ & \main_swapix3_outR12\ & \main_swapix3_outR13\ & \main_swapix3_outR14\ & \main_swapix3_outR15\ & \main_swapix3_outR16\ & \main_swapix3_outR17\ & \main_swapix3_outR18\ & \main_swapix3_outR19\ & \main_swapix3_outR20\ & \main_swapix3_outR21\ & \main_swapix3_outR22\ & \main_swapix3_outR23\ & \main_swapix3_outR24\ & \main_swapix3_outR25\ & \main_swapix3_outR26\ & \main_swapix3_outR27\ & \main_swapix3_outR28\ & \main_swapix3_outR29\ & \main_swapix3_outR30\ & \main_swapix3_outR31\);
      \instR163\ : \Main_xor\ port map (\connR2\, \main_xor_outR1\);
      zi5 <= \main_xor_outR1\;
      \instR164\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00000"), main_swapix4_out);
      \instR165\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00001"), \main_swapix4_outR1\);
      \instR166\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00010"), \main_swapix4_outR2\);
      \instR167\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00011"), \main_swapix4_outR3\);
      \instR168\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00100"), \main_swapix4_outR4\);
      \instR169\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00101"), \main_swapix4_outR5\);
      \instR170\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00110"), \main_swapix4_outR6\);
      \instR171\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00111"), \main_swapix4_outR7\);
      \instR172\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01000"), \main_swapix4_outR8\);
      \instR173\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01001"), \main_swapix4_outR9\);
      \instR174\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01010"), \main_swapix4_outR10\);
      \instR175\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01011"), \main_swapix4_outR11\);
      \instR176\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01100"), \main_swapix4_outR12\);
      \instR177\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01101"), \main_swapix4_outR13\);
      \instR178\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01110"), \main_swapix4_outR14\);
      \instR179\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01111"), \main_swapix4_outR15\);
      \instR180\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10000"), \main_swapix4_outR16\);
      \instR181\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10001"), \main_swapix4_outR17\);
      \instR182\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10010"), \main_swapix4_outR18\);
      \instR183\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10011"), \main_swapix4_outR19\);
      \instR184\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10100"), \main_swapix4_outR20\);
      \instR185\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10101"), \main_swapix4_outR21\);
      \instR186\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10110"), \main_swapix4_outR22\);
      \instR187\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10111"), \main_swapix4_outR23\);
      \instR188\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11000"), \main_swapix4_outR24\);
      \instR189\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11001"), \main_swapix4_outR25\);
      \instR190\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11010"), \main_swapix4_outR26\);
      \instR191\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11011"), \main_swapix4_outR27\);
      \instR192\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11100"), \main_swapix4_outR28\);
      \instR193\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11101"), \main_swapix4_outR29\);
      \instR194\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11110"), \main_swapix4_outR30\);
      \instR195\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11111"), \main_swapix4_outR31\);
      zi6 <= (main_swapix4_out & \main_swapix4_outR1\ & \main_swapix4_outR2\ & \main_swapix4_outR3\ & \main_swapix4_outR4\ & \main_swapix4_outR5\ & \main_swapix4_outR6\ & \main_swapix4_outR7\ & \main_swapix4_outR8\ & \main_swapix4_outR9\ & \main_swapix4_outR10\ & \main_swapix4_outR11\ & \main_swapix4_outR12\ & \main_swapix4_outR13\ & \main_swapix4_outR14\ & \main_swapix4_outR15\ & \main_swapix4_outR16\ & \main_swapix4_outR17\ & \main_swapix4_outR18\ & \main_swapix4_outR19\ & \main_swapix4_outR20\ & \main_swapix4_outR21\ & \main_swapix4_outR22\ & \main_swapix4_outR23\ & \main_swapix4_outR24\ & \main_swapix4_outR25\ & \main_swapix4_outR26\ & \main_swapix4_outR27\ & \main_swapix4_outR28\ & \main_swapix4_outR29\ & \main_swapix4_outR30\ & \main_swapix4_outR31\);
      \instR196\ : \main_$L___unused5$838\ port map (zi6, \main_$l__unused5$838_out\);
      \instR197\ : \main_$L___unused5$838\ port map (\__st0\, \main_$l__unused5$838_outR1\);
      zres <= rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), \main_$l__unused5$838_out\, \main_$l__unused5$838_outR1\);
      \__st0_next\ <= zres(1023 downto 0);
      \__out0\ <= zres(2047 downto 1024);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"0000000000000000000000000010000000000000000000000000000000000001000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_$L___unused5$838\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      res : out std_logic_vector (2047 downto 0));
end entity;

architecture rtl of \main_$L___unused5$838\ is

begin
res <= (arg0 & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_explode5\ is
port (arg0 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \Main_explode5\ is

begin
res <= (rw_resize(rw_shiftr(arg0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100")), 1) & rw_resize(rw_shiftr(arg0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011")), 1) & rw_resize(rw_shiftr(arg0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010")), 1) & rw_resize(rw_shiftr(arg0, std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), 1) & rw_resize(rw_shiftr(arg0, rw_repl(128, std_logic_vector'(B"0"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_rot\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (1023 downto 0);
      arg2 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_rot\ is
component \Main_explode5\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal zi0 : std_logic_vector (4 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal ds : std_logic_vector (32 downto 0);
      signal w0 : std_logic_vector (31 downto 0);
      signal \w0R1\ : std_logic_vector (31 downto 0);
      signal h : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
begin
inst : \Main_explode5\ port map (arg2, main_explode5_out);
      zi0 <= main_explode5_out;
      zi1 <= zi0(4 downto 4);
      ds <= (zi1 & rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg2, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
      w0 <= ds(31 downto 0);
      \w0R1\ <= w0;
      h <= ds(32 downto 32);
      zt0 <= h;
      res <= rw_cond(rw_eq(zt0, std_logic_vector'(B"0")), rw_or(rw_shiftl(\w0R1\, arg0), rw_shiftr(\w0R1\, rw_sub(std_logic_vector'(B"00000000000000000000000000100000"), arg0))), \w0R1\);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_addix\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_addix\ is
component \Main_explode5\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal zi0 : std_logic_vector (4 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal ds : std_logic_vector (64 downto 0);
      signal v0 : std_logic_vector (31 downto 0);
      signal \v0R1\ : std_logic_vector (31 downto 0);
      signal h : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal v1 : std_logic_vector (31 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      zi0 <= main_explode5_out;
      zi1 <= zi0(3 downto 3);
      zi2 <= zi1;
      zi3 <= zi0(2 downto 2);
      zi4 <= zi3;
      zi5 <= zi0(1 downto 1);
      zi6 <= zi5;
      zi7 <= zi0(0 downto 0);
      zi8 <= zi7;
      zi9 <= zi0(4 downto 4);
      ds <= (zi9 & rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"0") & zi2 & zi4 & zi6 & zi8), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32) & rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"1") & zi2 & zi4 & zi6 & zi8), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
      v0 <= ds(63 downto 32);
      \v0R1\ <= v0;
      h <= ds(64 downto 64);
      zt0 <= h;
      v1 <= ds(31 downto 0);
      res <= rw_cond(rw_eq(zt0, std_logic_vector'(B"0")), \v0R1\, rw_add(\v0R1\, v1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_add\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      res : out std_logic_vector (1023 downto 0));
end entity;

architecture rtl of \Main_add\ is
component \Main_addix\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal main_addix_out : std_logic_vector (31 downto 0);
      signal \main_addix_outR1\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR2\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR3\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR4\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR5\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR6\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR7\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR8\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR9\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR10\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR11\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR12\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR13\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR14\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR15\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR16\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR17\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR18\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR19\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR20\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR21\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR22\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR23\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR24\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR25\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR26\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR27\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR28\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR29\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR30\ : std_logic_vector (31 downto 0);
      signal \main_addix_outR31\ : std_logic_vector (31 downto 0);
begin
inst : \Main_addix\ port map (arg0, std_logic_vector'(B"00000"), main_addix_out);
      \instR1\ : \Main_addix\ port map (arg0, std_logic_vector'(B"00001"), \main_addix_outR1\);
      \instR2\ : \Main_addix\ port map (arg0, std_logic_vector'(B"00010"), \main_addix_outR2\);
      \instR3\ : \Main_addix\ port map (arg0, std_logic_vector'(B"00011"), \main_addix_outR3\);
      \instR4\ : \Main_addix\ port map (arg0, std_logic_vector'(B"00100"), \main_addix_outR4\);
      \instR5\ : \Main_addix\ port map (arg0, std_logic_vector'(B"00101"), \main_addix_outR5\);
      \instR6\ : \Main_addix\ port map (arg0, std_logic_vector'(B"00110"), \main_addix_outR6\);
      \instR7\ : \Main_addix\ port map (arg0, std_logic_vector'(B"00111"), \main_addix_outR7\);
      \instR8\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01000"), \main_addix_outR8\);
      \instR9\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01001"), \main_addix_outR9\);
      \instR10\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01010"), \main_addix_outR10\);
      \instR11\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01011"), \main_addix_outR11\);
      \instR12\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01100"), \main_addix_outR12\);
      \instR13\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01101"), \main_addix_outR13\);
      \instR14\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01110"), \main_addix_outR14\);
      \instR15\ : \Main_addix\ port map (arg0, std_logic_vector'(B"01111"), \main_addix_outR15\);
      \instR16\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10000"), \main_addix_outR16\);
      \instR17\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10001"), \main_addix_outR17\);
      \instR18\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10010"), \main_addix_outR18\);
      \instR19\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10011"), \main_addix_outR19\);
      \instR20\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10100"), \main_addix_outR20\);
      \instR21\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10101"), \main_addix_outR21\);
      \instR22\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10110"), \main_addix_outR22\);
      \instR23\ : \Main_addix\ port map (arg0, std_logic_vector'(B"10111"), \main_addix_outR23\);
      \instR24\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11000"), \main_addix_outR24\);
      \instR25\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11001"), \main_addix_outR25\);
      \instR26\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11010"), \main_addix_outR26\);
      \instR27\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11011"), \main_addix_outR27\);
      \instR28\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11100"), \main_addix_outR28\);
      \instR29\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11101"), \main_addix_outR29\);
      \instR30\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11110"), \main_addix_outR30\);
      \instR31\ : \Main_addix\ port map (arg0, std_logic_vector'(B"11111"), \main_addix_outR31\);
      res <= (main_addix_out & \main_addix_outR1\ & \main_addix_outR2\ & \main_addix_outR3\ & \main_addix_outR4\ & \main_addix_outR5\ & \main_addix_outR6\ & \main_addix_outR7\ & \main_addix_outR8\ & \main_addix_outR9\ & \main_addix_outR10\ & \main_addix_outR11\ & \main_addix_outR12\ & \main_addix_outR13\ & \main_addix_outR14\ & \main_addix_outR15\ & \main_addix_outR16\ & \main_addix_outR17\ & \main_addix_outR18\ & \main_addix_outR19\ & \main_addix_outR20\ & \main_addix_outR21\ & \main_addix_outR22\ & \main_addix_outR23\ & \main_addix_outR24\ & \main_addix_outR25\ & \main_addix_outR26\ & \main_addix_outR27\ & \main_addix_outR28\ & \main_addix_outR29\ & \main_addix_outR30\ & \main_addix_outR31\);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix1\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix1\ is
component \Main_explode5\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal ds : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal \b1R1\ : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal \b2R1\ : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal \kR1\ : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal \lR1\ : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal \mR1\ : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
      signal zt2 : std_logic_vector (0 downto 0);
      signal zt3 : std_logic_vector (0 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      ds <= main_explode5_out;
      b1 <= ds(4 downto 4);
      \b1R1\ <= b1;
      b2 <= ds(3 downto 3);
      \b2R1\ <= b2;
      k <= ds(2 downto 2);
      \kR1\ <= k;
      l <= ds(1 downto 1);
      \lR1\ <= l;
      m <= ds(0 downto 0);
      \mR1\ <= m;
      zt0 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
      zt1 <= rw_cond(rw_eq(zt0, std_logic_vector'(B"1")), \b2R1\, std_logic_vector'(B"0"));
      zt2 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
      zt3 <= rw_cond(rw_eq(zt2, std_logic_vector'(B"1")), rw_cond(rw_eq(\b2R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1")), std_logic_vector'(B"0"));
      res <= rw_cond(rw_eq(zt1, std_logic_vector'(B"0")), rw_cond(rw_eq(zt3, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"01") & \kR1\ & \lR1\ & \mR1\), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32)), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"00") & \kR1\ & \lR1\ & \mR1\), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix2\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix2\ is
component \Main_explode5\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal ds : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal \b1R1\ : std_logic_vector (0 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal \jR1\ : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal \kR1\ : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal \b2R1\ : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal \mR1\ : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      ds <= main_explode5_out;
      b1 <= ds(4 downto 4);
      \b1R1\ <= b1;
      j <= ds(3 downto 3);
      \jR1\ <= j;
      k <= ds(2 downto 2);
      \kR1\ <= k;
      b2 <= ds(1 downto 1);
      \b2R1\ <= b2;
      m <= ds(0 downto 0);
      \mR1\ <= m;
      zt0 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), rw_cond(rw_eq(\b2R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1")), std_logic_vector'(B"0"));
      zt1 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), \b2R1\, std_logic_vector'(B"0"));
      res <= rw_cond(rw_eq(zt0, std_logic_vector'(B"0")), rw_cond(rw_eq(zt1, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"1") & \jR1\ & \kR1\ & std_logic_vector'(B"0") & \mR1\), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32)), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"1") & \jR1\ & \kR1\ & std_logic_vector'(B"1") & \mR1\), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix3\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix3\ is
component \Main_explode5\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal ds : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal \b1R1\ : std_logic_vector (0 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal \jR1\ : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal \b2R1\ : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal \lR1\ : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal \mR1\ : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
      signal zt2 : std_logic_vector (0 downto 0);
      signal zt3 : std_logic_vector (0 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      ds <= main_explode5_out;
      b1 <= ds(4 downto 4);
      \b1R1\ <= b1;
      j <= ds(3 downto 3);
      \jR1\ <= j;
      b2 <= ds(2 downto 2);
      \b2R1\ <= b2;
      l <= ds(1 downto 1);
      \lR1\ <= l;
      m <= ds(0 downto 0);
      \mR1\ <= m;
      zt0 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
      zt1 <= rw_cond(rw_eq(zt0, std_logic_vector'(B"1")), rw_cond(rw_eq(\b2R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1")), std_logic_vector'(B"0"));
      zt2 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
      zt3 <= rw_cond(rw_eq(zt2, std_logic_vector'(B"1")), \b2R1\, std_logic_vector'(B"0"));
      res <= rw_cond(rw_eq(zt1, std_logic_vector'(B"0")), rw_cond(rw_eq(zt3, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"0") & \jR1\ & std_logic_vector'(B"0") & \lR1\ & \mR1\), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32)), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"0") & \jR1\ & std_logic_vector'(B"1") & \lR1\ & \mR1\), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_swapix4\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_swapix4\ is
component \Main_explode5\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal ds : std_logic_vector (4 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal \b1R1\ : std_logic_vector (0 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal \jR1\ : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal \kR1\ : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal \lR1\ : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal \b2R1\ : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      ds <= main_explode5_out;
      b1 <= ds(4 downto 4);
      \b1R1\ <= b1;
      j <= ds(3 downto 3);
      \jR1\ <= j;
      k <= ds(2 downto 2);
      \kR1\ <= k;
      l <= ds(1 downto 1);
      \lR1\ <= l;
      b2 <= ds(0 downto 0);
      \b2R1\ <= b2;
      zt0 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), rw_cond(rw_eq(\b2R1\, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1")), std_logic_vector'(B"0"));
      zt1 <= rw_cond(rw_eq(\b1R1\, std_logic_vector'(B"1")), \b2R1\, std_logic_vector'(B"0"));
      res <= rw_cond(rw_eq(zt0, std_logic_vector'(B"0")), rw_cond(rw_eq(zt1, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"1") & \jR1\ & \kR1\ & \lR1\ & std_logic_vector'(B"0")), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32)), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"1") & \jR1\ & \kR1\ & \lR1\ & std_logic_vector'(B"1")), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_xorix\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_xorix\ is
component \Main_explode5\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal ds : std_logic_vector (4 downto 0);
      signal j : std_logic_vector (0 downto 0);
      signal \jR1\ : std_logic_vector (0 downto 0);
      signal k : std_logic_vector (0 downto 0);
      signal \kR1\ : std_logic_vector (0 downto 0);
      signal l : std_logic_vector (0 downto 0);
      signal \lR1\ : std_logic_vector (0 downto 0);
      signal m : std_logic_vector (0 downto 0);
      signal \mR1\ : std_logic_vector (0 downto 0);
      signal i1 : std_logic_vector (4 downto 0);
      signal h : std_logic_vector (0 downto 0);
      signal zt0 : std_logic_vector (0 downto 0);
      signal zt1 : std_logic_vector (0 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      ds <= main_explode5_out;
      j <= ds(3 downto 3);
      \jR1\ <= j;
      k <= ds(2 downto 2);
      \kR1\ <= k;
      l <= ds(1 downto 1);
      \lR1\ <= l;
      m <= ds(0 downto 0);
      \mR1\ <= m;
      i1 <= (std_logic_vector'(B"1") & \jR1\ & \kR1\ & \lR1\ & \mR1\);
      h <= ds(4 downto 4);
      zt0 <= h;
      zt1 <= rw_cond(rw_eq(zt0, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
      res <= rw_cond(rw_eq(zt1, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(i1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_xor(rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize((std_logic_vector'(B"0") & \jR1\ & \kR1\ & \lR1\ & \mR1\), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(i1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_xor\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      res : out std_logic_vector (1023 downto 0));
end entity;

architecture rtl of \Main_xor\ is
component \Main_xorix\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal main_xorix_out : std_logic_vector (31 downto 0);
      signal \main_xorix_outR1\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR2\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR3\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR4\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR5\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR6\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR7\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR8\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR9\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR10\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR11\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR12\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR13\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR14\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR15\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR16\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR17\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR18\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR19\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR20\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR21\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR22\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR23\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR24\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR25\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR26\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR27\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR28\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR29\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR30\ : std_logic_vector (31 downto 0);
      signal \main_xorix_outR31\ : std_logic_vector (31 downto 0);
begin
inst : \Main_xorix\ port map (arg0, std_logic_vector'(B"00000"), main_xorix_out);
      \instR1\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"00001"), \main_xorix_outR1\);
      \instR2\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"00010"), \main_xorix_outR2\);
      \instR3\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"00011"), \main_xorix_outR3\);
      \instR4\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"00100"), \main_xorix_outR4\);
      \instR5\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"00101"), \main_xorix_outR5\);
      \instR6\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"00110"), \main_xorix_outR6\);
      \instR7\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"00111"), \main_xorix_outR7\);
      \instR8\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01000"), \main_xorix_outR8\);
      \instR9\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01001"), \main_xorix_outR9\);
      \instR10\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01010"), \main_xorix_outR10\);
      \instR11\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01011"), \main_xorix_outR11\);
      \instR12\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01100"), \main_xorix_outR12\);
      \instR13\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01101"), \main_xorix_outR13\);
      \instR14\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01110"), \main_xorix_outR14\);
      \instR15\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"01111"), \main_xorix_outR15\);
      \instR16\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10000"), \main_xorix_outR16\);
      \instR17\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10001"), \main_xorix_outR17\);
      \instR18\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10010"), \main_xorix_outR18\);
      \instR19\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10011"), \main_xorix_outR19\);
      \instR20\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10100"), \main_xorix_outR20\);
      \instR21\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10101"), \main_xorix_outR21\);
      \instR22\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10110"), \main_xorix_outR22\);
      \instR23\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"10111"), \main_xorix_outR23\);
      \instR24\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11000"), \main_xorix_outR24\);
      \instR25\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11001"), \main_xorix_outR25\);
      \instR26\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11010"), \main_xorix_outR26\);
      \instR27\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11011"), \main_xorix_outR27\);
      \instR28\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11100"), \main_xorix_outR28\);
      \instR29\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11101"), \main_xorix_outR29\);
      \instR30\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11110"), \main_xorix_outR30\);
      \instR31\ : \Main_xorix\ port map (arg0, std_logic_vector'(B"11111"), \main_xorix_outR31\);
      res <= (main_xorix_out & \main_xorix_outR1\ & \main_xorix_outR2\ & \main_xorix_outR3\ & \main_xorix_outR4\ & \main_xorix_outR5\ & \main_xorix_outR6\ & \main_xorix_outR7\ & \main_xorix_outR8\ & \main_xorix_outR9\ & \main_xorix_outR10\ & \main_xorix_outR11\ & \main_xorix_outR12\ & \main_xorix_outR13\ & \main_xorix_outR14\ & \main_xorix_outR15\ & \main_xorix_outR16\ & \main_xorix_outR17\ & \main_xorix_outR18\ & \main_xorix_outR19\ & \main_xorix_outR20\ & \main_xorix_outR21\ & \main_xorix_outR22\ & \main_xorix_outR23\ & \main_xorix_outR24\ & \main_xorix_outR25\ & \main_xorix_outR26\ & \main_xorix_outR27\ & \main_xorix_outR28\ & \main_xorix_outR29\ & \main_xorix_outR30\ & \main_xorix_outR31\);
end architecture;