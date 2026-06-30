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
      component \ZLL_Main_nopipeline3\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (2048 downto 0));
      end component;
      component \ZLL_Main_rotate\ is
      port (arg0 : in std_logic_vector (1055 downto 0);
            res : out std_logic_vector (1023 downto 0));
      end component;
      signal \__st0\ : std_logic_vector (1023 downto 0) := std_logic_vector'(B"0000000000000000000000000010000000000000000000000000000000000001000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (1023 downto 0);
      signal main_add_out : std_logic_vector (1023 downto 0);
      signal zi0 : std_logic_vector (1023 downto 0);
      signal conn : std_logic_vector (1055 downto 0);
      signal zll_main_rotate_out : std_logic_vector (1023 downto 0);
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
      signal \connR1\ : std_logic_vector (1023 downto 0);
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
      signal \connR2\ : std_logic_vector (1023 downto 0);
      signal \main_add_outR1\ : std_logic_vector (1023 downto 0);
      signal zi3 : std_logic_vector (1023 downto 0);
      signal \connR3\ : std_logic_vector (1055 downto 0);
      signal \zll_main_rotate_outR1\ : std_logic_vector (1023 downto 0);
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
      signal \connR4\ : std_logic_vector (1023 downto 0);
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
      signal zi7 : std_logic_vector (2048 downto 0);
      signal zi8 : std_logic_vector (1023 downto 0);
      signal zll_main_nopipeline3_out : std_logic_vector (2048 downto 0);
      signal \zll_main_nopipeline3_outR1\ : std_logic_vector (2048 downto 0);
      signal zres : std_logic_vector (2048 downto 0);
begin
inst : \Main_add\ port map (\__st0\, main_add_out);
      zi0 <= main_add_out;
      conn <= (std_logic_vector'(B"00000000000000000000000000000111") & zi0);
      \instR1\ : \ZLL_Main_rotate\ port map (conn, zll_main_rotate_out);
      zi1 <= zll_main_rotate_out;
      \instR2\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00000"), main_swapix1_out);
      \instR3\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00001"), \main_swapix1_outR1\);
      \instR4\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00010"), \main_swapix1_outR2\);
      \instR5\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00011"), \main_swapix1_outR3\);
      \instR6\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00100"), \main_swapix1_outR4\);
      \instR7\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00101"), \main_swapix1_outR5\);
      \instR8\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00110"), \main_swapix1_outR6\);
      \instR9\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"00111"), \main_swapix1_outR7\);
      \instR10\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01000"), \main_swapix1_outR8\);
      \instR11\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01001"), \main_swapix1_outR9\);
      \instR12\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01010"), \main_swapix1_outR10\);
      \instR13\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01011"), \main_swapix1_outR11\);
      \instR14\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01100"), \main_swapix1_outR12\);
      \instR15\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01101"), \main_swapix1_outR13\);
      \instR16\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01110"), \main_swapix1_outR14\);
      \instR17\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"01111"), \main_swapix1_outR15\);
      \instR18\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10000"), \main_swapix1_outR16\);
      \instR19\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10001"), \main_swapix1_outR17\);
      \instR20\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10010"), \main_swapix1_outR18\);
      \instR21\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10011"), \main_swapix1_outR19\);
      \instR22\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10100"), \main_swapix1_outR20\);
      \instR23\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10101"), \main_swapix1_outR21\);
      \instR24\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10110"), \main_swapix1_outR22\);
      \instR25\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"10111"), \main_swapix1_outR23\);
      \instR26\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11000"), \main_swapix1_outR24\);
      \instR27\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11001"), \main_swapix1_outR25\);
      \instR28\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11010"), \main_swapix1_outR26\);
      \instR29\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11011"), \main_swapix1_outR27\);
      \instR30\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11100"), \main_swapix1_outR28\);
      \instR31\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11101"), \main_swapix1_outR29\);
      \instR32\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11110"), \main_swapix1_outR30\);
      \instR33\ : \Main_swapix1\ port map (zi1, std_logic_vector'(B"11111"), \main_swapix1_outR31\);
      \connR1\ <= (main_swapix1_out & \main_swapix1_outR1\ & \main_swapix1_outR2\ & \main_swapix1_outR3\ & \main_swapix1_outR4\ & \main_swapix1_outR5\ & \main_swapix1_outR6\ & \main_swapix1_outR7\ & \main_swapix1_outR8\ & \main_swapix1_outR9\ & \main_swapix1_outR10\ & \main_swapix1_outR11\ & \main_swapix1_outR12\ & \main_swapix1_outR13\ & \main_swapix1_outR14\ & \main_swapix1_outR15\ & \main_swapix1_outR16\ & \main_swapix1_outR17\ & \main_swapix1_outR18\ & \main_swapix1_outR19\ & \main_swapix1_outR20\ & \main_swapix1_outR21\ & \main_swapix1_outR22\ & \main_swapix1_outR23\ & \main_swapix1_outR24\ & \main_swapix1_outR25\ & \main_swapix1_outR26\ & \main_swapix1_outR27\ & \main_swapix1_outR28\ & \main_swapix1_outR29\ & \main_swapix1_outR30\ & \main_swapix1_outR31\);
      \instR34\ : \Main_xor\ port map (\connR1\, main_xor_out);
      zi2 <= main_xor_out;
      \instR35\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00000"), main_swapix2_out);
      \instR36\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00001"), \main_swapix2_outR1\);
      \instR37\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00010"), \main_swapix2_outR2\);
      \instR38\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00011"), \main_swapix2_outR3\);
      \instR39\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00100"), \main_swapix2_outR4\);
      \instR40\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00101"), \main_swapix2_outR5\);
      \instR41\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00110"), \main_swapix2_outR6\);
      \instR42\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"00111"), \main_swapix2_outR7\);
      \instR43\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01000"), \main_swapix2_outR8\);
      \instR44\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01001"), \main_swapix2_outR9\);
      \instR45\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01010"), \main_swapix2_outR10\);
      \instR46\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01011"), \main_swapix2_outR11\);
      \instR47\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01100"), \main_swapix2_outR12\);
      \instR48\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01101"), \main_swapix2_outR13\);
      \instR49\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01110"), \main_swapix2_outR14\);
      \instR50\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"01111"), \main_swapix2_outR15\);
      \instR51\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10000"), \main_swapix2_outR16\);
      \instR52\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10001"), \main_swapix2_outR17\);
      \instR53\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10010"), \main_swapix2_outR18\);
      \instR54\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10011"), \main_swapix2_outR19\);
      \instR55\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10100"), \main_swapix2_outR20\);
      \instR56\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10101"), \main_swapix2_outR21\);
      \instR57\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10110"), \main_swapix2_outR22\);
      \instR58\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"10111"), \main_swapix2_outR23\);
      \instR59\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11000"), \main_swapix2_outR24\);
      \instR60\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11001"), \main_swapix2_outR25\);
      \instR61\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11010"), \main_swapix2_outR26\);
      \instR62\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11011"), \main_swapix2_outR27\);
      \instR63\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11100"), \main_swapix2_outR28\);
      \instR64\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11101"), \main_swapix2_outR29\);
      \instR65\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11110"), \main_swapix2_outR30\);
      \instR66\ : \Main_swapix2\ port map (zi2, std_logic_vector'(B"11111"), \main_swapix2_outR31\);
      \connR2\ <= (main_swapix2_out & \main_swapix2_outR1\ & \main_swapix2_outR2\ & \main_swapix2_outR3\ & \main_swapix2_outR4\ & \main_swapix2_outR5\ & \main_swapix2_outR6\ & \main_swapix2_outR7\ & \main_swapix2_outR8\ & \main_swapix2_outR9\ & \main_swapix2_outR10\ & \main_swapix2_outR11\ & \main_swapix2_outR12\ & \main_swapix2_outR13\ & \main_swapix2_outR14\ & \main_swapix2_outR15\ & \main_swapix2_outR16\ & \main_swapix2_outR17\ & \main_swapix2_outR18\ & \main_swapix2_outR19\ & \main_swapix2_outR20\ & \main_swapix2_outR21\ & \main_swapix2_outR22\ & \main_swapix2_outR23\ & \main_swapix2_outR24\ & \main_swapix2_outR25\ & \main_swapix2_outR26\ & \main_swapix2_outR27\ & \main_swapix2_outR28\ & \main_swapix2_outR29\ & \main_swapix2_outR30\ & \main_swapix2_outR31\);
      \instR67\ : \Main_add\ port map (\connR2\, \main_add_outR1\);
      zi3 <= \main_add_outR1\;
      \connR3\ <= (std_logic_vector'(B"00000000000000000000000000001011") & zi3);
      \instR68\ : \ZLL_Main_rotate\ port map (\connR3\, \zll_main_rotate_outR1\);
      zi4 <= \zll_main_rotate_outR1\;
      \instR69\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00000"), main_swapix3_out);
      \instR70\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00001"), \main_swapix3_outR1\);
      \instR71\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00010"), \main_swapix3_outR2\);
      \instR72\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00011"), \main_swapix3_outR3\);
      \instR73\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00100"), \main_swapix3_outR4\);
      \instR74\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00101"), \main_swapix3_outR5\);
      \instR75\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00110"), \main_swapix3_outR6\);
      \instR76\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"00111"), \main_swapix3_outR7\);
      \instR77\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01000"), \main_swapix3_outR8\);
      \instR78\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01001"), \main_swapix3_outR9\);
      \instR79\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01010"), \main_swapix3_outR10\);
      \instR80\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01011"), \main_swapix3_outR11\);
      \instR81\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01100"), \main_swapix3_outR12\);
      \instR82\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01101"), \main_swapix3_outR13\);
      \instR83\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01110"), \main_swapix3_outR14\);
      \instR84\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"01111"), \main_swapix3_outR15\);
      \instR85\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10000"), \main_swapix3_outR16\);
      \instR86\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10001"), \main_swapix3_outR17\);
      \instR87\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10010"), \main_swapix3_outR18\);
      \instR88\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10011"), \main_swapix3_outR19\);
      \instR89\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10100"), \main_swapix3_outR20\);
      \instR90\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10101"), \main_swapix3_outR21\);
      \instR91\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10110"), \main_swapix3_outR22\);
      \instR92\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"10111"), \main_swapix3_outR23\);
      \instR93\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11000"), \main_swapix3_outR24\);
      \instR94\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11001"), \main_swapix3_outR25\);
      \instR95\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11010"), \main_swapix3_outR26\);
      \instR96\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11011"), \main_swapix3_outR27\);
      \instR97\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11100"), \main_swapix3_outR28\);
      \instR98\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11101"), \main_swapix3_outR29\);
      \instR99\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11110"), \main_swapix3_outR30\);
      \instR100\ : \Main_swapix3\ port map (zi4, std_logic_vector'(B"11111"), \main_swapix3_outR31\);
      \connR4\ <= (main_swapix3_out & \main_swapix3_outR1\ & \main_swapix3_outR2\ & \main_swapix3_outR3\ & \main_swapix3_outR4\ & \main_swapix3_outR5\ & \main_swapix3_outR6\ & \main_swapix3_outR7\ & \main_swapix3_outR8\ & \main_swapix3_outR9\ & \main_swapix3_outR10\ & \main_swapix3_outR11\ & \main_swapix3_outR12\ & \main_swapix3_outR13\ & \main_swapix3_outR14\ & \main_swapix3_outR15\ & \main_swapix3_outR16\ & \main_swapix3_outR17\ & \main_swapix3_outR18\ & \main_swapix3_outR19\ & \main_swapix3_outR20\ & \main_swapix3_outR21\ & \main_swapix3_outR22\ & \main_swapix3_outR23\ & \main_swapix3_outR24\ & \main_swapix3_outR25\ & \main_swapix3_outR26\ & \main_swapix3_outR27\ & \main_swapix3_outR28\ & \main_swapix3_outR29\ & \main_swapix3_outR30\ & \main_swapix3_outR31\);
      \instR101\ : \Main_xor\ port map (\connR4\, \main_xor_outR1\);
      zi5 <= \main_xor_outR1\;
      \instR102\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00000"), main_swapix4_out);
      \instR103\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00001"), \main_swapix4_outR1\);
      \instR104\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00010"), \main_swapix4_outR2\);
      \instR105\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00011"), \main_swapix4_outR3\);
      \instR106\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00100"), \main_swapix4_outR4\);
      \instR107\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00101"), \main_swapix4_outR5\);
      \instR108\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00110"), \main_swapix4_outR6\);
      \instR109\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"00111"), \main_swapix4_outR7\);
      \instR110\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01000"), \main_swapix4_outR8\);
      \instR111\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01001"), \main_swapix4_outR9\);
      \instR112\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01010"), \main_swapix4_outR10\);
      \instR113\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01011"), \main_swapix4_outR11\);
      \instR114\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01100"), \main_swapix4_outR12\);
      \instR115\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01101"), \main_swapix4_outR13\);
      \instR116\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01110"), \main_swapix4_outR14\);
      \instR117\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"01111"), \main_swapix4_outR15\);
      \instR118\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10000"), \main_swapix4_outR16\);
      \instR119\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10001"), \main_swapix4_outR17\);
      \instR120\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10010"), \main_swapix4_outR18\);
      \instR121\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10011"), \main_swapix4_outR19\);
      \instR122\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10100"), \main_swapix4_outR20\);
      \instR123\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10101"), \main_swapix4_outR21\);
      \instR124\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10110"), \main_swapix4_outR22\);
      \instR125\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"10111"), \main_swapix4_outR23\);
      \instR126\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11000"), \main_swapix4_outR24\);
      \instR127\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11001"), \main_swapix4_outR25\);
      \instR128\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11010"), \main_swapix4_outR26\);
      \instR129\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11011"), \main_swapix4_outR27\);
      \instR130\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11100"), \main_swapix4_outR28\);
      \instR131\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11101"), \main_swapix4_outR29\);
      \instR132\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11110"), \main_swapix4_outR30\);
      \instR133\ : \Main_swapix4\ port map (zi5, std_logic_vector'(B"11111"), \main_swapix4_outR31\);
      zi6 <= (main_swapix4_out & \main_swapix4_outR1\ & \main_swapix4_outR2\ & \main_swapix4_outR3\ & \main_swapix4_outR4\ & \main_swapix4_outR5\ & \main_swapix4_outR6\ & \main_swapix4_outR7\ & \main_swapix4_outR8\ & \main_swapix4_outR9\ & \main_swapix4_outR10\ & \main_swapix4_outR11\ & \main_swapix4_outR12\ & \main_swapix4_outR13\ & \main_swapix4_outR14\ & \main_swapix4_outR15\ & \main_swapix4_outR16\ & \main_swapix4_outR17\ & \main_swapix4_outR18\ & \main_swapix4_outR19\ & \main_swapix4_outR20\ & \main_swapix4_outR21\ & \main_swapix4_outR22\ & \main_swapix4_outR23\ & \main_swapix4_outR24\ & \main_swapix4_outR25\ & \main_swapix4_outR26\ & \main_swapix4_outR27\ & \main_swapix4_outR28\ & \main_swapix4_outR29\ & \main_swapix4_outR30\ & \main_swapix4_outR31\);
      zi7 <= (rw_repl(1025, std_logic_vector'(B"0")) & zi6);
      zi8 <= zi7(1023 downto 0);
      \instR134\ : \ZLL_Main_nopipeline3\ port map (zi8, zi8, zll_main_nopipeline3_out);
      \instR135\ : \ZLL_Main_nopipeline3\ port map (\__st0\, \__st0\, \zll_main_nopipeline3_outR1\);
      zres <= rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), zll_main_nopipeline3_out, \zll_main_nopipeline3_outR1\);
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
entity \ZLL_Main_swapix38\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \ZLL_Main_swapix38\ is

begin
res <= rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32);
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
      signal zi7 : std_logic_vector (4 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (32 downto 0);
      signal zi10 : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (31 downto 0);
begin
inst : \Main_explode5\ port map (arg2, main_explode5_out);
      zi7 <= main_explode5_out;
      zi8 <= zi7(4 downto 4);
      zi9 <= (zi8 & rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg2, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
      zi10 <= zi9(32 downto 32);
      zi11 <= zi9(31 downto 0);
      res <= rw_cond(rw_eq(zi10, std_logic_vector'(B"1")), zi11, rw_or(rw_shiftl(zi11, arg0), rw_shiftr(zi11, rw_sub(std_logic_vector'(B"00000000000000000000000000100000"), arg0))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ReWire_Prelude_not\ is
port (arg0 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ReWire_Prelude_not\ is

begin
res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_nopipeline3\ is
port (arg0 : in std_logic_vector (1023 downto 0);
      arg1 : in std_logic_vector (1023 downto 0);
      res : out std_logic_vector (2048 downto 0));
end entity;

architecture rtl of \ZLL_Main_nopipeline3\ is

begin
res <= (std_logic_vector'(B"1") & arg0 & arg1);
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
      component \ReWire_Prelude_not\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ReWirezuPreludezuzaza\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_swapix44\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            arg1 : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal zi3 : std_logic_vector (4 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (4 downto 0);
      signal zi10 : std_logic_vector (4 downto 0);
      signal rewire_prelude_not_out : std_logic_vector (0 downto 0);
      signal rewirezupreludezuzazazuout : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal \rewirezupreludezuzazazuoutR1\ : std_logic_vector (0 downto 0);
      signal zi13 : std_logic_vector (0 downto 0);
      signal zll_main_swapix44_out : std_logic_vector (31 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      zi3 <= main_explode5_out;
      zi4 <= zi3(4 downto 4);
      zi5 <= zi3(3 downto 3);
      zi6 <= zi3(2 downto 2);
      zi7 <= zi3(1 downto 1);
      zi8 <= zi3(0 downto 0);
      zi9 <= (std_logic_vector'(B"1") & zi5 & zi6 & zi7 & std_logic_vector'(B"1"));
      zi10 <= (std_logic_vector'(B"1") & zi5 & zi6 & zi7 & std_logic_vector'(B"0"));
      \instR1\ : \ReWire_Prelude_not\ port map (zi8, rewire_prelude_not_out);
      \instR2\ : \ReWirezuPreludezuzaza\ port map (zi4, rewire_prelude_not_out, rewirezupreludezuzazazuout);
      zi11 <= rewirezupreludezuzazazuout;
      \instR3\ : \ReWirezuPreludezuzaza\ port map (zi4, zi8, \rewirezupreludezuzazazuoutR1\);
      zi13 <= \rewirezupreludezuzazazuoutR1\;
      \instR4\ : \ZLL_Main_swapix44\ port map (arg1, arg0, zll_main_swapix44_out);
      res <= rw_cond(rw_eq(zi11, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi9, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_cond(rw_eq(zi13, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi10, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), zll_main_swapix44_out));
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
      component \ReWire_Prelude_not\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ReWirezuPreludezuzaza\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_swapix38\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal zi3 : std_logic_vector (4 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (4 downto 0);
      signal zi10 : std_logic_vector (4 downto 0);
      signal rewire_prelude_not_out : std_logic_vector (0 downto 0);
      signal rewirezupreludezuzazazuout : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal \rewirezupreludezuzazazuoutR1\ : std_logic_vector (0 downto 0);
      signal zi13 : std_logic_vector (0 downto 0);
      signal zll_main_swapix38_out : std_logic_vector (31 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      zi3 <= main_explode5_out;
      zi4 <= zi3(4 downto 4);
      zi5 <= zi3(3 downto 3);
      zi6 <= zi3(2 downto 2);
      zi7 <= zi3(1 downto 1);
      zi8 <= zi3(0 downto 0);
      zi9 <= (std_logic_vector'(B"1") & zi5 & zi6 & std_logic_vector'(B"1") & zi8);
      zi10 <= (std_logic_vector'(B"1") & zi5 & zi6 & std_logic_vector'(B"0") & zi8);
      \instR1\ : \ReWire_Prelude_not\ port map (zi7, rewire_prelude_not_out);
      \instR2\ : \ReWirezuPreludezuzaza\ port map (zi4, rewire_prelude_not_out, rewirezupreludezuzazazuout);
      zi11 <= rewirezupreludezuzazazuout;
      \instR3\ : \ReWirezuPreludezuzaza\ port map (zi4, zi7, \rewirezupreludezuzazazuoutR1\);
      zi13 <= \rewirezupreludezuzazazuoutR1\;
      \instR4\ : \ZLL_Main_swapix38\ port map (arg0, arg1, zll_main_swapix38_out);
      res <= rw_cond(rw_eq(zi11, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi9, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_cond(rw_eq(zi13, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi10, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), zll_main_swapix38_out));
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_swapix44\ is
port (arg0 : in std_logic_vector (4 downto 0);
      arg1 : in std_logic_vector (1023 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \ZLL_Main_swapix44\ is

begin
res <= rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(arg0, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_rotate\ is
port (arg0 : in std_logic_vector (1055 downto 0);
      res : out std_logic_vector (1023 downto 0));
end entity;

architecture rtl of \ZLL_Main_rotate\ is
component \Main_rot\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (1023 downto 0);
            arg2 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal rc : std_logic_vector (31 downto 0);
      signal s : std_logic_vector (1023 downto 0);
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
begin
rc <= arg0(1055 downto 1024);
      s <= arg0(1023 downto 0);
      inst : \Main_rot\ port map (rc, s, std_logic_vector'(B"00000"), main_rot_out);
      \instR1\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"00001"), \main_rot_outR1\);
      \instR2\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"00010"), \main_rot_outR2\);
      \instR3\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"00011"), \main_rot_outR3\);
      \instR4\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"00100"), \main_rot_outR4\);
      \instR5\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"00101"), \main_rot_outR5\);
      \instR6\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"00110"), \main_rot_outR6\);
      \instR7\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"00111"), \main_rot_outR7\);
      \instR8\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01000"), \main_rot_outR8\);
      \instR9\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01001"), \main_rot_outR9\);
      \instR10\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01010"), \main_rot_outR10\);
      \instR11\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01011"), \main_rot_outR11\);
      \instR12\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01100"), \main_rot_outR12\);
      \instR13\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01101"), \main_rot_outR13\);
      \instR14\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01110"), \main_rot_outR14\);
      \instR15\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"01111"), \main_rot_outR15\);
      \instR16\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10000"), \main_rot_outR16\);
      \instR17\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10001"), \main_rot_outR17\);
      \instR18\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10010"), \main_rot_outR18\);
      \instR19\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10011"), \main_rot_outR19\);
      \instR20\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10100"), \main_rot_outR20\);
      \instR21\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10101"), \main_rot_outR21\);
      \instR22\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10110"), \main_rot_outR22\);
      \instR23\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"10111"), \main_rot_outR23\);
      \instR24\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11000"), \main_rot_outR24\);
      \instR25\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11001"), \main_rot_outR25\);
      \instR26\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11010"), \main_rot_outR26\);
      \instR27\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11011"), \main_rot_outR27\);
      \instR28\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11100"), \main_rot_outR28\);
      \instR29\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11101"), \main_rot_outR29\);
      \instR30\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11110"), \main_rot_outR30\);
      \instR31\ : \Main_rot\ port map (rc, s, std_logic_vector'(B"11111"), \main_rot_outR31\);
      res <= (main_rot_out & \main_rot_outR1\ & \main_rot_outR2\ & \main_rot_outR3\ & \main_rot_outR4\ & \main_rot_outR5\ & \main_rot_outR6\ & \main_rot_outR7\ & \main_rot_outR8\ & \main_rot_outR9\ & \main_rot_outR10\ & \main_rot_outR11\ & \main_rot_outR12\ & \main_rot_outR13\ & \main_rot_outR14\ & \main_rot_outR15\ & \main_rot_outR16\ & \main_rot_outR17\ & \main_rot_outR18\ & \main_rot_outR19\ & \main_rot_outR20\ & \main_rot_outR21\ & \main_rot_outR22\ & \main_rot_outR23\ & \main_rot_outR24\ & \main_rot_outR25\ & \main_rot_outR26\ & \main_rot_outR27\ & \main_rot_outR28\ & \main_rot_outR29\ & \main_rot_outR30\ & \main_rot_outR31\);
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
      component \ReWire_Prelude_not\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal zi3 : std_logic_vector (4 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (4 downto 0);
      signal zi10 : std_logic_vector (4 downto 0);
      signal rewire_prelude_not_out : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      zi3 <= main_explode5_out;
      zi4 <= zi3(4 downto 4);
      zi5 <= zi3(3 downto 3);
      zi6 <= zi3(2 downto 2);
      zi7 <= zi3(1 downto 1);
      zi8 <= zi3(0 downto 0);
      zi9 <= (std_logic_vector'(B"0") & zi5 & zi6 & zi7 & zi8);
      zi10 <= (std_logic_vector'(B"1") & zi5 & zi6 & zi7 & zi8);
      \instR1\ : \ReWire_Prelude_not\ port map (zi4, rewire_prelude_not_out);
      zi11 <= rewire_prelude_not_out;
      res <= rw_cond(rw_eq(zi11, std_logic_vector'(B"1")), rw_xor(rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi9, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi10, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32)), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi10, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ReWirezuPreludezuzaza\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ReWirezuPreludezuzaza\ is

begin
res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"1")), arg1, std_logic_vector'(B"0"));
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
      component \ReWire_Prelude_not\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ReWirezuPreludezuzaza\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_swapix38\ is
      port (arg0 : in std_logic_vector (1023 downto 0);
            arg1 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal zi3 : std_logic_vector (4 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (4 downto 0);
      signal zi10 : std_logic_vector (4 downto 0);
      signal rewire_prelude_not_out : std_logic_vector (0 downto 0);
      signal \rewire_prelude_not_outR1\ : std_logic_vector (0 downto 0);
      signal rewirezupreludezuzazazuout : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal \rewire_prelude_not_outR2\ : std_logic_vector (0 downto 0);
      signal \rewirezupreludezuzazazuoutR1\ : std_logic_vector (0 downto 0);
      signal zi13 : std_logic_vector (0 downto 0);
      signal zll_main_swapix38_out : std_logic_vector (31 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      zi3 <= main_explode5_out;
      zi4 <= zi3(4 downto 4);
      zi5 <= zi3(3 downto 3);
      zi6 <= zi3(2 downto 2);
      zi7 <= zi3(1 downto 1);
      zi8 <= zi3(0 downto 0);
      zi9 <= (std_logic_vector'(B"0") & zi5 & std_logic_vector'(B"1") & zi7 & zi8);
      zi10 <= (std_logic_vector'(B"0") & zi5 & std_logic_vector'(B"0") & zi7 & zi8);
      \instR1\ : \ReWire_Prelude_not\ port map (zi4, rewire_prelude_not_out);
      \instR2\ : \ReWire_Prelude_not\ port map (zi6, \rewire_prelude_not_outR1\);
      \instR3\ : \ReWirezuPreludezuzaza\ port map (rewire_prelude_not_out, \rewire_prelude_not_outR1\, rewirezupreludezuzazazuout);
      zi11 <= rewirezupreludezuzazazuout;
      \instR4\ : \ReWire_Prelude_not\ port map (zi4, \rewire_prelude_not_outR2\);
      \instR5\ : \ReWirezuPreludezuzaza\ port map (\rewire_prelude_not_outR2\, zi6, \rewirezupreludezuzazazuoutR1\);
      zi13 <= \rewirezupreludezuzazazuoutR1\;
      \instR6\ : \ZLL_Main_swapix38\ port map (arg0, arg1, zll_main_swapix38_out);
      res <= rw_cond(rw_eq(zi11, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi9, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_cond(rw_eq(zi13, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi10, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), zll_main_swapix38_out));
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
      component \ReWire_Prelude_not\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ReWirezuPreludezuzaza\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_swapix44\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            arg1 : in std_logic_vector (1023 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal main_explode5_out : std_logic_vector (4 downto 0);
      signal zi3 : std_logic_vector (4 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (4 downto 0);
      signal zi10 : std_logic_vector (4 downto 0);
      signal rewire_prelude_not_out : std_logic_vector (0 downto 0);
      signal rewirezupreludezuzazazuout : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal \rewire_prelude_not_outR1\ : std_logic_vector (0 downto 0);
      signal \rewire_prelude_not_outR2\ : std_logic_vector (0 downto 0);
      signal \rewirezupreludezuzazazuoutR1\ : std_logic_vector (0 downto 0);
      signal zi13 : std_logic_vector (0 downto 0);
      signal zll_main_swapix44_out : std_logic_vector (31 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      zi3 <= main_explode5_out;
      zi4 <= zi3(4 downto 4);
      zi5 <= zi3(3 downto 3);
      zi6 <= zi3(2 downto 2);
      zi7 <= zi3(1 downto 1);
      zi8 <= zi3(0 downto 0);
      zi9 <= (std_logic_vector'(B"00") & zi6 & zi7 & zi8);
      zi10 <= (std_logic_vector'(B"01") & zi6 & zi7 & zi8);
      \instR1\ : \ReWire_Prelude_not\ port map (zi4, rewire_prelude_not_out);
      \instR2\ : \ReWirezuPreludezuzaza\ port map (rewire_prelude_not_out, zi5, rewirezupreludezuzazazuout);
      zi11 <= rewirezupreludezuzazazuout;
      \instR3\ : \ReWire_Prelude_not\ port map (zi4, \rewire_prelude_not_outR1\);
      \instR4\ : \ReWire_Prelude_not\ port map (zi5, \rewire_prelude_not_outR2\);
      \instR5\ : \ReWirezuPreludezuzaza\ port map (\rewire_prelude_not_outR1\, \rewire_prelude_not_outR2\, \rewirezupreludezuzazazuoutR1\);
      zi13 <= \rewirezupreludezuzazazuoutR1\;
      \instR6\ : \ZLL_Main_swapix44\ port map (arg1, arg0, zll_main_swapix44_out);
      res <= rw_cond(rw_eq(zi11, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi9, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), rw_cond(rw_eq(zi13, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi10, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32), zll_main_swapix44_out));
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
      signal zi6 : std_logic_vector (4 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal zi10 : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal zi12 : std_logic_vector (4 downto 0);
      signal zi13 : std_logic_vector (4 downto 0);
      signal zi14 : std_logic_vector (64 downto 0);
      signal zi15 : std_logic_vector (0 downto 0);
      signal zi16 : std_logic_vector (31 downto 0);
      signal zi17 : std_logic_vector (31 downto 0);
begin
inst : \Main_explode5\ port map (arg1, main_explode5_out);
      zi6 <= main_explode5_out;
      zi7 <= zi6(4 downto 4);
      zi8 <= zi6(3 downto 3);
      zi9 <= zi6(2 downto 2);
      zi10 <= zi6(1 downto 1);
      zi11 <= zi6(0 downto 0);
      zi12 <= (std_logic_vector'(B"0") & zi8 & zi9 & zi10 & zi11);
      zi13 <= (std_logic_vector'(B"1") & zi8 & zi9 & zi10 & zi11);
      zi14 <= (zi7 & rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi12, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32) & rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"), rw_resize(zi13, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000"))), 32));
      zi15 <= zi14(64 downto 64);
      zi16 <= zi14(63 downto 32);
      zi17 <= zi14(31 downto 0);
      res <= rw_cond(rw_eq(zi15, std_logic_vector'(B"1")), rw_add(zi16, zi17), zi16);
end architecture;