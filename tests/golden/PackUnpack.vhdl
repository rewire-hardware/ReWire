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
component \ZLL_Main_dev11\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev12\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev14\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev22\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zll_main_dev12_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev12_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev12_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev12_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev12_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev12_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev12_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev12_outR7\ : std_logic_vector (0 downto 0);
      signal zll_main_dev14_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev14_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev14_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev14_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev14_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev14_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev14_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev14_outR7\ : std_logic_vector (0 downto 0);
      signal zll_main_dev22_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev22_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev22_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev22_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev22_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev22_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev22_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev22_outR7\ : std_logic_vector (0 downto 0);
      signal zll_main_dev11_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev11_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev11_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev11_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev11_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev11_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev11_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev11_outR7\ : std_logic_vector (0 downto 0);
      signal zres : std_logic_vector (31 downto 0);
begin
zi0 <= rw_mul(\__in0\, std_logic_vector'(B"00000010"));
      inst : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"000"), zll_main_dev12_out);
      \instR1\ : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"001"), \zll_main_dev12_outR1\);
      \instR2\ : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"010"), \zll_main_dev12_outR2\);
      \instR3\ : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"011"), \zll_main_dev12_outR3\);
      \instR4\ : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), \zll_main_dev12_outR4\);
      \instR5\ : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"101"), \zll_main_dev12_outR5\);
      \instR6\ : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"110"), \zll_main_dev12_outR6\);
      \instR7\ : \ZLL_Main_dev12\ port map (\__in0\, zi0, std_logic_vector'(B"010"), std_logic_vector'(B"111"), \zll_main_dev12_outR7\);
      \instR8\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"000"), zll_main_dev14_out);
      \instR9\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"001"), \zll_main_dev14_outR1\);
      \instR10\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"010"), \zll_main_dev14_outR2\);
      \instR11\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"011"), \zll_main_dev14_outR3\);
      \instR12\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"100"), \zll_main_dev14_outR4\);
      \instR13\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"101"), \zll_main_dev14_outR5\);
      \instR14\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"110"), \zll_main_dev14_outR6\);
      \instR15\ : \ZLL_Main_dev14\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), std_logic_vector'(B"111"), \zll_main_dev14_outR7\);
      \instR16\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"000"), zll_main_dev22_out);
      \instR17\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"001"), \zll_main_dev22_outR1\);
      \instR18\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"010"), \zll_main_dev22_outR2\);
      \instR19\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"011"), \zll_main_dev22_outR3\);
      \instR20\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"100"), \zll_main_dev22_outR4\);
      \instR21\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"101"), \zll_main_dev22_outR5\);
      \instR22\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"110"), \zll_main_dev22_outR6\);
      \instR23\ : \ZLL_Main_dev22\ port map (\__in0\, zi0, std_logic_vector'(B"100"), std_logic_vector'(B"010"), std_logic_vector'(B"111"), \zll_main_dev22_outR7\);
      \instR24\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"000"), zll_main_dev11_out);
      \instR25\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"001"), \zll_main_dev11_outR1\);
      \instR26\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"010"), \zll_main_dev11_outR2\);
      \instR27\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"011"), \zll_main_dev11_outR3\);
      \instR28\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"100"), \zll_main_dev11_outR4\);
      \instR29\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"101"), \zll_main_dev11_outR5\);
      \instR30\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"110"), \zll_main_dev11_outR6\);
      \instR31\ : \ZLL_Main_dev11\ port map (\__in0\, std_logic_vector'(B"001"), zi0, std_logic_vector'(B"010"), std_logic_vector'(B"100"), std_logic_vector'(B"111"), \zll_main_dev11_outR7\);
      zres <= (zll_main_dev12_out & \zll_main_dev12_outR1\ & \zll_main_dev12_outR2\ & \zll_main_dev12_outR3\ & \zll_main_dev12_outR4\ & \zll_main_dev12_outR5\ & \zll_main_dev12_outR6\ & \zll_main_dev12_outR7\ & zll_main_dev14_out & \zll_main_dev14_outR1\ & \zll_main_dev14_outR2\ & \zll_main_dev14_outR3\ & \zll_main_dev14_outR4\ & \zll_main_dev14_outR5\ & \zll_main_dev14_outR6\ & \zll_main_dev14_outR7\ & zll_main_dev22_out & \zll_main_dev22_outR1\ & \zll_main_dev22_outR2\ & \zll_main_dev22_outR3\ & \zll_main_dev22_outR4\ & \zll_main_dev22_outR5\ & \zll_main_dev22_outR6\ & \zll_main_dev22_outR7\ & zll_main_dev11_out & \zll_main_dev11_outR1\ & \zll_main_dev11_outR2\ & \zll_main_dev11_outR3\ & \zll_main_dev11_outR4\ & \zll_main_dev11_outR5\ & \zll_main_dev11_outR6\ & \zll_main_dev11_outR7\);
      \__out0\ <= zres(31 downto 24);
      \__out1\ <= zres(23 downto 16);
      \__out2\ <= zres(15 downto 8);
      \__out3\ <= zres(7 downto 0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev22\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev22\ is
signal zi0 : std_logic_vector (0 downto 0);
begin
zi0 <= rw_lt(rw_resize(arg4, 128), rw_resize(arg2, 128));
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_mul(rw_resize(rw_resize(rw_mod(rw_sub(rw_resize(arg4, 128), rw_resize(arg2, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_mul(rw_resize(arg4, 128), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev16\ is
port (arg0 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev16\ is

begin
res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev14\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev14\ is
component \ZLL_Main_dev16\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zt0 : std_logic_vector (0 downto 0);
      signal zll_main_dev16_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
begin
zt0 <= rw_resize(rw_resize(arg5, 128), 1);
      inst : \ZLL_Main_dev16\ port map (zt0, zll_main_dev16_out);
      zi0 <= zll_main_dev16_out;
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_add(rw_resize(arg2, 128), rw_resize(rw_resize(rw_mod(rw_div(rw_resize(rw_resize(rw_mod(rw_sub(rw_resize(arg5, 128), rw_resize(arg4, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_add(rw_resize(arg2, 128), rw_resize(rw_resize(rw_mod(rw_div(rw_resize(arg5, 128), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev12\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev12\ is
component \ZLL_Main_dev16\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zt0 : std_logic_vector (0 downto 0);
      signal zll_main_dev16_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
begin
zt0 <= rw_resize(rw_resize(arg3, 128), 1);
      inst : \ZLL_Main_dev16\ port map (zt0, zll_main_dev16_out);
      zi0 <= zll_main_dev16_out;
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_div(rw_resize(rw_resize(rw_mod(rw_sub(rw_resize(arg3, 128), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128), rw_resize(arg2, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_div(rw_resize(arg3, 128), rw_resize(arg2, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev11\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev11\ is
signal zi0 : std_logic_vector (0 downto 0);
begin
zi0 <= rw_lt(rw_resize(arg5, 128), rw_resize(arg4, 128));
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg2, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_add(rw_resize(rw_resize(rw_mod(rw_mul(rw_resize(rw_resize(rw_mod(rw_sub(rw_resize(arg5, 128), rw_resize(arg4, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(rw_resize(rw_mod(rw_add(rw_resize(rw_resize(rw_mod(rw_mul(rw_resize(arg5, 128), rw_resize(arg3, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3), 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;