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
component \ZLL_Main_dev\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev19\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev21\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev3\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev31\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev33\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev35\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev36\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev45\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zll_main_dev36_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev36_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev36_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev36_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev36_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev36_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev36_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev36_outR7\ : std_logic_vector (0 downto 0);
      signal zll_main_dev33_out : std_logic_vector (2 downto 0);
      signal zll_main_dev45_out : std_logic_vector (2 downto 0);
      signal zi1 : std_logic_vector (2 downto 0);
      signal \zll_main_dev33_outR1\ : std_logic_vector (2 downto 0);
      signal zll_main_dev3_out : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zll_main_dev_out : std_logic_vector (2 downto 0);
      signal zi3 : std_logic_vector (2 downto 0);
      signal zll_main_dev19_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev19_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev19_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev19_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev19_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev19_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev19_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev19_outR7\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev33_outR2\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev45_outR1\ : std_logic_vector (2 downto 0);
      signal zi4 : std_logic_vector (2 downto 0);
      signal \zll_main_dev33_outR3\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev3_outR1\ : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zll_main_dev31_out : std_logic_vector (2 downto 0);
      signal zi6 : std_logic_vector (2 downto 0);
      signal zll_main_dev21_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev21_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev21_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev21_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev21_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev21_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev21_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev21_outR7\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev33_outR4\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev45_outR2\ : std_logic_vector (2 downto 0);
      signal zi7 : std_logic_vector (2 downto 0);
      signal \zll_main_dev33_outR5\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev3_outR2\ : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal \zll_main_dev_outR1\ : std_logic_vector (2 downto 0);
      signal zi9 : std_logic_vector (2 downto 0);
      signal zll_main_dev35_out : std_logic_vector (0 downto 0);
      signal \zll_main_dev35_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev35_outR2\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev35_outR3\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev35_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev35_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev35_outR6\ : std_logic_vector (0 downto 0);
      signal \zll_main_dev35_outR7\ : std_logic_vector (0 downto 0);
      signal zres : std_logic_vector (31 downto 0);
begin
zi0 <= rw_mul(\__in0\, std_logic_vector'(B"00000010"));
      inst : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"000"), zll_main_dev36_out);
      \instR1\ : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"001"), \zll_main_dev36_outR1\);
      \instR2\ : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"010"), \zll_main_dev36_outR2\);
      \instR3\ : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"011"), \zll_main_dev36_outR3\);
      \instR4\ : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"100"), \zll_main_dev36_outR4\);
      \instR5\ : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"101"), \zll_main_dev36_outR5\);
      \instR6\ : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"110"), \zll_main_dev36_outR6\);
      \instR7\ : \ZLL_Main_dev36\ port map (std_logic_vector'(B"010"), std_logic_vector'(B"001"), zi0, \__in0\, std_logic_vector'(B"111"), \zll_main_dev36_outR7\);
      \instR8\ : \ZLL_Main_dev33\ port map (\__in0\, zll_main_dev33_out);
      \instR9\ : \ZLL_Main_dev45\ port map (zll_main_dev33_out, std_logic_vector'(B"010"), zll_main_dev45_out);
      zi1 <= zll_main_dev45_out;
      \instR10\ : \ZLL_Main_dev33\ port map (\__in0\, \zll_main_dev33_outR1\);
      \instR11\ : \ZLL_Main_dev3\ port map (\zll_main_dev33_outR1\, zll_main_dev3_out);
      zi2 <= zll_main_dev3_out;
      \instR12\ : \ZLL_Main_dev\ port map (zi1, std_logic_vector'(B"001"), zll_main_dev_out);
      zi3 <= rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), zll_main_dev_out, zi1);
      \instR13\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"000"), zll_main_dev19_out);
      \instR14\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"001"), \zll_main_dev19_outR1\);
      \instR15\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"010"), \zll_main_dev19_outR2\);
      \instR16\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"011"), \zll_main_dev19_outR3\);
      \instR17\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"100"), \zll_main_dev19_outR4\);
      \instR18\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"101"), \zll_main_dev19_outR5\);
      \instR19\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"110"), \zll_main_dev19_outR6\);
      \instR20\ : \ZLL_Main_dev19\ port map (std_logic_vector'(B"001"), std_logic_vector'(B"010"), \__in0\, zi0, zi3, std_logic_vector'(B"111"), \zll_main_dev19_outR7\);
      \instR21\ : \ZLL_Main_dev33\ port map (\__in0\, \zll_main_dev33_outR2\);
      \instR22\ : \ZLL_Main_dev45\ port map (\zll_main_dev33_outR2\, std_logic_vector'(B"010"), \zll_main_dev45_outR1\);
      zi4 <= \zll_main_dev45_outR1\;
      \instR23\ : \ZLL_Main_dev33\ port map (\__in0\, \zll_main_dev33_outR3\);
      \instR24\ : \ZLL_Main_dev3\ port map (\zll_main_dev33_outR3\, \zll_main_dev3_outR1\);
      zi5 <= \zll_main_dev3_outR1\;
      \instR25\ : \ZLL_Main_dev31\ port map (zi4, std_logic_vector'(B"001"), zll_main_dev31_out);
      zi6 <= rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), zll_main_dev31_out, zi4);
      \instR26\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"000"), zll_main_dev21_out);
      \instR27\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"001"), \zll_main_dev21_outR1\);
      \instR28\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"010"), \zll_main_dev21_outR2\);
      \instR29\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"011"), \zll_main_dev21_outR3\);
      \instR30\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"100"), \zll_main_dev21_outR4\);
      \instR31\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"101"), \zll_main_dev21_outR5\);
      \instR32\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"110"), \zll_main_dev21_outR6\);
      \instR33\ : \ZLL_Main_dev21\ port map (zi0, \__in0\, zi6, std_logic_vector'(B"010"), std_logic_vector'(B"111"), \zll_main_dev21_outR7\);
      \instR34\ : \ZLL_Main_dev33\ port map (\__in0\, \zll_main_dev33_outR4\);
      \instR35\ : \ZLL_Main_dev45\ port map (\zll_main_dev33_outR4\, std_logic_vector'(B"010"), \zll_main_dev45_outR2\);
      zi7 <= \zll_main_dev45_outR2\;
      \instR36\ : \ZLL_Main_dev33\ port map (\__in0\, \zll_main_dev33_outR5\);
      \instR37\ : \ZLL_Main_dev3\ port map (\zll_main_dev33_outR5\, \zll_main_dev3_outR2\);
      zi8 <= \zll_main_dev3_outR2\;
      \instR38\ : \ZLL_Main_dev\ port map (zi7, std_logic_vector'(B"001"), \zll_main_dev_outR1\);
      zi9 <= rw_cond(rw_eq(zi8, std_logic_vector'(B"0")), \zll_main_dev_outR1\, zi7);
      \instR39\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"000"), zll_main_dev35_out);
      \instR40\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"001"), \zll_main_dev35_outR1\);
      \instR41\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"010"), \zll_main_dev35_outR2\);
      \instR42\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"011"), \zll_main_dev35_outR3\);
      \instR43\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"100"), \zll_main_dev35_outR4\);
      \instR44\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"101"), \zll_main_dev35_outR5\);
      \instR45\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"110"), \zll_main_dev35_outR6\);
      \instR46\ : \ZLL_Main_dev35\ port map (\__in0\, std_logic_vector'(B"010"), zi0, zi9, std_logic_vector'(B"001"), std_logic_vector'(B"111"), \zll_main_dev35_outR7\);
      zres <= (zll_main_dev36_out & \zll_main_dev36_outR1\ & \zll_main_dev36_outR2\ & \zll_main_dev36_outR3\ & \zll_main_dev36_outR4\ & \zll_main_dev36_outR5\ & \zll_main_dev36_outR6\ & \zll_main_dev36_outR7\ & (zll_main_dev19_out & \zll_main_dev19_outR1\ & \zll_main_dev19_outR2\ & \zll_main_dev19_outR3\ & \zll_main_dev19_outR4\ & \zll_main_dev19_outR5\ & \zll_main_dev19_outR6\ & \zll_main_dev19_outR7\) & (zll_main_dev21_out & \zll_main_dev21_outR1\ & \zll_main_dev21_outR2\ & \zll_main_dev21_outR3\ & \zll_main_dev21_outR4\ & \zll_main_dev21_outR5\ & \zll_main_dev21_outR6\ & \zll_main_dev21_outR7\) & (zll_main_dev35_out & \zll_main_dev35_outR1\ & \zll_main_dev35_outR2\ & \zll_main_dev35_outR3\ & \zll_main_dev35_outR4\ & \zll_main_dev35_outR5\ & \zll_main_dev35_outR6\ & \zll_main_dev35_outR7\));
      \__out0\ <= zres(31 downto 24);
      \__out1\ <= zres(23 downto 16);
      \__out2\ <= zres(15 downto 8);
      \__out3\ <= zres(7 downto 0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev45\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev45\ is

begin
res <= rw_resize(rw_mod(rw_div(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev41\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev41\ is

begin
res <= rw_resize(rw_mod(rw_sub(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev36\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev36\ is
component \ZLL_Main_dev3\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev41\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev45\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_dev3_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_dev41_out : std_logic_vector (2 downto 0);
      signal zll_main_dev45_out : std_logic_vector (2 downto 0);
      signal \zll_main_dev45_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_dev3\ port map (arg4, zll_main_dev3_out);
      zi0 <= zll_main_dev3_out;
      \instR1\ : \ZLL_Main_dev41\ port map (arg4, arg1, zll_main_dev41_out);
      \instR2\ : \ZLL_Main_dev45\ port map (zll_main_dev41_out, arg0, zll_main_dev45_out);
      \instR3\ : \ZLL_Main_dev45\ port map (arg4, arg0, \zll_main_dev45_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg2, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_dev45_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg3, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_dev45_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev35\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev35\ is
component \ZLL_Main_dev\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev10\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev41\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev6\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zll_main_dev6_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_dev41_out : std_logic_vector (2 downto 0);
      signal zll_main_dev10_out : std_logic_vector (2 downto 0);
      signal zll_main_dev_out : std_logic_vector (2 downto 0);
      signal \zll_main_dev10_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_dev6\ port map (arg5, arg3, zll_main_dev6_out);
      zi0 <= zll_main_dev6_out;
      \instR1\ : \ZLL_Main_dev41\ port map (arg5, arg3, zll_main_dev41_out);
      \instR2\ : \ZLL_Main_dev10\ port map (zll_main_dev41_out, arg1, zll_main_dev10_out);
      \instR3\ : \ZLL_Main_dev\ port map (zll_main_dev10_out, arg4, zll_main_dev_out);
      \instR4\ : \ZLL_Main_dev10\ port map (arg5, arg1, \zll_main_dev10_outR1\);
      \instR5\ : \ZLL_Main_dev\ port map (\zll_main_dev10_outR1\, arg4, \zll_main_dev_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg2, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_dev_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_dev_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev33\ is
port (arg0 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev33\ is

begin
res <= std_logic_vector'(B"111");
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev31\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev31\ is

begin
res <= rw_resize(rw_mod(rw_add(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev21\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev21\ is
component \ZLL_Main_dev10\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev41\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev6\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zll_main_dev6_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_dev41_out : std_logic_vector (2 downto 0);
      signal zll_main_dev10_out : std_logic_vector (2 downto 0);
      signal \zll_main_dev10_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_dev6\ port map (arg4, arg2, zll_main_dev6_out);
      zi0 <= zll_main_dev6_out;
      \instR1\ : \ZLL_Main_dev41\ port map (arg4, arg2, zll_main_dev41_out);
      \instR2\ : \ZLL_Main_dev10\ port map (zll_main_dev41_out, arg3, zll_main_dev10_out);
      \instR3\ : \ZLL_Main_dev10\ port map (arg4, arg3, \zll_main_dev10_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_dev10_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_dev10_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev19\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev19\ is
component \ZLL_Main_dev\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev3\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_dev41\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_dev45\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_dev3_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_dev41_out : std_logic_vector (2 downto 0);
      signal zll_main_dev45_out : std_logic_vector (2 downto 0);
      signal zll_main_dev_out : std_logic_vector (2 downto 0);
      signal \zll_main_dev45_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_dev_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_dev3\ port map (arg5, zll_main_dev3_out);
      zi0 <= zll_main_dev3_out;
      \instR1\ : \ZLL_Main_dev41\ port map (arg5, arg0, zll_main_dev41_out);
      \instR2\ : \ZLL_Main_dev45\ port map (zll_main_dev41_out, arg1, zll_main_dev45_out);
      \instR3\ : \ZLL_Main_dev\ port map (arg4, zll_main_dev45_out, zll_main_dev_out);
      \instR4\ : \ZLL_Main_dev45\ port map (arg5, arg1, \zll_main_dev45_outR1\);
      \instR5\ : \ZLL_Main_dev\ port map (arg4, \zll_main_dev45_outR1\, \zll_main_dev_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_resize(rw_shiftr(arg3, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_dev_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1), rw_resize(rw_shiftr(arg2, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_dev_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))), 1));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev10\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev10\ is

begin
res <= rw_resize(rw_mod(rw_mul(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev6\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev6\ is

begin
res <= rw_lt(rw_resize(arg0, 128), rw_resize(arg1, 128));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev3\ is
port (arg0 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev3\ is
signal zi0 : std_logic_vector (127 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
begin
zi0 <= rw_resize(arg0, 128);
      zi1 <= rw_resize(zi0, 1);
      zi2 <= zi1;
      res <= rw_cond(rw_eq(zi2, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_dev\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_dev\ is
component \ZLL_Main_dev31\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_dev31_out : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_dev31\ port map (arg0, arg1, zll_main_dev31_out);
      res <= zll_main_dev31_out;
end architecture;