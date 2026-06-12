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
port (\__in0\ : in std_logic_vector (63 downto 0);
      \__in1\ : in std_logic_vector (63 downto 0);
      \__out0\ : out std_logic_vector (63 downto 0);
      \__out1\ : out std_logic_vector (63 downto 0));
end entity;

architecture rtl of top_level is
component \ZLL_Main_compute175\ is
      port (arg0 : in std_logic_vector (63 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (63 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_compute190\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (63 downto 0);
            arg3 : in std_logic_vector (63 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_compute282\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (63 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute346\ is
      port (arg0 : in std_logic_vector (63 downto 0);
            arg1 : in std_logic_vector (63 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_compute391\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute396\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute403\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_compute417\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute437\ is
      port (arg0 : in std_logic_vector (63 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute452\ is
      port (arg0 : in std_logic_vector (63 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (63 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_compute53\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (63 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (2 downto 0);
            arg4 : in std_logic_vector (63 downto 0);
            arg5 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_Main_compute85\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (63 downto 0);
            arg2 : in std_logic_vector (2 downto 0);
            arg3 : in std_logic_vector (63 downto 0);
            arg4 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      signal zll_main_compute437_out : std_logic_vector (2 downto 0);
      signal zll_main_compute396_out : std_logic_vector (2 downto 0);
      signal zi5 : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR1\ : std_logic_vector (2 downto 0);
      signal zll_main_compute403_out : std_logic_vector (0 downto 0);
      signal zll_main_compute282_out : std_logic_vector (2 downto 0);
      signal zi6 : std_logic_vector (2 downto 0);
      signal zll_main_compute85_out : std_logic_vector (7 downto 0);
      signal \zll_main_compute85_outR1\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute85_outR2\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute85_outR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute85_outR4\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute85_outR5\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute85_outR6\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute85_outR7\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute437_outR2\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute396_outR1\ : std_logic_vector (2 downto 0);
      signal zi10 : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR3\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute403_outR1\ : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal zll_main_compute417_out : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR4\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute403_outR2\ : std_logic_vector (0 downto 0);
      signal zll_main_compute391_out : std_logic_vector (2 downto 0);
      signal zi12 : std_logic_vector (2 downto 0);
      signal zll_main_compute346_out : std_logic_vector (7 downto 0);
      signal \zll_main_compute346_outR1\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute346_outR2\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute346_outR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute346_outR4\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute346_outR5\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute346_outR6\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute346_outR7\ : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (127 downto 0);
      signal zi14 : std_logic_vector (63 downto 0);
      signal zi15 : std_logic_vector (63 downto 0);
      signal \zll_main_compute437_outR5\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute396_outR2\ : std_logic_vector (2 downto 0);
      signal zi19 : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR6\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute403_outR3\ : std_logic_vector (0 downto 0);
      signal zi20 : std_logic_vector (0 downto 0);
      signal \zll_main_compute417_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR7\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute403_outR4\ : std_logic_vector (0 downto 0);
      signal \zll_main_compute391_outR1\ : std_logic_vector (2 downto 0);
      signal zi21 : std_logic_vector (2 downto 0);
      signal zll_main_compute190_out : std_logic_vector (7 downto 0);
      signal \zll_main_compute190_outR1\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute190_outR2\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute190_outR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute190_outR4\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute190_outR5\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute190_outR6\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute190_outR7\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute437_outR8\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute396_outR3\ : std_logic_vector (2 downto 0);
      signal zi25 : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR9\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute403_outR5\ : std_logic_vector (0 downto 0);
      signal \zll_main_compute282_outR1\ : std_logic_vector (2 downto 0);
      signal zi26 : std_logic_vector (2 downto 0);
      signal zll_main_compute53_out : std_logic_vector (7 downto 0);
      signal \zll_main_compute53_outR1\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute53_outR2\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute53_outR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute53_outR4\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute53_outR5\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute53_outR6\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute53_outR7\ : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (127 downto 0);
      signal zi28 : std_logic_vector (63 downto 0);
      signal zi29 : std_logic_vector (63 downto 0);
      signal zll_main_compute175_out : std_logic_vector (7 downto 0);
      signal \zll_main_compute175_outR1\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute175_outR2\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute175_outR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute175_outR4\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute175_outR5\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute175_outR6\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute175_outR7\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute437_outR10\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute396_outR4\ : std_logic_vector (2 downto 0);
      signal zi36 : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR11\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute403_outR6\ : std_logic_vector (0 downto 0);
      signal zi37 : std_logic_vector (0 downto 0);
      signal \zll_main_compute417_outR2\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute437_outR12\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute403_outR7\ : std_logic_vector (0 downto 0);
      signal \zll_main_compute391_outR2\ : std_logic_vector (2 downto 0);
      signal zi38 : std_logic_vector (2 downto 0);
      signal zll_main_compute452_out : std_logic_vector (7 downto 0);
      signal \zll_main_compute452_outR1\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute452_outR2\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute452_outR3\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute452_outR4\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute452_outR5\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute452_outR6\ : std_logic_vector (7 downto 0);
      signal \zll_main_compute452_outR7\ : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (127 downto 0);
      signal zi40 : std_logic_vector (63 downto 0);
      signal zi41 : std_logic_vector (63 downto 0);
      signal zi42 : std_logic_vector (128 downto 0);
      signal zi43 : std_logic_vector (127 downto 0);
      signal zres : std_logic_vector (128 downto 0);
begin
inst : \ZLL_Main_compute437\ port map (\__in0\, zll_main_compute437_out);
      \instR1\ : \ZLL_Main_compute396\ port map (zll_main_compute437_out, std_logic_vector'(B"010"), zll_main_compute396_out);
      zi5 <= zll_main_compute396_out;
      \instR2\ : \ZLL_Main_compute437\ port map (\__in0\, \zll_main_compute437_outR1\);
      \instR3\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR1\, zll_main_compute403_out);
      \instR4\ : \ZLL_Main_compute282\ port map (zi5, \__in0\, std_logic_vector'(B"001"), zll_main_compute403_out, zll_main_compute282_out);
      zi6 <= zll_main_compute282_out;
      \instR5\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"000"), zll_main_compute85_out);
      \instR6\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"001"), \zll_main_compute85_outR1\);
      \instR7\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"010"), \zll_main_compute85_outR2\);
      \instR8\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"011"), \zll_main_compute85_outR3\);
      \instR9\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"100"), \zll_main_compute85_outR4\);
      \instR10\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"101"), \zll_main_compute85_outR5\);
      \instR11\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"110"), \zll_main_compute85_outR6\);
      \instR12\ : \ZLL_Main_compute85\ port map (zi6, \__in1\, std_logic_vector'(B"010"), \__in0\, std_logic_vector'(B"111"), \zll_main_compute85_outR7\);
      \instR13\ : \ZLL_Main_compute437\ port map (\__in0\, \zll_main_compute437_outR2\);
      \instR14\ : \ZLL_Main_compute396\ port map (\zll_main_compute437_outR2\, std_logic_vector'(B"010"), \zll_main_compute396_outR1\);
      zi10 <= \zll_main_compute396_outR1\;
      \instR15\ : \ZLL_Main_compute437\ port map (\__in0\, \zll_main_compute437_outR3\);
      \instR16\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR3\, \zll_main_compute403_outR1\);
      zi11 <= \zll_main_compute403_outR1\;
      \instR17\ : \ZLL_Main_compute417\ port map (zi10, zll_main_compute417_out);
      \instR18\ : \ZLL_Main_compute437\ port map (\__in0\, \zll_main_compute437_outR4\);
      \instR19\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR4\, \zll_main_compute403_outR2\);
      \instR20\ : \ZLL_Main_compute391\ port map (std_logic_vector'(B"001"), zi10, \zll_main_compute403_outR2\, zll_main_compute391_out);
      zi12 <= rw_cond(rw_eq(zi11, std_logic_vector'(B"1")), zll_main_compute417_out, zll_main_compute391_out);
      \instR21\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"000"), zll_main_compute346_out);
      \instR22\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"001"), \zll_main_compute346_outR1\);
      \instR23\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"010"), \zll_main_compute346_outR2\);
      \instR24\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"011"), \zll_main_compute346_outR3\);
      \instR25\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"100"), \zll_main_compute346_outR4\);
      \instR26\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"101"), \zll_main_compute346_outR5\);
      \instR27\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"110"), \zll_main_compute346_outR6\);
      \instR28\ : \ZLL_Main_compute346\ port map (\__in0\, \__in1\, std_logic_vector'(B"001"), zi12, std_logic_vector'(B"010"), std_logic_vector'(B"111"), \zll_main_compute346_outR7\);
      zi13 <= ((zll_main_compute85_out & \zll_main_compute85_outR1\ & \zll_main_compute85_outR2\ & \zll_main_compute85_outR3\ & \zll_main_compute85_outR4\ & \zll_main_compute85_outR5\ & \zll_main_compute85_outR6\ & \zll_main_compute85_outR7\) & (zll_main_compute346_out & \zll_main_compute346_outR1\ & \zll_main_compute346_outR2\ & \zll_main_compute346_outR3\ & \zll_main_compute346_outR4\ & \zll_main_compute346_outR5\ & \zll_main_compute346_outR6\ & \zll_main_compute346_outR7\));
      zi14 <= zi13(127 downto 64);
      zi15 <= zi13(63 downto 0);
      \instR29\ : \ZLL_Main_compute437\ port map (zi15, \zll_main_compute437_outR5\);
      \instR30\ : \ZLL_Main_compute396\ port map (\zll_main_compute437_outR5\, std_logic_vector'(B"010"), \zll_main_compute396_outR2\);
      zi19 <= \zll_main_compute396_outR2\;
      \instR31\ : \ZLL_Main_compute437\ port map (zi15, \zll_main_compute437_outR6\);
      \instR32\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR6\, \zll_main_compute403_outR3\);
      zi20 <= \zll_main_compute403_outR3\;
      \instR33\ : \ZLL_Main_compute417\ port map (zi19, \zll_main_compute417_outR1\);
      \instR34\ : \ZLL_Main_compute437\ port map (zi15, \zll_main_compute437_outR7\);
      \instR35\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR7\, \zll_main_compute403_outR4\);
      \instR36\ : \ZLL_Main_compute391\ port map (std_logic_vector'(B"001"), zi19, \zll_main_compute403_outR4\, \zll_main_compute391_outR1\);
      zi21 <= rw_cond(rw_eq(zi20, std_logic_vector'(B"1")), \zll_main_compute417_outR1\, \zll_main_compute391_outR1\);
      \instR37\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"000"), zll_main_compute190_out);
      \instR38\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"001"), \zll_main_compute190_outR1\);
      \instR39\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"010"), \zll_main_compute190_outR2\);
      \instR40\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"011"), \zll_main_compute190_outR3\);
      \instR41\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"100"), \zll_main_compute190_outR4\);
      \instR42\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"101"), \zll_main_compute190_outR5\);
      \instR43\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"110"), \zll_main_compute190_outR6\);
      \instR44\ : \ZLL_Main_compute190\ port map (zi21, std_logic_vector'(B"010"), zi14, zi15, std_logic_vector'(B"111"), \zll_main_compute190_outR7\);
      \instR45\ : \ZLL_Main_compute437\ port map (zi15, \zll_main_compute437_outR8\);
      \instR46\ : \ZLL_Main_compute396\ port map (\zll_main_compute437_outR8\, std_logic_vector'(B"010"), \zll_main_compute396_outR3\);
      zi25 <= \zll_main_compute396_outR3\;
      \instR47\ : \ZLL_Main_compute437\ port map (zi15, \zll_main_compute437_outR9\);
      \instR48\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR9\, \zll_main_compute403_outR5\);
      \instR49\ : \ZLL_Main_compute282\ port map (zi25, zi15, std_logic_vector'(B"001"), \zll_main_compute403_outR5\, \zll_main_compute282_outR1\);
      zi26 <= \zll_main_compute282_outR1\;
      \instR50\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"000"), zll_main_compute53_out);
      \instR51\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"001"), \zll_main_compute53_outR1\);
      \instR52\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"010"), \zll_main_compute53_outR2\);
      \instR53\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"011"), \zll_main_compute53_outR3\);
      \instR54\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"100"), \zll_main_compute53_outR4\);
      \instR55\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"101"), \zll_main_compute53_outR5\);
      \instR56\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"110"), \zll_main_compute53_outR6\);
      \instR57\ : \ZLL_Main_compute53\ port map (zi26, zi15, std_logic_vector'(B"001"), std_logic_vector'(B"010"), zi14, std_logic_vector'(B"111"), \zll_main_compute53_outR7\);
      zi27 <= ((zll_main_compute190_out & \zll_main_compute190_outR1\ & \zll_main_compute190_outR2\ & \zll_main_compute190_outR3\ & \zll_main_compute190_outR4\ & \zll_main_compute190_outR5\ & \zll_main_compute190_outR6\ & \zll_main_compute190_outR7\) & (zll_main_compute53_out & \zll_main_compute53_outR1\ & \zll_main_compute53_outR2\ & \zll_main_compute53_outR3\ & \zll_main_compute53_outR4\ & \zll_main_compute53_outR5\ & \zll_main_compute53_outR6\ & \zll_main_compute53_outR7\));
      zi28 <= zi27(127 downto 64);
      zi29 <= zi27(63 downto 0);
      \instR58\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"000"), zll_main_compute175_out);
      \instR59\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"001"), \zll_main_compute175_outR1\);
      \instR60\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"010"), \zll_main_compute175_outR2\);
      \instR61\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"011"), \zll_main_compute175_outR3\);
      \instR62\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"100"), \zll_main_compute175_outR4\);
      \instR63\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"101"), \zll_main_compute175_outR5\);
      \instR64\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"110"), \zll_main_compute175_outR6\);
      \instR65\ : \ZLL_Main_compute175\ port map (zi29, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), std_logic_vector'(B"111"), \zll_main_compute175_outR7\);
      \instR66\ : \ZLL_Main_compute437\ port map (zi28, \zll_main_compute437_outR10\);
      \instR67\ : \ZLL_Main_compute396\ port map (\zll_main_compute437_outR10\, std_logic_vector'(B"010"), \zll_main_compute396_outR4\);
      zi36 <= \zll_main_compute396_outR4\;
      \instR68\ : \ZLL_Main_compute437\ port map (zi28, \zll_main_compute437_outR11\);
      \instR69\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR11\, \zll_main_compute403_outR6\);
      zi37 <= \zll_main_compute403_outR6\;
      \instR70\ : \ZLL_Main_compute417\ port map (zi36, \zll_main_compute417_outR2\);
      \instR71\ : \ZLL_Main_compute437\ port map (zi28, \zll_main_compute437_outR12\);
      \instR72\ : \ZLL_Main_compute403\ port map (\zll_main_compute437_outR12\, \zll_main_compute403_outR7\);
      \instR73\ : \ZLL_Main_compute391\ port map (std_logic_vector'(B"001"), zi36, \zll_main_compute403_outR7\, \zll_main_compute391_outR2\);
      zi38 <= rw_cond(rw_eq(zi37, std_logic_vector'(B"1")), \zll_main_compute417_outR2\, \zll_main_compute391_outR2\);
      \instR74\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"000"), zll_main_compute452_out);
      \instR75\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"001"), \zll_main_compute452_outR1\);
      \instR76\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"010"), \zll_main_compute452_outR2\);
      \instR77\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"011"), \zll_main_compute452_outR3\);
      \instR78\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"100"), \zll_main_compute452_outR4\);
      \instR79\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"101"), \zll_main_compute452_outR5\);
      \instR80\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"110"), \zll_main_compute452_outR6\);
      \instR81\ : \ZLL_Main_compute452\ port map (zi29, std_logic_vector'(B"001"), zi38, std_logic_vector'(B"010"), zi28, std_logic_vector'(B"111"), \zll_main_compute452_outR7\);
      zi39 <= (zll_main_compute175_out & \zll_main_compute175_outR1\ & \zll_main_compute175_outR2\ & \zll_main_compute175_outR3\ & \zll_main_compute175_outR4\ & \zll_main_compute175_outR5\ & \zll_main_compute175_outR6\ & \zll_main_compute175_outR7\ & (zll_main_compute452_out & \zll_main_compute452_outR1\ & \zll_main_compute452_outR2\ & \zll_main_compute452_outR3\ & \zll_main_compute452_outR4\ & \zll_main_compute452_outR5\ & \zll_main_compute452_outR6\ & \zll_main_compute452_outR7\));
      zi40 <= zi39(127 downto 64);
      zi41 <= zi39(63 downto 0);
      zi42 <= (std_logic_vector'(B"0") & (zi40 & zi41));
      zi43 <= zi42(127 downto 0);
      zres <= (std_logic_vector'(B"1") & zi43);
      \__out0\ <= zres(127 downto 64);
      \__out1\ <= zres(63 downto 0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute452\ is
port (arg0 : in std_logic_vector (63 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (63 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute452\ is
component \ZLL_Main_compute352\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute396\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute401\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute403\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zll_main_compute403_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_compute396_out : std_logic_vector (2 downto 0);
      signal zll_main_compute401_out : std_logic_vector (2 downto 0);
      signal zll_main_compute352_out : std_logic_vector (2 downto 0);
      signal \zll_main_compute396_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute401_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute403\ port map (arg5, zll_main_compute403_out);
      zi0 <= zll_main_compute403_out;
      \instR1\ : \ZLL_Main_compute396\ port map (arg5, arg3, zll_main_compute396_out);
      \instR2\ : \ZLL_Main_compute401\ port map (arg2, zll_main_compute396_out, zll_main_compute401_out);
      \instR3\ : \ZLL_Main_compute352\ port map (arg5, arg1, zll_main_compute352_out);
      \instR4\ : \ZLL_Main_compute396\ port map (zll_main_compute352_out, arg3, \zll_main_compute396_outR1\);
      \instR5\ : \ZLL_Main_compute401\ port map (arg2, \zll_main_compute396_outR1\, \zll_main_compute401_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg4, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_compute401_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_compute401_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute437\ is
port (arg0 : in std_logic_vector (63 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute437\ is

begin
res <= std_logic_vector'(B"111");
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute435\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute435\ is

begin
res <= rw_resize(rw_mod(rw_mul(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute417\ is
port (arg0 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute417\ is

begin
res <= arg0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute403\ is
port (arg0 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute403\ is
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
entity \ZLL_Main_compute401\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute401\ is

begin
res <= rw_resize(rw_mod(rw_add(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute396\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute396\ is

begin
res <= rw_resize(rw_mod(rw_div(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute391\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute391\ is
component \ZLL_Main_compute401\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_compute401_out : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute401\ port map (arg1, arg0, zll_main_compute401_out);
      res <= zll_main_compute401_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute352\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute352\ is

begin
res <= rw_resize(rw_mod(rw_sub(rw_resize(arg0, 128), rw_resize(arg1, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000")), 3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute346\ is
port (arg0 : in std_logic_vector (63 downto 0);
      arg1 : in std_logic_vector (63 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute346\ is
component \ZLL_Main_compute307\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_compute352\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute401\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute435\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_compute307_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_compute435_out : std_logic_vector (2 downto 0);
      signal zll_main_compute401_out : std_logic_vector (2 downto 0);
      signal zll_main_compute352_out : std_logic_vector (2 downto 0);
      signal \zll_main_compute435_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute401_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute307\ port map (arg5, arg3, zll_main_compute307_out);
      zi0 <= zll_main_compute307_out;
      \instR1\ : \ZLL_Main_compute435\ port map (arg5, arg4, zll_main_compute435_out);
      \instR2\ : \ZLL_Main_compute401\ port map (zll_main_compute435_out, arg2, zll_main_compute401_out);
      \instR3\ : \ZLL_Main_compute352\ port map (arg5, arg3, zll_main_compute352_out);
      \instR4\ : \ZLL_Main_compute435\ port map (zll_main_compute352_out, arg4, \zll_main_compute435_outR1\);
      \instR5\ : \ZLL_Main_compute401\ port map (\zll_main_compute435_outR1\, arg2, \zll_main_compute401_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_compute401_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8), rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_compute401_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute307\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute307\ is

begin
res <= rw_lt(rw_resize(arg0, 128), rw_resize(arg1, 128));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute282\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (63 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (2 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute282\ is
component \ZLL_Main_compute401\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute417\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_compute417_out : std_logic_vector (2 downto 0);
      signal zll_main_compute401_out : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute417\ port map (arg0, zll_main_compute417_out);
      \instR1\ : \ZLL_Main_compute401\ port map (arg0, arg2, zll_main_compute401_out);
      res <= rw_cond(rw_eq(arg3, std_logic_vector'(B"1")), zll_main_compute417_out, zll_main_compute401_out);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute190\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (63 downto 0);
      arg3 : in std_logic_vector (63 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute190\ is
component \ZLL_Main_compute307\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_compute352\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute435\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_compute307_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_compute435_out : std_logic_vector (2 downto 0);
      signal zll_main_compute352_out : std_logic_vector (2 downto 0);
      signal \zll_main_compute435_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute307\ port map (arg4, arg0, zll_main_compute307_out);
      zi0 <= zll_main_compute307_out;
      \instR1\ : \ZLL_Main_compute435\ port map (arg4, arg1, zll_main_compute435_out);
      \instR2\ : \ZLL_Main_compute352\ port map (arg4, arg0, zll_main_compute352_out);
      \instR3\ : \ZLL_Main_compute435\ port map (zll_main_compute352_out, arg1, \zll_main_compute435_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg3, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_compute435_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8), rw_resize(rw_shiftr(arg2, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_compute435_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute175\ is
port (arg0 : in std_logic_vector (63 downto 0);
      arg1 : in std_logic_vector (2 downto 0);
      arg2 : in std_logic_vector (63 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute175\ is
component \ZLL_Main_compute352\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute396\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute403\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      signal zll_main_compute403_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_compute396_out : std_logic_vector (2 downto 0);
      signal zll_main_compute352_out : std_logic_vector (2 downto 0);
      signal \zll_main_compute396_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute403\ port map (arg4, zll_main_compute403_out);
      zi0 <= zll_main_compute403_out;
      \instR1\ : \ZLL_Main_compute396\ port map (arg4, arg1, zll_main_compute396_out);
      \instR2\ : \ZLL_Main_compute352\ port map (arg4, arg3, zll_main_compute352_out);
      \instR3\ : \ZLL_Main_compute396\ port map (zll_main_compute352_out, arg1, \zll_main_compute396_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg2, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_compute396_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8), rw_resize(rw_shiftr(arg0, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_compute396_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute85\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (63 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (63 downto 0);
      arg4 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute85\ is
component \ZLL_Main_compute307\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_compute352\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute435\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_compute307_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_compute435_out : std_logic_vector (2 downto 0);
      signal zll_main_compute352_out : std_logic_vector (2 downto 0);
      signal \zll_main_compute435_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute307\ port map (arg4, arg0, zll_main_compute307_out);
      zi0 <= zll_main_compute307_out;
      \instR1\ : \ZLL_Main_compute435\ port map (arg4, arg2, zll_main_compute435_out);
      \instR2\ : \ZLL_Main_compute352\ port map (arg4, arg0, zll_main_compute352_out);
      \instR3\ : \ZLL_Main_compute435\ port map (zll_main_compute352_out, arg2, \zll_main_compute435_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg3, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_compute435_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8), rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_compute435_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_compute53\ is
port (arg0 : in std_logic_vector (2 downto 0);
      arg1 : in std_logic_vector (63 downto 0);
      arg2 : in std_logic_vector (2 downto 0);
      arg3 : in std_logic_vector (2 downto 0);
      arg4 : in std_logic_vector (63 downto 0);
      arg5 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \ZLL_Main_compute53\ is
component \ZLL_Main_compute307\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_compute352\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute401\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      component \ZLL_Main_compute435\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            arg1 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (2 downto 0));
      end component;
      signal zll_main_compute307_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zll_main_compute435_out : std_logic_vector (2 downto 0);
      signal zll_main_compute401_out : std_logic_vector (2 downto 0);
      signal zll_main_compute352_out : std_logic_vector (2 downto 0);
      signal \zll_main_compute435_outR1\ : std_logic_vector (2 downto 0);
      signal \zll_main_compute401_outR1\ : std_logic_vector (2 downto 0);
begin
inst : \ZLL_Main_compute307\ port map (arg5, arg0, zll_main_compute307_out);
      zi0 <= zll_main_compute307_out;
      \instR1\ : \ZLL_Main_compute435\ port map (arg5, arg3, zll_main_compute435_out);
      \instR2\ : \ZLL_Main_compute401\ port map (zll_main_compute435_out, arg2, zll_main_compute401_out);
      \instR3\ : \ZLL_Main_compute352\ port map (arg5, arg0, zll_main_compute352_out);
      \instR4\ : \ZLL_Main_compute435\ port map (zll_main_compute352_out, arg3, \zll_main_compute435_outR1\);
      \instR5\ : \ZLL_Main_compute401\ port map (\zll_main_compute435_outR1\, arg2, \zll_main_compute401_outR1\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"1")), rw_resize(rw_shiftr(arg1, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(zll_main_compute401_out, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8), rw_resize(rw_shiftr(arg4, rw_mul(rw_sub(rw_sub(std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"), rw_resize(\zll_main_compute401_outR1\, 128)), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")), std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000"))), 8));
end architecture;