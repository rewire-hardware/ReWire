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
  function rw_xnor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_not (a : std_logic_vector) return std_logic_vector;
  function rw_shiftl (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_shiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_ashiftr (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_land (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_lor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector;
  function rw_lnot (a : std_logic_vector) return std_logic_vector;
  function rw_rand (a : std_logic_vector) return std_logic_vector;
  function rw_rnand (a : std_logic_vector) return std_logic_vector;
  function rw_ror (a : std_logic_vector) return std_logic_vector;
  function rw_rnor (a : std_logic_vector) return std_logic_vector;
  function rw_rxor (a : std_logic_vector) return std_logic_vector;
  function rw_rxnor (a : std_logic_vector) return std_logic_vector;
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
  function rw_xnor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
    constant n : natural := rw_max(a'length, b'length);
  begin
    return rw_resize(a, n) xnor rw_resize(b, n);
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
  function rw_land (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(unsigned(a) /= 0 and unsigned(b) /= 0);
  end;
  function rw_lor (a : std_logic_vector; b : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(unsigned(a) /= 0 or unsigned(b) /= 0);
  end;
  function rw_lnot (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v(unsigned(a) = 0);
  end;
  function rw_rand (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((and a) = '1');
  end;
  function rw_rnand (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((and a) /= '1');
  end;
  function rw_ror (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((or a) = '1');
  end;
  function rw_rnor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((or a) /= '1');
  end;
  function rw_rxor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((xor a) = '1');
  end;
  function rw_rxnor (a : std_logic_vector) return std_logic_vector is
  begin
    return rw_b2v((xor a) /= '1');
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
      \__out0\ : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of top_level is
component \ReWire_Prelude_not\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_Main_sig15\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      component \ZLL_Main_sig24\ is
      port (arg0 : in std_logic_vector (4 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      component \ZLL_Main_sig35\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal \__st0\ : std_logic_vector (0 downto 0) := std_logic_vector'(B"1");
      signal \__st0_next\ : std_logic_vector (0 downto 0);
      signal \__st1\ : std_logic_vector (0 downto 0) := std_logic_vector'(B"1");
      signal \__st1_next\ : std_logic_vector (0 downto 0);
      signal zin : std_logic_vector (2 downto 0);
      signal zi0 : std_logic_vector (1 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (2 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal rewire_prelude_not_out : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (2 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (2 downto 0);
      signal zi10 : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal zi12 : std_logic_vector (0 downto 0);
      signal zi13 : std_logic_vector (4 downto 0);
      signal zi14 : std_logic_vector (0 downto 0);
      signal zi15 : std_logic_vector (0 downto 0);
      signal zi16 : std_logic_vector (0 downto 0);
      signal conn : std_logic_vector (2 downto 0);
      signal zll_main_sig35_out : std_logic_vector (4 downto 0);
      signal zi17 : std_logic_vector (4 downto 0);
      signal zi18 : std_logic_vector (5 downto 0);
      signal zi19 : std_logic_vector (0 downto 0);
      signal zi20 : std_logic_vector (0 downto 0);
      signal zi21 : std_logic_vector (0 downto 0);
      signal zi22 : std_logic_vector (0 downto 0);
      signal \connR1\ : std_logic_vector (1 downto 0);
      signal zll_main_sig15_out : std_logic_vector (4 downto 0);
      signal zi23 : std_logic_vector (4 downto 0);
      signal zi24 : std_logic_vector (6 downto 0);
      signal zi25 : std_logic_vector (0 downto 0);
      signal zi26 : std_logic_vector (0 downto 0);
      signal zi27 : std_logic_vector (0 downto 0);
      signal zi28 : std_logic_vector (0 downto 0);
      signal zi29 : std_logic_vector (1 downto 0);
      signal zi30 : std_logic_vector (0 downto 0);
      signal zi31 : std_logic_vector (0 downto 0);
      signal zi32 : std_logic_vector (0 downto 0);
      signal \connR2\ : std_logic_vector (1 downto 0);
      signal \zll_main_sig15_outR1\ : std_logic_vector (4 downto 0);
      signal zll_main_sig24_out : std_logic_vector (4 downto 0);
      signal \rewire_prelude_not_outR1\ : std_logic_vector (0 downto 0);
      signal zi33 : std_logic_vector (2 downto 0);
      signal zi34 : std_logic_vector (0 downto 0);
      signal zi35 : std_logic_vector (0 downto 0);
      signal \connR3\ : std_logic_vector (4 downto 0);
      signal \zll_main_sig24_outR1\ : std_logic_vector (4 downto 0);
      signal zres : std_logic_vector (4 downto 0);
begin
zin <= (\__st0\ & \__st1\ & \__in0\);
      zi0 <= zin(2 downto 1);
      zi1 <= zin(0 downto 0);
      zi2 <= (zi1 & zi0);
      zi3 <= zi2(2 downto 2);
      zi4 <= zi2(1 downto 1);
      zi5 <= zi2(0 downto 0);
      inst : \ReWire_Prelude_not\ port map (zi3, rewire_prelude_not_out);
      zi6 <= (zi5 & zi4 & rewire_prelude_not_out);
      zi7 <= zi6(2 downto 2);
      zi8 <= zi6(1 downto 1);
      zi9 <= (zi8 & zi8 & zi7);
      zi10 <= zi9(2 downto 2);
      zi11 <= zi9(1 downto 1);
      zi12 <= zi9(0 downto 0);
      zi13 <= (std_logic_vector'(B"00") & zi10 & zi11 & zi12);
      zi14 <= zi13(2 downto 2);
      zi15 <= zi13(1 downto 1);
      zi16 <= zi13(0 downto 0);
      conn <= (zi16 & zi15 & zi16);
      \instR1\ : \ZLL_Main_sig35\ port map (conn, zll_main_sig35_out);
      zi17 <= zll_main_sig35_out;
      zi18 <= (zi14 & zi17);
      zi19 <= zi18(5 downto 5);
      zi20 <= zi18(2 downto 2);
      zi21 <= zi18(1 downto 1);
      zi22 <= zi18(0 downto 0);
      \connR1\ <= (zi20 & zi22);
      \instR2\ : \ZLL_Main_sig15\ port map (\connR1\, zll_main_sig15_out);
      zi23 <= zll_main_sig15_out;
      zi24 <= (zi20 & zi19 & zi23);
      zi25 <= zi24(6 downto 6);
      zi26 <= zi24(5 downto 5);
      zi27 <= zi24(1 downto 1);
      zi28 <= zi24(0 downto 0);
      zi29 <= (zi26 & zi25);
      zi30 <= zi29(1 downto 1);
      zi31 <= zi29(0 downto 0);
      zi32 <= rw_xor(zi30, zi31);
      \connR2\ <= (zi27 & zi32);
      \instR3\ : \ZLL_Main_sig15\ port map (\connR2\, \zll_main_sig15_outR1\);
      \instR4\ : \ZLL_Main_sig24\ port map (\zll_main_sig15_outR1\, zll_main_sig24_out);
      \instR5\ : \ReWire_Prelude_not\ port map (zi3, \rewire_prelude_not_outR1\);
      zi33 <= (zi5 & zi4 & \rewire_prelude_not_outR1\);
      zi34 <= zi33(2 downto 2);
      zi35 <= zi33(1 downto 1);
      \connR3\ <= (std_logic_vector'(B"010") & zi35 & zi34);
      \instR6\ : \ZLL_Main_sig24\ port map (\connR3\, \zll_main_sig24_outR1\);
      zres <= rw_cond(rw_eq(zi6(0 downto 0), std_logic_vector'(B"1")), zll_main_sig24_out, \zll_main_sig24_outR1\);
      \__st0_next\ <= zres(1 downto 1);
      \__st1_next\ <= zres(0 downto 0);
      \__out0\ <= zres(2 downto 2);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"1");
                  \__st1\ <= std_logic_vector'(B"1");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
            end if;
      end process;
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
entity \ZLL_Main_sig35\ is
port (arg0 : in std_logic_vector (2 downto 0);
      res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \ZLL_Main_sig35\ is
signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
begin
zi0 <= arg0(2 downto 2);
      zi1 <= arg0(1 downto 1);
      zi2 <= arg0(0 downto 0);
      res <= (std_logic_vector'(B"00") & zi0 & zi1 & zi2);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_sig24\ is
port (arg0 : in std_logic_vector (4 downto 0);
      res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \ZLL_Main_sig24\ is
component \ZLL_Main_sig35\ is
      port (arg0 : in std_logic_vector (2 downto 0);
            res : out std_logic_vector (4 downto 0));
      end component;
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal conn : std_logic_vector (2 downto 0);
      signal zll_main_sig35_out : std_logic_vector (4 downto 0);
      signal zi2 : std_logic_vector (4 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
begin
zi0 <= arg0(1 downto 1);
      zi1 <= arg0(0 downto 0);
      conn <= (zi0 & zi0 & zi1);
      inst : \ZLL_Main_sig35\ port map (conn, zll_main_sig35_out);
      zi2 <= zll_main_sig35_out;
      zi3 <= zi2(2 downto 2);
      zi4 <= zi2(1 downto 1);
      zi5 <= zi2(0 downto 0);
      res <= (std_logic_vector'(B"10") & zi3 & zi4 & zi5);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_sig15\ is
port (arg0 : in std_logic_vector (1 downto 0);
      res : out std_logic_vector (4 downto 0));
end entity;

architecture rtl of \ZLL_Main_sig15\ is
signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
begin
zi0 <= arg0(1 downto 1);
      zi1 <= arg0(0 downto 0);
      res <= (std_logic_vector'(B"010") & zi0 & zi1);
end architecture;