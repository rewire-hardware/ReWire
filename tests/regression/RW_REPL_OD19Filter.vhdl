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
      \__in1\ : in std_logic_vector (31 downto 0);
      \__out0\ : out std_logic_vector (0 downto 0);
      \__out1\ : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of top_level is
component \Main_getReg\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (70 downto 0));
      end component;
      component \Main_nextPC\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (38 downto 0));
      end component;
      component \ZLL_Main_repl101\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZLL_Main_repl112\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            arg1 : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZLL_Main_repl43\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZLL_Main_repl77\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            arg1 : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZLL_Main_repl92\ is
      port (arg0 : in std_logic_vector (70 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component test2 is
      port (p0 : in std_logic_vector (31 downto 0);
            p1 : in std_logic_vector (31 downto 0);
            p2 : out std_logic_vector (0 downto 0));
      end component;
      component test3 is
      port (p0 : in std_logic_vector (31 downto 0);
            p1 : in std_logic_vector (31 downto 0);
            p2 : out std_logic_vector (0 downto 0));
      end component;
      component test4 is
      port (p0 : in std_logic_vector (31 downto 0);
            p1 : in std_logic_vector (31 downto 0);
            p2 : out std_logic_vector (0 downto 0));
      end component;
      signal \__st0\ : std_logic_vector (31 downto 0) := std_logic_vector'(B"00000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (31 downto 0);
      signal \__st1\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000000");
      signal \__st1_next\ : std_logic_vector (6 downto 0);
      signal zin : std_logic_vector (71 downto 0);
      signal zi0 : std_logic_vector (38 downto 0);
      signal zi1 : std_logic_vector (32 downto 0);
      signal zi2 : std_logic_vector (71 downto 0);
      signal zi3 : std_logic_vector (32 downto 0);
      signal zi4 : std_logic_vector (38 downto 0);
      signal zi5 : std_logic_vector (71 downto 0);
      signal zi6 : std_logic_vector (38 downto 0);
      signal zi7 : std_logic_vector (31 downto 0);
      signal zi8 : std_logic_vector (77 downto 0);
      signal zi9 : std_logic_vector (38 downto 0);
      signal zi10 : std_logic_vector (38 downto 0);
      signal zi11 : std_logic_vector (80 downto 0);
      signal zi12 : std_logic_vector (112 downto 0);
      signal zi13 : std_logic_vector (31 downto 0);
      signal zi14 : std_logic_vector (38 downto 0);
      signal zi15 : std_logic_vector (38 downto 0);
      signal zi16 : std_logic_vector (109 downto 0);
      signal zi17 : std_logic_vector (38 downto 0);
      signal zi18 : std_logic_vector (31 downto 0);
      signal zi19 : std_logic_vector (31 downto 0);
      signal zi20 : std_logic_vector (6 downto 0);
      signal zi21 : std_logic_vector (77 downto 0);
      signal zll_main_repl77_out : std_logic_vector (80 downto 0);
      signal zi22 : std_logic_vector (77 downto 0);
      signal zll_main_repl112_out : std_logic_vector (80 downto 0);
      signal zi23 : std_logic_vector (77 downto 0);
      signal \zll_main_repl77_outR1\ : std_logic_vector (80 downto 0);
      signal zi24 : std_logic_vector (77 downto 0);
      signal \zll_main_repl112_outR1\ : std_logic_vector (80 downto 0);
      signal zi25 : std_logic_vector (77 downto 0);
      signal zi26 : std_logic_vector (38 downto 0);
      signal zi27 : std_logic_vector (31 downto 0);
      signal main_getreg_out : std_logic_vector (70 downto 0);
      signal zll_main_repl92_out : std_logic_vector (80 downto 0);
      signal zi28 : std_logic_vector (80 downto 0);
      signal zi29 : std_logic_vector (112 downto 0);
      signal zi30 : std_logic_vector (31 downto 0);
      signal zi31 : std_logic_vector (31 downto 0);
      signal zi32 : std_logic_vector (38 downto 0);
      signal extres : std_logic_vector (0 downto 0);
      signal zi33 : std_logic_vector (77 downto 0);
      signal zi34 : std_logic_vector (38 downto 0);
      signal zi35 : std_logic_vector (31 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (70 downto 0);
      signal \zll_main_repl92_outR1\ : std_logic_vector (80 downto 0);
      signal zi36 : std_logic_vector (80 downto 0);
      signal zi37 : std_logic_vector (112 downto 0);
      signal zi38 : std_logic_vector (31 downto 0);
      signal zi39 : std_logic_vector (31 downto 0);
      signal zi40 : std_logic_vector (38 downto 0);
      signal \extresR1\ : std_logic_vector (0 downto 0);
      signal zi41 : std_logic_vector (77 downto 0);
      signal zi42 : std_logic_vector (38 downto 0);
      signal zi43 : std_logic_vector (31 downto 0);
      signal \main_getreg_outR2\ : std_logic_vector (70 downto 0);
      signal \zll_main_repl92_outR2\ : std_logic_vector (80 downto 0);
      signal zi44 : std_logic_vector (80 downto 0);
      signal zi45 : std_logic_vector (112 downto 0);
      signal zi46 : std_logic_vector (31 downto 0);
      signal zi47 : std_logic_vector (31 downto 0);
      signal zi48 : std_logic_vector (38 downto 0);
      signal \extresR2\ : std_logic_vector (0 downto 0);
      signal main_nextpc_out : std_logic_vector (38 downto 0);
      signal zll_main_repl101_out : std_logic_vector (80 downto 0);
      signal zll_main_repl43_out : std_logic_vector (80 downto 0);
      signal zi49 : std_logic_vector (71 downto 0);
      signal zi50 : std_logic_vector (38 downto 0);
      signal zres : std_logic_vector (80 downto 0);
begin
zin <= (\__st0\ & \__st1\ & \__in0\ & \__in1\);
      zi0 <= zin(71 downto 33);
      zi1 <= zin(32 downto 0);
      zi2 <= (zi1 & zi0);
      zi3 <= zi2(71 downto 39);
      zi4 <= zi2(38 downto 0);
      zi5 <= (zi4 & zi3);
      zi6 <= zi5(71 downto 33);
      zi7 <= zi5(31 downto 0);
      zi8 <= (zi6 & zi6);
      zi9 <= zi8(77 downto 39);
      zi10 <= zi8(38 downto 0);
      zi11 <= (std_logic_vector'(B"010") & zi9 & zi10);
      zi12 <= (zi7 & zi11);
      zi13 <= zi12(112 downto 81);
      zi14 <= zi12(77 downto 39);
      zi15 <= zi12(38 downto 0);
      zi16 <= (zi15 & zi13 & zi14);
      zi17 <= zi16(109 downto 71);
      zi18 <= zi16(70 downto 39);
      zi19 <= zi16(38 downto 7);
      zi20 <= zi16(6 downto 0);
      zi21 <= (zi17 & zi18 & zi20);
      inst : \ZLL_Main_repl77\ port map (zi21(77 downto 39), zi21(38 downto 7), zll_main_repl77_out);
      zi22 <= (zi17 & zi18 & zi20);
      \instR1\ : \ZLL_Main_repl112\ port map (zi22(77 downto 39), zi22(38 downto 7), zll_main_repl112_out);
      zi23 <= (zi17 & zi18 & zi20);
      \instR2\ : \ZLL_Main_repl77\ port map (zi23(77 downto 39), zi23(38 downto 7), \zll_main_repl77_outR1\);
      zi24 <= (zi17 & zi18 & zi20);
      \instR3\ : \ZLL_Main_repl112\ port map (zi24(77 downto 39), zi24(38 downto 7), \zll_main_repl112_outR1\);
      zi25 <= (zi17 & zi18 & zi20);
      zi26 <= zi25(77 downto 39);
      zi27 <= zi25(38 downto 7);
      \instR4\ : \Main_getReg\ port map (zi26, main_getreg_out);
      \instR5\ : \ZLL_Main_repl92\ port map (main_getreg_out, zll_main_repl92_out);
      zi28 <= zll_main_repl92_out;
      zi29 <= (zi27 & zi28);
      zi30 <= zi29(112 downto 81);
      zi31 <= zi29(70 downto 39);
      zi32 <= zi29(38 downto 0);
      \instR6\ : test2 port map (p0 => zi30, p1 => zi31, p2 => extres(0 downto 0));
      zi33 <= (zi17 & zi18 & zi20);
      zi34 <= zi33(77 downto 39);
      zi35 <= zi33(38 downto 7);
      \instR7\ : \Main_getReg\ port map (zi34, \main_getreg_outR1\);
      \instR8\ : \ZLL_Main_repl92\ port map (\main_getreg_outR1\, \zll_main_repl92_outR1\);
      zi36 <= \zll_main_repl92_outR1\;
      zi37 <= (zi35 & zi36);
      zi38 <= zi37(112 downto 81);
      zi39 <= zi37(70 downto 39);
      zi40 <= zi37(38 downto 0);
      \instR9\ : test3 port map (p0 => zi38, p1 => zi39, p2 => \extresR1\(0 downto 0));
      zi41 <= (zi17 & zi18 & zi20);
      zi42 <= zi41(77 downto 39);
      zi43 <= zi41(38 downto 7);
      \instR10\ : \Main_getReg\ port map (zi42, \main_getreg_outR2\);
      \instR11\ : \ZLL_Main_repl92\ port map (\main_getreg_outR2\, \zll_main_repl92_outR2\);
      zi44 <= \zll_main_repl92_outR2\;
      zi45 <= (zi43 & zi44);
      zi46 <= zi45(112 downto 81);
      zi47 <= zi45(70 downto 39);
      zi48 <= zi45(38 downto 0);
      \instR12\ : test4 port map (p0 => zi46, p1 => zi47, p2 => \extresR2\(0 downto 0));
      \instR13\ : \Main_nextPC\ port map (zi17, main_nextpc_out);
      \instR14\ : \ZLL_Main_repl101\ port map (main_nextpc_out, zll_main_repl101_out);
      \instR15\ : \ZLL_Main_repl43\ port map (zll_main_repl101_out, zll_main_repl43_out);
      zi49 <= (zi4 & zi3);
      zi50 <= zi49(71 downto 33);
      zres <= rw_cond(rw_eq(zi5(32 downto 32), std_logic_vector'(B"0")), rw_cond(rw_eq(zi21(6 downto 0), std_logic_vector'(B"0000000")), zll_main_repl77_out, rw_cond(rw_eq(zi22(6 downto 0), std_logic_vector'(B"0000001")), zll_main_repl112_out, rw_cond(rw_eq(zi23(6 downto 0), std_logic_vector'(B"0000010")), \zll_main_repl77_outR1\, rw_cond(rw_eq(zi24(6 downto 0), std_logic_vector'(B"0000011")), \zll_main_repl112_outR1\, rw_cond(rw_eq(zi25(6 downto 0), std_logic_vector'(B"0000100")), ((std_logic_vector'(B"1") & rw_repl(40, std_logic_vector'(B"0"))) & extres & zi32), rw_cond(rw_eq(zi33(6 downto 0), std_logic_vector'(B"0000101")), ((std_logic_vector'(B"1") & rw_repl(40, std_logic_vector'(B"0"))) & \extresR1\ & zi40), rw_cond(rw_eq(zi41(6 downto 0), std_logic_vector'(B"0000110")), ((std_logic_vector'(B"1") & rw_repl(40, std_logic_vector'(B"0"))) & \extresR2\ & zi48), zll_main_repl43_out))))))), (std_logic_vector'(B"100000000000000000000000000000000000000010") & zi50));
      \__st0_next\ <= zres(38 downto 7);
      \__st1_next\ <= zres(6 downto 0);
      \__out0\ <= zres(40 downto 40);
      \__out1\ <= zres(39 downto 39);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"00000000000000000000000000000000");
                  \__st1\ <= std_logic_vector'(B"0000000");
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
entity \ZLL_Main_repl112\ is
port (arg0 : in std_logic_vector (38 downto 0);
      arg1 : in std_logic_vector (31 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \ZLL_Main_repl112\ is
component \Main_getReg\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (70 downto 0));
      end component;
      component \ZLL_Main_repl92\ is
      port (arg0 : in std_logic_vector (70 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component test1 is
      port (p0 : in std_logic_vector (31 downto 0);
            p1 : in std_logic_vector (31 downto 0);
            p2 : out std_logic_vector (0 downto 0));
      end component;
      signal main_getreg_out : std_logic_vector (70 downto 0);
      signal zll_main_repl92_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal zi1 : std_logic_vector (112 downto 0);
      signal zi2 : std_logic_vector (31 downto 0);
      signal zi3 : std_logic_vector (31 downto 0);
      signal zi4 : std_logic_vector (38 downto 0);
      signal extres : std_logic_vector (0 downto 0);
begin
inst : \Main_getReg\ port map (arg0, main_getreg_out);
      \instR1\ : \ZLL_Main_repl92\ port map (main_getreg_out, zll_main_repl92_out);
      zi0 <= zll_main_repl92_out;
      zi1 <= (arg1 & zi0);
      zi2 <= zi1(112 downto 81);
      zi3 <= zi1(70 downto 39);
      zi4 <= zi1(38 downto 0);
      \instR2\ : test1 port map (p0 => zi2, p1 => zi3, p2 => extres(0 downto 0));
      res <= ((std_logic_vector'(B"1") & rw_repl(40, std_logic_vector'(B"0"))) & extres & zi4);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_repl101\ is
port (arg0 : in std_logic_vector (38 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \ZLL_Main_repl101\ is

begin
res <= ((std_logic_vector'(B"001") & rw_repl(39, std_logic_vector'(B"0"))) & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_repl92\ is
port (arg0 : in std_logic_vector (70 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \ZLL_Main_repl92\ is
signal zi0 : std_logic_vector (31 downto 0);
      signal zi1 : std_logic_vector (38 downto 0);
begin
zi0 <= arg0(70 downto 39);
      zi1 <= arg0(38 downto 0);
      res <= (std_logic_vector'(B"0000000000") & zi0 & zi1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_getReg\ is
port (arg0 : in std_logic_vector (38 downto 0);
      res : out std_logic_vector (70 downto 0));
end entity;

architecture rtl of \Main_getReg\ is
signal zi0 : std_logic_vector (77 downto 0);
      signal zi1 : std_logic_vector (38 downto 0);
      signal zi2 : std_logic_vector (38 downto 0);
      signal zi3 : std_logic_vector (77 downto 0);
      signal zi4 : std_logic_vector (38 downto 0);
      signal zi5 : std_logic_vector (31 downto 0);
      signal zi6 : std_logic_vector (6 downto 0);
begin
zi0 <= (arg0 & arg0);
      zi1 <= zi0(77 downto 39);
      zi2 <= zi0(38 downto 0);
      zi3 <= (zi2 & zi1);
      zi4 <= zi3(77 downto 39);
      zi5 <= zi3(38 downto 7);
      zi6 <= zi3(6 downto 0);
      res <= (zi5 & zi4);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_repl77\ is
port (arg0 : in std_logic_vector (38 downto 0);
      arg1 : in std_logic_vector (31 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \ZLL_Main_repl77\ is
component \Main_nextPC\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (38 downto 0));
      end component;
      component \ZLL_Main_repl101\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZLL_Main_repl43\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      signal zi0 : std_logic_vector (77 downto 0);
      signal zi1 : std_logic_vector (109 downto 0);
      signal zi2 : std_logic_vector (31 downto 0);
      signal zi3 : std_logic_vector (38 downto 0);
      signal zi4 : std_logic_vector (38 downto 0);
      signal zi5 : std_logic_vector (109 downto 0);
      signal zi6 : std_logic_vector (38 downto 0);
      signal zi7 : std_logic_vector (31 downto 0);
      signal zi8 : std_logic_vector (31 downto 0);
      signal zi9 : std_logic_vector (6 downto 0);
      signal conn : std_logic_vector (38 downto 0);
      signal zll_main_repl101_out : std_logic_vector (80 downto 0);
      signal zi10 : std_logic_vector (80 downto 0);
      signal zi11 : std_logic_vector (38 downto 0);
      signal main_nextpc_out : std_logic_vector (38 downto 0);
      signal \zll_main_repl101_outR1\ : std_logic_vector (80 downto 0);
      signal zll_main_repl43_out : std_logic_vector (80 downto 0);
begin
zi0 <= (arg0 & arg0);
      zi1 <= (arg1 & zi0);
      zi2 <= zi1(109 downto 78);
      zi3 <= zi1(77 downto 39);
      zi4 <= zi1(38 downto 0);
      zi5 <= (zi4 & zi2 & zi3);
      zi6 <= zi5(109 downto 71);
      zi7 <= zi5(70 downto 39);
      zi8 <= zi5(38 downto 7);
      zi9 <= zi5(6 downto 0);
      conn <= (zi7 & zi9);
      inst : \ZLL_Main_repl101\ port map (conn, zll_main_repl101_out);
      zi10 <= zll_main_repl101_out;
      zi11 <= zi10(38 downto 0);
      \instR1\ : \Main_nextPC\ port map (zi11, main_nextpc_out);
      \instR2\ : \ZLL_Main_repl101\ port map (main_nextpc_out, \zll_main_repl101_outR1\);
      \instR3\ : \ZLL_Main_repl43\ port map (\zll_main_repl101_outR1\, zll_main_repl43_out);
      res <= zll_main_repl43_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_repl43\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \ZLL_Main_repl43\ is
signal zi0 : std_logic_vector (38 downto 0);
begin
zi0 <= arg0(38 downto 0);
      res <= (std_logic_vector'(B"100000000000000000000000000000000000000010") & zi0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_nextPC\ is
port (arg0 : in std_logic_vector (38 downto 0);
      res : out std_logic_vector (38 downto 0));
end entity;

architecture rtl of \Main_nextPC\ is
signal zi0 : std_logic_vector (77 downto 0);
      signal zi1 : std_logic_vector (38 downto 0);
      signal zi2 : std_logic_vector (38 downto 0);
      signal zi3 : std_logic_vector (77 downto 0);
      signal zi4 : std_logic_vector (38 downto 0);
      signal zi5 : std_logic_vector (31 downto 0);
      signal zi6 : std_logic_vector (6 downto 0);
begin
zi0 <= (arg0 & arg0);
      zi1 <= zi0(77 downto 39);
      zi2 <= zi0(38 downto 0);
      zi3 <= (zi2 & zi1);
      zi4 <= zi3(77 downto 39);
      zi5 <= zi3(38 downto 7);
      zi6 <= zi3(6 downto 0);
      res <= (zi5 & rw_cond(rw_eq(zi6, std_logic_vector'(B"0000000")), std_logic_vector'(B"0000001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0000001")), std_logic_vector'(B"0000010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0000010")), std_logic_vector'(B"0000011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0000011")), std_logic_vector'(B"0000100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0000100")), std_logic_vector'(B"0000101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0000101")), std_logic_vector'(B"0000110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0000110")), std_logic_vector'(B"0000111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0000111")), std_logic_vector'(B"0001000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001000")), std_logic_vector'(B"0001001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001001")), std_logic_vector'(B"0001010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001010")), std_logic_vector'(B"0001011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001011")), std_logic_vector'(B"0001100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001100")), std_logic_vector'(B"0001101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001101")), std_logic_vector'(B"0001110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001110")), std_logic_vector'(B"0001111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0001111")), std_logic_vector'(B"0010000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010000")), std_logic_vector'(B"0010001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010001")), std_logic_vector'(B"0010010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010010")), std_logic_vector'(B"0010011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010011")), std_logic_vector'(B"0010100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010100")), std_logic_vector'(B"0010101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010101")), std_logic_vector'(B"0010110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010110")), std_logic_vector'(B"0010111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0010111")), std_logic_vector'(B"0011000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011000")), std_logic_vector'(B"0011001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011001")), std_logic_vector'(B"0011010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011010")), std_logic_vector'(B"0011011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011011")), std_logic_vector'(B"0011100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011100")), std_logic_vector'(B"0011101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011101")), std_logic_vector'(B"0011110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011110")), std_logic_vector'(B"0011111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0011111")), std_logic_vector'(B"0100000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100000")), std_logic_vector'(B"0100001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100001")), std_logic_vector'(B"0100010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100010")), std_logic_vector'(B"0100011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100011")), std_logic_vector'(B"0100100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100100")), std_logic_vector'(B"0100101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100101")), std_logic_vector'(B"0100110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100110")), std_logic_vector'(B"0100111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0100111")), std_logic_vector'(B"0101000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101000")), std_logic_vector'(B"0101001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101001")), std_logic_vector'(B"0101010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101010")), std_logic_vector'(B"0101011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101011")), std_logic_vector'(B"0101100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101100")), std_logic_vector'(B"0101101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101101")), std_logic_vector'(B"0101110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101110")), std_logic_vector'(B"0101111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0101111")), std_logic_vector'(B"0110000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110000")), std_logic_vector'(B"0110001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110001")), std_logic_vector'(B"0110010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110010")), std_logic_vector'(B"0110011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110011")), std_logic_vector'(B"0110100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110100")), std_logic_vector'(B"0110101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110101")), std_logic_vector'(B"0110110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110110")), std_logic_vector'(B"0110111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0110111")), std_logic_vector'(B"0111000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111000")), std_logic_vector'(B"0111001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111001")), std_logic_vector'(B"0111010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111010")), std_logic_vector'(B"0111011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111011")), std_logic_vector'(B"0111100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111100")), std_logic_vector'(B"0111101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111101")), std_logic_vector'(B"0111110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111110")), std_logic_vector'(B"0111111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"0111111")), std_logic_vector'(B"1000000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000000")), std_logic_vector'(B"1000001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000001")), std_logic_vector'(B"1000010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000010")), std_logic_vector'(B"1000011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000011")), std_logic_vector'(B"1000100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000100")), std_logic_vector'(B"1000101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000101")), std_logic_vector'(B"1000110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000110")), std_logic_vector'(B"1000111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1000111")), std_logic_vector'(B"1001000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001000")), std_logic_vector'(B"1001001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001001")), std_logic_vector'(B"1001010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001010")), std_logic_vector'(B"1001011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001011")), std_logic_vector'(B"1001100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001100")), std_logic_vector'(B"1001101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001101")), std_logic_vector'(B"1001110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001110")), std_logic_vector'(B"1001111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1001111")), std_logic_vector'(B"1010000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010000")), std_logic_vector'(B"1010001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010001")), std_logic_vector'(B"1010010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010010")), std_logic_vector'(B"1010011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010011")), std_logic_vector'(B"1010100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010100")), std_logic_vector'(B"1010101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010101")), std_logic_vector'(B"1010110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010110")), std_logic_vector'(B"1010111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1010111")), std_logic_vector'(B"1011000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011000")), std_logic_vector'(B"1011001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011001")), std_logic_vector'(B"1011010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011010")), std_logic_vector'(B"1011011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011011")), std_logic_vector'(B"1011100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011100")), std_logic_vector'(B"1011101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011101")), std_logic_vector'(B"1011110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011110")), std_logic_vector'(B"1011111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1011111")), std_logic_vector'(B"1100000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100000")), std_logic_vector'(B"1100001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100001")), std_logic_vector'(B"1100010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100010")), std_logic_vector'(B"1100011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100011")), std_logic_vector'(B"1100100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100100")), std_logic_vector'(B"1100101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100101")), std_logic_vector'(B"1100110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100110")), std_logic_vector'(B"1100111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1100111")), std_logic_vector'(B"1101000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101000")), std_logic_vector'(B"1101001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101001")), std_logic_vector'(B"1101010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101010")), std_logic_vector'(B"1101011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101011")), std_logic_vector'(B"1101100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101100")), std_logic_vector'(B"1101101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101101")), std_logic_vector'(B"1101110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101110")), std_logic_vector'(B"1101111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1101111")), std_logic_vector'(B"1110000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110000")), std_logic_vector'(B"1110001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110001")), std_logic_vector'(B"1110010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110010")), std_logic_vector'(B"1110011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110011")), std_logic_vector'(B"1110100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110100")), std_logic_vector'(B"1110101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110101")), std_logic_vector'(B"1110110"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110110")), std_logic_vector'(B"1110111"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1110111")), std_logic_vector'(B"1111000"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1111000")), std_logic_vector'(B"1111001"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1111001")), std_logic_vector'(B"1111010"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1111010")), std_logic_vector'(B"1111011"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1111011")), std_logic_vector'(B"1111100"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1111100")), std_logic_vector'(B"1111101"), rw_cond(rw_eq(zi6, std_logic_vector'(B"1111101")), std_logic_vector'(B"1111110"), std_logic_vector'(B"0000000"))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
end architecture;