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
      \__in0\ : in std_logic_vector (99 downto 0);
      \__out0\ : out std_logic_vector (99 downto 0));
end entity;

architecture rtl of top_level is
component \Main_x2\ is
      port (arg0 : in std_logic_vector (99 downto 0);
            res : out std_logic_vector (99 downto 0));
      end component;
      component \ZLL_Main_ss$3\ is
      port (arg0 : in std_logic_vector (899 downto 0);
            arg1 : in std_logic_vector (99 downto 0);
            res : out std_logic_vector (999 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (999 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__resumption_tag_next\ : std_logic_vector (999 downto 0);
      signal \__st0\ : std_logic_vector (999 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (999 downto 0);
      signal zin : std_logic_vector (2099 downto 0);
      signal zi0 : std_logic_vector (1999 downto 0);
      signal zi1 : std_logic_vector (99 downto 0);
      signal zi2 : std_logic_vector (2099 downto 0);
      signal zi3 : std_logic_vector (99 downto 0);
      signal zi4 : std_logic_vector (999 downto 0);
      signal zi5 : std_logic_vector (999 downto 0);
      signal zi6 : std_logic_vector (1099 downto 0);
      signal zi7 : std_logic_vector (999 downto 0);
      signal zi8 : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_out\ : std_logic_vector (999 downto 0);
      signal zi9 : std_logic_vector (999 downto 0);
      signal main_x2_out : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR1\ : std_logic_vector (999 downto 0);
      signal zi10 : std_logic_vector (999 downto 0);
      signal \main_x2_outR1\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR2\ : std_logic_vector (999 downto 0);
      signal zi11 : std_logic_vector (999 downto 0);
      signal \main_x2_outR2\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR3\ : std_logic_vector (999 downto 0);
      signal zi12 : std_logic_vector (999 downto 0);
      signal \main_x2_outR3\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR4\ : std_logic_vector (999 downto 0);
      signal zi13 : std_logic_vector (999 downto 0);
      signal \main_x2_outR4\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR5\ : std_logic_vector (999 downto 0);
      signal zi14 : std_logic_vector (999 downto 0);
      signal \main_x2_outR5\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR6\ : std_logic_vector (999 downto 0);
      signal zi15 : std_logic_vector (999 downto 0);
      signal \main_x2_outR6\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR7\ : std_logic_vector (999 downto 0);
      signal zi16 : std_logic_vector (999 downto 0);
      signal \main_x2_outR7\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR8\ : std_logic_vector (999 downto 0);
      signal zi17 : std_logic_vector (999 downto 0);
      signal \main_x2_outR8\ : std_logic_vector (99 downto 0);
      signal \zll_main_ss$3_outR9\ : std_logic_vector (999 downto 0);
      signal zi18 : std_logic_vector (999 downto 0);
      signal \main_x2_outR9\ : std_logic_vector (99 downto 0);
      signal zi19 : std_logic_vector (999 downto 0);
      signal zi20 : std_logic_vector (2100 downto 0);
      signal zi21 : std_logic_vector (999 downto 0);
      signal zi22 : std_logic_vector (1999 downto 0);
      signal zi23 : std_logic_vector (999 downto 0);
      signal zi24 : std_logic_vector (999 downto 0);
      signal zi25 : std_logic_vector (2100 downto 0);
      signal zi26 : std_logic_vector (999 downto 0);
      signal zi27 : std_logic_vector (999 downto 0);
      signal zres : std_logic_vector (2100 downto 0);
begin
zin <= (\__resumption_tag\ & \__st0\ & \__in0\);
      zi0 <= zin(2099 downto 100);
      zi1 <= zin(99 downto 0);
      zi2 <= (zi1 & zi0);
      zi3 <= zi2(2099 downto 2000);
      zi4 <= zi2(1999 downto 1000);
      zi5 <= zi2(999 downto 0);
      zi6 <= (zi4 & zi3);
      zi7 <= zi6(1099 downto 100);
      zi8 <= zi6(99 downto 0);
      inst : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_out\);
      zi9 <= \zll_main_ss$3_out\;
      \instR1\ : \Main_x2\ port map (zi9(999 downto 900), main_x2_out);
      \instR2\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR1\);
      zi10 <= \zll_main_ss$3_outR1\;
      \instR3\ : \Main_x2\ port map (zi10(899 downto 800), \main_x2_outR1\);
      \instR4\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR2\);
      zi11 <= \zll_main_ss$3_outR2\;
      \instR5\ : \Main_x2\ port map (zi11(799 downto 700), \main_x2_outR2\);
      \instR6\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR3\);
      zi12 <= \zll_main_ss$3_outR3\;
      \instR7\ : \Main_x2\ port map (zi12(699 downto 600), \main_x2_outR3\);
      \instR8\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR4\);
      zi13 <= \zll_main_ss$3_outR4\;
      \instR9\ : \Main_x2\ port map (zi13(599 downto 500), \main_x2_outR4\);
      \instR10\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR5\);
      zi14 <= \zll_main_ss$3_outR5\;
      \instR11\ : \Main_x2\ port map (zi14(499 downto 400), \main_x2_outR5\);
      \instR12\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR6\);
      zi15 <= \zll_main_ss$3_outR6\;
      \instR13\ : \Main_x2\ port map (zi15(399 downto 300), \main_x2_outR6\);
      \instR14\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR7\);
      zi16 <= \zll_main_ss$3_outR7\;
      \instR15\ : \Main_x2\ port map (zi16(299 downto 200), \main_x2_outR7\);
      \instR16\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR8\);
      zi17 <= \zll_main_ss$3_outR8\;
      \instR17\ : \Main_x2\ port map (zi17(199 downto 100), \main_x2_outR8\);
      \instR18\ : \ZLL_Main_ss$3\ port map (zi7(899 downto 0), zi8, \zll_main_ss$3_outR9\);
      zi18 <= \zll_main_ss$3_outR9\;
      \instR19\ : \Main_x2\ port map (zi18(99 downto 0), \main_x2_outR9\);
      zi19 <= (main_x2_out & \main_x2_outR1\ & \main_x2_outR2\ & \main_x2_outR3\ & \main_x2_outR4\ & \main_x2_outR5\ & \main_x2_outR6\ & \main_x2_outR7\ & \main_x2_outR8\ & \main_x2_outR9\);
      zi20 <= ((std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001") & rw_repl(1000, std_logic_vector'(B"0"))) & zi19);
      zi21 <= zi20(999 downto 0);
      zi22 <= (zi21 & zi21);
      zi23 <= zi22(1999 downto 1000);
      zi24 <= zi22(999 downto 0);
      zi25 <= (rw_repl(101, std_logic_vector'(B"0")) & zi23 & zi24);
      zi26 <= zi25(1999 downto 1000);
      zi27 <= zi25(999 downto 0);
      zres <= (std_logic_vector'(B"1") & zi26(999 downto 900) & zi26 & zi27);
      \__resumption_tag_next\ <= zres(1999 downto 1000);
      \__st0_next\ <= zres(999 downto 0);
      \__out0\ <= zres(2099 downto 2000);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
                  \__st0\ <= std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__resumption_tag\ <= \__resumption_tag_next\;
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_ss$3\ is
port (arg0 : in std_logic_vector (899 downto 0);
      arg1 : in std_logic_vector (99 downto 0);
      res : out std_logic_vector (999 downto 0));
end entity;

architecture rtl of \ZLL_Main_ss$3\ is
signal zi0 : std_logic_vector (999 downto 0);
      signal zi1 : std_logic_vector (899 downto 0);
      signal zi2 : std_logic_vector (99 downto 0);
begin
zi0 <= (arg0 & arg1);
      zi1 <= zi0(999 downto 100);
      zi2 <= zi0(99 downto 0);
      res <= (zi1 & zi2);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_x2\ is
port (arg0 : in std_logic_vector (99 downto 0);
      res : out std_logic_vector (99 downto 0));
end entity;

architecture rtl of \Main_x2\ is

begin
res <= rw_mul(arg0, std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010"));
end architecture;