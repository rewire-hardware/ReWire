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
      \__in0\ : in std_logic_vector (16 downto 0);
      \__out0\ : out std_logic_vector (14 downto 0));
end entity;

architecture rtl of top_level is
component \Main_inputs\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (16 downto 0));
      end component;
      component \Main_outputs\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (14 downto 0));
      end component;
      component \Main_pc\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (5 downto 0));
      end component;
      component \ZLL_L_o2321\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (14 downto 0));
      end component;
      component \ZLL_L_s89\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZL_Main_getIns196\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \ZL_Main_getPC34\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \ZL_Main_getPC40\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"100");
      signal \__resumption_tag_next\ : std_logic_vector (2 downto 0);
      signal \__st0\ : std_logic_vector (69 downto 0) := std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (5 downto 0);
      signal zi7 : std_logic_vector (14 downto 0);
      signal conn : std_logic_vector (69 downto 0);
      signal zll_l_s89_out : std_logic_vector (69 downto 0);
      signal zl_main_getins196_out : std_logic_vector (87 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (5 downto 0);
      signal zi14 : std_logic_vector (14 downto 0);
      signal \connR1\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR1\ : std_logic_vector (69 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal main_inputs_out : std_logic_vector (16 downto 0);
      signal zi16 : std_logic_vector (16 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (5 downto 0);
      signal zi23 : std_logic_vector (16 downto 0);
      signal zi24 : std_logic_vector (14 downto 0);
      signal \connR2\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR2\ : std_logic_vector (69 downto 0);
      signal \zl_main_getins196_outR1\ : std_logic_vector (87 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (7 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal zi29 : std_logic_vector (7 downto 0);
      signal zi30 : std_logic_vector (5 downto 0);
      signal zi31 : std_logic_vector (14 downto 0);
      signal \connR3\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR3\ : std_logic_vector (69 downto 0);
      signal zi32 : std_logic_vector (69 downto 0);
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal zi33 : std_logic_vector (5 downto 0);
      signal zi34 : std_logic_vector (5 downto 0);
      signal zi35 : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (7 downto 0);
      signal zi37 : std_logic_vector (7 downto 0);
      signal zi38 : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (16 downto 0);
      signal zi40 : std_logic_vector (14 downto 0);
      signal \connR4\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR4\ : std_logic_vector (69 downto 0);
      signal zi41 : std_logic_vector (69 downto 0);
      signal \main_pc_outR1\ : std_logic_vector (5 downto 0);
      signal zi42 : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal zi43 : std_logic_vector (14 downto 0);
      signal zi44 : std_logic_vector (0 downto 0);
      signal zi45 : std_logic_vector (7 downto 0);
      signal \connR5\ : std_logic_vector (14 downto 0);
      signal zll_l_o2321_out : std_logic_vector (14 downto 0);
      signal zi46 : std_logic_vector (14 downto 0);
      signal zi47 : std_logic_vector (7 downto 0);
      signal zi48 : std_logic_vector (7 downto 0);
      signal zi49 : std_logic_vector (7 downto 0);
      signal zi50 : std_logic_vector (7 downto 0);
      signal zi51 : std_logic_vector (5 downto 0);
      signal zi52 : std_logic_vector (16 downto 0);
      signal \connR6\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR5\ : std_logic_vector (69 downto 0);
      signal zi53 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (14 downto 0);
      signal zi54 : std_logic_vector (14 downto 0);
      signal zi55 : std_logic_vector (5 downto 0);
      signal zi56 : std_logic_vector (7 downto 0);
      signal \connR7\ : std_logic_vector (14 downto 0);
      signal \zll_l_o2321_outR1\ : std_logic_vector (14 downto 0);
      signal zi57 : std_logic_vector (14 downto 0);
      signal zi58 : std_logic_vector (7 downto 0);
      signal zi59 : std_logic_vector (7 downto 0);
      signal zi60 : std_logic_vector (7 downto 0);
      signal zi61 : std_logic_vector (7 downto 0);
      signal zi62 : std_logic_vector (5 downto 0);
      signal zi63 : std_logic_vector (16 downto 0);
      signal \connR8\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR6\ : std_logic_vector (69 downto 0);
      signal zi64 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (14 downto 0);
      signal zi65 : std_logic_vector (14 downto 0);
      signal zi67 : std_logic_vector (7 downto 0);
      signal zi68 : std_logic_vector (7 downto 0);
      signal zi69 : std_logic_vector (7 downto 0);
      signal zi70 : std_logic_vector (7 downto 0);
      signal zi71 : std_logic_vector (5 downto 0);
      signal zi72 : std_logic_vector (14 downto 0);
      signal \connR9\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR7\ : std_logic_vector (69 downto 0);
      signal zl_main_getpc40_out : std_logic_vector (87 downto 0);
      signal zi74 : std_logic_vector (7 downto 0);
      signal zi75 : std_logic_vector (7 downto 0);
      signal zi76 : std_logic_vector (7 downto 0);
      signal zi77 : std_logic_vector (7 downto 0);
      signal zi78 : std_logic_vector (5 downto 0);
      signal zi79 : std_logic_vector (14 downto 0);
      signal \connR10\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR8\ : std_logic_vector (69 downto 0);
      signal zl_main_getpc34_out : std_logic_vector (87 downto 0);
      signal zres : std_logic_vector (87 downto 0);
begin
zi2 <= \__st0\(69 downto 62);
      zi3 <= \__st0\(61 downto 54);
      zi4 <= \__st0\(53 downto 46);
      zi5 <= \__st0\(45 downto 38);
      zi6 <= \__st0\(37 downto 32);
      zi7 <= \__st0\(14 downto 0);
      conn <= (zi2 & zi3 & zi4 & zi5 & zi6 & \__in0\ & zi7);
      inst : \ZLL_L_s89\ port map (conn, zll_l_s89_out);
      \instR1\ : \ZL_Main_getIns196\ port map (zll_l_s89_out, zl_main_getins196_out);
      zi9 <= \__st0\(69 downto 62);
      zi10 <= \__st0\(61 downto 54);
      zi11 <= \__st0\(53 downto 46);
      zi12 <= \__st0\(45 downto 38);
      zi13 <= \__st0\(37 downto 32);
      zi14 <= \__st0\(14 downto 0);
      \connR1\ <= (zi9 & zi10 & zi11 & zi12 & zi13 & \__in0\ & zi14);
      \instR2\ : \ZLL_L_s89\ port map (\connR1\, \zll_l_s89_outR1\);
      zi15 <= \zll_l_s89_outR1\;
      \instR3\ : \Main_inputs\ port map (zi15, main_inputs_out);
      zi16 <= main_inputs_out;
      zi17 <= zi16(7 downto 0);
      zi18 <= zi17;
      zi19 <= zi15(61 downto 54);
      zi20 <= zi15(53 downto 46);
      zi21 <= zi15(45 downto 38);
      zi22 <= zi15(37 downto 32);
      zi23 <= zi15(31 downto 15);
      zi24 <= zi15(14 downto 0);
      \connR2\ <= (zi18 & zi19 & zi20 & zi21 & zi22 & zi23 & zi24);
      \instR4\ : \ZLL_L_s89\ port map (\connR2\, \zll_l_s89_outR2\);
      \instR5\ : \ZL_Main_getIns196\ port map (\zll_l_s89_outR2\, \zl_main_getins196_outR1\);
      zi26 <= \__st0\(69 downto 62);
      zi27 <= \__st0\(61 downto 54);
      zi28 <= \__st0\(53 downto 46);
      zi29 <= \__st0\(45 downto 38);
      zi30 <= \__st0\(37 downto 32);
      zi31 <= \__st0\(14 downto 0);
      \connR3\ <= (zi26 & zi27 & zi28 & zi29 & zi30 & \__in0\ & zi31);
      \instR6\ : \ZLL_L_s89\ port map (\connR3\, \zll_l_s89_outR3\);
      zi32 <= \zll_l_s89_outR3\;
      \instR7\ : \Main_pc\ port map (zi32, main_pc_out);
      zi33 <= main_pc_out;
      zi34 <= rw_add(zi33, std_logic_vector'(B"000001"));
      zi35 <= zi32(69 downto 62);
      zi36 <= zi32(61 downto 54);
      zi37 <= zi32(53 downto 46);
      zi38 <= zi32(45 downto 38);
      zi39 <= zi32(31 downto 15);
      zi40 <= zi32(14 downto 0);
      \connR4\ <= (zi35 & zi36 & zi37 & zi38 & zi34 & zi39 & zi40);
      \instR8\ : \ZLL_L_s89\ port map (\connR4\, \zll_l_s89_outR4\);
      zi41 <= \zll_l_s89_outR4\;
      \instR9\ : \Main_pc\ port map (zi41, \main_pc_outR1\);
      zi42 <= \main_pc_outR1\;
      \instR10\ : \Main_outputs\ port map (zi41, main_outputs_out);
      zi43 <= main_outputs_out;
      zi44 <= zi43(14 downto 14);
      zi45 <= zi43(7 downto 0);
      \connR5\ <= (zi44 & zi42 & zi45);
      \instR11\ : \ZLL_L_o2321\ port map (\connR5\, zll_l_o2321_out);
      zi46 <= zll_l_o2321_out;
      zi47 <= zi41(69 downto 62);
      zi48 <= zi41(61 downto 54);
      zi49 <= zi41(53 downto 46);
      zi50 <= zi41(45 downto 38);
      zi51 <= zi41(37 downto 32);
      zi52 <= zi41(31 downto 15);
      \connR6\ <= (zi47 & zi48 & zi49 & zi50 & zi51 & zi52 & zi46);
      \instR12\ : \ZLL_L_s89\ port map (\connR6\, \zll_l_s89_outR5\);
      zi53 <= \zll_l_s89_outR5\;
      \instR13\ : \Main_outputs\ port map (zi53, \main_outputs_outR1\);
      zi54 <= \main_outputs_outR1\;
      zi55 <= zi54(13 downto 8);
      zi56 <= zi54(7 downto 0);
      \connR7\ <= (std_logic_vector'(B"0") & zi55 & zi56);
      \instR14\ : \ZLL_L_o2321\ port map (\connR7\, \zll_l_o2321_outR1\);
      zi57 <= \zll_l_o2321_outR1\;
      zi58 <= zi53(69 downto 62);
      zi59 <= zi53(61 downto 54);
      zi60 <= zi53(53 downto 46);
      zi61 <= zi53(45 downto 38);
      zi62 <= zi53(37 downto 32);
      zi63 <= zi53(31 downto 15);
      \connR8\ <= (zi58 & zi59 & zi60 & zi61 & zi62 & zi63 & zi57);
      \instR15\ : \ZLL_L_s89\ port map (\connR8\, \zll_l_s89_outR6\);
      zi64 <= \zll_l_s89_outR6\;
      \instR16\ : \Main_outputs\ port map (zi64, \main_outputs_outR2\);
      zi65 <= \main_outputs_outR2\;
      zi67 <= \__st0\(69 downto 62);
      zi68 <= \__st0\(61 downto 54);
      zi69 <= \__st0\(53 downto 46);
      zi70 <= \__st0\(45 downto 38);
      zi71 <= \__st0\(37 downto 32);
      zi72 <= \__st0\(14 downto 0);
      \connR9\ <= (zi67 & zi68 & zi69 & zi70 & zi71 & \__in0\ & zi72);
      \instR17\ : \ZLL_L_s89\ port map (\connR9\, \zll_l_s89_outR7\);
      \instR18\ : \ZL_Main_getPC40\ port map (\zll_l_s89_outR7\, zl_main_getpc40_out);
      zi74 <= \__st0\(69 downto 62);
      zi75 <= \__st0\(61 downto 54);
      zi76 <= \__st0\(53 downto 46);
      zi77 <= \__st0\(45 downto 38);
      zi78 <= \__st0\(37 downto 32);
      zi79 <= \__st0\(14 downto 0);
      \connR10\ <= (zi74 & zi75 & zi76 & zi77 & zi78 & \__in0\ & zi79);
      \instR19\ : \ZLL_L_s89\ port map (\connR10\, \zll_l_s89_outR8\);
      \instR20\ : \ZL_Main_getPC34\ port map (\zll_l_s89_outR8\, zl_main_getpc34_out);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"000")), zl_main_getins196_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"001")), \zl_main_getins196_outR1\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"010")), (zi65 & std_logic_vector'(B"001") & zi64), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"011")), zl_main_getpc40_out, zl_main_getpc34_out))));
      \__resumption_tag_next\ <= zres(72 downto 70);
      \__st0_next\ <= zres(69 downto 0);
      \__out0\ <= zres(87 downto 73);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"100");
                  \__st0\ <= std_logic_vector'(B"0000000000000000000000000000000000000000000000000000000000000000000000");
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
entity \ZLL_L_o2321\ is
port (arg0 : in std_logic_vector (14 downto 0);
      res : out std_logic_vector (14 downto 0));
end entity;

architecture rtl of \ZLL_L_o2321\ is

begin
res <= arg0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s89\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (69 downto 0));
end entity;

architecture rtl of \ZLL_L_s89\ is

begin
res <= arg0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_x177\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZLL_L_x177\ is
component \ZL_Main_getReg165\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (1 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal zl_main_getreg165_out : std_logic_vector (87 downto 0);
begin
inst : \ZL_Main_getReg165\ port map (arg1, arg3, arg0, arg2, zl_main_getreg165_out);
      res <= zl_main_getreg165_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_Main_putPC38\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZL_Main_putPC38\ is
component \ZLL_L_s89\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZL_Main_getPC34\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
      signal conn : std_logic_vector (69 downto 0);
      signal zll_l_s89_out : std_logic_vector (69 downto 0);
      signal zl_main_getpc34_out : std_logic_vector (87 downto 0);
begin
zi0 <= arg1(69 downto 62);
      zi1 <= arg1(61 downto 54);
      zi2 <= arg1(53 downto 46);
      zi3 <= arg1(45 downto 38);
      zi4 <= arg1(31 downto 15);
      zi5 <= arg1(14 downto 0);
      conn <= (zi0 & zi1 & zi2 & zi3 & arg0 & zi4 & zi5);
      inst : \ZLL_L_s89\ port map (conn, zll_l_s89_out);
      \instR1\ : \ZL_Main_getPC34\ port map (zll_l_s89_out, zl_main_getpc34_out);
      res <= zl_main_getpc34_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_pc\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (5 downto 0));
end entity;

architecture rtl of \Main_pc\ is
signal ds5 : std_logic_vector (5 downto 0);
begin
ds5 <= arg0(37 downto 32);
      res <= ds5;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_x172\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZLL_L_x172\ is
component \ZL_Main_putReg156\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal conn : std_logic_vector (7 downto 0);
      signal zl_main_putreg156_out : std_logic_vector (87 downto 0);
begin
conn <= rw_not(rw_and(arg1, arg3));
      inst : \ZL_Main_putReg156\ port map (arg0, conn, arg2, zl_main_putreg156_out);
      res <= zl_main_putreg156_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_inputs\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (16 downto 0));
end entity;

architecture rtl of \Main_inputs\ is
signal ds6 : std_logic_vector (16 downto 0);
begin
ds6 <= arg0(31 downto 15);
      res <= ds6;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r0\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r0\ is
signal ds1 : std_logic_vector (7 downto 0);
begin
ds1 <= arg0(69 downto 62);
      res <= ds1;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r1\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r1\ is
signal ds2 : std_logic_vector (7 downto 0);
begin
ds2 <= arg0(61 downto 54);
      res <= ds2;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_Main_getPC34\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZL_Main_getPC34\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (14 downto 0));
      end component;
      component \Main_pc\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (5 downto 0));
      end component;
      component \ZLL_L_o2321\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (14 downto 0));
      end component;
      component \ZLL_L_s89\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal zi0 : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal zi1 : std_logic_vector (14 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal zll_l_o2321_out : std_logic_vector (14 downto 0);
      signal zi4 : std_logic_vector (14 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (5 downto 0);
      signal zi10 : std_logic_vector (16 downto 0);
      signal \connR1\ : std_logic_vector (69 downto 0);
      signal zll_l_s89_out : std_logic_vector (69 downto 0);
      signal zi11 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (14 downto 0);
      signal zi12 : std_logic_vector (14 downto 0);
      signal zi13 : std_logic_vector (5 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal \connR2\ : std_logic_vector (14 downto 0);
      signal \zll_l_o2321_outR1\ : std_logic_vector (14 downto 0);
      signal zi15 : std_logic_vector (14 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (5 downto 0);
      signal zi21 : std_logic_vector (16 downto 0);
      signal \connR3\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR1\ : std_logic_vector (69 downto 0);
      signal zi22 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (14 downto 0);
      signal zi23 : std_logic_vector (14 downto 0);
begin
inst : \Main_pc\ port map (arg0, main_pc_out);
      zi0 <= main_pc_out;
      \instR1\ : \Main_outputs\ port map (arg0, main_outputs_out);
      zi1 <= main_outputs_out;
      zi2 <= zi1(14 downto 14);
      zi3 <= zi1(7 downto 0);
      conn <= (zi2 & zi0 & zi3);
      \instR2\ : \ZLL_L_o2321\ port map (conn, zll_l_o2321_out);
      zi4 <= zll_l_o2321_out;
      zi5 <= arg0(69 downto 62);
      zi6 <= arg0(61 downto 54);
      zi7 <= arg0(53 downto 46);
      zi8 <= arg0(45 downto 38);
      zi9 <= arg0(37 downto 32);
      zi10 <= arg0(31 downto 15);
      \connR1\ <= (zi5 & zi6 & zi7 & zi8 & zi9 & zi10 & zi4);
      \instR3\ : \ZLL_L_s89\ port map (\connR1\, zll_l_s89_out);
      zi11 <= zll_l_s89_out;
      \instR4\ : \Main_outputs\ port map (zi11, \main_outputs_outR1\);
      zi12 <= \main_outputs_outR1\;
      zi13 <= zi12(13 downto 8);
      zi14 <= zi12(7 downto 0);
      \connR2\ <= (std_logic_vector'(B"0") & zi13 & zi14);
      \instR5\ : \ZLL_L_o2321\ port map (\connR2\, \zll_l_o2321_outR1\);
      zi15 <= \zll_l_o2321_outR1\;
      zi16 <= zi11(69 downto 62);
      zi17 <= zi11(61 downto 54);
      zi18 <= zi11(53 downto 46);
      zi19 <= zi11(45 downto 38);
      zi20 <= zi11(37 downto 32);
      zi21 <= zi11(31 downto 15);
      \connR3\ <= (zi16 & zi17 & zi18 & zi19 & zi20 & zi21 & zi15);
      \instR6\ : \ZLL_L_s89\ port map (\connR3\, \zll_l_s89_outR1\);
      zi22 <= \zll_l_s89_outR1\;
      \instR7\ : \Main_outputs\ port map (zi22, \main_outputs_outR2\);
      zi23 <= \main_outputs_outR2\;
      res <= (zi23 & std_logic_vector'(B"000") & zi22);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r3\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r3\ is
signal ds4 : std_logic_vector (7 downto 0);
begin
ds4 <= arg0(45 downto 38);
      res <= ds4;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r2\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r2\ is
signal ds3 : std_logic_vector (7 downto 0);
begin
ds3 <= arg0(53 downto 46);
      res <= ds3;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_Main_putReg156\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZL_Main_putReg156\ is
component \ZLL_L_s89\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZL_Main_getPC40\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
      signal conn : std_logic_vector (69 downto 0);
      signal zll_l_s89_out : std_logic_vector (69 downto 0);
      signal zl_main_getpc40_out : std_logic_vector (87 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (5 downto 0);
      signal zi10 : std_logic_vector (16 downto 0);
      signal zi11 : std_logic_vector (14 downto 0);
      signal \connR1\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR1\ : std_logic_vector (69 downto 0);
      signal \zl_main_getpc40_outR1\ : std_logic_vector (87 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal zi15 : std_logic_vector (5 downto 0);
      signal zi16 : std_logic_vector (16 downto 0);
      signal zi17 : std_logic_vector (14 downto 0);
      signal \connR2\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR2\ : std_logic_vector (69 downto 0);
      signal \zl_main_getpc40_outR2\ : std_logic_vector (87 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (5 downto 0);
      signal zi22 : std_logic_vector (16 downto 0);
      signal zi23 : std_logic_vector (14 downto 0);
      signal \connR3\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR3\ : std_logic_vector (69 downto 0);
      signal \zl_main_getpc40_outR3\ : std_logic_vector (87 downto 0);
begin
zi0 <= arg2(61 downto 54);
      zi1 <= arg2(53 downto 46);
      zi2 <= arg2(45 downto 38);
      zi3 <= arg2(37 downto 32);
      zi4 <= arg2(31 downto 15);
      zi5 <= arg2(14 downto 0);
      conn <= (arg1 & zi0 & zi1 & zi2 & zi3 & zi4 & zi5);
      inst : \ZLL_L_s89\ port map (conn, zll_l_s89_out);
      \instR1\ : \ZL_Main_getPC40\ port map (zll_l_s89_out, zl_main_getpc40_out);
      zi6 <= arg2(69 downto 62);
      zi7 <= arg2(53 downto 46);
      zi8 <= arg2(45 downto 38);
      zi9 <= arg2(37 downto 32);
      zi10 <= arg2(31 downto 15);
      zi11 <= arg2(14 downto 0);
      \connR1\ <= (zi6 & arg1 & zi7 & zi8 & zi9 & zi10 & zi11);
      \instR2\ : \ZLL_L_s89\ port map (\connR1\, \zll_l_s89_outR1\);
      \instR3\ : \ZL_Main_getPC40\ port map (\zll_l_s89_outR1\, \zl_main_getpc40_outR1\);
      zi12 <= arg2(69 downto 62);
      zi13 <= arg2(61 downto 54);
      zi14 <= arg2(45 downto 38);
      zi15 <= arg2(37 downto 32);
      zi16 <= arg2(31 downto 15);
      zi17 <= arg2(14 downto 0);
      \connR2\ <= (zi12 & zi13 & arg1 & zi14 & zi15 & zi16 & zi17);
      \instR4\ : \ZLL_L_s89\ port map (\connR2\, \zll_l_s89_outR2\);
      \instR5\ : \ZL_Main_getPC40\ port map (\zll_l_s89_outR2\, \zl_main_getpc40_outR2\);
      zi18 <= arg2(69 downto 62);
      zi19 <= arg2(61 downto 54);
      zi20 <= arg2(53 downto 46);
      zi21 <= arg2(37 downto 32);
      zi22 <= arg2(31 downto 15);
      zi23 <= arg2(14 downto 0);
      \connR3\ <= (zi18 & zi19 & zi20 & arg1 & zi21 & zi22 & zi23);
      \instR6\ : \ZLL_L_s89\ port map (\connR3\, \zll_l_s89_outR3\);
      \instR7\ : \ZL_Main_getPC40\ port map (\zll_l_s89_outR3\, \zl_main_getpc40_outR3\);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), zl_main_getpc40_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), \zl_main_getpc40_outR1\, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), \zl_main_getpc40_outR2\, \zl_main_getpc40_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_Main_getPC40\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZL_Main_getPC40\ is
component \Main_pc\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (5 downto 0));
      end component;
      component \ZL_Main_putPC38\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal zi0 : std_logic_vector (5 downto 0);
      signal conn : std_logic_vector (5 downto 0);
      signal zl_main_putpc38_out : std_logic_vector (87 downto 0);
begin
inst : \Main_pc\ port map (arg0, main_pc_out);
      zi0 <= main_pc_out;
      conn <= rw_add(zi0, std_logic_vector'(B"000001"));
      \instR1\ : \ZL_Main_putPC38\ port map (conn, arg0, zl_main_putpc38_out);
      res <= zl_main_putpc38_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_outputs\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (14 downto 0));
end entity;

architecture rtl of \Main_outputs\ is
signal ds7 : std_logic_vector (14 downto 0);
begin
ds7 <= arg0(14 downto 0);
      res <= ds7;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_Main_getIns196\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZL_Main_getIns196\ is
component \Main_inputs\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (16 downto 0));
      end component;
      component \Main_outputs\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (14 downto 0));
      end component;
      component \Main_r0\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_L_o2321\ is
      port (arg0 : in std_logic_vector (14 downto 0);
            res : out std_logic_vector (14 downto 0));
      end component;
      component \ZLL_L_s89\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (69 downto 0));
      end component;
      component \ZLL_L_x177\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \ZL_Main_getPC40\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \ZL_Main_getReg165\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (1 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \ZL_Main_putPC38\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal main_inputs_out : std_logic_vector (16 downto 0);
      signal zi0 : std_logic_vector (16 downto 0);
      signal zi1 : std_logic_vector (8 downto 0);
      signal zi2 : std_logic_vector (8 downto 0);
      signal zl_main_getpc40_out : std_logic_vector (87 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal zi4 : std_logic_vector (14 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (14 downto 0);
      signal zll_l_o2321_out : std_logic_vector (14 downto 0);
      signal zi7 : std_logic_vector (14 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (5 downto 0);
      signal zi13 : std_logic_vector (16 downto 0);
      signal \connR1\ : std_logic_vector (69 downto 0);
      signal zll_l_s89_out : std_logic_vector (69 downto 0);
      signal zi14 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (14 downto 0);
      signal zi15 : std_logic_vector (14 downto 0);
      signal zi16 : std_logic_vector (5 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal \connR2\ : std_logic_vector (14 downto 0);
      signal \zll_l_o2321_outR1\ : std_logic_vector (14 downto 0);
      signal zi18 : std_logic_vector (14 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (5 downto 0);
      signal zi24 : std_logic_vector (16 downto 0);
      signal \connR3\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR1\ : std_logic_vector (69 downto 0);
      signal zi25 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (14 downto 0);
      signal zi26 : std_logic_vector (14 downto 0);
      signal zi27 : std_logic_vector (5 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal \main_outputs_outR3\ : std_logic_vector (14 downto 0);
      signal zi29 : std_logic_vector (14 downto 0);
      signal zi30 : std_logic_vector (0 downto 0);
      signal zi31 : std_logic_vector (7 downto 0);
      signal \connR4\ : std_logic_vector (14 downto 0);
      signal \zll_l_o2321_outR2\ : std_logic_vector (14 downto 0);
      signal zi32 : std_logic_vector (14 downto 0);
      signal zi33 : std_logic_vector (7 downto 0);
      signal zi34 : std_logic_vector (7 downto 0);
      signal zi35 : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (7 downto 0);
      signal zi37 : std_logic_vector (5 downto 0);
      signal zi38 : std_logic_vector (16 downto 0);
      signal \connR5\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR2\ : std_logic_vector (69 downto 0);
      signal zi39 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR4\ : std_logic_vector (14 downto 0);
      signal zi40 : std_logic_vector (14 downto 0);
      signal zi41 : std_logic_vector (0 downto 0);
      signal zi42 : std_logic_vector (5 downto 0);
      signal \connR6\ : std_logic_vector (14 downto 0);
      signal \zll_l_o2321_outR3\ : std_logic_vector (14 downto 0);
      signal zi43 : std_logic_vector (14 downto 0);
      signal zi44 : std_logic_vector (7 downto 0);
      signal zi45 : std_logic_vector (7 downto 0);
      signal zi46 : std_logic_vector (7 downto 0);
      signal zi47 : std_logic_vector (7 downto 0);
      signal zi48 : std_logic_vector (5 downto 0);
      signal zi49 : std_logic_vector (16 downto 0);
      signal \connR7\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR3\ : std_logic_vector (69 downto 0);
      signal zi50 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR5\ : std_logic_vector (14 downto 0);
      signal zi51 : std_logic_vector (14 downto 0);
      signal zi52 : std_logic_vector (5 downto 0);
      signal zi53 : std_logic_vector (7 downto 0);
      signal \connR8\ : std_logic_vector (14 downto 0);
      signal \zll_l_o2321_outR4\ : std_logic_vector (14 downto 0);
      signal zi54 : std_logic_vector (14 downto 0);
      signal zi55 : std_logic_vector (7 downto 0);
      signal zi56 : std_logic_vector (7 downto 0);
      signal zi57 : std_logic_vector (7 downto 0);
      signal zi58 : std_logic_vector (7 downto 0);
      signal zi59 : std_logic_vector (5 downto 0);
      signal zi60 : std_logic_vector (16 downto 0);
      signal \connR9\ : std_logic_vector (69 downto 0);
      signal \zll_l_s89_outR4\ : std_logic_vector (69 downto 0);
      signal zi61 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR6\ : std_logic_vector (14 downto 0);
      signal zi62 : std_logic_vector (14 downto 0);
      signal zi63 : std_logic_vector (1 downto 0);
      signal zi64 : std_logic_vector (1 downto 0);
      signal zi65 : std_logic_vector (1 downto 0);
      signal \main_r0_outR1\ : std_logic_vector (7 downto 0);
      signal zll_l_x177_out : std_logic_vector (87 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zll_l_x177_outR1\ : std_logic_vector (87 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi66 : std_logic_vector (7 downto 0);
      signal zl_main_getreg165_out : std_logic_vector (87 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi67 : std_logic_vector (7 downto 0);
      signal \zl_main_getreg165_outR1\ : std_logic_vector (87 downto 0);
      signal zi68 : std_logic_vector (5 downto 0);
      signal \main_r0_outR2\ : std_logic_vector (7 downto 0);
      signal zi69 : std_logic_vector (7 downto 0);
      signal zi70 : std_logic_vector (0 downto 0);
      signal zl_main_putpc38_out : std_logic_vector (87 downto 0);
      signal \zl_main_getpc40_outR1\ : std_logic_vector (87 downto 0);
begin
inst : \Main_inputs\ port map (arg0, main_inputs_out);
      zi0 <= main_inputs_out;
      zi1 <= zi0(16 downto 8);
      zi2 <= zi1;
      \instR1\ : \ZL_Main_getPC40\ port map (arg0, zl_main_getpc40_out);
      zi3 <= zi2(5 downto 0);
      \instR2\ : \Main_outputs\ port map (arg0, main_outputs_out);
      zi4 <= main_outputs_out;
      zi5 <= zi4(14 downto 14);
      zi6 <= zi4(7 downto 0);
      conn <= (zi5 & zi3 & zi6);
      \instR3\ : \ZLL_L_o2321\ port map (conn, zll_l_o2321_out);
      zi7 <= zll_l_o2321_out;
      zi8 <= arg0(69 downto 62);
      zi9 <= arg0(61 downto 54);
      zi10 <= arg0(53 downto 46);
      zi11 <= arg0(45 downto 38);
      zi12 <= arg0(37 downto 32);
      zi13 <= arg0(31 downto 15);
      \connR1\ <= (zi8 & zi9 & zi10 & zi11 & zi12 & zi13 & zi7);
      \instR4\ : \ZLL_L_s89\ port map (\connR1\, zll_l_s89_out);
      zi14 <= zll_l_s89_out;
      \instR5\ : \Main_outputs\ port map (zi14, \main_outputs_outR1\);
      zi15 <= \main_outputs_outR1\;
      zi16 <= zi15(13 downto 8);
      zi17 <= zi15(7 downto 0);
      \connR2\ <= (std_logic_vector'(B"0") & zi16 & zi17);
      \instR6\ : \ZLL_L_o2321\ port map (\connR2\, \zll_l_o2321_outR1\);
      zi18 <= \zll_l_o2321_outR1\;
      zi19 <= zi14(69 downto 62);
      zi20 <= zi14(61 downto 54);
      zi21 <= zi14(53 downto 46);
      zi22 <= zi14(45 downto 38);
      zi23 <= zi14(37 downto 32);
      zi24 <= zi14(31 downto 15);
      \connR3\ <= (zi19 & zi20 & zi21 & zi22 & zi23 & zi24 & zi18);
      \instR7\ : \ZLL_L_s89\ port map (\connR3\, \zll_l_s89_outR1\);
      zi25 <= \zll_l_s89_outR1\;
      \instR8\ : \Main_outputs\ port map (zi25, \main_outputs_outR2\);
      zi26 <= \main_outputs_outR2\;
      zi27 <= zi2(5 downto 0);
      \instR9\ : \Main_r0\ port map (arg0, main_r0_out);
      zi28 <= main_r0_out;
      \instR10\ : \Main_outputs\ port map (arg0, \main_outputs_outR3\);
      zi29 <= \main_outputs_outR3\;
      zi30 <= zi29(14 downto 14);
      zi31 <= zi29(7 downto 0);
      \connR4\ <= (zi30 & zi27 & zi31);
      \instR11\ : \ZLL_L_o2321\ port map (\connR4\, \zll_l_o2321_outR2\);
      zi32 <= \zll_l_o2321_outR2\;
      zi33 <= arg0(69 downto 62);
      zi34 <= arg0(61 downto 54);
      zi35 <= arg0(53 downto 46);
      zi36 <= arg0(45 downto 38);
      zi37 <= arg0(37 downto 32);
      zi38 <= arg0(31 downto 15);
      \connR5\ <= (zi33 & zi34 & zi35 & zi36 & zi37 & zi38 & zi32);
      \instR12\ : \ZLL_L_s89\ port map (\connR5\, \zll_l_s89_outR2\);
      zi39 <= \zll_l_s89_outR2\;
      \instR13\ : \Main_outputs\ port map (zi39, \main_outputs_outR4\);
      zi40 <= \main_outputs_outR4\;
      zi41 <= zi40(14 downto 14);
      zi42 <= zi40(13 downto 8);
      \connR6\ <= (zi41 & zi42 & zi28);
      \instR14\ : \ZLL_L_o2321\ port map (\connR6\, \zll_l_o2321_outR3\);
      zi43 <= \zll_l_o2321_outR3\;
      zi44 <= zi39(69 downto 62);
      zi45 <= zi39(61 downto 54);
      zi46 <= zi39(53 downto 46);
      zi47 <= zi39(45 downto 38);
      zi48 <= zi39(37 downto 32);
      zi49 <= zi39(31 downto 15);
      \connR7\ <= (zi44 & zi45 & zi46 & zi47 & zi48 & zi49 & zi43);
      \instR15\ : \ZLL_L_s89\ port map (\connR7\, \zll_l_s89_outR3\);
      zi50 <= \zll_l_s89_outR3\;
      \instR16\ : \Main_outputs\ port map (zi50, \main_outputs_outR5\);
      zi51 <= \main_outputs_outR5\;
      zi52 <= zi51(13 downto 8);
      zi53 <= zi51(7 downto 0);
      \connR8\ <= (std_logic_vector'(B"1") & zi52 & zi53);
      \instR17\ : \ZLL_L_o2321\ port map (\connR8\, \zll_l_o2321_outR4\);
      zi54 <= \zll_l_o2321_outR4\;
      zi55 <= zi50(69 downto 62);
      zi56 <= zi50(61 downto 54);
      zi57 <= zi50(53 downto 46);
      zi58 <= zi50(45 downto 38);
      zi59 <= zi50(37 downto 32);
      zi60 <= zi50(31 downto 15);
      \connR9\ <= (zi55 & zi56 & zi57 & zi58 & zi59 & zi60 & zi54);
      \instR18\ : \ZLL_L_s89\ port map (\connR9\, \zll_l_s89_outR4\);
      zi61 <= \zll_l_s89_outR4\;
      \instR19\ : \Main_outputs\ port map (zi61, \main_outputs_outR6\);
      zi62 <= \main_outputs_outR6\;
      zi63 <= zi2(5 downto 4);
      zi64 <= zi2(3 downto 2);
      zi65 <= zi2(1 downto 0);
      \instR20\ : \Main_r0\ port map (arg0, \main_r0_outR1\);
      \instR21\ : \ZLL_L_x177\ port map (zi65, zi63, arg0, \main_r0_outR1\, zll_l_x177_out);
      \instR22\ : \Main_r1\ port map (arg0, main_r1_out);
      \instR23\ : \ZLL_L_x177\ port map (zi65, zi63, arg0, main_r1_out, \zll_l_x177_outR1\);
      \instR24\ : \Main_r2\ port map (arg0, main_r2_out);
      zi66 <= main_r2_out;
      \instR25\ : \ZL_Main_getReg165\ port map (zi63, zi66, zi65, arg0, zl_main_getreg165_out);
      \instR26\ : \Main_r3\ port map (arg0, main_r3_out);
      zi67 <= main_r3_out;
      \instR27\ : \ZL_Main_getReg165\ port map (zi63, zi67, zi65, arg0, \zl_main_getreg165_outR1\);
      zi68 <= zi2(5 downto 0);
      \instR28\ : \Main_r0\ port map (arg0, \main_r0_outR2\);
      zi69 <= \main_r0_outR2\;
      zi70 <= rw_eq(zi69, std_logic_vector'(B"00000000"));
      \instR29\ : \ZL_Main_putPC38\ port map (zi68, arg0, zl_main_putpc38_out);
      \instR30\ : \ZL_Main_getPC40\ port map (arg0, \zl_main_getpc40_outR1\);
      res <= rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"000")), zl_main_getpc40_out, rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"001")), (zi26 & std_logic_vector'(B"010") & zi25), rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"010")), (zi62 & std_logic_vector'(B"011") & zi61), rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"011")), rw_cond(rw_eq(zi64, std_logic_vector'(B"00")), zll_l_x177_out, rw_cond(rw_eq(zi64, std_logic_vector'(B"01")), \zll_l_x177_outR1\, rw_cond(rw_eq(zi64, std_logic_vector'(B"10")), zl_main_getreg165_out, \zl_main_getreg165_outR1\))), rw_cond(rw_eq(zi70, std_logic_vector'(B"0")), zl_main_putpc38_out, \zl_main_getpc40_outR1\)))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_Main_getReg165\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (1 downto 0);
      arg3 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \ZL_Main_getReg165\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_L_x172\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \ZL_Main_putReg156\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zll_l_x172_out : std_logic_vector (87 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (7 downto 0);
      signal zl_main_putreg156_out : std_logic_vector (87 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal \connR1\ : std_logic_vector (7 downto 0);
      signal \zl_main_putreg156_outR1\ : std_logic_vector (87 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zll_l_x172_outR1\ : std_logic_vector (87 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZLL_L_x172\ port map (arg0, arg1, arg3, main_r0_out, zll_l_x172_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      zi0 <= main_r1_out;
      conn <= rw_not(rw_and(arg1, zi0));
      \instR3\ : \ZL_Main_putReg156\ port map (arg0, conn, arg3, zl_main_putreg156_out);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      zi1 <= main_r2_out;
      \connR1\ <= rw_not(rw_and(arg1, zi1));
      \instR5\ : \ZL_Main_putReg156\ port map (arg0, \connR1\, arg3, \zl_main_putreg156_outR1\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZLL_L_x172\ port map (arg0, arg1, arg3, main_r3_out, \zll_l_x172_outR1\);
      res <= rw_cond(rw_eq(arg2, std_logic_vector'(B"00")), zll_l_x172_out, rw_cond(rw_eq(arg2, std_logic_vector'(B"01")), zl_main_putreg156_out, rw_cond(rw_eq(arg2, std_logic_vector'(B"10")), \zl_main_putreg156_outR1\, \zll_l_x172_outR1\)));
end architecture;