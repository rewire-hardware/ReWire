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
      component \main_getIns2\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \main_getPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \main_getPC2\ is
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
      signal zi8 : std_logic_vector (69 downto 0);
      signal zi9 : std_logic_vector (69 downto 0);
      signal main_getins2_out : std_logic_vector (87 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal zi15 : std_logic_vector (5 downto 0);
      signal zi16 : std_logic_vector (14 downto 0);
      signal zi17 : std_logic_vector (69 downto 0);
      signal zi18 : std_logic_vector (69 downto 0);
      signal main_inputs_out : std_logic_vector (16 downto 0);
      signal zi19 : std_logic_vector (16 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (7 downto 0);
      signal zi24 : std_logic_vector (7 downto 0);
      signal zi25 : std_logic_vector (5 downto 0);
      signal zi26 : std_logic_vector (16 downto 0);
      signal zi27 : std_logic_vector (14 downto 0);
      signal zi28 : std_logic_vector (69 downto 0);
      signal zi29 : std_logic_vector (69 downto 0);
      signal \main_getins2_outR1\ : std_logic_vector (87 downto 0);
      signal zi31 : std_logic_vector (7 downto 0);
      signal zi32 : std_logic_vector (7 downto 0);
      signal zi33 : std_logic_vector (7 downto 0);
      signal zi34 : std_logic_vector (7 downto 0);
      signal zi35 : std_logic_vector (5 downto 0);
      signal zi36 : std_logic_vector (14 downto 0);
      signal zi37 : std_logic_vector (69 downto 0);
      signal zi38 : std_logic_vector (69 downto 0);
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal zi39 : std_logic_vector (5 downto 0);
      signal zi40 : std_logic_vector (5 downto 0);
      signal zi41 : std_logic_vector (7 downto 0);
      signal zi42 : std_logic_vector (7 downto 0);
      signal zi43 : std_logic_vector (7 downto 0);
      signal zi44 : std_logic_vector (7 downto 0);
      signal zi45 : std_logic_vector (16 downto 0);
      signal zi46 : std_logic_vector (14 downto 0);
      signal zi47 : std_logic_vector (69 downto 0);
      signal zi48 : std_logic_vector (69 downto 0);
      signal \main_pc_outR1\ : std_logic_vector (5 downto 0);
      signal zi49 : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal zi50 : std_logic_vector (14 downto 0);
      signal zi51 : std_logic_vector (0 downto 0);
      signal zi52 : std_logic_vector (7 downto 0);
      signal zi53 : std_logic_vector (14 downto 0);
      signal zi54 : std_logic_vector (14 downto 0);
      signal zi55 : std_logic_vector (7 downto 0);
      signal zi56 : std_logic_vector (7 downto 0);
      signal zi57 : std_logic_vector (7 downto 0);
      signal zi58 : std_logic_vector (7 downto 0);
      signal zi59 : std_logic_vector (5 downto 0);
      signal zi60 : std_logic_vector (16 downto 0);
      signal zi61 : std_logic_vector (69 downto 0);
      signal zi62 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (14 downto 0);
      signal zi63 : std_logic_vector (14 downto 0);
      signal zi64 : std_logic_vector (5 downto 0);
      signal zi65 : std_logic_vector (7 downto 0);
      signal zi66 : std_logic_vector (14 downto 0);
      signal zi67 : std_logic_vector (14 downto 0);
      signal zi68 : std_logic_vector (7 downto 0);
      signal zi69 : std_logic_vector (7 downto 0);
      signal zi70 : std_logic_vector (7 downto 0);
      signal zi71 : std_logic_vector (7 downto 0);
      signal zi72 : std_logic_vector (5 downto 0);
      signal zi73 : std_logic_vector (16 downto 0);
      signal zi74 : std_logic_vector (69 downto 0);
      signal zi75 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (14 downto 0);
      signal zi76 : std_logic_vector (14 downto 0);
      signal zi78 : std_logic_vector (7 downto 0);
      signal zi79 : std_logic_vector (7 downto 0);
      signal zi80 : std_logic_vector (7 downto 0);
      signal zi81 : std_logic_vector (7 downto 0);
      signal zi82 : std_logic_vector (5 downto 0);
      signal zi83 : std_logic_vector (14 downto 0);
      signal zi84 : std_logic_vector (69 downto 0);
      signal zi85 : std_logic_vector (69 downto 0);
      signal main_getpc2_out : std_logic_vector (87 downto 0);
      signal zi87 : std_logic_vector (7 downto 0);
      signal zi88 : std_logic_vector (7 downto 0);
      signal zi89 : std_logic_vector (7 downto 0);
      signal zi90 : std_logic_vector (7 downto 0);
      signal zi91 : std_logic_vector (5 downto 0);
      signal zi92 : std_logic_vector (14 downto 0);
      signal zi93 : std_logic_vector (69 downto 0);
      signal zi94 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (87 downto 0);
      signal zres : std_logic_vector (87 downto 0);
begin
zi2 <= \__st0\(69 downto 62);
      zi3 <= \__st0\(61 downto 54);
      zi4 <= \__st0\(53 downto 46);
      zi5 <= \__st0\(45 downto 38);
      zi6 <= \__st0\(37 downto 32);
      zi7 <= \__st0\(14 downto 0);
      zi8 <= (zi2 & zi3 & zi4 & zi5 & zi6 & \__in0\ & zi7);
      zi9 <= zi8;
      inst : \main_getIns2\ port map (zi9, main_getins2_out);
      zi11 <= \__st0\(69 downto 62);
      zi12 <= \__st0\(61 downto 54);
      zi13 <= \__st0\(53 downto 46);
      zi14 <= \__st0\(45 downto 38);
      zi15 <= \__st0\(37 downto 32);
      zi16 <= \__st0\(14 downto 0);
      zi17 <= (zi11 & zi12 & zi13 & zi14 & zi15 & \__in0\ & zi16);
      zi18 <= zi17;
      \instR1\ : \Main_inputs\ port map (zi18, main_inputs_out);
      zi19 <= main_inputs_out;
      zi20 <= zi19(7 downto 0);
      zi21 <= zi20;
      zi22 <= zi18(61 downto 54);
      zi23 <= zi18(53 downto 46);
      zi24 <= zi18(45 downto 38);
      zi25 <= zi18(37 downto 32);
      zi26 <= zi18(31 downto 15);
      zi27 <= zi18(14 downto 0);
      zi28 <= (zi21 & zi22 & zi23 & zi24 & zi25 & zi26 & zi27);
      zi29 <= zi28;
      \instR2\ : \main_getIns2\ port map (zi29, \main_getins2_outR1\);
      zi31 <= \__st0\(69 downto 62);
      zi32 <= \__st0\(61 downto 54);
      zi33 <= \__st0\(53 downto 46);
      zi34 <= \__st0\(45 downto 38);
      zi35 <= \__st0\(37 downto 32);
      zi36 <= \__st0\(14 downto 0);
      zi37 <= (zi31 & zi32 & zi33 & zi34 & zi35 & \__in0\ & zi36);
      zi38 <= zi37;
      \instR3\ : \Main_pc\ port map (zi38, main_pc_out);
      zi39 <= main_pc_out;
      zi40 <= rw_add(zi39, std_logic_vector'(B"000001"));
      zi41 <= zi38(69 downto 62);
      zi42 <= zi38(61 downto 54);
      zi43 <= zi38(53 downto 46);
      zi44 <= zi38(45 downto 38);
      zi45 <= zi38(31 downto 15);
      zi46 <= zi38(14 downto 0);
      zi47 <= (zi41 & zi42 & zi43 & zi44 & zi40 & zi45 & zi46);
      zi48 <= zi47;
      \instR4\ : \Main_pc\ port map (zi48, \main_pc_outR1\);
      zi49 <= \main_pc_outR1\;
      \instR5\ : \Main_outputs\ port map (zi48, main_outputs_out);
      zi50 <= main_outputs_out;
      zi51 <= zi50(14 downto 14);
      zi52 <= zi50(7 downto 0);
      zi53 <= (zi51 & zi49 & zi52);
      zi54 <= zi53;
      zi55 <= zi48(69 downto 62);
      zi56 <= zi48(61 downto 54);
      zi57 <= zi48(53 downto 46);
      zi58 <= zi48(45 downto 38);
      zi59 <= zi48(37 downto 32);
      zi60 <= zi48(31 downto 15);
      zi61 <= (zi55 & zi56 & zi57 & zi58 & zi59 & zi60 & zi54);
      zi62 <= zi61;
      \instR6\ : \Main_outputs\ port map (zi62, \main_outputs_outR1\);
      zi63 <= \main_outputs_outR1\;
      zi64 <= zi63(13 downto 8);
      zi65 <= zi63(7 downto 0);
      zi66 <= (std_logic_vector'(B"0") & zi64 & zi65);
      zi67 <= zi66;
      zi68 <= zi62(69 downto 62);
      zi69 <= zi62(61 downto 54);
      zi70 <= zi62(53 downto 46);
      zi71 <= zi62(45 downto 38);
      zi72 <= zi62(37 downto 32);
      zi73 <= zi62(31 downto 15);
      zi74 <= (zi68 & zi69 & zi70 & zi71 & zi72 & zi73 & zi67);
      zi75 <= zi74;
      \instR7\ : \Main_outputs\ port map (zi75, \main_outputs_outR2\);
      zi76 <= \main_outputs_outR2\;
      zi78 <= \__st0\(69 downto 62);
      zi79 <= \__st0\(61 downto 54);
      zi80 <= \__st0\(53 downto 46);
      zi81 <= \__st0\(45 downto 38);
      zi82 <= \__st0\(37 downto 32);
      zi83 <= \__st0\(14 downto 0);
      zi84 <= (zi78 & zi79 & zi80 & zi81 & zi82 & \__in0\ & zi83);
      zi85 <= zi84;
      \instR8\ : \main_getPC2\ port map (zi85, main_getpc2_out);
      zi87 <= \__st0\(69 downto 62);
      zi88 <= \__st0\(61 downto 54);
      zi89 <= \__st0\(53 downto 46);
      zi90 <= \__st0\(45 downto 38);
      zi91 <= \__st0\(37 downto 32);
      zi92 <= \__st0\(14 downto 0);
      zi93 <= (zi87 & zi88 & zi89 & zi90 & zi91 & \__in0\ & zi92);
      zi94 <= zi93;
      \instR9\ : \main_getPC\ port map (zi94, main_getpc_out);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"000")), main_getins2_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"001")), \main_getins2_outR1\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"010")), (zi76 & std_logic_vector'(B"001") & zi75), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"011")), main_getpc2_out, main_getpc_out))));
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
entity \main_getPC\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getPC\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (14 downto 0));
      end component;
      component \Main_pc\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (5 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal zi0 : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal zi1 : std_logic_vector (14 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (14 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (5 downto 0);
      signal zi11 : std_logic_vector (16 downto 0);
      signal zi12 : std_logic_vector (69 downto 0);
      signal zi13 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (14 downto 0);
      signal zi14 : std_logic_vector (14 downto 0);
      signal zi15 : std_logic_vector (5 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (14 downto 0);
      signal zi18 : std_logic_vector (14 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (5 downto 0);
      signal zi24 : std_logic_vector (16 downto 0);
      signal zi25 : std_logic_vector (69 downto 0);
      signal zi26 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (14 downto 0);
      signal zi27 : std_logic_vector (14 downto 0);
begin
inst : \Main_pc\ port map (arg0, main_pc_out);
      zi0 <= main_pc_out;
      \instR1\ : \Main_outputs\ port map (arg0, main_outputs_out);
      zi1 <= main_outputs_out;
      zi2 <= zi1(14 downto 14);
      zi3 <= zi1(7 downto 0);
      zi4 <= (zi2 & zi0 & zi3);
      zi5 <= zi4;
      zi6 <= arg0(69 downto 62);
      zi7 <= arg0(61 downto 54);
      zi8 <= arg0(53 downto 46);
      zi9 <= arg0(45 downto 38);
      zi10 <= arg0(37 downto 32);
      zi11 <= arg0(31 downto 15);
      zi12 <= (zi6 & zi7 & zi8 & zi9 & zi10 & zi11 & zi5);
      zi13 <= zi12;
      \instR2\ : \Main_outputs\ port map (zi13, \main_outputs_outR1\);
      zi14 <= \main_outputs_outR1\;
      zi15 <= zi14(13 downto 8);
      zi16 <= zi14(7 downto 0);
      zi17 <= (std_logic_vector'(B"0") & zi15 & zi16);
      zi18 <= zi17;
      zi19 <= zi13(69 downto 62);
      zi20 <= zi13(61 downto 54);
      zi21 <= zi13(53 downto 46);
      zi22 <= zi13(45 downto 38);
      zi23 <= zi13(37 downto 32);
      zi24 <= zi13(31 downto 15);
      zi25 <= (zi19 & zi20 & zi21 & zi22 & zi23 & zi24 & zi18);
      zi26 <= zi25;
      \instR3\ : \Main_outputs\ port map (zi26, \main_outputs_outR2\);
      zi27 <= \main_outputs_outR2\;
      res <= (zi27 & std_logic_vector'(B"000") & zi26);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_putPC\ is
port (arg0 : in std_logic_vector (5 downto 0);
      arg1 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_putPC\ is
component \main_getPC\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
      signal zi6 : std_logic_vector (69 downto 0);
      signal zi7 : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (87 downto 0);
begin
zi0 <= arg1(69 downto 62);
      zi1 <= arg1(61 downto 54);
      zi2 <= arg1(53 downto 46);
      zi3 <= arg1(45 downto 38);
      zi4 <= arg1(31 downto 15);
      zi5 <= arg1(14 downto 0);
      zi6 <= (zi0 & zi1 & zi2 & zi3 & arg0 & zi4 & zi5);
      zi7 <= zi6;
      inst : \main_getPC\ port map (zi7, main_getpc_out);
      res <= main_getpc_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getPC2\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getPC2\ is
component \Main_pc\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (5 downto 0));
      end component;
      component \main_putPC\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal zi0 : std_logic_vector (5 downto 0);
      signal conn : std_logic_vector (5 downto 0);
      signal main_putpc_out : std_logic_vector (87 downto 0);
begin
inst : \Main_pc\ port map (arg0, main_pc_out);
      zi0 <= main_pc_out;
      conn <= rw_add(zi0, std_logic_vector'(B"000001"));
      \instR1\ : \main_putPC\ port map (conn, arg0, main_putpc_out);
      res <= main_putpc_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_putReg\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_putReg\ is
component \main_getPC2\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
      signal zi4 : std_logic_vector (16 downto 0);
      signal zi5 : std_logic_vector (14 downto 0);
      signal zi6 : std_logic_vector (69 downto 0);
      signal zi7 : std_logic_vector (69 downto 0);
      signal main_getpc2_out : std_logic_vector (87 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (5 downto 0);
      signal zi12 : std_logic_vector (16 downto 0);
      signal zi13 : std_logic_vector (14 downto 0);
      signal zi14 : std_logic_vector (69 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal \main_getpc2_outR1\ : std_logic_vector (87 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (5 downto 0);
      signal zi20 : std_logic_vector (16 downto 0);
      signal zi21 : std_logic_vector (14 downto 0);
      signal zi22 : std_logic_vector (69 downto 0);
      signal zi23 : std_logic_vector (69 downto 0);
      signal \main_getpc2_outR2\ : std_logic_vector (87 downto 0);
      signal zi24 : std_logic_vector (7 downto 0);
      signal zi25 : std_logic_vector (7 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (5 downto 0);
      signal zi28 : std_logic_vector (16 downto 0);
      signal zi29 : std_logic_vector (14 downto 0);
      signal zi30 : std_logic_vector (69 downto 0);
      signal zi31 : std_logic_vector (69 downto 0);
      signal \main_getpc2_outR3\ : std_logic_vector (87 downto 0);
begin
zi0 <= arg2(61 downto 54);
      zi1 <= arg2(53 downto 46);
      zi2 <= arg2(45 downto 38);
      zi3 <= arg2(37 downto 32);
      zi4 <= arg2(31 downto 15);
      zi5 <= arg2(14 downto 0);
      zi6 <= (arg1 & zi0 & zi1 & zi2 & zi3 & zi4 & zi5);
      zi7 <= zi6;
      inst : \main_getPC2\ port map (zi7, main_getpc2_out);
      zi8 <= arg2(69 downto 62);
      zi9 <= arg2(53 downto 46);
      zi10 <= arg2(45 downto 38);
      zi11 <= arg2(37 downto 32);
      zi12 <= arg2(31 downto 15);
      zi13 <= arg2(14 downto 0);
      zi14 <= (zi8 & arg1 & zi9 & zi10 & zi11 & zi12 & zi13);
      zi15 <= zi14;
      \instR1\ : \main_getPC2\ port map (zi15, \main_getpc2_outR1\);
      zi16 <= arg2(69 downto 62);
      zi17 <= arg2(61 downto 54);
      zi18 <= arg2(45 downto 38);
      zi19 <= arg2(37 downto 32);
      zi20 <= arg2(31 downto 15);
      zi21 <= arg2(14 downto 0);
      zi22 <= (zi16 & zi17 & arg1 & zi18 & zi19 & zi20 & zi21);
      zi23 <= zi22;
      \instR2\ : \main_getPC2\ port map (zi23, \main_getpc2_outR2\);
      zi24 <= arg2(69 downto 62);
      zi25 <= arg2(61 downto 54);
      zi26 <= arg2(53 downto 46);
      zi27 <= arg2(37 downto 32);
      zi28 <= arg2(31 downto 15);
      zi29 <= arg2(14 downto 0);
      zi30 <= (zi24 & zi25 & zi26 & arg1 & zi27 & zi28 & zi29);
      zi31 <= zi30;
      \instR3\ : \main_getPC2\ port map (zi31, \main_getpc2_outR3\);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_getpc2_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), \main_getpc2_outR1\, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), \main_getpc2_outR2\, \main_getpc2_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getReg\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (1 downto 0);
      arg3 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getReg\ is
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
      component \main_putReg\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (7 downto 0);
      signal main_putreg_out : std_logic_vector (87 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal \connR1\ : std_logic_vector (7 downto 0);
      signal \main_putreg_outR1\ : std_logic_vector (87 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal \connR2\ : std_logic_vector (7 downto 0);
      signal \main_putreg_outR2\ : std_logic_vector (87 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal \connR3\ : std_logic_vector (7 downto 0);
      signal \main_putreg_outR3\ : std_logic_vector (87 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      zi0 <= main_r0_out;
      conn <= rw_not(rw_and(arg1, zi0));
      \instR1\ : \main_putReg\ port map (arg0, conn, arg3, main_putreg_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      zi1 <= main_r1_out;
      \connR1\ <= rw_not(rw_and(arg1, zi1));
      \instR3\ : \main_putReg\ port map (arg0, \connR1\, arg3, \main_putreg_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      zi2 <= main_r2_out;
      \connR2\ <= rw_not(rw_and(arg1, zi2));
      \instR5\ : \main_putReg\ port map (arg0, \connR2\, arg3, \main_putreg_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      zi3 <= main_r3_out;
      \connR3\ <= rw_not(rw_and(arg1, zi3));
      \instR7\ : \main_putReg\ port map (arg0, \connR3\, arg3, \main_putreg_outR3\);
      res <= rw_cond(rw_eq(arg2, std_logic_vector'(B"00")), main_putreg_out, rw_cond(rw_eq(arg2, std_logic_vector'(B"01")), \main_putreg_outR1\, rw_cond(rw_eq(arg2, std_logic_vector'(B"10")), \main_putreg_outR2\, \main_putreg_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getIns2\ is
port (arg0 : in std_logic_vector (69 downto 0);
      res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getIns2\ is
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
      component \main_getPC2\ is
      port (arg0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \main_getReg\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (1 downto 0);
            arg3 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      component \main_putPC\ is
      port (arg0 : in std_logic_vector (5 downto 0);
            arg1 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
      end component;
      signal main_inputs_out : std_logic_vector (16 downto 0);
      signal zi0 : std_logic_vector (16 downto 0);
      signal zi1 : std_logic_vector (8 downto 0);
      signal zi2 : std_logic_vector (8 downto 0);
      signal main_getpc2_out : std_logic_vector (87 downto 0);
      signal zi3 : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal zi4 : std_logic_vector (14 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (14 downto 0);
      signal zi8 : std_logic_vector (14 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal zi11 : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (5 downto 0);
      signal zi14 : std_logic_vector (16 downto 0);
      signal zi15 : std_logic_vector (69 downto 0);
      signal zi16 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (14 downto 0);
      signal zi17 : std_logic_vector (14 downto 0);
      signal zi18 : std_logic_vector (5 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (14 downto 0);
      signal zi21 : std_logic_vector (14 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (7 downto 0);
      signal zi24 : std_logic_vector (7 downto 0);
      signal zi25 : std_logic_vector (7 downto 0);
      signal zi26 : std_logic_vector (5 downto 0);
      signal zi27 : std_logic_vector (16 downto 0);
      signal zi28 : std_logic_vector (69 downto 0);
      signal zi29 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (14 downto 0);
      signal zi30 : std_logic_vector (14 downto 0);
      signal zi31 : std_logic_vector (5 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi32 : std_logic_vector (7 downto 0);
      signal \main_outputs_outR3\ : std_logic_vector (14 downto 0);
      signal zi33 : std_logic_vector (14 downto 0);
      signal zi34 : std_logic_vector (0 downto 0);
      signal zi35 : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (14 downto 0);
      signal zi37 : std_logic_vector (14 downto 0);
      signal zi38 : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (7 downto 0);
      signal zi40 : std_logic_vector (7 downto 0);
      signal zi41 : std_logic_vector (7 downto 0);
      signal zi42 : std_logic_vector (5 downto 0);
      signal zi43 : std_logic_vector (16 downto 0);
      signal zi44 : std_logic_vector (69 downto 0);
      signal zi45 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR4\ : std_logic_vector (14 downto 0);
      signal zi46 : std_logic_vector (14 downto 0);
      signal zi47 : std_logic_vector (0 downto 0);
      signal zi48 : std_logic_vector (5 downto 0);
      signal zi49 : std_logic_vector (14 downto 0);
      signal zi50 : std_logic_vector (14 downto 0);
      signal zi51 : std_logic_vector (7 downto 0);
      signal zi52 : std_logic_vector (7 downto 0);
      signal zi53 : std_logic_vector (7 downto 0);
      signal zi54 : std_logic_vector (7 downto 0);
      signal zi55 : std_logic_vector (5 downto 0);
      signal zi56 : std_logic_vector (16 downto 0);
      signal zi57 : std_logic_vector (69 downto 0);
      signal zi58 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR5\ : std_logic_vector (14 downto 0);
      signal zi59 : std_logic_vector (14 downto 0);
      signal zi60 : std_logic_vector (5 downto 0);
      signal zi61 : std_logic_vector (7 downto 0);
      signal zi62 : std_logic_vector (14 downto 0);
      signal zi63 : std_logic_vector (14 downto 0);
      signal zi64 : std_logic_vector (7 downto 0);
      signal zi65 : std_logic_vector (7 downto 0);
      signal zi66 : std_logic_vector (7 downto 0);
      signal zi67 : std_logic_vector (7 downto 0);
      signal zi68 : std_logic_vector (5 downto 0);
      signal zi69 : std_logic_vector (16 downto 0);
      signal zi70 : std_logic_vector (69 downto 0);
      signal zi71 : std_logic_vector (69 downto 0);
      signal \main_outputs_outR6\ : std_logic_vector (14 downto 0);
      signal zi72 : std_logic_vector (14 downto 0);
      signal zi73 : std_logic_vector (1 downto 0);
      signal zi74 : std_logic_vector (1 downto 0);
      signal zi75 : std_logic_vector (1 downto 0);
      signal \main_r0_outR1\ : std_logic_vector (7 downto 0);
      signal zi76 : std_logic_vector (7 downto 0);
      signal main_getreg_out : std_logic_vector (87 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi77 : std_logic_vector (7 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (87 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi78 : std_logic_vector (7 downto 0);
      signal \main_getreg_outR2\ : std_logic_vector (87 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi79 : std_logic_vector (7 downto 0);
      signal \main_getreg_outR3\ : std_logic_vector (87 downto 0);
      signal zi80 : std_logic_vector (5 downto 0);
      signal \main_r0_outR2\ : std_logic_vector (7 downto 0);
      signal zi81 : std_logic_vector (7 downto 0);
      signal zi82 : std_logic_vector (0 downto 0);
      signal main_putpc_out : std_logic_vector (87 downto 0);
      signal \main_getpc2_outR1\ : std_logic_vector (87 downto 0);
begin
inst : \Main_inputs\ port map (arg0, main_inputs_out);
      zi0 <= main_inputs_out;
      zi1 <= zi0(16 downto 8);
      zi2 <= zi1;
      \instR1\ : \main_getPC2\ port map (arg0, main_getpc2_out);
      zi3 <= zi2(5 downto 0);
      \instR2\ : \Main_outputs\ port map (arg0, main_outputs_out);
      zi4 <= main_outputs_out;
      zi5 <= zi4(14 downto 14);
      zi6 <= zi4(7 downto 0);
      zi7 <= (zi5 & zi3 & zi6);
      zi8 <= zi7;
      zi9 <= arg0(69 downto 62);
      zi10 <= arg0(61 downto 54);
      zi11 <= arg0(53 downto 46);
      zi12 <= arg0(45 downto 38);
      zi13 <= arg0(37 downto 32);
      zi14 <= arg0(31 downto 15);
      zi15 <= (zi9 & zi10 & zi11 & zi12 & zi13 & zi14 & zi8);
      zi16 <= zi15;
      \instR3\ : \Main_outputs\ port map (zi16, \main_outputs_outR1\);
      zi17 <= \main_outputs_outR1\;
      zi18 <= zi17(13 downto 8);
      zi19 <= zi17(7 downto 0);
      zi20 <= (std_logic_vector'(B"0") & zi18 & zi19);
      zi21 <= zi20;
      zi22 <= zi16(69 downto 62);
      zi23 <= zi16(61 downto 54);
      zi24 <= zi16(53 downto 46);
      zi25 <= zi16(45 downto 38);
      zi26 <= zi16(37 downto 32);
      zi27 <= zi16(31 downto 15);
      zi28 <= (zi22 & zi23 & zi24 & zi25 & zi26 & zi27 & zi21);
      zi29 <= zi28;
      \instR4\ : \Main_outputs\ port map (zi29, \main_outputs_outR2\);
      zi30 <= \main_outputs_outR2\;
      zi31 <= zi2(5 downto 0);
      \instR5\ : \Main_r0\ port map (arg0, main_r0_out);
      zi32 <= main_r0_out;
      \instR6\ : \Main_outputs\ port map (arg0, \main_outputs_outR3\);
      zi33 <= \main_outputs_outR3\;
      zi34 <= zi33(14 downto 14);
      zi35 <= zi33(7 downto 0);
      zi36 <= (zi34 & zi31 & zi35);
      zi37 <= zi36;
      zi38 <= arg0(69 downto 62);
      zi39 <= arg0(61 downto 54);
      zi40 <= arg0(53 downto 46);
      zi41 <= arg0(45 downto 38);
      zi42 <= arg0(37 downto 32);
      zi43 <= arg0(31 downto 15);
      zi44 <= (zi38 & zi39 & zi40 & zi41 & zi42 & zi43 & zi37);
      zi45 <= zi44;
      \instR7\ : \Main_outputs\ port map (zi45, \main_outputs_outR4\);
      zi46 <= \main_outputs_outR4\;
      zi47 <= zi46(14 downto 14);
      zi48 <= zi46(13 downto 8);
      zi49 <= (zi47 & zi48 & zi32);
      zi50 <= zi49;
      zi51 <= zi45(69 downto 62);
      zi52 <= zi45(61 downto 54);
      zi53 <= zi45(53 downto 46);
      zi54 <= zi45(45 downto 38);
      zi55 <= zi45(37 downto 32);
      zi56 <= zi45(31 downto 15);
      zi57 <= (zi51 & zi52 & zi53 & zi54 & zi55 & zi56 & zi50);
      zi58 <= zi57;
      \instR8\ : \Main_outputs\ port map (zi58, \main_outputs_outR5\);
      zi59 <= \main_outputs_outR5\;
      zi60 <= zi59(13 downto 8);
      zi61 <= zi59(7 downto 0);
      zi62 <= (std_logic_vector'(B"1") & zi60 & zi61);
      zi63 <= zi62;
      zi64 <= zi58(69 downto 62);
      zi65 <= zi58(61 downto 54);
      zi66 <= zi58(53 downto 46);
      zi67 <= zi58(45 downto 38);
      zi68 <= zi58(37 downto 32);
      zi69 <= zi58(31 downto 15);
      zi70 <= (zi64 & zi65 & zi66 & zi67 & zi68 & zi69 & zi63);
      zi71 <= zi70;
      \instR9\ : \Main_outputs\ port map (zi71, \main_outputs_outR6\);
      zi72 <= \main_outputs_outR6\;
      zi73 <= zi2(5 downto 4);
      zi74 <= zi2(3 downto 2);
      zi75 <= zi2(1 downto 0);
      \instR10\ : \Main_r0\ port map (arg0, \main_r0_outR1\);
      zi76 <= \main_r0_outR1\;
      \instR11\ : \main_getReg\ port map (zi73, zi76, zi75, arg0, main_getreg_out);
      \instR12\ : \Main_r1\ port map (arg0, main_r1_out);
      zi77 <= main_r1_out;
      \instR13\ : \main_getReg\ port map (zi73, zi77, zi75, arg0, \main_getreg_outR1\);
      \instR14\ : \Main_r2\ port map (arg0, main_r2_out);
      zi78 <= main_r2_out;
      \instR15\ : \main_getReg\ port map (zi73, zi78, zi75, arg0, \main_getreg_outR2\);
      \instR16\ : \Main_r3\ port map (arg0, main_r3_out);
      zi79 <= main_r3_out;
      \instR17\ : \main_getReg\ port map (zi73, zi79, zi75, arg0, \main_getreg_outR3\);
      zi80 <= zi2(5 downto 0);
      \instR18\ : \Main_r0\ port map (arg0, \main_r0_outR2\);
      zi81 <= \main_r0_outR2\;
      zi82 <= rw_eq(zi81, std_logic_vector'(B"00000000"));
      \instR19\ : \main_putPC\ port map (zi80, arg0, main_putpc_out);
      \instR20\ : \main_getPC2\ port map (arg0, \main_getpc2_outR1\);
      res <= rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"000")), main_getpc2_out, rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"001")), (zi30 & std_logic_vector'(B"010") & zi29), rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"010")), (zi72 & std_logic_vector'(B"011") & zi71), rw_cond(rw_eq(zi2(8 downto 6), std_logic_vector'(B"011")), rw_cond(rw_eq(zi74, std_logic_vector'(B"00")), main_getreg_out, rw_cond(rw_eq(zi74, std_logic_vector'(B"01")), \main_getreg_outR1\, rw_cond(rw_eq(zi74, std_logic_vector'(B"10")), \main_getreg_outR2\, \main_getreg_outR3\))), rw_cond(rw_not(zi82), main_putpc_out, \main_getpc2_outR1\)))));
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
signal zds7 : std_logic_vector (14 downto 0);
begin
zds7 <= arg0(14 downto 0);
      res <= zds7;
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
signal zds6 : std_logic_vector (16 downto 0);
begin
zds6 <= arg0(31 downto 15);
      res <= zds6;
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
signal zds5 : std_logic_vector (5 downto 0);
begin
zds5 <= arg0(37 downto 32);
      res <= zds5;
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
signal zds4 : std_logic_vector (7 downto 0);
begin
zds4 <= arg0(45 downto 38);
      res <= zds4;
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
signal zds3 : std_logic_vector (7 downto 0);
begin
zds3 <= arg0(53 downto 46);
      res <= zds3;
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
signal zds2 : std_logic_vector (7 downto 0);
begin
zds2 <= arg0(61 downto 54);
      res <= zds2;
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
signal zds1 : std_logic_vector (7 downto 0);
begin
zds1 <= arg0(69 downto 62);
      res <= zds1;
end architecture;