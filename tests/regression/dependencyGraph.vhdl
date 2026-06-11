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
    if unsigned(b) = 0 then return std_logic_vector(to_unsigned(0, n) - 1); end if;
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
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity top_level is
port (clk : in std_logic_vector (0 downto 0);
      rst : in std_logic_vector (0 downto 0);
      \__in0\ : in std_logic_vector (9 downto 0);
      \__out0\ : out std_logic_vector (15 downto 0));
end entity;

architecture rtl of top_level is
signal \__padding\ : std_logic_vector (31 downto 0);
      signal \__st0_next\ : std_logic_vector (29 downto 0);
      signal \__st0\ : std_logic_vector (29 downto 0) := std_logic_vector'(B"000000000000000000000000000000");
      signal rewire_monad_iterst_in : std_logic_vector (39 downto 0);
      signal zll_rewire_monad_iterst_in : std_logic_vector (69 downto 0);
      signal zll_rewire_monad_iterst32_in : std_logic_vector (69 downto 0);
      signal main_loop_in : std_logic_vector (39 downto 0);
      signal zll_main_loop3_in : std_logic_vector (39 downto 0);
      signal zll_main_loop6_in : std_logic_vector (39 downto 0);
      signal zll_main_loop5_in : std_logic_vector (29 downto 0);
      signal id_in : std_logic_vector (29 downto 0);
      signal main_inputtomystate_in : std_logic_vector (24 downto 0);
      signal zll_main_inputtomystate_in : std_logic_vector (49 downto 0);
      signal zll_main_inputtomystate18_in : std_logic_vector (49 downto 0);
      signal zll_main_inputtomystate17_in : std_logic_vector (49 downto 0);
      signal zll_main_inputtomystate9_in : std_logic_vector (49 downto 0);
      signal zll_main_inputtomystate1_in : std_logic_vector (24 downto 0);
      signal zll_main_inputtomystate3_in : std_logic_vector (24 downto 0);
      signal zll_main_inputtomystate12_in : std_logic_vector (13 downto 0);
      signal zll_main_inputtomystate14_in : std_logic_vector (24 downto 0);
      signal zll_main_inputtomystate13_in : std_logic_vector (24 downto 0);
      signal zll_main_inputtomystate8_in : std_logic_vector (12 downto 0);
      signal zll_main_inputtomystate16_in : std_logic_vector (24 downto 0);
      signal zll_main_inputtomystate2_in : std_logic_vector (22 downto 0);
      signal zll_main_inputtomystate10_in : std_logic_vector (24 downto 0);
      signal zll_main_inputtomystate11_in : std_logic_vector (18 downto 0);
      signal zll_main_inputtomystate15_in : std_logic_vector (13 downto 0);
      signal zll_main_incrpipeline5_in : std_logic_vector (44 downto 0);
      signal zll_main_incrpipeline4_in : std_logic_vector (44 downto 0);
      signal \id_inR1\ : std_logic_vector (29 downto 0);
      signal zll_main_incrpipeline8_in : std_logic_vector (29 downto 0);
      signal zll_main_incrpipeline6_in : std_logic_vector (29 downto 0);
      signal \id_inR2\ : std_logic_vector (29 downto 0);
      signal zll_main_incrpipeline10_in : std_logic_vector (29 downto 0);
      signal resize_in : std_logic_vector (29 downto 0);
      signal binop_in : std_logic_vector (255 downto 0);
      signal \resize_inR1\ : std_logic_vector (127 downto 0);
      signal zll_main_loop1_in : std_logic_vector (44 downto 0);
      signal zll_main_loop4_in : std_logic_vector (44 downto 0);
      signal main_mystatetooutput_in : std_logic_vector (14 downto 0);
      signal zll_main_mystatetooutput4_in : std_logic_vector (29 downto 0);
      signal zll_main_mystatetooutput7_in : std_logic_vector (29 downto 0);
      signal zll_main_mystatetooutput1_in : std_logic_vector (14 downto 0);
      signal zll_main_mystatetooutput3_in : std_logic_vector (14 downto 0);
      signal zll_main_mystatetooutput_in : std_logic_vector (8 downto 0);
      signal zll_main_mystatetooutput2_in : std_logic_vector (14 downto 0);
      signal zll_main_mystatetooutput8_in : std_logic_vector (12 downto 0);
      signal zll_main_mystatetooutput5_in : std_logic_vector (14 downto 0);
      signal zll_main_mystatetooutput6_in : std_logic_vector (8 downto 0);
      signal zll_rewire_monad_iterst27_in : std_logic_vector (75 downto 0);
      signal zll_rewire_monad_iterst25_in : std_logic_vector (75 downto 0);
      signal zll_rewire_monad_iterst36_in : std_logic_vector (77 downto 0);
      signal zll_rewire_monad_iterst26_in : std_logic_vector (77 downto 0);
      signal zll_rewire_monad_iterst35_in : std_logic_vector (75 downto 0);
      signal zll_rewire_monad_iterst22_in : std_logic_vector (75 downto 0);
      signal zll_rewire_monad_iterst37_in : std_logic_vector (75 downto 0);
      signal zll_rewire_monad_iterst31_in : std_logic_vector (75 downto 0);
      signal zll_rewire_monad_iterst15_in : std_logic_vector (29 downto 0);
      signal zll_rewire_monad_iterst4_in : std_logic_vector (93 downto 0);
      signal zll_rewire_monad_iterst29_in : std_logic_vector (93 downto 0);
      signal zll_rewire_monad_iterst33_in : std_logic_vector (45 downto 0);
      signal pause : std_logic_vector (77 downto 0);
begin
rewire_monad_iterst_in <= (\__in0\ & \__st0\);
      zll_rewire_monad_iterst_in <= (rewire_monad_iterst_in(39 downto 30) & rewire_monad_iterst_in(29 downto 0) & rewire_monad_iterst_in(29 downto 0));
      zll_rewire_monad_iterst32_in <= (zll_rewire_monad_iterst_in(69 downto 60) & zll_rewire_monad_iterst_in(59 downto 0));
      main_loop_in <= (zll_rewire_monad_iterst32_in(69 downto 60) & zll_rewire_monad_iterst32_in(59 downto 30));
      zll_main_loop3_in <= (main_loop_in(39 downto 30) & main_loop_in(29 downto 0));
      zll_main_loop6_in <= zll_main_loop3_in(39 downto 0);
      zll_main_loop5_in <= zll_main_loop6_in(29 downto 0);
      id_in <= zll_main_loop5_in(29 downto 0);
      main_inputtomystate_in <= (zll_main_loop6_in(39 downto 30) & id_in(29 downto 15));
      zll_main_inputtomystate_in <= (main_inputtomystate_in(14 downto 0) & main_inputtomystate_in(24 downto 15) & main_inputtomystate_in(24 downto 15) & main_inputtomystate_in(14 downto 0));
      zll_main_inputtomystate18_in <= (zll_main_inputtomystate_in(49 downto 35) & zll_main_inputtomystate_in(34 downto 25) & zll_main_inputtomystate_in(34 downto 25) & zll_main_inputtomystate_in(49 downto 35));
      zll_main_inputtomystate17_in <= (zll_main_inputtomystate18_in(49 downto 35) & zll_main_inputtomystate18_in(34 downto 25) & zll_main_inputtomystate18_in(34 downto 25) & zll_main_inputtomystate18_in(49 downto 35));
      zll_main_inputtomystate9_in <= (zll_main_inputtomystate17_in(49 downto 35) & zll_main_inputtomystate17_in(34 downto 25) & zll_main_inputtomystate17_in(34 downto 25) & zll_main_inputtomystate17_in(49 downto 35));
      zll_main_inputtomystate1_in <= (zll_main_inputtomystate9_in(34 downto 25) & zll_main_inputtomystate9_in(49 downto 35));
      zll_main_inputtomystate3_in <= zll_main_inputtomystate1_in(24 downto 0);
      zll_main_inputtomystate12_in <= (zll_main_inputtomystate3_in(13 downto 9) & zll_main_inputtomystate3_in(8 downto 0));
      zll_main_inputtomystate14_in <= zll_main_inputtomystate9_in(24 downto 0);
      zll_main_inputtomystate13_in <= zll_main_inputtomystate17_in(24 downto 0);
      zll_main_inputtomystate8_in <= (zll_main_inputtomystate13_in(12 downto 9) & zll_main_inputtomystate13_in(8 downto 0));
      zll_main_inputtomystate16_in <= zll_main_inputtomystate18_in(24 downto 0);
      zll_main_inputtomystate2_in <= (zll_main_inputtomystate16_in(22 downto 15) & zll_main_inputtomystate16_in(14 downto 14) & zll_main_inputtomystate16_in(13 downto 9) & zll_main_inputtomystate16_in(8 downto 0));
      zll_main_inputtomystate10_in <= zll_main_inputtomystate_in(24 downto 0);
      zll_main_inputtomystate11_in <= (zll_main_inputtomystate10_in(18 downto 15) & zll_main_inputtomystate10_in(14 downto 14) & zll_main_inputtomystate10_in(13 downto 9) & zll_main_inputtomystate10_in(8 downto 0));
      zll_main_inputtomystate15_in <= (zll_main_inputtomystate11_in(18 downto 15) & zll_main_inputtomystate11_in(14 downto 14) & zll_main_inputtomystate11_in(8 downto 0));
      zll_main_incrpipeline5_in <= (rw_cond(rw_eq(zll_main_inputtomystate10_in(24 downto 23), std_logic_vector'(B"00")), (zll_main_inputtomystate15_in(9 downto 9) & std_logic_vector'(B"1") & zll_main_inputtomystate15_in(13 downto 10) & std_logic_vector'(B"00000") & zll_main_inputtomystate15_in(13 downto 10)), rw_cond(rw_eq(zll_main_inputtomystate16_in(24 downto 23), std_logic_vector'(B"01")), (zll_main_inputtomystate2_in(14 downto 14) & zll_main_inputtomystate2_in(13 downto 9) & std_logic_vector'(B"1") & zll_main_inputtomystate2_in(22 downto 15)), rw_cond(rw_land(rw_eq(zll_main_inputtomystate13_in(24 downto 23), std_logic_vector'(B"10")), rw_land(rw_eq(zll_main_inputtomystate13_in(15 downto 15), std_logic_vector'(B"1")), rw_eq(zll_main_inputtomystate13_in(13 downto 13), std_logic_vector'(B"1")))), (std_logic_vector'(B"10000000000") & zll_main_inputtomystate8_in(12 downto 9)), rw_cond(rw_land(rw_eq(zll_main_inputtomystate14_in(24 downto 23), std_logic_vector'(B"10")), rw_land(rw_eq(zll_main_inputtomystate14_in(15 downto 15), std_logic_vector'(B"1")), rw_eq(zll_main_inputtomystate14_in(13 downto 13), std_logic_vector'(B"0")))), (std_logic_vector'(B"100000") & zll_main_inputtomystate14_in(8 downto 0)), (std_logic_vector'(B"000000") & zll_main_inputtomystate12_in(8 downto 0)))))) & zll_main_loop6_in(29 downto 0));
      zll_main_incrpipeline4_in <= zll_main_incrpipeline5_in(44 downto 0);
      \id_inR1\ <= zll_main_incrpipeline4_in(29 downto 0);
      zll_main_incrpipeline8_in <= (zll_main_incrpipeline4_in(44 downto 30) & \id_inR1\(29 downto 15));
      zll_main_incrpipeline6_in <= (zll_main_incrpipeline8_in(29 downto 15) & zll_main_incrpipeline8_in(14 downto 0));
      \id_inR2\ <= zll_main_incrpipeline6_in(29 downto 0);
      zll_main_incrpipeline10_in <= zll_main_incrpipeline4_in(29 downto 0);
      resize_in <= zll_main_incrpipeline10_in(29 downto 0);
      binop_in <= (rw_resize(resize_in(29 downto 0), 128) & rw_repl(128, std_logic_vector'(B"0")));
      \resize_inR1\ <= rw_shiftr(binop_in(255 downto 128), binop_in(127 downto 0));
      zll_main_loop1_in <= ((\id_inR2\(29 downto 15) & \id_inR2\(14 downto 0)) & rw_resize(\resize_inR1\(127 downto 0), 15));
      zll_main_loop4_in <= zll_main_loop1_in(44 downto 0);
      main_mystatetooutput_in <= zll_main_loop4_in(14 downto 0);
      zll_main_mystatetooutput4_in <= (main_mystatetooutput_in(14 downto 0) & main_mystatetooutput_in(14 downto 0));
      zll_main_mystatetooutput7_in <= (zll_main_mystatetooutput4_in(29 downto 15) & zll_main_mystatetooutput4_in(29 downto 15));
      zll_main_mystatetooutput1_in <= zll_main_mystatetooutput7_in(29 downto 15);
      zll_main_mystatetooutput3_in <= zll_main_mystatetooutput1_in(14 downto 0);
      zll_main_mystatetooutput_in <= zll_main_mystatetooutput3_in(8 downto 0);
      zll_main_mystatetooutput2_in <= zll_main_mystatetooutput7_in(14 downto 0);
      zll_main_mystatetooutput8_in <= (zll_main_mystatetooutput2_in(13 downto 9) & zll_main_mystatetooutput2_in(7 downto 0));
      zll_main_mystatetooutput5_in <= zll_main_mystatetooutput4_in(14 downto 0);
      zll_main_mystatetooutput6_in <= (zll_main_mystatetooutput5_in(13 downto 9) & zll_main_mystatetooutput5_in(3 downto 0));
      zll_rewire_monad_iterst27_in <= ((rw_cond(rw_land(rw_eq(zll_main_mystatetooutput5_in(14 downto 14), std_logic_vector'(B"1")), rw_eq(zll_main_mystatetooutput5_in(8 downto 8), std_logic_vector'(B"0"))), (std_logic_vector'(B"11") & zll_main_mystatetooutput6_in(8 downto 4) & std_logic_vector'(B"00000") & zll_main_mystatetooutput6_in(3 downto 0)), rw_cond(rw_land(rw_eq(zll_main_mystatetooutput2_in(14 downto 14), std_logic_vector'(B"1")), rw_eq(zll_main_mystatetooutput2_in(8 downto 8), std_logic_vector'(B"1"))), (std_logic_vector'(B"01") & zll_main_mystatetooutput8_in(12 downto 8) & std_logic_vector'(B"1") & zll_main_mystatetooutput8_in(7 downto 0)), (std_logic_vector'(B"0000000") & zll_main_mystatetooutput_in(8 downto 0)))) & zll_main_loop4_in(44 downto 15)) & zll_rewire_monad_iterst32_in(29 downto 0));
      zll_rewire_monad_iterst25_in <= zll_rewire_monad_iterst27_in(75 downto 0);
      zll_rewire_monad_iterst36_in <= (std_logic_vector'(B"00") & zll_rewire_monad_iterst25_in(75 downto 30) & zll_rewire_monad_iterst25_in(29 downto 0));
      zll_rewire_monad_iterst26_in <= zll_rewire_monad_iterst36_in(77 downto 0);
      zll_rewire_monad_iterst35_in <= (zll_rewire_monad_iterst26_in(75 downto 30) & zll_rewire_monad_iterst26_in(29 downto 0));
      zll_rewire_monad_iterst22_in <= (zll_rewire_monad_iterst35_in(29 downto 0) & zll_rewire_monad_iterst35_in(75 downto 30));
      zll_rewire_monad_iterst37_in <= (zll_rewire_monad_iterst22_in(45 downto 30) & zll_rewire_monad_iterst22_in(75 downto 46) & zll_rewire_monad_iterst22_in(29 downto 0));
      zll_rewire_monad_iterst31_in <= (zll_rewire_monad_iterst37_in(75 downto 60) & zll_rewire_monad_iterst37_in(29 downto 0) & zll_rewire_monad_iterst37_in(59 downto 30));
      zll_rewire_monad_iterst15_in <= zll_rewire_monad_iterst31_in(59 downto 30);
      zll_rewire_monad_iterst4_in <= (zll_rewire_monad_iterst31_in(75 downto 60) & ((std_logic_vector'(B"01") & rw_repl(46, std_logic_vector'(B"0"))) & zll_rewire_monad_iterst15_in(29 downto 0)));
      zll_rewire_monad_iterst29_in <= (zll_rewire_monad_iterst4_in(93 downto 78) & zll_rewire_monad_iterst4_in(77 downto 0));
      zll_rewire_monad_iterst33_in <= (zll_rewire_monad_iterst29_in(93 downto 78) & zll_rewire_monad_iterst29_in(29 downto 0));
      pause <= ((std_logic_vector'(B"1") & rw_repl(31, std_logic_vector'(B"0"))) & zll_rewire_monad_iterst33_in(45 downto 30) & zll_rewire_monad_iterst33_in(29 downto 0));
      \__padding\ <= pause(77 downto 46);
      \__out0\ <= pause(45 downto 30);
      \__st0_next\ <= pause(29 downto 0);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;