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
      \__in0\ : in std_logic_vector (7 downto 0);
      \__in1\ : in std_logic_vector (0 downto 0);
      \__in2\ : in std_logic_vector (0 downto 0);
      \__out0\ : out std_logic_vector (7 downto 0);
      \__out1\ : out std_logic_vector (7 downto 0);
      \__out2\ : out std_logic_vector (0 downto 0);
      \__out3\ : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of top_level is
component \Main_dataIn\ is
      port (arg0 : in std_logic_vector (9 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_inputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (9 downto 0));
      end component;
      component \Main_mkReg\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_setAddrOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setInputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (9 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setOutputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setWeOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \ZLL_L_s44\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (0 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_Main_loop3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL___unused14\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_d36\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_i139\ is
      port (arg0 : in std_logic_vector (9 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110000");
      signal \__resumption_tag_next\ : std_logic_vector (6 downto 0);
      signal \__st0\ : std_logic_vector (80 downto 0) := std_logic_vector'(B"000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (80 downto 0);
      signal zi1 : std_logic_vector (9 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (1 downto 0);
      signal main_setinputs_out : std_logic_vector (80 downto 0);
      signal zi5 : std_logic_vector (80 downto 0);
      signal zl_main_loop3_out : std_logic_vector (105 downto 0);
      signal main_inputs_out : std_logic_vector (9 downto 0);
      signal zi6 : std_logic_vector (9 downto 0);
      signal main_datain_out : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR1\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR2\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR3\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR4\ : std_logic_vector (105 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal zi10 : std_logic_vector (1 downto 0);
      signal \main_setinputs_outR1\ : std_logic_vector (80 downto 0);
      signal zi12 : std_logic_vector (80 downto 0);
      signal \main_inputs_outR1\ : std_logic_vector (9 downto 0);
      signal zi13 : std_logic_vector (9 downto 0);
      signal \main_datain_outR1\ : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi15 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal zi16 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal zi17 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (17 downto 0);
      signal zi18 : std_logic_vector (17 downto 0);
      signal main_setweout_out : std_logic_vector (17 downto 0);
      signal zi19 : std_logic_vector (17 downto 0);
      signal \main_setoutputs_outR1\ : std_logic_vector (80 downto 0);
      signal zi20 : std_logic_vector (80 downto 0);
      signal \zl__unused14_out\ : std_logic_vector (105 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal zl_d36_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal \zl_d36_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zll_l_s44_out : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zll_l_s44_outR1\ : std_logic_vector (105 downto 0);
      signal zi23 : std_logic_vector (0 downto 0);
      signal zi24 : std_logic_vector (0 downto 0);
      signal \main_setinputs_outR2\ : std_logic_vector (80 downto 0);
      signal zi26 : std_logic_vector (80 downto 0);
      signal \main_inputs_outR2\ : std_logic_vector (9 downto 0);
      signal zi27 : std_logic_vector (9 downto 0);
      signal \main_datain_outR2\ : std_logic_vector (7 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi29 : std_logic_vector (1 downto 0);
      signal \main_setr0_outR1\ : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR5\ : std_logic_vector (105 downto 0);
      signal \main_setr1_outR1\ : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR6\ : std_logic_vector (105 downto 0);
      signal \main_setr2_outR1\ : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR7\ : std_logic_vector (105 downto 0);
      signal \main_setr3_outR1\ : std_logic_vector (80 downto 0);
      signal \zl_main_loop3_outR8\ : std_logic_vector (105 downto 0);
      signal zl_i139_out : std_logic_vector (105 downto 0);
      signal \zl_i139_outR1\ : std_logic_vector (105 downto 0);
      signal zres : std_logic_vector (105 downto 0);
begin
zi1 <= (\__in0\ & \__in1\ & \__in2\);
      zi2 <= \__resumption_tag\(2 downto 2);
      zi3 <= \__resumption_tag\(1 downto 0);
      inst : \Main_setInputs\ port map (\__st0\, zi1, main_setinputs_out);
      zi5 <= main_setinputs_out;
      \instR1\ : \ZL_Main_loop3\ port map (zi5, zl_main_loop3_out);
      \instR2\ : \Main_inputs\ port map (zi5, main_inputs_out);
      zi6 <= main_inputs_out;
      \instR3\ : \Main_dataIn\ port map (zi6, main_datain_out);
      zi7 <= main_datain_out;
      \instR4\ : \Main_setR0\ port map (zi5, zi7, main_setr0_out);
      \instR5\ : \ZL_Main_loop3\ port map (main_setr0_out, \zl_main_loop3_outR1\);
      \instR6\ : \Main_setR1\ port map (zi5, zi7, main_setr1_out);
      \instR7\ : \ZL_Main_loop3\ port map (main_setr1_out, \zl_main_loop3_outR2\);
      \instR8\ : \Main_setR2\ port map (zi5, zi7, main_setr2_out);
      \instR9\ : \ZL_Main_loop3\ port map (main_setr2_out, \zl_main_loop3_outR3\);
      \instR10\ : \Main_setR3\ port map (zi5, zi7, main_setr3_out);
      \instR11\ : \ZL_Main_loop3\ port map (main_setr3_out, \zl_main_loop3_outR4\);
      zi8 <= \__resumption_tag\(3 downto 3);
      zi9 <= \__resumption_tag\(2 downto 2);
      zi10 <= \__resumption_tag\(1 downto 0);
      \instR12\ : \Main_setInputs\ port map (\__st0\, zi1, \main_setinputs_outR1\);
      zi12 <= \main_setinputs_outR1\;
      \instR13\ : \Main_inputs\ port map (zi12, \main_inputs_outR1\);
      zi13 <= \main_inputs_outR1\;
      \instR14\ : \Main_dataIn\ port map (zi13, \main_datain_outR1\);
      zi14 <= \main_datain_outR1\;
      \instR15\ : \Main_outputs\ port map (zi12, main_outputs_out);
      zi15 <= main_outputs_out;
      \instR16\ : \Main_setAddrOut\ port map (zi15, zi14, main_setaddrout_out);
      zi16 <= main_setaddrout_out;
      \instR17\ : \Main_setOutputs\ port map (zi12, zi16, main_setoutputs_out);
      zi17 <= main_setoutputs_out;
      \instR18\ : \Main_outputs\ port map (zi17, \main_outputs_outR1\);
      zi18 <= \main_outputs_outR1\;
      \instR19\ : \Main_setWeOut\ port map (zi18, zi9, main_setweout_out);
      zi19 <= main_setweout_out;
      \instR20\ : \Main_setOutputs\ port map (zi17, zi19, \main_setoutputs_outR1\);
      zi20 <= \main_setoutputs_outR1\;
      \instR21\ : \ZL___unused14\ port map (zi8, zi10, zi20, \zl__unused14_out\);
      \instR22\ : \Main_r0\ port map (zi20, main_r0_out);
      zi21 <= main_r0_out;
      \instR23\ : \ZL_d36\ port map (zi8, zi10, zi21, zi20, zl_d36_out);
      \instR24\ : \Main_r1\ port map (zi20, main_r1_out);
      zi22 <= main_r1_out;
      \instR25\ : \ZL_d36\ port map (zi8, zi10, zi22, zi20, \zl_d36_outR1\);
      \instR26\ : \Main_r2\ port map (zi20, main_r2_out);
      \instR27\ : \ZLL_L_s44\ port map (zi10, zi20, zi8, main_r2_out, zll_l_s44_out);
      \instR28\ : \Main_r3\ port map (zi20, main_r3_out);
      \instR29\ : \ZLL_L_s44\ port map (zi10, zi20, zi8, main_r3_out, \zll_l_s44_outR1\);
      zi23 <= \__resumption_tag\(1 downto 1);
      zi24 <= \__resumption_tag\(0 downto 0);
      \instR30\ : \Main_setInputs\ port map (\__st0\, zi1, \main_setinputs_outR2\);
      zi26 <= \main_setinputs_outR2\;
      \instR31\ : \Main_inputs\ port map (zi26, \main_inputs_outR2\);
      zi27 <= \main_inputs_outR2\;
      \instR32\ : \Main_dataIn\ port map (zi27, \main_datain_outR2\);
      zi28 <= \main_datain_outR2\;
      \instR33\ : \Main_mkReg\ port map (zi23, zi24, main_mkreg_out);
      zi29 <= main_mkreg_out;
      \instR34\ : \Main_setR0\ port map (zi26, zi28, \main_setr0_outR1\);
      \instR35\ : \ZL_Main_loop3\ port map (\main_setr0_outR1\, \zl_main_loop3_outR5\);
      \instR36\ : \Main_setR1\ port map (zi26, zi28, \main_setr1_outR1\);
      \instR37\ : \ZL_Main_loop3\ port map (\main_setr1_outR1\, \zl_main_loop3_outR6\);
      \instR38\ : \Main_setR2\ port map (zi26, zi28, \main_setr2_outR1\);
      \instR39\ : \ZL_Main_loop3\ port map (\main_setr2_outR1\, \zl_main_loop3_outR7\);
      \instR40\ : \Main_setR3\ port map (zi26, zi28, \main_setr3_outR1\);
      \instR41\ : \ZL_Main_loop3\ port map (\main_setr3_outR1\, \zl_main_loop3_outR8\);
      \instR42\ : \ZL_i139\ port map (zi1, \__st0\, zl_i139_out);
      \instR43\ : \ZL_i139\ port map (zi1, \__st0\, \zl_i139_outR1\);
      zres <= rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"000")), rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), zl_main_loop3_out, rw_cond(rw_eq(zi3, std_logic_vector'(B"00")), \zl_main_loop3_outR1\, rw_cond(rw_eq(zi3, std_logic_vector'(B"01")), \zl_main_loop3_outR2\, rw_cond(rw_eq(zi3, std_logic_vector'(B"10")), \zl_main_loop3_outR3\, \zl_main_loop3_outR4\)))), rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"001")), rw_cond(rw_eq(zi9, std_logic_vector'(B"0")), \zl__unused14_out\, rw_cond(rw_eq(zi10, std_logic_vector'(B"00")), zl_d36_out, rw_cond(rw_eq(zi10, std_logic_vector'(B"01")), \zl_d36_outR1\, rw_cond(rw_eq(zi10, std_logic_vector'(B"10")), zll_l_s44_out, \zll_l_s44_outR1\)))), rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"010")), rw_cond(rw_eq(zi29, std_logic_vector'(B"00")), \zl_main_loop3_outR5\, rw_cond(rw_eq(zi29, std_logic_vector'(B"01")), \zl_main_loop3_outR6\, rw_cond(rw_eq(zi29, std_logic_vector'(B"10")), \zl_main_loop3_outR7\, \zl_main_loop3_outR8\))), rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"011")), zl_i139_out, \zl_i139_outR1\))));
      \__resumption_tag_next\ <= zres(87 downto 81);
      \__st0_next\ <= zres(80 downto 0);
      \__out0\ <= zres(105 downto 98);
      \__out1\ <= zres(97 downto 90);
      \__out2\ <= zres(89 downto 89);
      \__out3\ <= zres(88 downto 88);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"0110000");
                  \__st0\ <= std_logic_vector'(B"000000000000000000000000000000000000000000000000000000000000000000000000000000000");
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
entity \Main_setOutputs\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (17 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setOutputs\ is
signal i : std_logic_vector (9 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (i & arg1 & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setDataOut\ is
port (arg0 : in std_logic_vector (17 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_setDataOut\ is
signal a_o : std_logic_vector (7 downto 0);
      signal we_o : std_logic_vector (0 downto 0);
      signal iack_o : std_logic_vector (0 downto 0);
begin
a_o <= arg0(17 downto 10);
      we_o <= arg0(1 downto 1);
      iack_o <= arg0(0 downto 0);
      res <= (a_o & arg1 & we_o & iack_o);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS135\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS135\ is
component \ZLL_L_cin240\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_Main_plusCW8$s2\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      signal conn : std_logic_vector (8 downto 0);
      signal \zll_main_pluscw8$s2_out\ : std_logic_vector (8 downto 0);
      signal zll_l_cin240_out : std_logic_vector (105 downto 0);
begin
conn <= rw_add(rw_add(rw_resize(arg1, 9), rw_resize(arg2, 9)), std_logic_vector'(B"000000000"));
      inst : \ZLL_Main_plusCW8$s2\ port map (conn, \zll_main_pluscw8$s2_out\);
      \instR1\ : \ZLL_L_cin240\ port map (arg0, arg3, \zll_main_pluscw8$s2_out\, zll_l_cin240_out);
      res <= zll_l_cin240_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s446\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s446\ is
component \Main_setPC\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setPC\ port map (arg0, arg1, main_setpc_out);
      \instR1\ : \ZL___unused103\ port map (main_setpc_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s664\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s664\ is
component \ZL_v650\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_v650_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_v650\ port map (arg0, arg2, arg1, zl_v650_out);
      res <= zl_v650_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm315\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm315\ is
component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused299\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \zl__unused299_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR3\ port map (arg1, arg0, main_setr3_out);
      \instR1\ : \ZL___unused299\ port map (arg0, main_setr3_out, \zl__unused299_out\);
      res <= \zl__unused299_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_v69512\ is
port (arg0 : in std_logic_vector (8 downto 0);
      res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \ZLL_L_v69512\ is

begin
res <= arg0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setAddrOut\ is
port (arg0 : in std_logic_vector (17 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_setAddrOut\ is
signal d_o : std_logic_vector (7 downto 0);
      signal we_o : std_logic_vector (0 downto 0);
      signal iack_o : std_logic_vector (0 downto 0);
begin
d_o <= arg0(9 downto 2);
      we_o <= arg0(1 downto 1);
      iack_o <= arg0(0 downto 0);
      res <= (arg1 & d_o & we_o & iack_o);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_vS205\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (8 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_vS205\ is
component \ZL_s150\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (8 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_s150_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_s150\ port map (arg1, arg2, arg0, arg0, zl_s150_out);
      res <= zl_s150_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s44\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      arg2 : in std_logic_vector (0 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s44\ is
component \ZL_d36\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_d36_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_d36\ port map (arg2, arg0, arg3, arg1, zl_d36_out);
      res <= zl_d36_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s616\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s616\ is
component \Main_plusCW8$s1\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component \ZL_arm607\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm609\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm611\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm613\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_pluscw8$s1_out\ : std_logic_vector (8 downto 0);
      signal zi0 : std_logic_vector (8 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zl_arm607_out : std_logic_vector (105 downto 0);
      signal zl_arm609_out : std_logic_vector (105 downto 0);
      signal zl_arm611_out : std_logic_vector (105 downto 0);
      signal zl_arm613_out : std_logic_vector (105 downto 0);
begin
inst : \Main_plusCW8$s1\ port map (arg2, \main_pluscw8$s1_out\);
      zi0 <= \main_pluscw8$s1_out\;
      zi1 <= zi0(7 downto 0);
      zi2 <= zi1;
      \instR1\ : \ZL_arm607\ port map (zi0, zi2, arg1, zl_arm607_out);
      \instR2\ : \ZL_arm609\ port map (zi0, zi2, arg1, zl_arm609_out);
      \instR3\ : \ZL_arm611\ port map (zi0, zi2, arg1, zl_arm611_out);
      \instR4\ : \ZL_arm613\ port map (zi0, zi2, arg1, zl_arm613_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), zl_arm607_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), zl_arm609_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), zl_arm611_out, zl_arm613_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR0\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setR0\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & arg1 & r1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_plusCW8$s2\ is
port (arg0 : in std_logic_vector (8 downto 0);
      res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \ZLL_Main_plusCW8$s2\ is

begin
res <= (arg0(8 downto 8) & rw_resize(arg0, 8));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD369\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD369\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZL_vS370\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zl_vs370_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zl_vs370_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zl_vs370_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zl_vs370_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZL_vS370\ port map (arg0, arg2, main_r0_out, arg3, zl_vs370_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \ZL_vS370\ port map (arg0, arg2, main_r1_out, arg3, \zl_vs370_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \ZL_vS370\ port map (arg0, arg2, main_r2_out, arg3, \zl_vs370_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZL_vS370\ port map (arg0, arg2, main_r3_out, arg3, \zl_vs370_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_vs370_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \zl_vs370_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \zl_vs370_outR2\, \zl_vs370_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s427\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (0 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s427\ is
component \ZL_vD405\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_vd405_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_vD405\ port map (arg2, arg1, arg3, arg0, zl_vd405_out);
      res <= zl_vd405_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s643\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s643\ is
component \ZL_v623\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_v623_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_v623\ port map (arg1, arg2, arg0, zl_v623_out);
      res <= zl_v623_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD168\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD168\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZL_vS169\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zl_vs169_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zl_vs169_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zl_vs169_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zl_vs169_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZL_vS169\ port map (arg0, arg2, main_r0_out, arg3, zl_vs169_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \ZL_vS169\ port map (arg0, arg2, main_r1_out, arg3, \zl_vs169_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \ZL_vS169\ port map (arg0, arg2, main_r2_out, arg3, \zl_vs169_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZL_vS169\ port map (arg0, arg2, main_r3_out, arg3, \zl_vs169_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_vs169_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \zl_vs169_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \zl_vs169_outR2\, \zl_vs169_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_v6721\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_v6721\ is
component \ZL_arm566\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm568\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm570\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm572\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_arm566_out : std_logic_vector (105 downto 0);
      signal zl_arm568_out : std_logic_vector (105 downto 0);
      signal zl_arm570_out : std_logic_vector (105 downto 0);
      signal zl_arm572_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_arm566\ port map (arg2, arg1, zl_arm566_out);
      \instR1\ : \ZL_arm568\ port map (arg2, arg1, zl_arm568_out);
      \instR2\ : \ZL_arm570\ port map (arg2, arg1, zl_arm570_out);
      \instR3\ : \ZL_arm572\ port map (arg2, arg1, zl_arm572_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), zl_arm566_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), zl_arm568_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), zl_arm570_out, zl_arm572_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s577\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s577\ is
component \ZL_v559\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_v559_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_v559\ port map (arg1, arg2, arg0, zl_v559_out);
      res <= zl_v559_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s421\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s421\ is
component \ZL_vS406\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_vs406_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_vS406\ port map (arg1, arg2, arg0, zl_vs406_out);
      res <= zl_vs406_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_d36\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_d36\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setDataOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setOutputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused14\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi0 : std_logic_vector (17 downto 0);
      signal main_setdataout_out : std_logic_vector (17 downto 0);
      signal zi1 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal \zl__unused14_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_outputs\ port map (arg3, main_outputs_out);
      zi0 <= main_outputs_out;
      \instR1\ : \Main_setDataOut\ port map (zi0, arg2, main_setdataout_out);
      zi1 <= main_setdataout_out;
      \instR2\ : \Main_setOutputs\ port map (arg3, zi1, main_setoutputs_out);
      \instR3\ : \ZL___unused14\ port map (arg0, arg1, main_setoutputs_out, \zl__unused14_out\);
      res <= \zl__unused14_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS334\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS334\ is
component \ZL_arm309\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm311\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm313\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm315\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zl_arm309_out : std_logic_vector (105 downto 0);
      signal zl_arm311_out : std_logic_vector (105 downto 0);
      signal zl_arm313_out : std_logic_vector (105 downto 0);
      signal zl_arm315_out : std_logic_vector (105 downto 0);
begin
zi0 <= rw_and(arg1, arg2);
      inst : \ZL_arm309\ port map (zi0, arg3, zl_arm309_out);
      \instR1\ : \ZL_arm311\ port map (zi0, arg3, zl_arm311_out);
      \instR2\ : \ZL_arm313\ port map (zi0, arg3, zl_arm313_out);
      \instR3\ : \ZL_arm315\ port map (zi0, arg3, zl_arm315_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), zl_arm309_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), zl_arm311_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), zl_arm313_out, zl_arm315_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm566\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm566\ is
component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR0\ port map (arg1, arg0, main_setr0_out);
      \instR1\ : \ZL___unused103\ port map (main_setr0_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setIEFlag\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setIEFlag\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (i & o & z & c & arg1 & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s515\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s515\ is
component \Main_setPC\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setPC\ port map (arg0, arg1, main_setpc_out);
      \instR1\ : \ZL___unused103\ port map (main_setpc_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setCFlag\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setCFlag\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (i & o & z & arg1 & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s90\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      arg2 : in std_logic_vector (0 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s90\ is
component \ZL_a65\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_a65_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_a65\ port map (arg2, arg0, arg3, arg1, zl_a65_out);
      res <= zl_a65_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s688\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s688\ is
component \ZL_v672\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_v672_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_v672\ port map (arg0, arg2, arg1, zl_v672_out);
      res <= zl_v672_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm311\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm311\ is
component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused299\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \zl__unused299_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR1\ port map (arg1, arg0, main_setr1_out);
      \instR1\ : \ZL___unused299\ port map (arg0, main_setr1_out, \zl__unused299_out\);
      res <= \zl__unused299_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_Main_loop3\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_Main_loop3\ is
component \Main_cFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_inputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (9 downto 0));
      end component;
      component \Main_pc\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_setCFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setIEFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setOutputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_zFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_fail7\ is
      port (arg0 : in std_logic_vector (9 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_inputs_out : std_logic_vector (9 downto 0);
      signal zi0 : std_logic_vector (9 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal main_setieflag_out : std_logic_vector (80 downto 0);
      signal zi7 : std_logic_vector (80 downto 0);
      signal main_pc_out : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal main_zflag_out : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal zi10 : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (9 downto 0);
      signal zi12 : std_logic_vector (17 downto 0);
      signal zi13 : std_logic_vector (0 downto 0);
      signal zi14 : std_logic_vector (0 downto 0);
      signal zi15 : std_logic_vector (0 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (0 downto 0);
      signal zi18 : std_logic_vector (0 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal zi23 : std_logic_vector (80 downto 0);
      signal zi24 : std_logic_vector (9 downto 0);
      signal zi25 : std_logic_vector (17 downto 0);
      signal zi26 : std_logic_vector (0 downto 0);
      signal zi27 : std_logic_vector (0 downto 0);
      signal zi28 : std_logic_vector (0 downto 0);
      signal zi29 : std_logic_vector (7 downto 0);
      signal zi30 : std_logic_vector (0 downto 0);
      signal zi31 : std_logic_vector (7 downto 0);
      signal zi32 : std_logic_vector (7 downto 0);
      signal zi33 : std_logic_vector (7 downto 0);
      signal zi34 : std_logic_vector (7 downto 0);
      signal zi35 : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (80 downto 0);
      signal zi37 : std_logic_vector (9 downto 0);
      signal zi38 : std_logic_vector (17 downto 0);
      signal zi39 : std_logic_vector (0 downto 0);
      signal zi40 : std_logic_vector (0 downto 0);
      signal zi41 : std_logic_vector (0 downto 0);
      signal zi42 : std_logic_vector (7 downto 0);
      signal zi43 : std_logic_vector (0 downto 0);
      signal zi44 : std_logic_vector (7 downto 0);
      signal zi45 : std_logic_vector (7 downto 0);
      signal zi46 : std_logic_vector (7 downto 0);
      signal zi47 : std_logic_vector (7 downto 0);
      signal zi48 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
      signal zl_fail7_out : std_logic_vector (105 downto 0);
      signal \zl_fail7_outR1\ : std_logic_vector (105 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi49 : std_logic_vector (80 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal zi50 : std_logic_vector (80 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR1\ : std_logic_vector (105 downto 0);
begin
inst : \Main_inputs\ port map (arg0, main_inputs_out);
      zi0 <= main_inputs_out;
      zi1 <= zi0(1 downto 1);
      zi2 <= zi1;
      zi3 <= arg0(50 downto 50);
      zi4 <= zi3;
      zi5 <= zi0(0 downto 0);
      zi6 <= zi5;
      \instR1\ : \Main_setIEFlag\ port map (arg0, std_logic_vector'(B"0"), main_setieflag_out);
      zi7 <= main_setieflag_out;
      \instR2\ : \Main_pc\ port map (zi7, main_pc_out);
      zi8 <= main_pc_out;
      \instR3\ : \Main_zFlag\ port map (zi7, main_zflag_out);
      zi9 <= main_zflag_out;
      \instR4\ : \Main_cFlag\ port map (zi7, main_cflag_out);
      zi10 <= main_cflag_out;
      zi11 <= zi7(80 downto 71);
      zi12 <= zi7(70 downto 53);
      zi13 <= zi7(52 downto 52);
      zi14 <= zi7(51 downto 51);
      zi15 <= zi7(50 downto 50);
      zi16 <= zi7(49 downto 42);
      zi17 <= zi7(41 downto 41);
      zi18 <= zi7(40 downto 40);
      zi19 <= zi7(31 downto 24);
      zi20 <= zi7(23 downto 16);
      zi21 <= zi7(15 downto 8);
      zi22 <= zi7(7 downto 0);
      zi23 <= (zi11 & zi12 & zi13 & zi14 & zi15 & zi16 & zi17 & zi18 & zi8 & zi19 & zi20 & zi21 & zi22);
      zi24 <= zi23(80 downto 71);
      zi25 <= zi23(70 downto 53);
      zi26 <= zi23(52 downto 52);
      zi27 <= zi23(51 downto 51);
      zi28 <= zi23(50 downto 50);
      zi29 <= zi23(49 downto 42);
      zi30 <= zi23(40 downto 40);
      zi31 <= zi23(39 downto 32);
      zi32 <= zi23(31 downto 24);
      zi33 <= zi23(23 downto 16);
      zi34 <= zi23(15 downto 8);
      zi35 <= zi23(7 downto 0);
      zi36 <= (zi24 & zi25 & zi26 & zi27 & zi28 & zi29 & zi9 & zi30 & zi31 & zi32 & zi33 & zi34 & zi35);
      zi37 <= zi36(80 downto 71);
      zi38 <= zi36(70 downto 53);
      zi39 <= zi36(52 downto 52);
      zi40 <= zi36(51 downto 51);
      zi41 <= zi36(50 downto 50);
      zi42 <= zi36(49 downto 42);
      zi43 <= zi36(41 downto 41);
      zi44 <= zi36(39 downto 32);
      zi45 <= zi36(31 downto 24);
      zi46 <= zi36(23 downto 16);
      zi47 <= zi36(15 downto 8);
      zi48 <= zi36(7 downto 0);
      conn <= (zi37 & zi38 & zi39 & zi40 & zi41 & zi42 & zi43 & zi10 & zi44 & zi45 & zi46 & zi47 & zi48);
      \instR5\ : \ZL___unused103\ port map (conn, \zl__unused103_out\);
      \instR6\ : \ZL_fail7\ port map (zi0, arg0, zl_fail7_out);
      \instR7\ : \ZL_fail7\ port map (zi0, arg0, \zl_fail7_outR1\);
      \instR8\ : \Main_setCFlag\ port map (arg0, std_logic_vector'(B"0"), main_setcflag_out);
      zi49 <= main_setcflag_out;
      \instR9\ : \Main_setZFlag\ port map (zi49, std_logic_vector'(B"0"), main_setzflag_out);
      zi50 <= main_setzflag_out;
      \instR10\ : \Main_setOutputs\ port map (zi50, std_logic_vector'(B"000000000000000000"), main_setoutputs_out);
      \instR11\ : \ZL___unused103\ port map (main_setoutputs_out, \zl__unused103_outR1\);
      res <= rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), rw_cond(rw_eq(zi4, std_logic_vector'(B"1")), rw_cond(rw_eq(zi6, std_logic_vector'(B"1")), \zl__unused103_out\, zl_fail7_out), \zl_fail7_outR1\), \zl__unused103_outR1\);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setZFlag\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setZFlag\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (i & o & arg1 & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm568\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm568\ is
component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR1\ port map (arg1, arg0, main_setr1_out);
      \instR1\ : \ZL___unused103\ port map (main_setr1_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm309\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm309\ is
component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused299\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \zl__unused299_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR0\ port map (arg1, arg0, main_setr0_out);
      \instR1\ : \ZL___unused299\ port map (arg0, main_setr0_out, \zl__unused299_out\);
      res <= \zl__unused299_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_msbW8\ is
port (arg0 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_msbW8\ is

begin
res <= arg0(7 downto 7);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s692\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s692\ is
component \ZL_v672\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_v672_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_v672\ port map (arg1, arg2, arg0, zl_v672_out);
      res <= zl_v672_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_pc\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_pc\ is
signal pc : std_logic_vector (7 downto 0);
begin
pc <= arg0(49 downto 42);
      res <= pc;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_v695\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (1 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      arg4 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_v695\ is
component \Main_lsbW8\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_msbW8\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_L_v69512\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component \ZL_arm607\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm609\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm611\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm613\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_msbw8_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal \main_msbw8_outR1\ : std_logic_vector (0 downto 0);
      signal conn : std_logic_vector (8 downto 0);
      signal zll_l_v69512_out : std_logic_vector (8 downto 0);
      signal \main_msbw8_outR2\ : std_logic_vector (0 downto 0);
      signal \connR1\ : std_logic_vector (8 downto 0);
      signal \zll_l_v69512_outR1\ : std_logic_vector (8 downto 0);
      signal \connR2\ : std_logic_vector (8 downto 0);
      signal \zll_l_v69512_outR2\ : std_logic_vector (8 downto 0);
      signal main_lsbw8_out : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal \main_lsbw8_outR1\ : std_logic_vector (0 downto 0);
      signal \connR3\ : std_logic_vector (8 downto 0);
      signal \zll_l_v69512_outR3\ : std_logic_vector (8 downto 0);
      signal \main_lsbw8_outR2\ : std_logic_vector (0 downto 0);
      signal \connR4\ : std_logic_vector (8 downto 0);
      signal \zll_l_v69512_outR4\ : std_logic_vector (8 downto 0);
      signal \connR5\ : std_logic_vector (8 downto 0);
      signal \zll_l_v69512_outR5\ : std_logic_vector (8 downto 0);
      signal zi2 : std_logic_vector (8 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zl_arm607_out : std_logic_vector (105 downto 0);
      signal zl_arm609_out : std_logic_vector (105 downto 0);
      signal zl_arm611_out : std_logic_vector (105 downto 0);
      signal zl_arm613_out : std_logic_vector (105 downto 0);
begin
inst : \Main_msbW8\ port map (arg3, main_msbw8_out);
      zi0 <= main_msbw8_out;
      \instR1\ : \Main_msbW8\ port map (arg3, \main_msbw8_outR1\);
      conn <= (\main_msbw8_outR1\ & rw_or(rw_shiftl(arg3, std_logic_vector'(B"00000001")), rw_resize(zi0, 8)));
      \instR2\ : \ZLL_L_v69512\ port map (conn, zll_l_v69512_out);
      \instR3\ : \Main_msbW8\ port map (arg3, \main_msbw8_outR2\);
      \connR1\ <= (\main_msbw8_outR2\ & rw_or(rw_shiftl(arg3, std_logic_vector'(B"00000001")), std_logic_vector'(B"00000000")));
      \instR4\ : \ZLL_L_v69512\ port map (\connR1\, \zll_l_v69512_outR1\);
      \connR2\ <= rw_cond(rw_eq(arg0, std_logic_vector'(B"0")), zll_l_v69512_out, \zll_l_v69512_outR1\);
      \instR5\ : \ZLL_L_v69512\ port map (\connR2\, \zll_l_v69512_outR2\);
      \instR6\ : \Main_lsbW8\ port map (arg3, main_lsbw8_out);
      zi1 <= main_lsbw8_out;
      \instR7\ : \Main_lsbW8\ port map (arg3, \main_lsbw8_outR1\);
      \connR3\ <= (\main_lsbw8_outR1\ & rw_or(rw_shiftr(arg3, std_logic_vector'(B"00000001")), rw_shiftl(rw_resize(zi1, 8), std_logic_vector'(B"00000111"))));
      \instR8\ : \ZLL_L_v69512\ port map (\connR3\, \zll_l_v69512_outR3\);
      \instR9\ : \Main_lsbW8\ port map (arg3, \main_lsbw8_outR2\);
      \connR4\ <= (\main_lsbw8_outR2\ & rw_or(rw_shiftr(arg3, std_logic_vector'(B"00000001")), std_logic_vector'(B"00000000")));
      \instR10\ : \ZLL_L_v69512\ port map (\connR4\, \zll_l_v69512_outR4\);
      \connR5\ <= rw_cond(rw_eq(arg0, std_logic_vector'(B"0")), \zll_l_v69512_outR3\, \zll_l_v69512_outR4\);
      \instR11\ : \ZLL_L_v69512\ port map (\connR5\, \zll_l_v69512_outR5\);
      zi2 <= rw_cond(rw_eq(arg1, std_logic_vector'(B"0")), \zll_l_v69512_outR2\, \zll_l_v69512_outR5\);
      zi3 <= zi2(7 downto 0);
      zi4 <= zi3;
      \instR12\ : \ZL_arm607\ port map (zi2, zi4, arg4, zl_arm607_out);
      \instR13\ : \ZL_arm609\ port map (zi2, zi4, arg4, zl_arm609_out);
      \instR14\ : \ZL_arm611\ port map (zi2, zi4, arg4, zl_arm611_out);
      \instR15\ : \ZL_arm613\ port map (zi2, zi4, arg4, zl_arm613_out);
      res <= rw_cond(rw_eq(arg2, std_logic_vector'(B"00")), zl_arm607_out, rw_cond(rw_eq(arg2, std_logic_vector'(B"01")), zl_arm609_out, rw_cond(rw_eq(arg2, std_logic_vector'(B"10")), zl_arm611_out, zl_arm613_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setPC\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setPC\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (i & o & z & c & ie & arg1 & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_v672\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_v672\ is
component \ZLL_L_v6721\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal conn : std_logic_vector (7 downto 0);
      signal zll_l_v6721_out : std_logic_vector (105 downto 0);
begin
conn <= rw_or(rw_shiftr(arg1, std_logic_vector'(B"00000001")), rw_shiftl(arg1, std_logic_vector'(B"00000111")));
      inst : \ZLL_L_v6721\ port map (arg0, arg2, conn, zll_l_v6721_out);
      res <= zll_l_v6721_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_x274\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_x274\ is
component \Main_mkReg\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi0 : std_logic_vector (1 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR1\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR2\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      zi0 <= main_mkreg_out;
      \instR1\ : \Main_setR0\ port map (arg3, arg2, main_setr0_out);
      \instR2\ : \ZL___unused103\ port map (main_setr0_out, \zl__unused103_out\);
      \instR3\ : \Main_setR1\ port map (arg3, arg2, main_setr1_out);
      \instR4\ : \ZL___unused103\ port map (main_setr1_out, \zl__unused103_outR1\);
      \instR5\ : \Main_setR2\ port map (arg3, arg2, main_setr2_out);
      \instR6\ : \ZL___unused103\ port map (main_setr2_out, \zl__unused103_outR2\);
      \instR7\ : \Main_setR3\ port map (arg3, arg2, main_setr3_out);
      \instR8\ : \ZL___unused103\ port map (main_setr3_out, \zl__unused103_outR3\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"00")), \zl__unused103_out\, rw_cond(rw_eq(zi0, std_logic_vector'(B"01")), \zl__unused103_outR1\, rw_cond(rw_eq(zi0, std_logic_vector'(B"10")), \zl__unused103_outR2\, \zl__unused103_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR2\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setR2\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r3 <= arg0(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & arg1 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_cin240\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      arg2 : in std_logic_vector (8 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_cin240\ is
component \ZL_s150\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (8 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_s150_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_s150\ port map (arg0, arg2, arg1, arg1, zl_s150_out);
      res <= zl_s150_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS406\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS406\ is
component \Main_minusCW8$s1\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component \Main_setCFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_minuscw8$s1_out\ : std_logic_vector (8 downto 0);
      signal zi0 : std_logic_vector (8 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi3 : std_logic_vector (80 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal zi5 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_minusCW8$s1\ port map (arg0, arg1, \main_minuscw8$s1_out\);
      zi0 <= \main_minuscw8$s1_out\;
      zi1 <= zi0(8 downto 8);
      zi2 <= zi1;
      \instR1\ : \Main_setCFlag\ port map (arg2, zi2, main_setcflag_out);
      zi3 <= main_setcflag_out;
      zi4 <= zi0(7 downto 0);
      zi5 <= zi4;
      conn <= rw_eq(zi5, std_logic_vector'(B"00000000"));
      \instR2\ : \Main_setZFlag\ port map (zi3, conn, main_setzflag_out);
      \instR3\ : \ZL___unused103\ port map (main_setzflag_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD333\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD333\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZL_vS334\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zl_vs334_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zl_vs334_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zl_vs334_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zl_vs334_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZL_vS334\ port map (arg0, arg2, main_r0_out, arg3, zl_vs334_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \ZL_vS334\ port map (arg0, arg2, main_r1_out, arg3, \zl_vs334_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \ZL_vS334\ port map (arg0, arg2, main_r2_out, arg3, \zl_vs334_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZL_vS334\ port map (arg0, arg2, main_r3_out, arg3, \zl_vs334_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_vs334_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \zl_vs334_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \zl_vs334_outR2\, \zl_vs334_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL___unused597\ is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL___unused597\ is
component \Main_setCFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi2 : std_logic_vector (80 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
zi0 <= arg0(8 downto 8);
      zi1 <= zi0;
      inst : \Main_setCFlag\ port map (arg2, zi1, main_setcflag_out);
      zi2 <= main_setcflag_out;
      conn <= rw_eq(arg1, std_logic_vector'(B"00000000"));
      \instR1\ : \Main_setZFlag\ port map (zi2, conn, main_setzflag_out);
      \instR2\ : \ZL___unused103\ port map (main_setzflag_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s123\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s123\ is
component \ZL_v100\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_v100_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_v100\ port map (arg1, arg2, arg0, zl_v100_out);
      res <= zl_v100_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR1\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setR1\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & arg1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_inputs\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (9 downto 0));
end entity;

architecture rtl of \Main_inputs\ is
signal i : std_logic_vector (9 downto 0);
begin
i <= arg0(80 downto 71);
      res <= i;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_v623\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_v623\ is
component \ZLL_Main_plusCW8$s2\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component \ZL_arm607\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm609\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm611\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm613\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal conn : std_logic_vector (8 downto 0);
      signal \zll_main_pluscw8$s2_out\ : std_logic_vector (8 downto 0);
      signal zi0 : std_logic_vector (8 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zl_arm607_out : std_logic_vector (105 downto 0);
      signal zl_arm609_out : std_logic_vector (105 downto 0);
      signal zl_arm611_out : std_logic_vector (105 downto 0);
      signal zl_arm613_out : std_logic_vector (105 downto 0);
begin
conn <= rw_sub(rw_sub(rw_resize(arg1, 9), std_logic_vector'(B"000000001")), std_logic_vector'(B"000000000"));
      inst : \ZLL_Main_plusCW8$s2\ port map (conn, \zll_main_pluscw8$s2_out\);
      zi0 <= \zll_main_pluscw8$s2_out\;
      zi1 <= zi0(7 downto 0);
      zi2 <= zi1;
      \instR1\ : \ZL_arm607\ port map (zi0, zi2, arg2, zl_arm607_out);
      \instR2\ : \ZL_arm609\ port map (zi0, zi2, arg2, zl_arm609_out);
      \instR3\ : \ZL_arm611\ port map (zi0, zi2, arg2, zl_arm611_out);
      \instR4\ : \ZL_arm613\ port map (zi0, zi2, arg2, zl_arm613_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), zl_arm607_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), zl_arm609_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), zl_arm611_out, zl_arm613_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setInputs\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (9 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setInputs\ is
signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
begin
o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      r3 <= arg0(7 downto 0);
      res <= (arg1 & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_vS370\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_vS370\ is
component \ZL_arm309\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm311\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm313\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm315\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_arm309_out : std_logic_vector (105 downto 0);
      signal zl_arm311_out : std_logic_vector (105 downto 0);
      signal zl_arm313_out : std_logic_vector (105 downto 0);
      signal zl_arm315_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_arm309\ port map (arg2, arg0, zl_arm309_out);
      \instR1\ : \ZL_arm311\ port map (arg2, arg0, zl_arm311_out);
      \instR2\ : \ZL_arm313\ port map (arg2, arg0, zl_arm313_out);
      \instR3\ : \ZL_arm315\ port map (arg2, arg0, zl_arm315_out);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_arm309_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), zl_arm311_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), zl_arm313_out, zl_arm315_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s666\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s666\ is
component \ZL_v650\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zl_v650_out : std_logic_vector (105 downto 0);
begin
inst : \ZL_v650\ port map (arg1, arg2, arg0, zl_v650_out);
      res <= zl_v650_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL___unused137\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL___unused137\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi0 : std_logic_vector (17 downto 0);
begin
inst : \Main_outputs\ port map (arg0, main_outputs_out);
      zi0 <= main_outputs_out;
      res <= (zi0 & std_logic_vector'(B"1000000") & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r0\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r0\ is
signal r0 : std_logic_vector (7 downto 0);
begin
r0 <= arg0(31 downto 24);
      res <= r0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_a99\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_a99\ is
component \Main_mkReg\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_L_s123\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_v100\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi0 : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zll_l_s123_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zl_v100_out : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zll_l_s123_outR1\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zll_l_s123_outR2\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      zi0 <= main_mkreg_out;
      \instR1\ : \Main_r0\ port map (arg3, main_r0_out);
      \instR2\ : \ZLL_L_s123\ port map (arg3, arg2, main_r0_out, zll_l_s123_out);
      \instR3\ : \Main_r1\ port map (arg3, main_r1_out);
      zi1 <= main_r1_out;
      \instR4\ : \ZL_v100\ port map (arg2, zi1, arg3, zl_v100_out);
      \instR5\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR6\ : \ZLL_L_s123\ port map (arg3, arg2, main_r2_out, \zll_l_s123_outR1\);
      \instR7\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR8\ : \ZLL_L_s123\ port map (arg3, arg2, main_r3_out, \zll_l_s123_outR2\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"00")), zll_l_s123_out, rw_cond(rw_eq(zi0, std_logic_vector'(B"01")), zl_v100_out, rw_cond(rw_eq(zi0, std_logic_vector'(B"10")), \zll_l_s123_outR1\, \zll_l_s123_outR2\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm313\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm313\ is
component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused299\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \zl__unused299_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR2\ port map (arg1, arg0, main_setr2_out);
      \instR1\ : \ZL___unused299\ port map (arg0, main_setr2_out, \zl__unused299_out\);
      res <= \zl__unused299_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_v650\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_v650\ is
component \ZL_arm566\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm568\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm570\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm572\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zi0 : std_logic_vector (7 downto 0);
      signal zl_arm566_out : std_logic_vector (105 downto 0);
      signal zl_arm568_out : std_logic_vector (105 downto 0);
      signal zl_arm570_out : std_logic_vector (105 downto 0);
      signal zl_arm572_out : std_logic_vector (105 downto 0);
begin
zi0 <= rw_or(rw_shiftl(arg1, std_logic_vector'(B"00000001")), rw_shiftr(arg1, std_logic_vector'(B"00000111")));
      inst : \ZL_arm566\ port map (zi0, arg2, zl_arm566_out);
      \instR1\ : \ZL_arm568\ port map (zi0, arg2, zl_arm568_out);
      \instR2\ : \ZL_arm570\ port map (zi0, arg2, zl_arm570_out);
      \instR3\ : \ZL_arm572\ port map (zi0, arg2, zl_arm572_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), zl_arm566_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), zl_arm568_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), zl_arm570_out, zl_arm572_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_i139\ is
port (arg0 : in std_logic_vector (9 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_i139\ is
component \Main_setInputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (9 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL_Main_loop3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setinputs_out : std_logic_vector (80 downto 0);
      signal zl_main_loop3_out : std_logic_vector (105 downto 0);
begin
inst : \Main_setInputs\ port map (arg1, arg0, main_setinputs_out);
      \instR1\ : \ZL_Main_loop3\ port map (main_setinputs_out, zl_main_loop3_out);
      res <= zl_main_loop3_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_notb\ is
port (arg0 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_notb\ is

begin
res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"0")), std_logic_vector'(B"1"), std_logic_vector'(B"0"));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_dataIn\ is
port (arg0 : in std_logic_vector (9 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_dataIn\ is
signal d_i : std_logic_vector (7 downto 0);
begin
d_i <= arg0(9 downto 2);
      res <= d_i;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_s150\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (8 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_s150\ is
component \Main_setCFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused137\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal x : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \zl__unused137_out\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \zl__unused137_outR1\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \zl__unused137_outR2\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \zl__unused137_outR3\ : std_logic_vector (105 downto 0);
begin
x <= arg1(8 downto 8);
      inst : \Main_setCFlag\ port map (arg2, x, main_setcflag_out);
      zi0 <= main_setcflag_out;
      zi1 <= arg1(7 downto 0);
      zi2 <= zi1;
      \instR1\ : \Main_setR0\ port map (zi0, zi2, main_setr0_out);
      \instR2\ : \ZL___unused137\ port map (main_setr0_out, \zl__unused137_out\);
      \instR3\ : \Main_setR1\ port map (zi0, zi2, main_setr1_out);
      \instR4\ : \ZL___unused137\ port map (main_setr1_out, \zl__unused137_outR1\);
      \instR5\ : \Main_setR2\ port map (zi0, zi2, main_setr2_out);
      \instR6\ : \ZL___unused137\ port map (main_setr2_out, \zl__unused137_outR2\);
      \instR7\ : \Main_setR3\ port map (zi0, zi2, main_setr3_out);
      \instR8\ : \ZL___unused137\ port map (main_setr3_out, \zl__unused137_outR3\);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), \zl__unused137_out\, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), \zl__unused137_outR1\, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), \zl__unused137_outR2\, \zl__unused137_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm572\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm572\ is
component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR3\ port map (arg1, arg0, main_setr3_out);
      \instR1\ : \ZL___unused103\ port map (main_setr3_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL___unused299\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL___unused299\ is
component \Main_setCFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused137\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal \zl__unused137_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setCFlag\ port map (arg1, std_logic_vector'(B"0"), main_setcflag_out);
      zi0 <= main_setcflag_out;
      conn <= rw_eq(arg0, std_logic_vector'(B"00000000"));
      \instR1\ : \Main_setZFlag\ port map (zi0, conn, main_setzflag_out);
      \instR2\ : \ZL___unused137\ port map (main_setzflag_out, \zl__unused137_out\);
      res <= \zl__unused137_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_lsbW8\ is
port (arg0 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_lsbW8\ is

begin
res <= arg0(0 downto 0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r1\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r1\ is
signal r1 : std_logic_vector (7 downto 0);
begin
r1 <= arg0(23 downto 16);
      res <= r1;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_s537\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (17 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_s537\ is
component \Main_setOutputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setOutputs\ port map (arg0, arg1, main_setoutputs_out);
      \instR1\ : \ZL___unused103\ port map (main_setoutputs_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_minusCW8$s1\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \Main_minusCW8$s1\ is
component \ZLL_Main_plusCW8$s2\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      signal conn : std_logic_vector (8 downto 0);
      signal \zll_main_pluscw8$s2_out\ : std_logic_vector (8 downto 0);
begin
conn <= rw_sub(rw_sub(rw_resize(arg0, 9), rw_resize(arg1, 9)), std_logic_vector'(B"000000000"));
      inst : \ZLL_Main_plusCW8$s2\ port map (conn, \zll_main_pluscw8$s2_out\);
      res <= \zll_main_pluscw8$s2_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR3\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (80 downto 0));
end entity;

architecture rtl of \Main_setR3\ is
signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
begin
i <= arg0(80 downto 71);
      o <= arg0(70 downto 53);
      z <= arg0(52 downto 52);
      c <= arg0(51 downto 51);
      ie <= arg0(50 downto 50);
      pc <= arg0(49 downto 42);
      zs <= arg0(41 downto 41);
      cs <= arg0(40 downto 40);
      pcs <= arg0(39 downto 32);
      r0 <= arg0(31 downto 24);
      r1 <= arg0(23 downto 16);
      r2 <= arg0(15 downto 8);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & arg1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_L_z451\ is
port (arg0 : in std_logic_vector (80 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (0 downto 0);
      arg3 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZLL_L_z451\ is
component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm448\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
      signal zl_arm448_out : std_logic_vector (105 downto 0);
begin
inst : \ZL___unused103\ port map (arg0, \zl__unused103_out\);
      \instR1\ : \ZL_arm448\ port map (arg2, arg1, arg0, zl_arm448_out);
      res <= rw_cond(rw_eq(arg3, std_logic_vector'(B"0")), \zl__unused103_out\, zl_arm448_out);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm611\ is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm611\ is
component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused597\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \zl__unused597_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR2\ port map (arg2, arg1, main_setr2_out);
      \instR1\ : \ZL___unused597\ port map (arg0, arg1, main_setr2_out, \zl__unused597_out\);
      res <= \zl__unused597_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD238\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD238\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZL_vS239\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zl_vs239_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zl_vs239_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zl_vs239_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zl_vs239_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZL_vS239\ port map (arg0, arg2, main_r0_out, arg3, zl_vs239_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \ZL_vS239\ port map (arg0, arg2, main_r1_out, arg3, \zl_vs239_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \ZL_vS239\ port map (arg0, arg2, main_r2_out, arg3, \zl_vs239_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZL_vS239\ port map (arg0, arg2, main_r3_out, arg3, \zl_vs239_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_vs239_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \zl_vs239_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \zl_vs239_outR2\, \zl_vs239_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm448\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm448\ is
component \Main_mkReg\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_L_s446\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi0 : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zll_l_s446_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zll_l_s446_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zll_l_s446_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zll_l_s446_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      zi0 <= main_mkreg_out;
      \instR1\ : \Main_r0\ port map (arg2, main_r0_out);
      \instR2\ : \ZLL_L_s446\ port map (arg2, main_r0_out, zll_l_s446_out);
      \instR3\ : \Main_r1\ port map (arg2, main_r1_out);
      \instR4\ : \ZLL_L_s446\ port map (arg2, main_r1_out, \zll_l_s446_outR1\);
      \instR5\ : \Main_r2\ port map (arg2, main_r2_out);
      \instR6\ : \ZLL_L_s446\ port map (arg2, main_r2_out, \zll_l_s446_outR2\);
      \instR7\ : \Main_r3\ port map (arg2, main_r3_out);
      \instR8\ : \ZLL_L_s446\ port map (arg2, main_r3_out, \zll_l_s446_outR3\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"00")), zll_l_s446_out, rw_cond(rw_eq(zi0, std_logic_vector'(B"01")), \zll_l_s446_outR1\, rw_cond(rw_eq(zi0, std_logic_vector'(B"10")), \zll_l_s446_outR2\, \zll_l_s446_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm570\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm570\ is
component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR2\ port map (arg1, arg0, main_setr2_out);
      \instR1\ : \ZL___unused103\ port map (main_setr2_out, \zl__unused103_out\);
      res <= \zl__unused103_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r3\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r3\ is
signal r3 : std_logic_vector (7 downto 0);
begin
r3 <= arg0(7 downto 0);
      res <= r3;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r2\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r2\ is
signal r2 : std_logic_vector (7 downto 0);
begin
r2 <= arg0(15 downto 8);
      res <= r2;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL___unused103\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL___unused103\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi0 : std_logic_vector (17 downto 0);
begin
inst : \Main_outputs\ port map (arg0, main_outputs_out);
      zi0 <= main_outputs_out;
      res <= (zi0 & std_logic_vector'(B"0110000") & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_plusCW8$s1\ is
port (arg0 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \Main_plusCW8$s1\ is
component \ZLL_Main_plusCW8$s2\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      signal conn : std_logic_vector (8 downto 0);
      signal \zll_main_pluscw8$s2_out\ : std_logic_vector (8 downto 0);
begin
conn <= rw_add(rw_add(rw_resize(arg0, 9), std_logic_vector'(B"000000001")), std_logic_vector'(B"000000000"));
      inst : \ZLL_Main_plusCW8$s2\ port map (conn, \zll_main_pluscw8$s2_out\);
      res <= \zll_main_pluscw8$s2_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_a65\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_a65\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setAddrOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setOutputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setWeOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi0 : std_logic_vector (17 downto 0);
      signal main_setweout_out : std_logic_vector (17 downto 0);
      signal zi1 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal zi2 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (17 downto 0);
      signal zi3 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal zi4 : std_logic_vector (17 downto 0);
      signal \main_setoutputs_outR1\ : std_logic_vector (80 downto 0);
      signal zi5 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (17 downto 0);
      signal zi6 : std_logic_vector (17 downto 0);
begin
inst : \Main_outputs\ port map (arg3, main_outputs_out);
      zi0 <= main_outputs_out;
      \instR1\ : \Main_setWeOut\ port map (zi0, std_logic_vector'(B"0"), main_setweout_out);
      zi1 <= main_setweout_out;
      \instR2\ : \Main_setOutputs\ port map (arg3, zi1, main_setoutputs_out);
      zi2 <= main_setoutputs_out;
      \instR3\ : \Main_outputs\ port map (zi2, \main_outputs_outR1\);
      zi3 <= \main_outputs_outR1\;
      \instR4\ : \Main_setAddrOut\ port map (zi3, arg2, main_setaddrout_out);
      zi4 <= main_setaddrout_out;
      \instR5\ : \Main_setOutputs\ port map (zi2, zi4, \main_setoutputs_outR1\);
      zi5 <= \main_setoutputs_outR1\;
      \instR6\ : \Main_outputs\ port map (zi5, \main_outputs_outR2\);
      zi6 <= \main_outputs_outR2\;
      res <= (zi6 & std_logic_vector'(B"01000") & arg0 & arg1 & zi5);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_fail7\ is
port (arg0 : in std_logic_vector (9 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_fail7\ is
component \Main_cFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_dataIn\ is
      port (arg0 : in std_logic_vector (9 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_mkReg\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_notb\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_pc\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_setAddrOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setCFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setIEFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setOutputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setPC\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_zFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_L_s427\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (0 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s515\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s537\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s577\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s616\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s643\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s664\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s666\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s688\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s692\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_s90\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (0 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_L_z451\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (0 downto 0);
            arg3 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL___unused103\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_a65\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_a99\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_arm448\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_v559\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_v623\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_v695\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (1 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            arg4 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD134\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD168\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD204\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD238\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD297\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD333\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD369\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vD405\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_x274\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_datain_out : std_logic_vector (7 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi9 : std_logic_vector (1 downto 0);
      signal main_pc_out : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi11 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal zi12 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal zi13 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (17 downto 0);
      signal zi14 : std_logic_vector (17 downto 0);
      signal \main_mkreg_outR1\ : std_logic_vector (1 downto 0);
      signal zi15 : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zll_l_s90_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi16 : std_logic_vector (7 downto 0);
      signal zl_a65_out : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi17 : std_logic_vector (7 downto 0);
      signal \zl_a65_outR1\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zll_l_s90_outR1\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR2\ : std_logic_vector (1 downto 0);
      signal zi18 : std_logic_vector (1 downto 0);
      signal \main_r0_outR1\ : std_logic_vector (7 downto 0);
      signal zi19 : std_logic_vector (7 downto 0);
      signal zl_a99_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR1\ : std_logic_vector (7 downto 0);
      signal zi20 : std_logic_vector (7 downto 0);
      signal \zl_a99_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR1\ : std_logic_vector (7 downto 0);
      signal zi21 : std_logic_vector (7 downto 0);
      signal \zl_a99_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR1\ : std_logic_vector (7 downto 0);
      signal zi22 : std_logic_vector (7 downto 0);
      signal \zl_a99_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR3\ : std_logic_vector (1 downto 0);
      signal zi23 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR4\ : std_logic_vector (1 downto 0);
      signal zi24 : std_logic_vector (1 downto 0);
      signal \main_r0_outR2\ : std_logic_vector (7 downto 0);
      signal zl_vd134_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR2\ : std_logic_vector (7 downto 0);
      signal \zl_vd134_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR2\ : std_logic_vector (7 downto 0);
      signal \zl_vd134_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR2\ : std_logic_vector (7 downto 0);
      signal \zl_vd134_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR5\ : std_logic_vector (1 downto 0);
      signal zi25 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR6\ : std_logic_vector (1 downto 0);
      signal zi26 : std_logic_vector (1 downto 0);
      signal \main_r0_outR3\ : std_logic_vector (7 downto 0);
      signal zl_vd168_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR3\ : std_logic_vector (7 downto 0);
      signal \zl_vd168_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR3\ : std_logic_vector (7 downto 0);
      signal \zl_vd168_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR3\ : std_logic_vector (7 downto 0);
      signal \zl_vd168_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR7\ : std_logic_vector (1 downto 0);
      signal zi27 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR8\ : std_logic_vector (1 downto 0);
      signal zi28 : std_logic_vector (1 downto 0);
      signal \main_r0_outR4\ : std_logic_vector (7 downto 0);
      signal zl_vd204_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR4\ : std_logic_vector (7 downto 0);
      signal \zl_vd204_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR4\ : std_logic_vector (7 downto 0);
      signal \zl_vd204_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR4\ : std_logic_vector (7 downto 0);
      signal \zl_vd204_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR9\ : std_logic_vector (1 downto 0);
      signal zi29 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR10\ : std_logic_vector (1 downto 0);
      signal zi30 : std_logic_vector (1 downto 0);
      signal \main_r0_outR5\ : std_logic_vector (7 downto 0);
      signal zl_vd238_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR5\ : std_logic_vector (7 downto 0);
      signal \zl_vd238_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR5\ : std_logic_vector (7 downto 0);
      signal \zl_vd238_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR5\ : std_logic_vector (7 downto 0);
      signal \zl_vd238_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR11\ : std_logic_vector (1 downto 0);
      signal zi31 : std_logic_vector (1 downto 0);
      signal \main_r0_outR6\ : std_logic_vector (7 downto 0);
      signal zi32 : std_logic_vector (7 downto 0);
      signal zl_x274_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR6\ : std_logic_vector (7 downto 0);
      signal zi33 : std_logic_vector (7 downto 0);
      signal \zl_x274_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR6\ : std_logic_vector (7 downto 0);
      signal zi34 : std_logic_vector (7 downto 0);
      signal \zl_x274_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR6\ : std_logic_vector (7 downto 0);
      signal zi35 : std_logic_vector (7 downto 0);
      signal \zl_x274_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR12\ : std_logic_vector (1 downto 0);
      signal zi36 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR13\ : std_logic_vector (1 downto 0);
      signal zi37 : std_logic_vector (1 downto 0);
      signal \main_r0_outR7\ : std_logic_vector (7 downto 0);
      signal zl_vd297_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR7\ : std_logic_vector (7 downto 0);
      signal \zl_vd297_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR7\ : std_logic_vector (7 downto 0);
      signal \zl_vd297_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR7\ : std_logic_vector (7 downto 0);
      signal \zl_vd297_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR14\ : std_logic_vector (1 downto 0);
      signal zi38 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR15\ : std_logic_vector (1 downto 0);
      signal zi39 : std_logic_vector (1 downto 0);
      signal \main_r0_outR8\ : std_logic_vector (7 downto 0);
      signal zl_vd333_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR8\ : std_logic_vector (7 downto 0);
      signal \zl_vd333_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR8\ : std_logic_vector (7 downto 0);
      signal \zl_vd333_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR8\ : std_logic_vector (7 downto 0);
      signal \zl_vd333_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR16\ : std_logic_vector (1 downto 0);
      signal zi40 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR17\ : std_logic_vector (1 downto 0);
      signal zi41 : std_logic_vector (1 downto 0);
      signal \main_r0_outR9\ : std_logic_vector (7 downto 0);
      signal zl_vd369_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR9\ : std_logic_vector (7 downto 0);
      signal \zl_vd369_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR9\ : std_logic_vector (7 downto 0);
      signal \zl_vd369_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR9\ : std_logic_vector (7 downto 0);
      signal \zl_vd369_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR18\ : std_logic_vector (1 downto 0);
      signal zi42 : std_logic_vector (1 downto 0);
      signal \main_r0_outR10\ : std_logic_vector (7 downto 0);
      signal zi43 : std_logic_vector (7 downto 0);
      signal zl_vd405_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR10\ : std_logic_vector (7 downto 0);
      signal zll_l_s427_out : std_logic_vector (105 downto 0);
      signal \main_r2_outR10\ : std_logic_vector (7 downto 0);
      signal \zll_l_s427_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR10\ : std_logic_vector (7 downto 0);
      signal \zll_l_s427_outR2\ : std_logic_vector (105 downto 0);
      signal main_zflag_out : std_logic_vector (0 downto 0);
      signal zi44 : std_logic_vector (0 downto 0);
      signal \zl__unused103_out\ : std_logic_vector (105 downto 0);
      signal zl_arm448_out : std_logic_vector (105 downto 0);
      signal \main_zflag_outR1\ : std_logic_vector (0 downto 0);
      signal zi45 : std_logic_vector (0 downto 0);
      signal main_notb_out : std_logic_vector (0 downto 0);
      signal zll_l_z451_out : std_logic_vector (105 downto 0);
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal zi46 : std_logic_vector (0 downto 0);
      signal \zl__unused103_outR1\ : std_logic_vector (105 downto 0);
      signal \zl_arm448_outR1\ : std_logic_vector (105 downto 0);
      signal \main_cflag_outR1\ : std_logic_vector (0 downto 0);
      signal zi47 : std_logic_vector (0 downto 0);
      signal \main_notb_outR1\ : std_logic_vector (0 downto 0);
      signal \zll_l_z451_outR1\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR19\ : std_logic_vector (1 downto 0);
      signal zi48 : std_logic_vector (1 downto 0);
      signal \main_r0_outR11\ : std_logic_vector (7 downto 0);
      signal zll_l_s515_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR11\ : std_logic_vector (7 downto 0);
      signal \zll_l_s515_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR11\ : std_logic_vector (7 downto 0);
      signal \zll_l_s515_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR11\ : std_logic_vector (7 downto 0);
      signal \zll_l_s515_outR3\ : std_logic_vector (105 downto 0);
      signal main_setieflag_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR2\ : std_logic_vector (105 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (17 downto 0);
      signal zi49 : std_logic_vector (17 downto 0);
      signal zi50 : std_logic_vector (7 downto 0);
      signal zi51 : std_logic_vector (7 downto 0);
      signal zi52 : std_logic_vector (0 downto 0);
      signal conn : std_logic_vector (17 downto 0);
      signal zll_l_s537_out : std_logic_vector (105 downto 0);
      signal \main_setieflag_outR1\ : std_logic_vector (80 downto 0);
      signal zi53 : std_logic_vector (80 downto 0);
      signal zi54 : std_logic_vector (7 downto 0);
      signal zi55 : std_logic_vector (7 downto 0);
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal zi56 : std_logic_vector (80 downto 0);
      signal zi57 : std_logic_vector (0 downto 0);
      signal zi58 : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal zi59 : std_logic_vector (80 downto 0);
      signal zi60 : std_logic_vector (0 downto 0);
      signal zi61 : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR20\ : std_logic_vector (1 downto 0);
      signal zi62 : std_logic_vector (1 downto 0);
      signal \main_r0_outR12\ : std_logic_vector (7 downto 0);
      signal zll_l_s577_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR12\ : std_logic_vector (7 downto 0);
      signal zi63 : std_logic_vector (7 downto 0);
      signal zl_v559_out : std_logic_vector (105 downto 0);
      signal \main_r2_outR12\ : std_logic_vector (7 downto 0);
      signal \zll_l_s577_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR12\ : std_logic_vector (7 downto 0);
      signal \zll_l_s577_outR2\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR21\ : std_logic_vector (1 downto 0);
      signal zi64 : std_logic_vector (1 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR4\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR5\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR6\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \zl__unused103_outR7\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR22\ : std_logic_vector (1 downto 0);
      signal zi65 : std_logic_vector (1 downto 0);
      signal \main_r0_outR13\ : std_logic_vector (7 downto 0);
      signal zll_l_s616_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR13\ : std_logic_vector (7 downto 0);
      signal \zll_l_s616_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR13\ : std_logic_vector (7 downto 0);
      signal \zll_l_s616_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR13\ : std_logic_vector (7 downto 0);
      signal \zll_l_s616_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR23\ : std_logic_vector (1 downto 0);
      signal zi66 : std_logic_vector (1 downto 0);
      signal \main_r0_outR14\ : std_logic_vector (7 downto 0);
      signal zll_l_s643_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR14\ : std_logic_vector (7 downto 0);
      signal \zll_l_s643_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR14\ : std_logic_vector (7 downto 0);
      signal zi67 : std_logic_vector (7 downto 0);
      signal zl_v623_out : std_logic_vector (105 downto 0);
      signal \main_r3_outR14\ : std_logic_vector (7 downto 0);
      signal \zll_l_s643_outR2\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR24\ : std_logic_vector (1 downto 0);
      signal zi68 : std_logic_vector (1 downto 0);
      signal \main_r0_outR15\ : std_logic_vector (7 downto 0);
      signal zll_l_s664_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR15\ : std_logic_vector (7 downto 0);
      signal zll_l_s666_out : std_logic_vector (105 downto 0);
      signal \main_r2_outR15\ : std_logic_vector (7 downto 0);
      signal \zll_l_s664_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR15\ : std_logic_vector (7 downto 0);
      signal \zll_l_s666_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r0_outR16\ : std_logic_vector (7 downto 0);
      signal zll_l_s688_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR16\ : std_logic_vector (7 downto 0);
      signal \zll_l_s688_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR16\ : std_logic_vector (7 downto 0);
      signal zll_l_s692_out : std_logic_vector (105 downto 0);
      signal \main_r3_outR16\ : std_logic_vector (7 downto 0);
      signal \zll_l_s692_outR1\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR25\ : std_logic_vector (1 downto 0);
      signal zi69 : std_logic_vector (1 downto 0);
      signal \main_r0_outR17\ : std_logic_vector (7 downto 0);
      signal zi70 : std_logic_vector (7 downto 0);
      signal zl_v695_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR17\ : std_logic_vector (7 downto 0);
      signal zi71 : std_logic_vector (7 downto 0);
      signal \zl_v695_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR17\ : std_logic_vector (7 downto 0);
      signal zi72 : std_logic_vector (7 downto 0);
      signal \zl_v695_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR17\ : std_logic_vector (7 downto 0);
      signal zi73 : std_logic_vector (7 downto 0);
      signal \zl_v695_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_dataIn\ port map (arg0, main_datain_out);
      zi0 <= main_datain_out;
      zi1 <= zi0(6 downto 6);
      zi2 <= zi0(5 downto 5);
      zi3 <= zi0(4 downto 4);
      zi4 <= zi0(3 downto 3);
      zi5 <= zi0(2 downto 2);
      zi6 <= zi0(1 downto 1);
      zi7 <= zi0(0 downto 0);
      zi8 <= zi0(7 downto 7);
      \instR1\ : \Main_mkReg\ port map (zi6, zi7, main_mkreg_out);
      zi9 <= main_mkreg_out;
      \instR2\ : \Main_pc\ port map (arg1, main_pc_out);
      zi10 <= main_pc_out;
      \instR3\ : \Main_outputs\ port map (arg1, main_outputs_out);
      zi11 <= main_outputs_out;
      \instR4\ : \Main_setAddrOut\ port map (zi11, zi10, main_setaddrout_out);
      zi12 <= main_setaddrout_out;
      \instR5\ : \Main_setOutputs\ port map (arg1, zi12, main_setoutputs_out);
      zi13 <= main_setoutputs_out;
      \instR6\ : \Main_outputs\ port map (zi13, \main_outputs_outR1\);
      zi14 <= \main_outputs_outR1\;
      \instR7\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR1\);
      zi15 <= \main_mkreg_outR1\;
      \instR8\ : \Main_r0\ port map (arg1, main_r0_out);
      \instR9\ : \ZLL_L_s90\ port map (zi5, arg1, zi4, main_r0_out, zll_l_s90_out);
      \instR10\ : \Main_r1\ port map (arg1, main_r1_out);
      zi16 <= main_r1_out;
      \instR11\ : \ZL_a65\ port map (zi4, zi5, zi16, arg1, zl_a65_out);
      \instR12\ : \Main_r2\ port map (arg1, main_r2_out);
      zi17 <= main_r2_out;
      \instR13\ : \ZL_a65\ port map (zi4, zi5, zi17, arg1, \zl_a65_outR1\);
      \instR14\ : \Main_r3\ port map (arg1, main_r3_out);
      \instR15\ : \ZLL_L_s90\ port map (zi5, arg1, zi4, main_r3_out, \zll_l_s90_outR1\);
      \instR16\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR2\);
      zi18 <= \main_mkreg_outR2\;
      \instR17\ : \Main_r0\ port map (arg1, \main_r0_outR1\);
      zi19 <= \main_r0_outR1\;
      \instR18\ : \ZL_a99\ port map (zi4, zi5, zi19, arg1, zl_a99_out);
      \instR19\ : \Main_r1\ port map (arg1, \main_r1_outR1\);
      zi20 <= \main_r1_outR1\;
      \instR20\ : \ZL_a99\ port map (zi4, zi5, zi20, arg1, \zl_a99_outR1\);
      \instR21\ : \Main_r2\ port map (arg1, \main_r2_outR1\);
      zi21 <= \main_r2_outR1\;
      \instR22\ : \ZL_a99\ port map (zi4, zi5, zi21, arg1, \zl_a99_outR2\);
      \instR23\ : \Main_r3\ port map (arg1, \main_r3_outR1\);
      zi22 <= \main_r3_outR1\;
      \instR24\ : \ZL_a99\ port map (zi4, zi5, zi22, arg1, \zl_a99_outR3\);
      \instR25\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR3\);
      zi23 <= \main_mkreg_outR3\;
      \instR26\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR4\);
      zi24 <= \main_mkreg_outR4\;
      \instR27\ : \Main_r0\ port map (arg1, \main_r0_outR2\);
      \instR28\ : \ZL_vD134\ port map (zi23, zi24, \main_r0_outR2\, arg1, zl_vd134_out);
      \instR29\ : \Main_r1\ port map (arg1, \main_r1_outR2\);
      \instR30\ : \ZL_vD134\ port map (zi23, zi24, \main_r1_outR2\, arg1, \zl_vd134_outR1\);
      \instR31\ : \Main_r2\ port map (arg1, \main_r2_outR2\);
      \instR32\ : \ZL_vD134\ port map (zi23, zi24, \main_r2_outR2\, arg1, \zl_vd134_outR2\);
      \instR33\ : \Main_r3\ port map (arg1, \main_r3_outR2\);
      \instR34\ : \ZL_vD134\ port map (zi23, zi24, \main_r3_outR2\, arg1, \zl_vd134_outR3\);
      \instR35\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR5\);
      zi25 <= \main_mkreg_outR5\;
      \instR36\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR6\);
      zi26 <= \main_mkreg_outR6\;
      \instR37\ : \Main_r0\ port map (arg1, \main_r0_outR3\);
      \instR38\ : \ZL_vD168\ port map (zi25, zi26, \main_r0_outR3\, arg1, zl_vd168_out);
      \instR39\ : \Main_r1\ port map (arg1, \main_r1_outR3\);
      \instR40\ : \ZL_vD168\ port map (zi25, zi26, \main_r1_outR3\, arg1, \zl_vd168_outR1\);
      \instR41\ : \Main_r2\ port map (arg1, \main_r2_outR3\);
      \instR42\ : \ZL_vD168\ port map (zi25, zi26, \main_r2_outR3\, arg1, \zl_vd168_outR2\);
      \instR43\ : \Main_r3\ port map (arg1, \main_r3_outR3\);
      \instR44\ : \ZL_vD168\ port map (zi25, zi26, \main_r3_outR3\, arg1, \zl_vd168_outR3\);
      \instR45\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR7\);
      zi27 <= \main_mkreg_outR7\;
      \instR46\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR8\);
      zi28 <= \main_mkreg_outR8\;
      \instR47\ : \Main_r0\ port map (arg1, \main_r0_outR4\);
      \instR48\ : \ZL_vD204\ port map (zi27, zi28, \main_r0_outR4\, arg1, zl_vd204_out);
      \instR49\ : \Main_r1\ port map (arg1, \main_r1_outR4\);
      \instR50\ : \ZL_vD204\ port map (zi27, zi28, \main_r1_outR4\, arg1, \zl_vd204_outR1\);
      \instR51\ : \Main_r2\ port map (arg1, \main_r2_outR4\);
      \instR52\ : \ZL_vD204\ port map (zi27, zi28, \main_r2_outR4\, arg1, \zl_vd204_outR2\);
      \instR53\ : \Main_r3\ port map (arg1, \main_r3_outR4\);
      \instR54\ : \ZL_vD204\ port map (zi27, zi28, \main_r3_outR4\, arg1, \zl_vd204_outR3\);
      \instR55\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR9\);
      zi29 <= \main_mkreg_outR9\;
      \instR56\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR10\);
      zi30 <= \main_mkreg_outR10\;
      \instR57\ : \Main_r0\ port map (arg1, \main_r0_outR5\);
      \instR58\ : \ZL_vD238\ port map (zi29, zi30, \main_r0_outR5\, arg1, zl_vd238_out);
      \instR59\ : \Main_r1\ port map (arg1, \main_r1_outR5\);
      \instR60\ : \ZL_vD238\ port map (zi29, zi30, \main_r1_outR5\, arg1, \zl_vd238_outR1\);
      \instR61\ : \Main_r2\ port map (arg1, \main_r2_outR5\);
      \instR62\ : \ZL_vD238\ port map (zi29, zi30, \main_r2_outR5\, arg1, \zl_vd238_outR2\);
      \instR63\ : \Main_r3\ port map (arg1, \main_r3_outR5\);
      \instR64\ : \ZL_vD238\ port map (zi29, zi30, \main_r3_outR5\, arg1, \zl_vd238_outR3\);
      \instR65\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR11\);
      zi31 <= \main_mkreg_outR11\;
      \instR66\ : \Main_r0\ port map (arg1, \main_r0_outR6\);
      zi32 <= \main_r0_outR6\;
      \instR67\ : \ZL_x274\ port map (zi4, zi5, zi32, arg1, zl_x274_out);
      \instR68\ : \Main_r1\ port map (arg1, \main_r1_outR6\);
      zi33 <= \main_r1_outR6\;
      \instR69\ : \ZL_x274\ port map (zi4, zi5, zi33, arg1, \zl_x274_outR1\);
      \instR70\ : \Main_r2\ port map (arg1, \main_r2_outR6\);
      zi34 <= \main_r2_outR6\;
      \instR71\ : \ZL_x274\ port map (zi4, zi5, zi34, arg1, \zl_x274_outR2\);
      \instR72\ : \Main_r3\ port map (arg1, \main_r3_outR6\);
      zi35 <= \main_r3_outR6\;
      \instR73\ : \ZL_x274\ port map (zi4, zi5, zi35, arg1, \zl_x274_outR3\);
      \instR74\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR12\);
      zi36 <= \main_mkreg_outR12\;
      \instR75\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR13\);
      zi37 <= \main_mkreg_outR13\;
      \instR76\ : \Main_r0\ port map (arg1, \main_r0_outR7\);
      \instR77\ : \ZL_vD297\ port map (zi36, zi37, \main_r0_outR7\, arg1, zl_vd297_out);
      \instR78\ : \Main_r1\ port map (arg1, \main_r1_outR7\);
      \instR79\ : \ZL_vD297\ port map (zi36, zi37, \main_r1_outR7\, arg1, \zl_vd297_outR1\);
      \instR80\ : \Main_r2\ port map (arg1, \main_r2_outR7\);
      \instR81\ : \ZL_vD297\ port map (zi36, zi37, \main_r2_outR7\, arg1, \zl_vd297_outR2\);
      \instR82\ : \Main_r3\ port map (arg1, \main_r3_outR7\);
      \instR83\ : \ZL_vD297\ port map (zi36, zi37, \main_r3_outR7\, arg1, \zl_vd297_outR3\);
      \instR84\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR14\);
      zi38 <= \main_mkreg_outR14\;
      \instR85\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR15\);
      zi39 <= \main_mkreg_outR15\;
      \instR86\ : \Main_r0\ port map (arg1, \main_r0_outR8\);
      \instR87\ : \ZL_vD333\ port map (zi38, zi39, \main_r0_outR8\, arg1, zl_vd333_out);
      \instR88\ : \Main_r1\ port map (arg1, \main_r1_outR8\);
      \instR89\ : \ZL_vD333\ port map (zi38, zi39, \main_r1_outR8\, arg1, \zl_vd333_outR1\);
      \instR90\ : \Main_r2\ port map (arg1, \main_r2_outR8\);
      \instR91\ : \ZL_vD333\ port map (zi38, zi39, \main_r2_outR8\, arg1, \zl_vd333_outR2\);
      \instR92\ : \Main_r3\ port map (arg1, \main_r3_outR8\);
      \instR93\ : \ZL_vD333\ port map (zi38, zi39, \main_r3_outR8\, arg1, \zl_vd333_outR3\);
      \instR94\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR16\);
      zi40 <= \main_mkreg_outR16\;
      \instR95\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR17\);
      zi41 <= \main_mkreg_outR17\;
      \instR96\ : \Main_r0\ port map (arg1, \main_r0_outR9\);
      \instR97\ : \ZL_vD369\ port map (zi40, zi41, \main_r0_outR9\, arg1, zl_vd369_out);
      \instR98\ : \Main_r1\ port map (arg1, \main_r1_outR9\);
      \instR99\ : \ZL_vD369\ port map (zi40, zi41, \main_r1_outR9\, arg1, \zl_vd369_outR1\);
      \instR100\ : \Main_r2\ port map (arg1, \main_r2_outR9\);
      \instR101\ : \ZL_vD369\ port map (zi40, zi41, \main_r2_outR9\, arg1, \zl_vd369_outR2\);
      \instR102\ : \Main_r3\ port map (arg1, \main_r3_outR9\);
      \instR103\ : \ZL_vD369\ port map (zi40, zi41, \main_r3_outR9\, arg1, \zl_vd369_outR3\);
      \instR104\ : \Main_mkReg\ port map (zi4, zi5, \main_mkreg_outR18\);
      zi42 <= \main_mkreg_outR18\;
      \instR105\ : \Main_r0\ port map (arg1, \main_r0_outR10\);
      zi43 <= \main_r0_outR10\;
      \instR106\ : \ZL_vD405\ port map (zi6, zi7, zi43, arg1, zl_vd405_out);
      \instR107\ : \Main_r1\ port map (arg1, \main_r1_outR10\);
      \instR108\ : \ZLL_L_s427\ port map (arg1, zi7, zi6, \main_r1_outR10\, zll_l_s427_out);
      \instR109\ : \Main_r2\ port map (arg1, \main_r2_outR10\);
      \instR110\ : \ZLL_L_s427\ port map (arg1, zi7, zi6, \main_r2_outR10\, \zll_l_s427_outR1\);
      \instR111\ : \Main_r3\ port map (arg1, \main_r3_outR10\);
      \instR112\ : \ZLL_L_s427\ port map (arg1, zi7, zi6, \main_r3_outR10\, \zll_l_s427_outR2\);
      \instR113\ : \Main_zFlag\ port map (arg1, main_zflag_out);
      zi44 <= main_zflag_out;
      \instR114\ : \ZL___unused103\ port map (arg1, \zl__unused103_out\);
      \instR115\ : \ZL_arm448\ port map (zi6, zi7, arg1, zl_arm448_out);
      \instR116\ : \Main_zFlag\ port map (arg1, \main_zflag_outR1\);
      zi45 <= \main_zflag_outR1\;
      \instR117\ : \Main_notb\ port map (zi45, main_notb_out);
      \instR118\ : \ZLL_L_z451\ port map (arg1, zi7, zi6, main_notb_out, zll_l_z451_out);
      \instR119\ : \Main_cFlag\ port map (arg1, main_cflag_out);
      zi46 <= main_cflag_out;
      \instR120\ : \ZL___unused103\ port map (arg1, \zl__unused103_outR1\);
      \instR121\ : \ZL_arm448\ port map (zi6, zi7, arg1, \zl_arm448_outR1\);
      \instR122\ : \Main_cFlag\ port map (arg1, \main_cflag_outR1\);
      zi47 <= \main_cflag_outR1\;
      \instR123\ : \Main_notb\ port map (zi47, \main_notb_outR1\);
      \instR124\ : \ZLL_L_z451\ port map (arg1, zi7, zi6, \main_notb_outR1\, \zll_l_z451_outR1\);
      \instR125\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR19\);
      zi48 <= \main_mkreg_outR19\;
      \instR126\ : \Main_r0\ port map (arg1, \main_r0_outR11\);
      \instR127\ : \ZLL_L_s515\ port map (arg1, \main_r0_outR11\, zll_l_s515_out);
      \instR128\ : \Main_r1\ port map (arg1, \main_r1_outR11\);
      \instR129\ : \ZLL_L_s515\ port map (arg1, \main_r1_outR11\, \zll_l_s515_outR1\);
      \instR130\ : \Main_r2\ port map (arg1, \main_r2_outR11\);
      \instR131\ : \ZLL_L_s515\ port map (arg1, \main_r2_outR11\, \zll_l_s515_outR2\);
      \instR132\ : \Main_r3\ port map (arg1, \main_r3_outR11\);
      \instR133\ : \ZLL_L_s515\ port map (arg1, \main_r3_outR11\, \zll_l_s515_outR3\);
      \instR134\ : \Main_setIEFlag\ port map (arg1, zi7, main_setieflag_out);
      \instR135\ : \ZL___unused103\ port map (main_setieflag_out, \zl__unused103_outR2\);
      \instR136\ : \Main_outputs\ port map (arg1, \main_outputs_outR2\);
      zi49 <= \main_outputs_outR2\;
      zi50 <= zi49(17 downto 10);
      zi51 <= zi49(9 downto 2);
      zi52 <= zi49(1 downto 1);
      conn <= (zi50 & zi51 & zi52 & std_logic_vector'(B"1"));
      \instR137\ : \ZLL_L_s537\ port map (arg1, conn, zll_l_s537_out);
      \instR138\ : \Main_setIEFlag\ port map (arg1, std_logic_vector'(B"1"), \main_setieflag_outR1\);
      zi53 <= \main_setieflag_outR1\;
      zi54 <= zi53(39 downto 32);
      zi55 <= zi54;
      \instR139\ : \Main_setPC\ port map (zi53, zi55, main_setpc_out);
      zi56 <= main_setpc_out;
      zi57 <= zi56(41 downto 41);
      zi58 <= zi57;
      \instR140\ : \Main_setZFlag\ port map (zi56, zi58, main_setzflag_out);
      zi59 <= main_setzflag_out;
      zi60 <= zi59(40 downto 40);
      zi61 <= zi60;
      \instR141\ : \Main_setCFlag\ port map (zi59, zi61, main_setcflag_out);
      \instR142\ : \ZL___unused103\ port map (main_setcflag_out, \zl__unused103_outR3\);
      \instR143\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR20\);
      zi62 <= \main_mkreg_outR20\;
      \instR144\ : \Main_r0\ port map (arg1, \main_r0_outR12\);
      \instR145\ : \ZLL_L_s577\ port map (arg1, zi62, \main_r0_outR12\, zll_l_s577_out);
      \instR146\ : \Main_r1\ port map (arg1, \main_r1_outR12\);
      zi63 <= \main_r1_outR12\;
      \instR147\ : \ZL_v559\ port map (zi62, zi63, arg1, zl_v559_out);
      \instR148\ : \Main_r2\ port map (arg1, \main_r2_outR12\);
      \instR149\ : \ZLL_L_s577\ port map (arg1, zi62, \main_r2_outR12\, \zll_l_s577_outR1\);
      \instR150\ : \Main_r3\ port map (arg1, \main_r3_outR12\);
      \instR151\ : \ZLL_L_s577\ port map (arg1, zi62, \main_r3_outR12\, \zll_l_s577_outR2\);
      \instR152\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR21\);
      zi64 <= \main_mkreg_outR21\;
      \instR153\ : \Main_setR0\ port map (arg1, std_logic_vector'(B"00000000"), main_setr0_out);
      \instR154\ : \ZL___unused103\ port map (main_setr0_out, \zl__unused103_outR4\);
      \instR155\ : \Main_setR1\ port map (arg1, std_logic_vector'(B"00000000"), main_setr1_out);
      \instR156\ : \ZL___unused103\ port map (main_setr1_out, \zl__unused103_outR5\);
      \instR157\ : \Main_setR2\ port map (arg1, std_logic_vector'(B"00000000"), main_setr2_out);
      \instR158\ : \ZL___unused103\ port map (main_setr2_out, \zl__unused103_outR6\);
      \instR159\ : \Main_setR3\ port map (arg1, std_logic_vector'(B"00000000"), main_setr3_out);
      \instR160\ : \ZL___unused103\ port map (main_setr3_out, \zl__unused103_outR7\);
      \instR161\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR22\);
      zi65 <= \main_mkreg_outR22\;
      \instR162\ : \Main_r0\ port map (arg1, \main_r0_outR13\);
      \instR163\ : \ZLL_L_s616\ port map (zi65, arg1, \main_r0_outR13\, zll_l_s616_out);
      \instR164\ : \Main_r1\ port map (arg1, \main_r1_outR13\);
      \instR165\ : \ZLL_L_s616\ port map (zi65, arg1, \main_r1_outR13\, \zll_l_s616_outR1\);
      \instR166\ : \Main_r2\ port map (arg1, \main_r2_outR13\);
      \instR167\ : \ZLL_L_s616\ port map (zi65, arg1, \main_r2_outR13\, \zll_l_s616_outR2\);
      \instR168\ : \Main_r3\ port map (arg1, \main_r3_outR13\);
      \instR169\ : \ZLL_L_s616\ port map (zi65, arg1, \main_r3_outR13\, \zll_l_s616_outR3\);
      \instR170\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR23\);
      zi66 <= \main_mkreg_outR23\;
      \instR171\ : \Main_r0\ port map (arg1, \main_r0_outR14\);
      \instR172\ : \ZLL_L_s643\ port map (arg1, zi66, \main_r0_outR14\, zll_l_s643_out);
      \instR173\ : \Main_r1\ port map (arg1, \main_r1_outR14\);
      \instR174\ : \ZLL_L_s643\ port map (arg1, zi66, \main_r1_outR14\, \zll_l_s643_outR1\);
      \instR175\ : \Main_r2\ port map (arg1, \main_r2_outR14\);
      zi67 <= \main_r2_outR14\;
      \instR176\ : \ZL_v623\ port map (zi66, zi67, arg1, zl_v623_out);
      \instR177\ : \Main_r3\ port map (arg1, \main_r3_outR14\);
      \instR178\ : \ZLL_L_s643\ port map (arg1, zi66, \main_r3_outR14\, \zll_l_s643_outR2\);
      \instR179\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR24\);
      zi68 <= \main_mkreg_outR24\;
      \instR180\ : \Main_r0\ port map (arg1, \main_r0_outR15\);
      \instR181\ : \ZLL_L_s664\ port map (zi68, arg1, \main_r0_outR15\, zll_l_s664_out);
      \instR182\ : \Main_r1\ port map (arg1, \main_r1_outR15\);
      \instR183\ : \ZLL_L_s666\ port map (arg1, zi68, \main_r1_outR15\, zll_l_s666_out);
      \instR184\ : \Main_r2\ port map (arg1, \main_r2_outR15\);
      \instR185\ : \ZLL_L_s664\ port map (zi68, arg1, \main_r2_outR15\, \zll_l_s664_outR1\);
      \instR186\ : \Main_r3\ port map (arg1, \main_r3_outR15\);
      \instR187\ : \ZLL_L_s666\ port map (arg1, zi68, \main_r3_outR15\, \zll_l_s666_outR1\);
      \instR188\ : \Main_r0\ port map (arg1, \main_r0_outR16\);
      \instR189\ : \ZLL_L_s688\ port map (zi68, arg1, \main_r0_outR16\, zll_l_s688_out);
      \instR190\ : \Main_r1\ port map (arg1, \main_r1_outR16\);
      \instR191\ : \ZLL_L_s688\ port map (zi68, arg1, \main_r1_outR16\, \zll_l_s688_outR1\);
      \instR192\ : \Main_r2\ port map (arg1, \main_r2_outR16\);
      \instR193\ : \ZLL_L_s692\ port map (arg1, zi68, \main_r2_outR16\, zll_l_s692_out);
      \instR194\ : \Main_r3\ port map (arg1, \main_r3_outR16\);
      \instR195\ : \ZLL_L_s692\ port map (arg1, zi68, \main_r3_outR16\, \zll_l_s692_outR1\);
      \instR196\ : \Main_mkReg\ port map (zi6, zi7, \main_mkreg_outR25\);
      zi69 <= \main_mkreg_outR25\;
      \instR197\ : \Main_r0\ port map (arg1, \main_r0_outR17\);
      zi70 <= \main_r0_outR17\;
      \instR198\ : \ZL_v695\ port map (zi4, zi5, zi69, zi70, arg1, zl_v695_out);
      \instR199\ : \Main_r1\ port map (arg1, \main_r1_outR17\);
      zi71 <= \main_r1_outR17\;
      \instR200\ : \ZL_v695\ port map (zi4, zi5, zi69, zi71, arg1, \zl_v695_outR1\);
      \instR201\ : \Main_r2\ port map (arg1, \main_r2_outR17\);
      zi72 <= \main_r2_outR17\;
      \instR202\ : \ZL_v695\ port map (zi4, zi5, zi69, zi72, arg1, \zl_v695_outR2\);
      \instR203\ : \Main_r3\ port map (arg1, \main_r3_outR17\);
      zi73 <= \main_r3_outR17\;
      \instR204\ : \ZL_v695\ port map (zi4, zi5, zi69, zi73, arg1, \zl_v695_outR3\);
      res <= rw_cond(rw_eq(zi8, std_logic_vector'(B"0")), rw_cond(rw_eq(zi1, std_logic_vector'(B"0")), rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), (zi14 & std_logic_vector'(B"001") & zi4 & zi5 & zi9 & zi13), rw_cond(rw_eq(zi15, std_logic_vector'(B"00")), zll_l_s90_out, rw_cond(rw_eq(zi15, std_logic_vector'(B"01")), zl_a65_out, rw_cond(rw_eq(zi15, std_logic_vector'(B"10")), \zl_a65_outR1\, \zll_l_s90_outR1\)))), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi18, std_logic_vector'(B"00")), zl_a99_out, rw_cond(rw_eq(zi18, std_logic_vector'(B"01")), \zl_a99_outR1\, rw_cond(rw_eq(zi18, std_logic_vector'(B"10")), \zl_a99_outR2\, \zl_a99_outR3\))), rw_cond(rw_eq(zi23, std_logic_vector'(B"00")), zl_vd134_out, rw_cond(rw_eq(zi23, std_logic_vector'(B"01")), \zl_vd134_outR1\, rw_cond(rw_eq(zi23, std_logic_vector'(B"10")), \zl_vd134_outR2\, \zl_vd134_outR3\))))), rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi25, std_logic_vector'(B"00")), zl_vd168_out, rw_cond(rw_eq(zi25, std_logic_vector'(B"01")), \zl_vd168_outR1\, rw_cond(rw_eq(zi25, std_logic_vector'(B"10")), \zl_vd168_outR2\, \zl_vd168_outR3\))), rw_cond(rw_eq(zi27, std_logic_vector'(B"00")), zl_vd204_out, rw_cond(rw_eq(zi27, std_logic_vector'(B"01")), \zl_vd204_outR1\, rw_cond(rw_eq(zi27, std_logic_vector'(B"10")), \zl_vd204_outR2\, \zl_vd204_outR3\)))), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi29, std_logic_vector'(B"00")), zl_vd238_out, rw_cond(rw_eq(zi29, std_logic_vector'(B"01")), \zl_vd238_outR1\, rw_cond(rw_eq(zi29, std_logic_vector'(B"10")), \zl_vd238_outR2\, \zl_vd238_outR3\))), rw_cond(rw_eq(zi31, std_logic_vector'(B"00")), zl_x274_out, rw_cond(rw_eq(zi31, std_logic_vector'(B"01")), \zl_x274_outR1\, rw_cond(rw_eq(zi31, std_logic_vector'(B"10")), \zl_x274_outR2\, \zl_x274_outR3\)))))), rw_cond(rw_eq(zi1, std_logic_vector'(B"0")), rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi36, std_logic_vector'(B"00")), zl_vd297_out, rw_cond(rw_eq(zi36, std_logic_vector'(B"01")), \zl_vd297_outR1\, rw_cond(rw_eq(zi36, std_logic_vector'(B"10")), \zl_vd297_outR2\, \zl_vd297_outR3\))), rw_cond(rw_eq(zi38, std_logic_vector'(B"00")), zl_vd333_out, rw_cond(rw_eq(zi38, std_logic_vector'(B"01")), \zl_vd333_outR1\, rw_cond(rw_eq(zi38, std_logic_vector'(B"10")), \zl_vd333_outR2\, \zl_vd333_outR3\)))), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi40, std_logic_vector'(B"00")), zl_vd369_out, rw_cond(rw_eq(zi40, std_logic_vector'(B"01")), \zl_vd369_outR1\, rw_cond(rw_eq(zi40, std_logic_vector'(B"10")), \zl_vd369_outR2\, \zl_vd369_outR3\))), rw_cond(rw_eq(zi42, std_logic_vector'(B"00")), zl_vd405_out, rw_cond(rw_eq(zi42, std_logic_vector'(B"01")), zll_l_s427_out, rw_cond(rw_eq(zi42, std_logic_vector'(B"10")), \zll_l_s427_outR1\, \zll_l_s427_outR2\))))), rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi4, std_logic_vector'(B"0")), rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), rw_cond(rw_eq(zi44, std_logic_vector'(B"0")), \zl__unused103_out\, zl_arm448_out), zll_l_z451_out), rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), rw_cond(rw_eq(zi46, std_logic_vector'(B"0")), \zl__unused103_outR1\, \zl_arm448_outR1\), \zll_l_z451_outR1\)), rw_cond(rw_eq(zi4, std_logic_vector'(B"0")), rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), rw_cond(rw_eq(zi48, std_logic_vector'(B"00")), zll_l_s515_out, rw_cond(rw_eq(zi48, std_logic_vector'(B"01")), \zll_l_s515_outR1\, rw_cond(rw_eq(zi48, std_logic_vector'(B"10")), \zll_l_s515_outR2\, \zll_l_s515_outR3\))), rw_cond(rw_eq(zi6, std_logic_vector'(B"0")), \zl__unused103_outR2\, rw_cond(rw_eq(zi7, std_logic_vector'(B"0")), zll_l_s537_out, \zl__unused103_outR3\))), rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), rw_cond(rw_eq(zi62, std_logic_vector'(B"00")), zll_l_s577_out, rw_cond(rw_eq(zi62, std_logic_vector'(B"01")), zl_v559_out, rw_cond(rw_eq(zi62, std_logic_vector'(B"10")), \zll_l_s577_outR1\, \zll_l_s577_outR2\))), rw_cond(rw_eq(zi64, std_logic_vector'(B"00")), \zl__unused103_outR4\, rw_cond(rw_eq(zi64, std_logic_vector'(B"01")), \zl__unused103_outR5\, rw_cond(rw_eq(zi64, std_logic_vector'(B"10")), \zl__unused103_outR6\, \zl__unused103_outR7\)))))), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi4, std_logic_vector'(B"0")), rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), rw_cond(rw_eq(zi65, std_logic_vector'(B"00")), zll_l_s616_out, rw_cond(rw_eq(zi65, std_logic_vector'(B"01")), \zll_l_s616_outR1\, rw_cond(rw_eq(zi65, std_logic_vector'(B"10")), \zll_l_s616_outR2\, \zll_l_s616_outR3\))), rw_cond(rw_eq(zi66, std_logic_vector'(B"00")), zll_l_s643_out, rw_cond(rw_eq(zi66, std_logic_vector'(B"01")), \zll_l_s643_outR1\, rw_cond(rw_eq(zi66, std_logic_vector'(B"10")), zl_v623_out, \zll_l_s643_outR2\)))), rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), rw_cond(rw_eq(zi68, std_logic_vector'(B"00")), zll_l_s664_out, rw_cond(rw_eq(zi68, std_logic_vector'(B"01")), zll_l_s666_out, rw_cond(rw_eq(zi68, std_logic_vector'(B"10")), \zll_l_s664_outR1\, \zll_l_s666_outR1\))), rw_cond(rw_eq(zi68, std_logic_vector'(B"00")), zll_l_s688_out, rw_cond(rw_eq(zi68, std_logic_vector'(B"01")), \zll_l_s688_outR1\, rw_cond(rw_eq(zi68, std_logic_vector'(B"10")), zll_l_s692_out, \zll_l_s692_outR1\))))), rw_cond(rw_eq(zi69, std_logic_vector'(B"00")), zl_v695_out, rw_cond(rw_eq(zi69, std_logic_vector'(B"01")), \zl_v695_outR1\, rw_cond(rw_eq(zi69, std_logic_vector'(B"10")), \zl_v695_outR2\, \zl_v695_outR3\)))))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm613\ is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm613\ is
component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused597\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \zl__unused597_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR3\ port map (arg2, arg1, main_setr3_out);
      \instR1\ : \ZL___unused597\ port map (arg0, arg1, main_setr3_out, \zl__unused597_out\);
      res <= \zl__unused597_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm609\ is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm609\ is
component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused597\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \zl__unused597_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR1\ port map (arg2, arg1, main_setr1_out);
      \instR1\ : \ZL___unused597\ port map (arg0, arg1, main_setr1_out, \zl__unused597_out\);
      res <= \zl__unused597_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_cFlag\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_cFlag\ is
signal c : std_logic_vector (0 downto 0);
begin
c <= arg0(51 downto 51);
      res <= c;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setWeOut\ is
port (arg0 : in std_logic_vector (17 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_setWeOut\ is
signal a_o : std_logic_vector (7 downto 0);
      signal d_o : std_logic_vector (7 downto 0);
      signal iack_o : std_logic_vector (0 downto 0);
begin
a_o <= arg0(17 downto 10);
      d_o <= arg0(9 downto 2);
      iack_o <= arg0(0 downto 0);
      res <= (a_o & d_o & arg1 & iack_o);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD297\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD297\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZL_vS298\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zl_vs298_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zl_vs298_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zl_vs298_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zl_vs298_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZL_vS298\ port map (arg0, arg2, main_r0_out, arg3, zl_vs298_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \ZL_vS298\ port map (arg0, arg2, main_r1_out, arg3, \zl_vs298_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \ZL_vS298\ port map (arg0, arg2, main_r2_out, arg3, \zl_vs298_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZL_vS298\ port map (arg0, arg2, main_r3_out, arg3, \zl_vs298_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_vs298_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \zl_vs298_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \zl_vs298_outR2\, \zl_vs298_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL___unused14\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL___unused14\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_pc\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_plusCW8$s1\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component \Main_setPC\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (7 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal \main_pluscw8$s1_out\ : std_logic_vector (8 downto 0);
      signal zi1 : std_logic_vector (8 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal zi4 : std_logic_vector (80 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi5 : std_logic_vector (17 downto 0);
begin
inst : \Main_pc\ port map (arg2, main_pc_out);
      zi0 <= main_pc_out;
      \instR1\ : \Main_plusCW8$s1\ port map (zi0, \main_pluscw8$s1_out\);
      zi1 <= \main_pluscw8$s1_out\;
      zi2 <= zi1(7 downto 0);
      zi3 <= zi2;
      \instR2\ : \Main_setPC\ port map (arg2, zi3, main_setpc_out);
      zi4 <= main_setpc_out;
      \instR3\ : \Main_outputs\ port map (zi4, main_outputs_out);
      zi5 <= main_outputs_out;
      res <= (zi5 & std_logic_vector'(B"0000") & arg0 & arg1 & zi4);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD204\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD204\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZL_vS205\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zl_vs205_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zl_vs205_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zl_vs205_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zl_vs205_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZL_vS205\ port map (arg0, arg2, main_r0_out, arg3, zl_vs205_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \ZL_vS205\ port map (arg0, arg2, main_r1_out, arg3, \zl_vs205_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \ZL_vS205\ port map (arg0, arg2, main_r2_out, arg3, \zl_vs205_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZL_vS205\ port map (arg0, arg2, main_r3_out, arg3, \zl_vs205_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_vs205_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \zl_vs205_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \zl_vs205_outR2\, \zl_vs205_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS298\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS298\ is
component \ZLL_L_vS370\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal conn : std_logic_vector (7 downto 0);
      signal zll_l_vs370_out : std_logic_vector (105 downto 0);
begin
conn <= rw_or(arg1, arg2);
      inst : \ZLL_L_vS370\ port map (arg3, arg0, conn, zll_l_vs370_out);
      res <= zll_l_vs370_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS239\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS239\ is
component \Main_cFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_L_cin240\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_Main_plusCW8$s2\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal conn : std_logic_vector (8 downto 0);
      signal \zll_main_pluscw8$s2_out\ : std_logic_vector (8 downto 0);
      signal zll_l_cin240_out : std_logic_vector (105 downto 0);
begin
inst : \Main_cFlag\ port map (arg3, main_cflag_out);
      zi0 <= main_cflag_out;
      conn <= rw_sub(rw_sub(rw_resize(arg1, 9), rw_resize(arg2, 9)), rw_resize(zi0, 9));
      \instR1\ : \ZLL_Main_plusCW8$s2\ port map (conn, \zll_main_pluscw8$s2_out\);
      \instR2\ : \ZLL_L_cin240\ port map (arg0, arg3, \zll_main_pluscw8$s2_out\, zll_l_cin240_out);
      res <= zll_l_cin240_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD134\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD134\ is
component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZL_vS135\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zl_vs135_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \zl_vs135_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zl_vs135_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zl_vs135_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \ZL_vS135\ port map (arg0, arg2, main_r0_out, arg3, zl_vs135_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \ZL_vS135\ port map (arg0, arg2, main_r1_out, arg3, \zl_vs135_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \ZL_vS135\ port map (arg0, arg2, main_r2_out, arg3, \zl_vs135_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \ZL_vS135\ port map (arg0, arg2, main_r3_out, arg3, \zl_vs135_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), zl_vs135_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \zl_vs135_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \zl_vs135_outR2\, \zl_vs135_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vD405\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vD405\ is
component \Main_mkReg\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_r0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \ZLL_L_s421\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZL_vS406\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi0 : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zll_l_s421_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal zl_vs406_out : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \zll_l_s421_outR1\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \zll_l_s421_outR2\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      zi0 <= main_mkreg_out;
      \instR1\ : \Main_r0\ port map (arg3, main_r0_out);
      \instR2\ : \ZLL_L_s421\ port map (arg3, arg2, main_r0_out, zll_l_s421_out);
      \instR3\ : \Main_r1\ port map (arg3, main_r1_out);
      zi1 <= main_r1_out;
      \instR4\ : \ZL_vS406\ port map (arg2, zi1, arg3, zl_vs406_out);
      \instR5\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR6\ : \ZLL_L_s421\ port map (arg3, arg2, main_r2_out, \zll_l_s421_outR1\);
      \instR7\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR8\ : \ZLL_L_s421\ port map (arg3, arg2, main_r3_out, \zll_l_s421_outR2\);
      res <= rw_cond(rw_eq(zi0, std_logic_vector'(B"00")), zll_l_s421_out, rw_cond(rw_eq(zi0, std_logic_vector'(B"01")), zl_vs406_out, rw_cond(rw_eq(zi0, std_logic_vector'(B"10")), \zll_l_s421_outR1\, \zll_l_s421_outR2\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS205\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS205\ is
component \Main_minusCW8$s1\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component \ZLL_L_vS205\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_minuscw8$s1_out\ : std_logic_vector (8 downto 0);
      signal zll_l_vs205_out : std_logic_vector (105 downto 0);
begin
inst : \Main_minusCW8$s1\ port map (arg1, arg2, \main_minuscw8$s1_out\);
      \instR1\ : \ZLL_L_vS205\ port map (arg3, arg0, \main_minuscw8$s1_out\, zll_l_vs205_out);
      res <= zll_l_vs205_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS370\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS370\ is
component \ZLL_L_vS370\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal conn : std_logic_vector (7 downto 0);
      signal zll_l_vs370_out : std_logic_vector (105 downto 0);
begin
conn <= rw_xor(arg1, arg2);
      inst : \ZLL_L_vS370\ port map (arg3, arg0, conn, zll_l_vs370_out);
      res <= zll_l_vs370_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_zFlag\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_zFlag\ is
signal z : std_logic_vector (0 downto 0);
begin
z <= arg0(52 downto 52);
      res <= z;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_vS169\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_vS169\ is
component \Main_cFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \ZLL_L_vS205\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \ZLL_Main_plusCW8$s2\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal conn : std_logic_vector (8 downto 0);
      signal \zll_main_pluscw8$s2_out\ : std_logic_vector (8 downto 0);
      signal zll_l_vs205_out : std_logic_vector (105 downto 0);
begin
inst : \Main_cFlag\ port map (arg3, main_cflag_out);
      zi0 <= main_cflag_out;
      conn <= rw_add(rw_add(rw_resize(arg1, 9), rw_resize(arg2, 9)), rw_resize(zi0, 9));
      \instR1\ : \ZLL_Main_plusCW8$s2\ port map (conn, \zll_main_pluscw8$s2_out\);
      \instR2\ : \ZLL_L_vS205\ port map (arg3, arg0, \zll_main_pluscw8$s2_out\, zll_l_vs205_out);
      res <= zll_l_vs205_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_arm607\ is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_arm607\ is
component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \ZL___unused597\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \zl__unused597_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR0\ port map (arg2, arg1, main_setr0_out);
      \instR1\ : \ZL___unused597\ port map (arg0, arg1, main_setr0_out, \zl__unused597_out\);
      res <= \zl__unused597_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_outputs\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_outputs\ is
signal o : std_logic_vector (17 downto 0);
begin
o <= arg0(70 downto 53);
      res <= o;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_v100\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_v100\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setAddrOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setDataOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setOutputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setWeOut\ is
      port (arg0 : in std_logic_vector (17 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \ZLL_L_s537\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (17 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi0 : std_logic_vector (17 downto 0);
      signal main_setweout_out : std_logic_vector (17 downto 0);
      signal zi1 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal zi2 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (17 downto 0);
      signal zi3 : std_logic_vector (17 downto 0);
      signal main_setdataout_out : std_logic_vector (17 downto 0);
      signal zi4 : std_logic_vector (17 downto 0);
      signal \main_setoutputs_outR1\ : std_logic_vector (80 downto 0);
      signal zi5 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (17 downto 0);
      signal zi6 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal zll_l_s537_out : std_logic_vector (105 downto 0);
begin
inst : \Main_outputs\ port map (arg2, main_outputs_out);
      zi0 <= main_outputs_out;
      \instR1\ : \Main_setWeOut\ port map (zi0, std_logic_vector'(B"1"), main_setweout_out);
      zi1 <= main_setweout_out;
      \instR2\ : \Main_setOutputs\ port map (arg2, zi1, main_setoutputs_out);
      zi2 <= main_setoutputs_out;
      \instR3\ : \Main_outputs\ port map (zi2, \main_outputs_outR1\);
      zi3 <= \main_outputs_outR1\;
      \instR4\ : \Main_setDataOut\ port map (zi3, arg1, main_setdataout_out);
      zi4 <= main_setdataout_out;
      \instR5\ : \Main_setOutputs\ port map (zi2, zi4, \main_setoutputs_outR1\);
      zi5 <= \main_setoutputs_outR1\;
      \instR6\ : \Main_outputs\ port map (zi5, \main_outputs_outR2\);
      zi6 <= \main_outputs_outR2\;
      \instR7\ : \Main_setAddrOut\ port map (zi6, arg0, main_setaddrout_out);
      \instR8\ : \ZLL_L_s537\ port map (zi5, main_setaddrout_out, zll_l_s537_out);
      res <= zll_l_s537_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZL_v559\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \ZL_v559\ is
component \ZLL_L_v6721\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal conn : std_logic_vector (7 downto 0);
      signal zll_l_v6721_out : std_logic_vector (105 downto 0);
begin
conn <= rw_not(arg1);
      inst : \ZLL_L_v6721\ port map (arg0, arg2, conn, zll_l_v6721_out);
      res <= zll_l_v6721_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_mkReg\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      res : out std_logic_vector (1 downto 0));
end entity;

architecture rtl of \Main_mkReg\ is

begin
res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"0")), rw_cond(rw_eq(arg1, std_logic_vector'(B"0")), std_logic_vector'(B"00"), std_logic_vector'(B"01")), rw_cond(rw_eq(arg1, std_logic_vector'(B"0")), std_logic_vector'(B"10"), std_logic_vector'(B"11")));
end architecture;