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
      component \main___unused7\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_d is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_loop is
      port (arg0 : in std_logic_vector (80 downto 0);
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
      signal main_loop_out : std_logic_vector (105 downto 0);
      signal main_inputs_out : std_logic_vector (9 downto 0);
      signal zi6 : std_logic_vector (9 downto 0);
      signal main_datain_out : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal zi8 : std_logic_vector (80 downto 0);
      signal \main_loop_outR1\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal zi9 : std_logic_vector (80 downto 0);
      signal \main_loop_outR2\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal zi10 : std_logic_vector (80 downto 0);
      signal \main_loop_outR3\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal zi11 : std_logic_vector (80 downto 0);
      signal \main_loop_outR4\ : std_logic_vector (105 downto 0);
      signal zi12 : std_logic_vector (0 downto 0);
      signal zi13 : std_logic_vector (0 downto 0);
      signal zi14 : std_logic_vector (1 downto 0);
      signal \main_setinputs_outR1\ : std_logic_vector (80 downto 0);
      signal zi16 : std_logic_vector (80 downto 0);
      signal \main_inputs_outR1\ : std_logic_vector (9 downto 0);
      signal zi17 : std_logic_vector (9 downto 0);
      signal \main_datain_outR1\ : std_logic_vector (7 downto 0);
      signal zi18 : std_logic_vector (7 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi19 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal zi20 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal zi21 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (17 downto 0);
      signal zi22 : std_logic_vector (17 downto 0);
      signal main_setweout_out : std_logic_vector (17 downto 0);
      signal zi23 : std_logic_vector (17 downto 0);
      signal \main_setoutputs_outR1\ : std_logic_vector (80 downto 0);
      signal zi24 : std_logic_vector (80 downto 0);
      signal \main__unused7_out\ : std_logic_vector (105 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi25 : std_logic_vector (7 downto 0);
      signal main_d_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal \main_d_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (7 downto 0);
      signal \main_d_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal \main_d_outR3\ : std_logic_vector (105 downto 0);
      signal zi29 : std_logic_vector (0 downto 0);
      signal zi30 : std_logic_vector (0 downto 0);
      signal \main_setinputs_outR2\ : std_logic_vector (80 downto 0);
      signal zi32 : std_logic_vector (80 downto 0);
      signal \main_inputs_outR2\ : std_logic_vector (9 downto 0);
      signal zi33 : std_logic_vector (9 downto 0);
      signal \main_datain_outR2\ : std_logic_vector (7 downto 0);
      signal zi34 : std_logic_vector (7 downto 0);
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi35 : std_logic_vector (1 downto 0);
      signal \main_setr0_outR1\ : std_logic_vector (80 downto 0);
      signal zi36 : std_logic_vector (80 downto 0);
      signal \main_loop_outR5\ : std_logic_vector (105 downto 0);
      signal \main_setr1_outR1\ : std_logic_vector (80 downto 0);
      signal zi37 : std_logic_vector (80 downto 0);
      signal \main_loop_outR6\ : std_logic_vector (105 downto 0);
      signal \main_setr2_outR1\ : std_logic_vector (80 downto 0);
      signal zi38 : std_logic_vector (80 downto 0);
      signal \main_loop_outR7\ : std_logic_vector (105 downto 0);
      signal \main_setr3_outR1\ : std_logic_vector (80 downto 0);
      signal zi39 : std_logic_vector (80 downto 0);
      signal \main_loop_outR8\ : std_logic_vector (105 downto 0);
      signal \main_setinputs_outR3\ : std_logic_vector (80 downto 0);
      signal zi41 : std_logic_vector (80 downto 0);
      signal \main_loop_outR9\ : std_logic_vector (105 downto 0);
      signal \main_setinputs_outR4\ : std_logic_vector (80 downto 0);
      signal zi43 : std_logic_vector (80 downto 0);
      signal \main_loop_outR10\ : std_logic_vector (105 downto 0);
      signal zres : std_logic_vector (105 downto 0);
begin
zi1 <= (\__in0\ & \__in1\ & \__in2\);
      zi2 <= \__resumption_tag\(2 downto 2);
      zi3 <= \__resumption_tag\(1 downto 0);
      inst : \Main_setInputs\ port map (\__st0\, zi1, main_setinputs_out);
      zi5 <= main_setinputs_out;
      \instR1\ : main_loop port map (zi5, main_loop_out);
      \instR2\ : \Main_inputs\ port map (zi5, main_inputs_out);
      zi6 <= main_inputs_out;
      \instR3\ : \Main_dataIn\ port map (zi6, main_datain_out);
      zi7 <= main_datain_out;
      \instR4\ : \Main_setR0\ port map (zi5, zi7, main_setr0_out);
      zi8 <= main_setr0_out;
      \instR5\ : main_loop port map (zi8, \main_loop_outR1\);
      \instR6\ : \Main_setR1\ port map (zi5, zi7, main_setr1_out);
      zi9 <= main_setr1_out;
      \instR7\ : main_loop port map (zi9, \main_loop_outR2\);
      \instR8\ : \Main_setR2\ port map (zi5, zi7, main_setr2_out);
      zi10 <= main_setr2_out;
      \instR9\ : main_loop port map (zi10, \main_loop_outR3\);
      \instR10\ : \Main_setR3\ port map (zi5, zi7, main_setr3_out);
      zi11 <= main_setr3_out;
      \instR11\ : main_loop port map (zi11, \main_loop_outR4\);
      zi12 <= \__resumption_tag\(3 downto 3);
      zi13 <= \__resumption_tag\(2 downto 2);
      zi14 <= \__resumption_tag\(1 downto 0);
      \instR12\ : \Main_setInputs\ port map (\__st0\, zi1, \main_setinputs_outR1\);
      zi16 <= \main_setinputs_outR1\;
      \instR13\ : \Main_inputs\ port map (zi16, \main_inputs_outR1\);
      zi17 <= \main_inputs_outR1\;
      \instR14\ : \Main_dataIn\ port map (zi17, \main_datain_outR1\);
      zi18 <= \main_datain_outR1\;
      \instR15\ : \Main_outputs\ port map (zi16, main_outputs_out);
      zi19 <= main_outputs_out;
      \instR16\ : \Main_setAddrOut\ port map (zi19, zi18, main_setaddrout_out);
      zi20 <= main_setaddrout_out;
      \instR17\ : \Main_setOutputs\ port map (zi16, zi20, main_setoutputs_out);
      zi21 <= main_setoutputs_out;
      \instR18\ : \Main_outputs\ port map (zi21, \main_outputs_outR1\);
      zi22 <= \main_outputs_outR1\;
      \instR19\ : \Main_setWeOut\ port map (zi22, zi13, main_setweout_out);
      zi23 <= main_setweout_out;
      \instR20\ : \Main_setOutputs\ port map (zi21, zi23, \main_setoutputs_outR1\);
      zi24 <= \main_setoutputs_outR1\;
      \instR21\ : \main___unused7\ port map (zi12, zi14, zi24, \main__unused7_out\);
      \instR22\ : \Main_r0\ port map (zi24, main_r0_out);
      zi25 <= main_r0_out;
      \instR23\ : main_d port map (zi12, zi14, zi25, zi24, main_d_out);
      \instR24\ : \Main_r1\ port map (zi24, main_r1_out);
      zi26 <= main_r1_out;
      \instR25\ : main_d port map (zi12, zi14, zi26, zi24, \main_d_outR1\);
      \instR26\ : \Main_r2\ port map (zi24, main_r2_out);
      zi27 <= main_r2_out;
      \instR27\ : main_d port map (zi12, zi14, zi27, zi24, \main_d_outR2\);
      \instR28\ : \Main_r3\ port map (zi24, main_r3_out);
      zi28 <= main_r3_out;
      \instR29\ : main_d port map (zi12, zi14, zi28, zi24, \main_d_outR3\);
      zi29 <= \__resumption_tag\(1 downto 1);
      zi30 <= \__resumption_tag\(0 downto 0);
      \instR30\ : \Main_setInputs\ port map (\__st0\, zi1, \main_setinputs_outR2\);
      zi32 <= \main_setinputs_outR2\;
      \instR31\ : \Main_inputs\ port map (zi32, \main_inputs_outR2\);
      zi33 <= \main_inputs_outR2\;
      \instR32\ : \Main_dataIn\ port map (zi33, \main_datain_outR2\);
      zi34 <= \main_datain_outR2\;
      \instR33\ : \Main_mkReg\ port map (zi29, zi30, main_mkreg_out);
      zi35 <= main_mkreg_out;
      \instR34\ : \Main_setR0\ port map (zi32, zi34, \main_setr0_outR1\);
      zi36 <= \main_setr0_outR1\;
      \instR35\ : main_loop port map (zi36, \main_loop_outR5\);
      \instR36\ : \Main_setR1\ port map (zi32, zi34, \main_setr1_outR1\);
      zi37 <= \main_setr1_outR1\;
      \instR37\ : main_loop port map (zi37, \main_loop_outR6\);
      \instR38\ : \Main_setR2\ port map (zi32, zi34, \main_setr2_outR1\);
      zi38 <= \main_setr2_outR1\;
      \instR39\ : main_loop port map (zi38, \main_loop_outR7\);
      \instR40\ : \Main_setR3\ port map (zi32, zi34, \main_setr3_outR1\);
      zi39 <= \main_setr3_outR1\;
      \instR41\ : main_loop port map (zi39, \main_loop_outR8\);
      \instR42\ : \Main_setInputs\ port map (\__st0\, zi1, \main_setinputs_outR3\);
      zi41 <= \main_setinputs_outR3\;
      \instR43\ : main_loop port map (zi41, \main_loop_outR9\);
      \instR44\ : \Main_setInputs\ port map (\__st0\, zi1, \main_setinputs_outR4\);
      zi43 <= \main_setinputs_outR4\;
      \instR45\ : main_loop port map (zi43, \main_loop_outR10\);
      zres <= rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"000")), rw_cond(rw_not(zi2), main_loop_out, rw_cond(rw_eq(zi3, std_logic_vector'(B"00")), \main_loop_outR1\, rw_cond(rw_eq(zi3, std_logic_vector'(B"01")), \main_loop_outR2\, rw_cond(rw_eq(zi3, std_logic_vector'(B"10")), \main_loop_outR3\, \main_loop_outR4\)))), rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"001")), rw_cond(rw_not(zi13), \main__unused7_out\, rw_cond(rw_eq(zi14, std_logic_vector'(B"00")), main_d_out, rw_cond(rw_eq(zi14, std_logic_vector'(B"01")), \main_d_outR1\, rw_cond(rw_eq(zi14, std_logic_vector'(B"10")), \main_d_outR2\, \main_d_outR3\)))), rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"010")), rw_cond(rw_eq(zi35, std_logic_vector'(B"00")), \main_loop_outR5\, rw_cond(rw_eq(zi35, std_logic_vector'(B"01")), \main_loop_outR6\, rw_cond(rw_eq(zi35, std_logic_vector'(B"10")), \main_loop_outR7\, \main_loop_outR8\))), rw_cond(rw_eq(\__resumption_tag\(6 downto 4), std_logic_vector'(B"011")), \main_loop_outR9\, \main_loop_outR10\))));
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
entity \main___unused7\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused7\ is
component \Main_outputs\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_pc\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_plusCW8$sMain__oneW8__False__Bool\ is
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
      signal \main_pluscw8$smain_onew8_false_bool_out\ : std_logic_vector (8 downto 0);
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
      \instR1\ : \Main_plusCW8$sMain__oneW8__False__Bool\ port map (zi0, \main_pluscw8$smain_onew8_false_bool_out\);
      zi1 <= \main_pluscw8$smain_onew8_false_bool_out\;
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
entity main_d is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_d is
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
      component \main___unused7\ is
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
      signal zi2 : std_logic_vector (80 downto 0);
      signal \main__unused7_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_outputs\ port map (arg3, main_outputs_out);
      zi0 <= main_outputs_out;
      \instR1\ : \Main_setDataOut\ port map (zi0, arg2, main_setdataout_out);
      zi1 <= main_setdataout_out;
      \instR2\ : \Main_setOutputs\ port map (arg3, zi1, main_setoutputs_out);
      zi2 <= main_setoutputs_out;
      \instR3\ : \main___unused7\ port map (arg0, arg1, zi2, \main__unused7_out\);
      res <= \main__unused7_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_a2 is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_a2 is
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
entity main_v2 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v2 is
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
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
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
      signal zi7 : std_logic_vector (17 downto 0);
      signal \main_setoutputs_outR2\ : std_logic_vector (80 downto 0);
      signal zi8 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
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
      zi7 <= main_setaddrout_out;
      \instR8\ : \Main_setOutputs\ port map (zi5, zi7, \main_setoutputs_outR2\);
      zi8 <= \main_setoutputs_outR2\;
      \instR9\ : main_zzz port map (zi8, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_a3 is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_a3 is
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
      component main_v2 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal za : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal main_v2_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal \main_v2_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal \main_v2_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal \main_v2_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      za <= main_mkreg_out;
      \instR1\ : \Main_r0\ port map (arg3, main_r0_out);
      zi0 <= main_r0_out;
      \instR2\ : main_v2 port map (arg2, zi0, arg3, main_v2_out);
      \instR3\ : \Main_r1\ port map (arg3, main_r1_out);
      zi1 <= main_r1_out;
      \instR4\ : main_v2 port map (arg2, zi1, arg3, \main_v2_outR1\);
      \instR5\ : \Main_r2\ port map (arg3, main_r2_out);
      zi2 <= main_r2_out;
      \instR6\ : main_v2 port map (arg2, zi2, arg3, \main_v2_outR2\);
      \instR7\ : \Main_r3\ port map (arg3, main_r3_out);
      zi3 <= main_r3_out;
      \instR8\ : main_v2 port map (arg2, zi3, arg3, \main_v2_outR3\);
      res <= rw_cond(rw_eq(za, std_logic_vector'(B"00")), main_v2_out, rw_cond(rw_eq(za, std_logic_vector'(B"01")), \main_v2_outR1\, rw_cond(rw_eq(za, std_logic_vector'(B"10")), \main_v2_outR2\, \main_v2_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused17\ is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused17\ is
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
entity main_s63 is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (8 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_s63 is
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
      component \main___unused17\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal x : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal s01 : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal zi2 : std_logic_vector (80 downto 0);
      signal \main__unused17_out\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal zi3 : std_logic_vector (80 downto 0);
      signal \main__unused17_outR1\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal zi4 : std_logic_vector (80 downto 0);
      signal \main__unused17_outR2\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal zi5 : std_logic_vector (80 downto 0);
      signal \main__unused17_outR3\ : std_logic_vector (105 downto 0);
begin
x <= arg1(8 downto 8);
      inst : \Main_setCFlag\ port map (arg2, x, main_setcflag_out);
      s01 <= main_setcflag_out;
      zi0 <= arg1(7 downto 0);
      zi1 <= zi0;
      \instR1\ : \Main_setR0\ port map (s01, zi1, main_setr0_out);
      zi2 <= main_setr0_out;
      \instR2\ : \main___unused17\ port map (zi2, \main__unused17_out\);
      \instR3\ : \Main_setR1\ port map (s01, zi1, main_setr1_out);
      zi3 <= main_setr1_out;
      \instR4\ : \main___unused17\ port map (zi3, \main__unused17_outR1\);
      \instR5\ : \Main_setR2\ port map (s01, zi1, main_setr2_out);
      zi4 <= main_setr2_out;
      \instR6\ : \main___unused17\ port map (zi4, \main__unused17_outR2\);
      \instR7\ : \Main_setR3\ port map (s01, zi1, main_setr3_out);
      zi5 <= main_setr3_out;
      \instR8\ : \main___unused17\ port map (zi5, \main__unused17_outR3\);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), \main__unused17_out\, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), \main__unused17_outR1\, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), \main__unused17_outR2\, \main__unused17_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS\ is
component main_s63 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (8 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zi0 : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
zi0 <= rw_add(rw_add(rw_resize(arg1, 9), rw_resize(arg2, 9)), std_logic_vector'(B"000000000"));
      p <= (zi0(8 downto 8) & rw_resize(zi0, 8));
      inst : main_s63 port map (arg0, p, arg3, arg3, main_s63_out);
      res <= main_s63_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD\ is
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
      component \main_vS\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \main_vs_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \main_vs_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \main_vs_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \main_vS\ port map (arg0, arg2, main_r0_out, arg3, main_vs_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \main_vS\ port map (arg0, arg2, main_r1_out, arg3, \main_vs_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \main_vS\ port map (arg0, arg2, main_r2_out, arg3, \main_vs_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \main_vS\ port map (arg0, arg2, main_r3_out, arg3, \main_vs_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), main_vs_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \main_vs_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \main_vs_outR2\, \main_vs_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS2\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS2\ is
component \Main_cFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component main_s63 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (8 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (8 downto 0);
      signal zi2 : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
inst : \Main_cFlag\ port map (arg3, main_cflag_out);
      zi0 <= main_cflag_out;
      zi1 <= rw_add(rw_add(rw_resize(arg1, 9), rw_resize(arg2, 9)), rw_resize(zi0, 9));
      zi2 <= (zi1(8 downto 8) & rw_resize(zi1, 8));
      \instR1\ : main_s63 port map (arg0, zi2, arg3, arg3, main_s63_out);
      res <= main_s63_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD2\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD2\ is
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
      component \main_vS2\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs2_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \main_vs2_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \main_vs2_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \main_vs2_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \main_vS2\ port map (arg0, arg2, main_r0_out, arg3, main_vs2_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \main_vS2\ port map (arg0, arg2, main_r1_out, arg3, \main_vs2_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \main_vS2\ port map (arg0, arg2, main_r2_out, arg3, \main_vs2_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \main_vS2\ port map (arg0, arg2, main_r3_out, arg3, \main_vs2_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), main_vs2_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \main_vs2_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \main_vs2_outR2\, \main_vs2_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS3\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS3\ is
component \Main_minusCW8$sFalse__Bool\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component main_s63 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (8 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_minuscw8$sfalse_bool_out\ : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
inst : \Main_minusCW8$sFalse__Bool\ port map (arg1, arg2, \main_minuscw8$sfalse_bool_out\);
      p <= \main_minuscw8$sfalse_bool_out\;
      \instR1\ : main_s63 port map (arg0, p, arg3, arg3, main_s63_out);
      res <= main_s63_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD3\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD3\ is
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
      component \main_vS3\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs3_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \main_vs3_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \main_vs3_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \main_vs3_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \main_vS3\ port map (arg0, arg2, main_r0_out, arg3, main_vs3_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \main_vS3\ port map (arg0, arg2, main_r1_out, arg3, \main_vs3_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \main_vS3\ port map (arg0, arg2, main_r2_out, arg3, \main_vs3_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \main_vS3\ port map (arg0, arg2, main_r3_out, arg3, \main_vs3_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), main_vs3_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \main_vs3_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \main_vs3_outR2\, \main_vs3_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS4\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS4\ is
component \Main_cFlag\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component main_s63 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (8 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (8 downto 0);
      signal zi2 : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
inst : \Main_cFlag\ port map (arg3, main_cflag_out);
      zi0 <= main_cflag_out;
      zi1 <= rw_sub(rw_sub(rw_resize(arg1, 9), rw_resize(arg2, 9)), rw_resize(zi0, 9));
      zi2 <= (zi1(8 downto 8) & rw_resize(zi1, 8));
      \instR1\ : main_s63 port map (arg0, zi2, arg3, arg3, main_s63_out);
      res <= main_s63_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD4\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD4\ is
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
      component \main_vS4\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs4_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \main_vs4_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \main_vs4_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \main_vs4_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \main_vS4\ port map (arg0, arg2, main_r0_out, arg3, main_vs4_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \main_vS4\ port map (arg0, arg2, main_r1_out, arg3, \main_vs4_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \main_vS4\ port map (arg0, arg2, main_r2_out, arg3, \main_vs4_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \main_vS4\ port map (arg0, arg2, main_r3_out, arg3, \main_vs4_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), main_vs4_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \main_vs4_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \main_vs4_outR2\, \main_vs4_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_zzz is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_zzz is
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
entity main_x2 is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_x2 is
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
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal za : std_logic_vector (1 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal zi1 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR1\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal zi2 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR2\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal zi3 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      za <= main_mkreg_out;
      \instR1\ : \Main_setR0\ port map (arg3, arg2, main_setr0_out);
      zi0 <= main_setr0_out;
      \instR2\ : main_zzz port map (zi0, main_zzz_out);
      \instR3\ : \Main_setR1\ port map (arg3, arg2, main_setr1_out);
      zi1 <= main_setr1_out;
      \instR4\ : main_zzz port map (zi1, \main_zzz_outR1\);
      \instR5\ : \Main_setR2\ port map (arg3, arg2, main_setr2_out);
      zi2 <= main_setr2_out;
      \instR6\ : main_zzz port map (zi2, \main_zzz_outR2\);
      \instR7\ : \Main_setR3\ port map (arg3, arg2, main_setr3_out);
      zi3 <= main_setr3_out;
      \instR8\ : main_zzz port map (zi3, \main_zzz_outR3\);
      res <= rw_cond(rw_eq(za, std_logic_vector'(B"00")), main_zzz_out, rw_cond(rw_eq(za, std_logic_vector'(B"01")), \main_zzz_outR1\, rw_cond(rw_eq(za, std_logic_vector'(B"10")), \main_zzz_outR2\, \main_zzz_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused24\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused24\ is
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
      component \main___unused17\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal zi1 : std_logic_vector (80 downto 0);
      signal \main__unused17_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setCFlag\ port map (arg1, std_logic_vector'(B"0"), main_setcflag_out);
      zi0 <= main_setcflag_out;
      conn <= rw_eq(arg0, std_logic_vector'(B"00000000"));
      \instR1\ : \Main_setZFlag\ port map (zi0, conn, main_setzflag_out);
      zi1 <= main_setzflag_out;
      \instR2\ : \main___unused17\ port map (zi1, \main__unused17_out\);
      res <= \main__unused17_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm90 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm90 is
component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR0\ port map (arg1, arg0, main_setr0_out);
      zi0 <= main_setr0_out;
      \instR1\ : \main___unused24\ port map (arg0, zi0, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm91 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm91 is
component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR1\ port map (arg1, arg0, main_setr1_out);
      zi0 <= main_setr1_out;
      \instR1\ : \main___unused24\ port map (arg0, zi0, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm92 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm92 is
component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR2\ port map (arg1, arg0, main_setr2_out);
      zi0 <= main_setr2_out;
      \instR1\ : \main___unused24\ port map (arg0, zi0, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm93 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm93 is
component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR3\ port map (arg1, arg0, main_setr3_out);
      zi0 <= main_setr3_out;
      \instR1\ : \main___unused24\ port map (arg0, zi0, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS5\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS5\ is
component main_arm90 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm91 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm92 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm93 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \vd$\ : std_logic_vector (7 downto 0);
      signal main_arm90_out : std_logic_vector (105 downto 0);
      signal main_arm91_out : std_logic_vector (105 downto 0);
      signal main_arm92_out : std_logic_vector (105 downto 0);
      signal main_arm93_out : std_logic_vector (105 downto 0);
begin
\vd$\ <= rw_or(arg1, arg2);
      inst : main_arm90 port map (\vd$\, arg3, main_arm90_out);
      \instR1\ : main_arm91 port map (\vd$\, arg3, main_arm91_out);
      \instR2\ : main_arm92 port map (\vd$\, arg3, main_arm92_out);
      \instR3\ : main_arm93 port map (\vd$\, arg3, main_arm93_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm90_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm91_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm92_out, main_arm93_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD5\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD5\ is
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
      component \main_vS5\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs5_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \main_vs5_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \main_vs5_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \main_vs5_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \main_vS5\ port map (arg0, arg2, main_r0_out, arg3, main_vs5_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \main_vS5\ port map (arg0, arg2, main_r1_out, arg3, \main_vs5_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \main_vS5\ port map (arg0, arg2, main_r2_out, arg3, \main_vs5_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \main_vS5\ port map (arg0, arg2, main_r3_out, arg3, \main_vs5_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), main_vs5_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \main_vs5_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \main_vs5_outR2\, \main_vs5_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS6\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS6\ is
component main_arm90 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm91 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm92 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm93 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \vd$\ : std_logic_vector (7 downto 0);
      signal main_arm90_out : std_logic_vector (105 downto 0);
      signal main_arm91_out : std_logic_vector (105 downto 0);
      signal main_arm92_out : std_logic_vector (105 downto 0);
      signal main_arm93_out : std_logic_vector (105 downto 0);
begin
\vd$\ <= rw_and(arg1, arg2);
      inst : main_arm90 port map (\vd$\, arg3, main_arm90_out);
      \instR1\ : main_arm91 port map (\vd$\, arg3, main_arm91_out);
      \instR2\ : main_arm92 port map (\vd$\, arg3, main_arm92_out);
      \instR3\ : main_arm93 port map (\vd$\, arg3, main_arm93_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm90_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm91_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm92_out, main_arm93_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD6\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD6\ is
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
      component \main_vS6\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs6_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \main_vs6_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \main_vs6_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \main_vs6_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \main_vS6\ port map (arg0, arg2, main_r0_out, arg3, main_vs6_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \main_vS6\ port map (arg0, arg2, main_r1_out, arg3, \main_vs6_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \main_vS6\ port map (arg0, arg2, main_r2_out, arg3, \main_vs6_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \main_vS6\ port map (arg0, arg2, main_r3_out, arg3, \main_vs6_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), main_vs6_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \main_vs6_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \main_vs6_outR2\, \main_vs6_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS7\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS7\ is
component main_arm90 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm91 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm92 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm93 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \vd$\ : std_logic_vector (7 downto 0);
      signal main_arm90_out : std_logic_vector (105 downto 0);
      signal main_arm91_out : std_logic_vector (105 downto 0);
      signal main_arm92_out : std_logic_vector (105 downto 0);
      signal main_arm93_out : std_logic_vector (105 downto 0);
begin
\vd$\ <= rw_xor(arg1, arg2);
      inst : main_arm90 port map (\vd$\, arg3, main_arm90_out);
      \instR1\ : main_arm91 port map (\vd$\, arg3, main_arm91_out);
      \instR2\ : main_arm92 port map (\vd$\, arg3, main_arm92_out);
      \instR3\ : main_arm93 port map (\vd$\, arg3, main_arm93_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm90_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm91_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm92_out, main_arm93_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD7\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (1 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD7\ is
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
      component \main_vS7\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs7_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal \main_vs7_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal \main_vs7_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal \main_vs7_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_r0\ port map (arg3, main_r0_out);
      \instR1\ : \main_vS7\ port map (arg0, arg2, main_r0_out, arg3, main_vs7_out);
      \instR2\ : \Main_r1\ port map (arg3, main_r1_out);
      \instR3\ : \main_vS7\ port map (arg0, arg2, main_r1_out, arg3, \main_vs7_outR1\);
      \instR4\ : \Main_r2\ port map (arg3, main_r2_out);
      \instR5\ : \main_vS7\ port map (arg0, arg2, main_r2_out, arg3, \main_vs7_outR2\);
      \instR6\ : \Main_r3\ port map (arg3, main_r3_out);
      \instR7\ : \main_vS7\ port map (arg0, arg2, main_r3_out, arg3, \main_vs7_outR3\);
      res <= rw_cond(rw_eq(arg1, std_logic_vector'(B"00")), main_vs7_out, rw_cond(rw_eq(arg1, std_logic_vector'(B"01")), \main_vs7_outR1\, rw_cond(rw_eq(arg1, std_logic_vector'(B"10")), \main_vs7_outR2\, \main_vs7_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS8\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS8\ is
component \Main_minusCW8$sFalse__Bool\ is
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
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_minuscw8$sfalse_bool_out\ : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi2 : std_logic_vector (80 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal zi4 : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal zi5 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
inst : \Main_minusCW8$sFalse__Bool\ port map (arg0, arg1, \main_minuscw8$sfalse_bool_out\);
      p <= \main_minuscw8$sfalse_bool_out\;
      zi0 <= p(8 downto 8);
      zi1 <= zi0;
      \instR1\ : \Main_setCFlag\ port map (arg2, zi1, main_setcflag_out);
      zi2 <= main_setcflag_out;
      zi3 <= p(7 downto 0);
      zi4 <= zi3;
      conn <= rw_eq(zi4, std_logic_vector'(B"00000000"));
      \instR2\ : \Main_setZFlag\ port map (zi2, conn, main_setzflag_out);
      zi5 <= main_setzflag_out;
      \instR3\ : main_zzz port map (zi5, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD8\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (7 downto 0);
      arg3 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD8\ is
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
      component \main_vS8\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal za : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal main_vs8_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal \main_vs8_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal \main_vs8_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal \main_vs8_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      za <= main_mkreg_out;
      \instR1\ : \Main_r0\ port map (arg3, main_r0_out);
      zi0 <= main_r0_out;
      \instR2\ : \main_vS8\ port map (arg2, zi0, arg3, main_vs8_out);
      \instR3\ : \Main_r1\ port map (arg3, main_r1_out);
      zi1 <= main_r1_out;
      \instR4\ : \main_vS8\ port map (arg2, zi1, arg3, \main_vs8_outR1\);
      \instR5\ : \Main_r2\ port map (arg3, main_r2_out);
      zi2 <= main_r2_out;
      \instR6\ : \main_vS8\ port map (arg2, zi2, arg3, \main_vs8_outR2\);
      \instR7\ : \Main_r3\ port map (arg3, main_r3_out);
      zi3 <= main_r3_out;
      \instR8\ : \main_vS8\ port map (arg2, zi3, arg3, \main_vs8_outR3\);
      res <= rw_cond(rw_eq(za, std_logic_vector'(B"00")), main_vs8_out, rw_cond(rw_eq(za, std_logic_vector'(B"01")), \main_vs8_outR1\, rw_cond(rw_eq(za, std_logic_vector'(B"10")), \main_vs8_outR2\, \main_vs8_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_pc3 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_pc3 is
component \Main_setPC\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
inst : \Main_setPC\ port map (arg1, arg0, main_setpc_out);
      zi0 <= main_setpc_out;
      \instR1\ : main_zzz port map (zi0, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm142 is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm142 is
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
      component main_pc3 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal za : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi0 : std_logic_vector (7 downto 0);
      signal main_pc3_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal \main_pc3_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi2 : std_logic_vector (7 downto 0);
      signal \main_pc3_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi3 : std_logic_vector (7 downto 0);
      signal \main_pc3_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_mkReg\ port map (arg0, arg1, main_mkreg_out);
      za <= main_mkreg_out;
      \instR1\ : \Main_r0\ port map (arg2, main_r0_out);
      zi0 <= main_r0_out;
      \instR2\ : main_pc3 port map (zi0, arg2, main_pc3_out);
      \instR3\ : \Main_r1\ port map (arg2, main_r1_out);
      zi1 <= main_r1_out;
      \instR4\ : main_pc3 port map (zi1, arg2, \main_pc3_outR1\);
      \instR5\ : \Main_r2\ port map (arg2, main_r2_out);
      zi2 <= main_r2_out;
      \instR6\ : main_pc3 port map (zi2, arg2, \main_pc3_outR2\);
      \instR7\ : \Main_r3\ port map (arg2, main_r3_out);
      zi3 <= main_r3_out;
      \instR8\ : main_pc3 port map (zi3, arg2, \main_pc3_outR3\);
      res <= rw_cond(rw_eq(za, std_logic_vector'(B"00")), main_pc3_out, rw_cond(rw_eq(za, std_logic_vector'(B"01")), \main_pc3_outR1\, rw_cond(rw_eq(za, std_logic_vector'(B"10")), \main_pc3_outR2\, \main_pc3_outR3\)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_x3 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_x3 is
component \Main_setPC\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
inst : \Main_setPC\ port map (arg1, arg0, main_setpc_out);
      zi0 <= main_setpc_out;
      \instR1\ : main_zzz port map (zi0, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm170 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm170 is
component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
inst : \Main_setR0\ port map (arg1, arg0, main_setr0_out);
      zi0 <= main_setr0_out;
      \instR1\ : main_zzz port map (zi0, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm171 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm171 is
component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
inst : \Main_setR1\ port map (arg1, arg0, main_setr1_out);
      zi0 <= main_setr1_out;
      \instR1\ : main_zzz port map (zi0, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm172 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm172 is
component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
inst : \Main_setR2\ port map (arg1, arg0, main_setr2_out);
      zi0 <= main_setr2_out;
      \instR1\ : main_zzz port map (zi0, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm173 is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm173 is
component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
inst : \Main_setR3\ port map (arg1, arg0, main_setr3_out);
      zi0 <= main_setr3_out;
      \instR1\ : main_zzz port map (zi0, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v3 is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v3 is
component main_arm170 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm171 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm172 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm173 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal v1 : std_logic_vector (7 downto 0);
      signal main_arm170_out : std_logic_vector (105 downto 0);
      signal main_arm171_out : std_logic_vector (105 downto 0);
      signal main_arm172_out : std_logic_vector (105 downto 0);
      signal main_arm173_out : std_logic_vector (105 downto 0);
begin
v1 <= rw_not(arg1);
      inst : main_arm170 port map (v1, arg2, main_arm170_out);
      \instR1\ : main_arm171 port map (v1, arg2, main_arm171_out);
      \instR2\ : main_arm172 port map (v1, arg2, main_arm172_out);
      \instR3\ : main_arm173 port map (v1, arg2, main_arm173_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm170_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm171_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm172_out, main_arm173_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused44\ is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused44\ is
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
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi2 : std_logic_vector (80 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal zi3 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
zi0 <= arg0(8 downto 8);
      zi1 <= zi0;
      inst : \Main_setCFlag\ port map (arg2, zi1, main_setcflag_out);
      zi2 <= main_setcflag_out;
      conn <= rw_eq(arg1, std_logic_vector'(B"00000000"));
      \instR1\ : \Main_setZFlag\ port map (zi2, conn, main_setzflag_out);
      zi3 <= main_setzflag_out;
      \instR2\ : main_zzz port map (zi3, main_zzz_out);
      res <= main_zzz_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm184 is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm184 is
component \Main_setR0\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR0\ port map (arg2, arg1, main_setr0_out);
      zi0 <= main_setr0_out;
      \instR1\ : \main___unused44\ port map (arg0, arg1, zi0, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm185 is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm185 is
component \Main_setR1\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR1\ port map (arg2, arg1, main_setr1_out);
      zi0 <= main_setr1_out;
      \instR1\ : \main___unused44\ port map (arg0, arg1, zi0, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm186 is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm186 is
component \Main_setR2\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR2\ port map (arg2, arg1, main_setr2_out);
      zi0 <= main_setr2_out;
      \instR1\ : \main___unused44\ port map (arg0, arg1, zi0, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm187 is
port (arg0 : in std_logic_vector (8 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm187 is
component \Main_setR3\ is
      port (arg0 : in std_logic_vector (80 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal zi0 : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
inst : \Main_setR3\ port map (arg2, arg1, main_setr3_out);
      zi0 <= main_setr3_out;
      \instR1\ : \main___unused44\ port map (arg0, arg1, zi0, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v4 is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v4 is
component \Main_plusCW8$sMain__oneW8__False__Bool\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
      end component;
      component main_arm184 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm185 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm186 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm187 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_pluscw8$smain_onew8_false_bool_out\ : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal \v$\ : std_logic_vector (7 downto 0);
      signal main_arm184_out : std_logic_vector (105 downto 0);
      signal main_arm185_out : std_logic_vector (105 downto 0);
      signal main_arm186_out : std_logic_vector (105 downto 0);
      signal main_arm187_out : std_logic_vector (105 downto 0);
begin
inst : \Main_plusCW8$sMain__oneW8__False__Bool\ port map (arg1, \main_pluscw8$smain_onew8_false_bool_out\);
      p <= \main_pluscw8$smain_onew8_false_bool_out\;
      y <= p(7 downto 0);
      \v$\ <= y;
      \instR1\ : main_arm184 port map (p, \v$\, arg2, main_arm184_out);
      \instR2\ : main_arm185 port map (p, \v$\, arg2, main_arm185_out);
      \instR3\ : main_arm186 port map (p, \v$\, arg2, main_arm186_out);
      \instR4\ : main_arm187 port map (p, \v$\, arg2, main_arm187_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm184_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm185_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm186_out, main_arm187_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v5 is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v5 is
component main_arm184 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm185 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm186 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm187 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal zi0 : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal \v$\ : std_logic_vector (7 downto 0);
      signal main_arm184_out : std_logic_vector (105 downto 0);
      signal main_arm185_out : std_logic_vector (105 downto 0);
      signal main_arm186_out : std_logic_vector (105 downto 0);
      signal main_arm187_out : std_logic_vector (105 downto 0);
begin
zi0 <= rw_sub(rw_sub(rw_resize(arg1, 9), std_logic_vector'(B"000000001")), std_logic_vector'(B"000000000"));
      p <= (zi0(8 downto 8) & rw_resize(zi0, 8));
      y <= p(7 downto 0);
      \v$\ <= y;
      inst : main_arm184 port map (p, \v$\, arg2, main_arm184_out);
      \instR1\ : main_arm185 port map (p, \v$\, arg2, main_arm185_out);
      \instR2\ : main_arm186 port map (p, \v$\, arg2, main_arm186_out);
      \instR3\ : main_arm187 port map (p, \v$\, arg2, main_arm187_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm184_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm185_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm186_out, main_arm187_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v6 is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v6 is
component main_arm170 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm171 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm172 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm173 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal v1 : std_logic_vector (7 downto 0);
      signal main_arm170_out : std_logic_vector (105 downto 0);
      signal main_arm171_out : std_logic_vector (105 downto 0);
      signal main_arm172_out : std_logic_vector (105 downto 0);
      signal main_arm173_out : std_logic_vector (105 downto 0);
begin
v1 <= rw_or(rw_shiftl(arg1, std_logic_vector'(B"00000001")), rw_shiftr(arg1, std_logic_vector'(B"00000111")));
      inst : main_arm170 port map (v1, arg2, main_arm170_out);
      \instR1\ : main_arm171 port map (v1, arg2, main_arm171_out);
      \instR2\ : main_arm172 port map (v1, arg2, main_arm172_out);
      \instR3\ : main_arm173 port map (v1, arg2, main_arm173_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm170_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm171_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm172_out, main_arm173_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v7 is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      arg2 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v7 is
component main_arm170 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm171 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm172 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm173 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal v1 : std_logic_vector (7 downto 0);
      signal main_arm170_out : std_logic_vector (105 downto 0);
      signal main_arm171_out : std_logic_vector (105 downto 0);
      signal main_arm172_out : std_logic_vector (105 downto 0);
      signal main_arm173_out : std_logic_vector (105 downto 0);
begin
v1 <= rw_or(rw_shiftr(arg1, std_logic_vector'(B"00000001")), rw_shiftl(arg1, std_logic_vector'(B"00000111")));
      inst : main_arm170 port map (v1, arg2, main_arm170_out);
      \instR1\ : main_arm171 port map (v1, arg2, main_arm171_out);
      \instR2\ : main_arm172 port map (v1, arg2, main_arm172_out);
      \instR3\ : main_arm173 port map (v1, arg2, main_arm173_out);
      res <= rw_cond(rw_eq(arg0, std_logic_vector'(B"00")), main_arm170_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"01")), main_arm171_out, rw_cond(rw_eq(arg0, std_logic_vector'(B"10")), main_arm172_out, main_arm173_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v8 is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (0 downto 0);
      arg2 : in std_logic_vector (1 downto 0);
      arg3 : in std_logic_vector (7 downto 0);
      arg4 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v8 is
component \Main_lsbW8\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_msbW8\ is
      port (arg0 : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component main_arm184 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm185 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm186 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm187 is
      port (arg0 : in std_logic_vector (8 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_msbw8_out : std_logic_vector (0 downto 0);
      signal za : std_logic_vector (0 downto 0);
      signal \main_msbw8_outR1\ : std_logic_vector (0 downto 0);
      signal za1 : std_logic_vector (8 downto 0);
      signal \main_msbw8_outR2\ : std_logic_vector (0 downto 0);
      signal \zaR1\ : std_logic_vector (8 downto 0);
      signal \zaR2\ : std_logic_vector (8 downto 0);
      signal main_lsbw8_out : std_logic_vector (0 downto 0);
      signal \zaR3\ : std_logic_vector (0 downto 0);
      signal \main_lsbw8_outR1\ : std_logic_vector (0 downto 0);
      signal \za1R1\ : std_logic_vector (8 downto 0);
      signal \main_lsbw8_outR2\ : std_logic_vector (0 downto 0);
      signal \zaR4\ : std_logic_vector (8 downto 0);
      signal \zaR5\ : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal \v$\ : std_logic_vector (7 downto 0);
      signal main_arm184_out : std_logic_vector (105 downto 0);
      signal main_arm185_out : std_logic_vector (105 downto 0);
      signal main_arm186_out : std_logic_vector (105 downto 0);
      signal main_arm187_out : std_logic_vector (105 downto 0);
begin
inst : \Main_msbW8\ port map (arg3, main_msbw8_out);
      za <= main_msbw8_out;
      \instR1\ : \Main_msbW8\ port map (arg3, \main_msbw8_outR1\);
      za1 <= (\main_msbw8_outR1\ & rw_or(rw_shiftl(arg3, std_logic_vector'(B"00000001")), rw_resize(za, 8)));
      \instR2\ : \Main_msbW8\ port map (arg3, \main_msbw8_outR2\);
      \zaR1\ <= (\main_msbw8_outR2\ & rw_or(rw_shiftl(arg3, std_logic_vector'(B"00000001")), std_logic_vector'(B"00000000")));
      \zaR2\ <= rw_cond(rw_not(arg0), za1, \zaR1\);
      \instR3\ : \Main_lsbW8\ port map (arg3, main_lsbw8_out);
      \zaR3\ <= main_lsbw8_out;
      \instR4\ : \Main_lsbW8\ port map (arg3, \main_lsbw8_outR1\);
      \za1R1\ <= (\main_lsbw8_outR1\ & rw_or(rw_shiftr(arg3, std_logic_vector'(B"00000001")), rw_shiftl(rw_resize(\zaR3\, 8), std_logic_vector'(B"00000111"))));
      \instR5\ : \Main_lsbW8\ port map (arg3, \main_lsbw8_outR2\);
      \zaR4\ <= (\main_lsbw8_outR2\ & rw_or(rw_shiftr(arg3, std_logic_vector'(B"00000001")), std_logic_vector'(B"00000000")));
      \zaR5\ <= rw_cond(rw_not(arg0), \za1R1\, \zaR4\);
      p <= rw_cond(rw_not(arg1), \zaR2\, \zaR5\);
      y <= p(7 downto 0);
      \v$\ <= y;
      \instR6\ : main_arm184 port map (p, \v$\, arg4, main_arm184_out);
      \instR7\ : main_arm185 port map (p, \v$\, arg4, main_arm185_out);
      \instR8\ : main_arm186 port map (p, \v$\, arg4, main_arm186_out);
      \instR9\ : main_arm187 port map (p, \v$\, arg4, main_arm187_out);
      res <= rw_cond(rw_eq(arg2, std_logic_vector'(B"00")), main_arm184_out, rw_cond(rw_eq(arg2, std_logic_vector'(B"01")), main_arm185_out, rw_cond(rw_eq(arg2, std_logic_vector'(B"10")), main_arm186_out, main_arm187_out)));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_$fail\ is
port (arg0 : in std_logic_vector (9 downto 0);
      arg1 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_$fail\ is
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
      component main_a2 is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_a3 is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm142 is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_v3 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_v4 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_v5 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_v6 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_v7 is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (7 downto 0);
            arg2 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_v8 is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (1 downto 0);
            arg3 : in std_logic_vector (7 downto 0);
            arg4 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD2\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD3\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD4\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD5\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD6\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD7\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (1 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD8\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_x2 is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (0 downto 0);
            arg2 : in std_logic_vector (7 downto 0);
            arg3 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_x3 is
      port (arg0 : in std_logic_vector (7 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      signal main_datain_out : std_logic_vector (7 downto 0);
      signal i : std_logic_vector (7 downto 0);
      signal zds1 : std_logic_vector (0 downto 0);
      signal zds2 : std_logic_vector (0 downto 0);
      signal zds3 : std_logic_vector (0 downto 0);
      signal ren : std_logic_vector (0 downto 0);
      signal wen : std_logic_vector (0 downto 0);
      signal b0 : std_logic_vector (0 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal za : std_logic_vector (0 downto 0);
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zi0 : std_logic_vector (1 downto 0);
      signal main_pc_out : std_logic_vector (7 downto 0);
      signal zi1 : std_logic_vector (7 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal zi2 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal zi3 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal zi4 : std_logic_vector (80 downto 0);
      signal \main_outputs_outR1\ : std_logic_vector (17 downto 0);
      signal zi5 : std_logic_vector (17 downto 0);
      signal \main_mkreg_outR1\ : std_logic_vector (1 downto 0);
      signal zi6 : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal zi7 : std_logic_vector (7 downto 0);
      signal main_a2_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal \main_a2_outR1\ : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (7 downto 0);
      signal \main_a2_outR2\ : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal zi10 : std_logic_vector (7 downto 0);
      signal \main_a2_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR2\ : std_logic_vector (1 downto 0);
      signal zi11 : std_logic_vector (1 downto 0);
      signal \main_r0_outR1\ : std_logic_vector (7 downto 0);
      signal zi12 : std_logic_vector (7 downto 0);
      signal main_a3_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR1\ : std_logic_vector (7 downto 0);
      signal zi13 : std_logic_vector (7 downto 0);
      signal \main_a3_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR1\ : std_logic_vector (7 downto 0);
      signal zi14 : std_logic_vector (7 downto 0);
      signal \main_a3_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR1\ : std_logic_vector (7 downto 0);
      signal zi15 : std_logic_vector (7 downto 0);
      signal \main_a3_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR3\ : std_logic_vector (1 downto 0);
      signal zi16 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR4\ : std_logic_vector (1 downto 0);
      signal zi17 : std_logic_vector (1 downto 0);
      signal \main_r0_outR2\ : std_logic_vector (7 downto 0);
      signal main_vd_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR2\ : std_logic_vector (7 downto 0);
      signal \main_vd_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR2\ : std_logic_vector (7 downto 0);
      signal \main_vd_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR2\ : std_logic_vector (7 downto 0);
      signal \main_vd_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR5\ : std_logic_vector (1 downto 0);
      signal zi18 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR6\ : std_logic_vector (1 downto 0);
      signal zi19 : std_logic_vector (1 downto 0);
      signal \main_r0_outR3\ : std_logic_vector (7 downto 0);
      signal main_vd2_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR3\ : std_logic_vector (7 downto 0);
      signal \main_vd2_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR3\ : std_logic_vector (7 downto 0);
      signal \main_vd2_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR3\ : std_logic_vector (7 downto 0);
      signal \main_vd2_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR7\ : std_logic_vector (1 downto 0);
      signal zi20 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR8\ : std_logic_vector (1 downto 0);
      signal zi21 : std_logic_vector (1 downto 0);
      signal \main_r0_outR4\ : std_logic_vector (7 downto 0);
      signal main_vd3_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR4\ : std_logic_vector (7 downto 0);
      signal \main_vd3_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR4\ : std_logic_vector (7 downto 0);
      signal \main_vd3_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR4\ : std_logic_vector (7 downto 0);
      signal \main_vd3_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR9\ : std_logic_vector (1 downto 0);
      signal zi22 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR10\ : std_logic_vector (1 downto 0);
      signal zi23 : std_logic_vector (1 downto 0);
      signal \main_r0_outR5\ : std_logic_vector (7 downto 0);
      signal main_vd4_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR5\ : std_logic_vector (7 downto 0);
      signal \main_vd4_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR5\ : std_logic_vector (7 downto 0);
      signal \main_vd4_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR5\ : std_logic_vector (7 downto 0);
      signal \main_vd4_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR11\ : std_logic_vector (1 downto 0);
      signal zi24 : std_logic_vector (1 downto 0);
      signal \main_r0_outR6\ : std_logic_vector (7 downto 0);
      signal zi25 : std_logic_vector (7 downto 0);
      signal main_x2_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR6\ : std_logic_vector (7 downto 0);
      signal zi26 : std_logic_vector (7 downto 0);
      signal \main_x2_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR6\ : std_logic_vector (7 downto 0);
      signal zi27 : std_logic_vector (7 downto 0);
      signal \main_x2_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR6\ : std_logic_vector (7 downto 0);
      signal zi28 : std_logic_vector (7 downto 0);
      signal \main_x2_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR12\ : std_logic_vector (1 downto 0);
      signal zi29 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR13\ : std_logic_vector (1 downto 0);
      signal zi30 : std_logic_vector (1 downto 0);
      signal \main_r0_outR7\ : std_logic_vector (7 downto 0);
      signal main_vd5_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR7\ : std_logic_vector (7 downto 0);
      signal \main_vd5_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR7\ : std_logic_vector (7 downto 0);
      signal \main_vd5_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR7\ : std_logic_vector (7 downto 0);
      signal \main_vd5_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR14\ : std_logic_vector (1 downto 0);
      signal zi31 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR15\ : std_logic_vector (1 downto 0);
      signal zi32 : std_logic_vector (1 downto 0);
      signal \main_r0_outR8\ : std_logic_vector (7 downto 0);
      signal main_vd6_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR8\ : std_logic_vector (7 downto 0);
      signal \main_vd6_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR8\ : std_logic_vector (7 downto 0);
      signal \main_vd6_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR8\ : std_logic_vector (7 downto 0);
      signal \main_vd6_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR16\ : std_logic_vector (1 downto 0);
      signal zi33 : std_logic_vector (1 downto 0);
      signal \main_mkreg_outR17\ : std_logic_vector (1 downto 0);
      signal zi34 : std_logic_vector (1 downto 0);
      signal \main_r0_outR9\ : std_logic_vector (7 downto 0);
      signal main_vd7_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR9\ : std_logic_vector (7 downto 0);
      signal \main_vd7_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR9\ : std_logic_vector (7 downto 0);
      signal \main_vd7_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR9\ : std_logic_vector (7 downto 0);
      signal \main_vd7_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR18\ : std_logic_vector (1 downto 0);
      signal zi35 : std_logic_vector (1 downto 0);
      signal \main_r0_outR10\ : std_logic_vector (7 downto 0);
      signal zi36 : std_logic_vector (7 downto 0);
      signal main_vd8_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR10\ : std_logic_vector (7 downto 0);
      signal zi37 : std_logic_vector (7 downto 0);
      signal \main_vd8_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR10\ : std_logic_vector (7 downto 0);
      signal zi38 : std_logic_vector (7 downto 0);
      signal \main_vd8_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR10\ : std_logic_vector (7 downto 0);
      signal zi39 : std_logic_vector (7 downto 0);
      signal \main_vd8_outR3\ : std_logic_vector (105 downto 0);
      signal main_zflag_out : std_logic_vector (0 downto 0);
      signal zi40 : std_logic_vector (0 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
      signal main_arm142_out : std_logic_vector (105 downto 0);
      signal \main_zflag_outR1\ : std_logic_vector (0 downto 0);
      signal zi41 : std_logic_vector (0 downto 0);
      signal main_notb_out : std_logic_vector (0 downto 0);
      signal zi42 : std_logic_vector (0 downto 0);
      signal \main_zzz_outR1\ : std_logic_vector (105 downto 0);
      signal \main_arm142_outR1\ : std_logic_vector (105 downto 0);
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal zi43 : std_logic_vector (0 downto 0);
      signal \main_zzz_outR2\ : std_logic_vector (105 downto 0);
      signal \main_arm142_outR2\ : std_logic_vector (105 downto 0);
      signal \main_cflag_outR1\ : std_logic_vector (0 downto 0);
      signal zi44 : std_logic_vector (0 downto 0);
      signal \main_notb_outR1\ : std_logic_vector (0 downto 0);
      signal zi45 : std_logic_vector (0 downto 0);
      signal \main_zzz_outR3\ : std_logic_vector (105 downto 0);
      signal \main_arm142_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR19\ : std_logic_vector (1 downto 0);
      signal zi46 : std_logic_vector (1 downto 0);
      signal \main_r0_outR11\ : std_logic_vector (7 downto 0);
      signal zi47 : std_logic_vector (7 downto 0);
      signal main_x3_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR11\ : std_logic_vector (7 downto 0);
      signal zi48 : std_logic_vector (7 downto 0);
      signal \main_x3_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR11\ : std_logic_vector (7 downto 0);
      signal zi49 : std_logic_vector (7 downto 0);
      signal \main_x3_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR11\ : std_logic_vector (7 downto 0);
      signal zi50 : std_logic_vector (7 downto 0);
      signal \main_x3_outR3\ : std_logic_vector (105 downto 0);
      signal main_setieflag_out : std_logic_vector (80 downto 0);
      signal zi51 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR4\ : std_logic_vector (105 downto 0);
      signal \main_outputs_outR2\ : std_logic_vector (17 downto 0);
      signal zi52 : std_logic_vector (17 downto 0);
      signal zi53 : std_logic_vector (7 downto 0);
      signal zi54 : std_logic_vector (7 downto 0);
      signal zi55 : std_logic_vector (0 downto 0);
      signal zi56 : std_logic_vector (17 downto 0);
      signal \main_setoutputs_outR1\ : std_logic_vector (80 downto 0);
      signal zi57 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR5\ : std_logic_vector (105 downto 0);
      signal \main_setieflag_outR1\ : std_logic_vector (80 downto 0);
      signal zi58 : std_logic_vector (80 downto 0);
      signal zi59 : std_logic_vector (7 downto 0);
      signal zi60 : std_logic_vector (7 downto 0);
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal zi61 : std_logic_vector (80 downto 0);
      signal zi62 : std_logic_vector (0 downto 0);
      signal zi63 : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal zi64 : std_logic_vector (80 downto 0);
      signal zi65 : std_logic_vector (0 downto 0);
      signal zi66 : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi67 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR6\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR20\ : std_logic_vector (1 downto 0);
      signal zi68 : std_logic_vector (1 downto 0);
      signal \main_r0_outR12\ : std_logic_vector (7 downto 0);
      signal zi69 : std_logic_vector (7 downto 0);
      signal main_v3_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR12\ : std_logic_vector (7 downto 0);
      signal zi70 : std_logic_vector (7 downto 0);
      signal \main_v3_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR12\ : std_logic_vector (7 downto 0);
      signal zi71 : std_logic_vector (7 downto 0);
      signal \main_v3_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR12\ : std_logic_vector (7 downto 0);
      signal zi72 : std_logic_vector (7 downto 0);
      signal \main_v3_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR21\ : std_logic_vector (1 downto 0);
      signal zi73 : std_logic_vector (1 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal zi74 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR7\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal zi75 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR8\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal zi76 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR9\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal zi77 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR10\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR22\ : std_logic_vector (1 downto 0);
      signal zi78 : std_logic_vector (1 downto 0);
      signal \main_r0_outR13\ : std_logic_vector (7 downto 0);
      signal zi79 : std_logic_vector (7 downto 0);
      signal main_v4_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR13\ : std_logic_vector (7 downto 0);
      signal zi80 : std_logic_vector (7 downto 0);
      signal \main_v4_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR13\ : std_logic_vector (7 downto 0);
      signal zi81 : std_logic_vector (7 downto 0);
      signal \main_v4_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR13\ : std_logic_vector (7 downto 0);
      signal zi82 : std_logic_vector (7 downto 0);
      signal \main_v4_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR23\ : std_logic_vector (1 downto 0);
      signal zi83 : std_logic_vector (1 downto 0);
      signal \main_r0_outR14\ : std_logic_vector (7 downto 0);
      signal zi84 : std_logic_vector (7 downto 0);
      signal main_v5_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR14\ : std_logic_vector (7 downto 0);
      signal zi85 : std_logic_vector (7 downto 0);
      signal \main_v5_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR14\ : std_logic_vector (7 downto 0);
      signal zi86 : std_logic_vector (7 downto 0);
      signal \main_v5_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR14\ : std_logic_vector (7 downto 0);
      signal zi87 : std_logic_vector (7 downto 0);
      signal \main_v5_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR24\ : std_logic_vector (1 downto 0);
      signal zi88 : std_logic_vector (1 downto 0);
      signal \main_r0_outR15\ : std_logic_vector (7 downto 0);
      signal zi89 : std_logic_vector (7 downto 0);
      signal main_v6_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR15\ : std_logic_vector (7 downto 0);
      signal zi90 : std_logic_vector (7 downto 0);
      signal \main_v6_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR15\ : std_logic_vector (7 downto 0);
      signal zi91 : std_logic_vector (7 downto 0);
      signal \main_v6_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR15\ : std_logic_vector (7 downto 0);
      signal zi92 : std_logic_vector (7 downto 0);
      signal \main_v6_outR3\ : std_logic_vector (105 downto 0);
      signal \main_r0_outR16\ : std_logic_vector (7 downto 0);
      signal zi93 : std_logic_vector (7 downto 0);
      signal main_v7_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR16\ : std_logic_vector (7 downto 0);
      signal zi94 : std_logic_vector (7 downto 0);
      signal \main_v7_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR16\ : std_logic_vector (7 downto 0);
      signal zi95 : std_logic_vector (7 downto 0);
      signal \main_v7_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR16\ : std_logic_vector (7 downto 0);
      signal zi96 : std_logic_vector (7 downto 0);
      signal \main_v7_outR3\ : std_logic_vector (105 downto 0);
      signal \main_mkreg_outR25\ : std_logic_vector (1 downto 0);
      signal zi97 : std_logic_vector (1 downto 0);
      signal \main_r0_outR17\ : std_logic_vector (7 downto 0);
      signal zi98 : std_logic_vector (7 downto 0);
      signal main_v8_out : std_logic_vector (105 downto 0);
      signal \main_r1_outR17\ : std_logic_vector (7 downto 0);
      signal zi99 : std_logic_vector (7 downto 0);
      signal \main_v8_outR1\ : std_logic_vector (105 downto 0);
      signal \main_r2_outR17\ : std_logic_vector (7 downto 0);
      signal zi100 : std_logic_vector (7 downto 0);
      signal \main_v8_outR2\ : std_logic_vector (105 downto 0);
      signal \main_r3_outR17\ : std_logic_vector (7 downto 0);
      signal zi101 : std_logic_vector (7 downto 0);
      signal \main_v8_outR3\ : std_logic_vector (105 downto 0);
begin
inst : \Main_dataIn\ port map (arg0, main_datain_out);
      i <= main_datain_out;
      zds1 <= i(6 downto 6);
      zds2 <= i(5 downto 5);
      zds3 <= i(4 downto 4);
      ren <= i(3 downto 3);
      wen <= i(2 downto 2);
      b0 <= i(1 downto 1);
      b1 <= i(0 downto 0);
      za <= i(7 downto 7);
      \instR1\ : \Main_mkReg\ port map (b0, b1, main_mkreg_out);
      zi0 <= main_mkreg_out;
      \instR2\ : \Main_pc\ port map (arg1, main_pc_out);
      zi1 <= main_pc_out;
      \instR3\ : \Main_outputs\ port map (arg1, main_outputs_out);
      zi2 <= main_outputs_out;
      \instR4\ : \Main_setAddrOut\ port map (zi2, zi1, main_setaddrout_out);
      zi3 <= main_setaddrout_out;
      \instR5\ : \Main_setOutputs\ port map (arg1, zi3, main_setoutputs_out);
      zi4 <= main_setoutputs_out;
      \instR6\ : \Main_outputs\ port map (zi4, \main_outputs_outR1\);
      zi5 <= \main_outputs_outR1\;
      \instR7\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR1\);
      zi6 <= \main_mkreg_outR1\;
      \instR8\ : \Main_r0\ port map (arg1, main_r0_out);
      zi7 <= main_r0_out;
      \instR9\ : main_a2 port map (ren, wen, zi7, arg1, main_a2_out);
      \instR10\ : \Main_r1\ port map (arg1, main_r1_out);
      zi8 <= main_r1_out;
      \instR11\ : main_a2 port map (ren, wen, zi8, arg1, \main_a2_outR1\);
      \instR12\ : \Main_r2\ port map (arg1, main_r2_out);
      zi9 <= main_r2_out;
      \instR13\ : main_a2 port map (ren, wen, zi9, arg1, \main_a2_outR2\);
      \instR14\ : \Main_r3\ port map (arg1, main_r3_out);
      zi10 <= main_r3_out;
      \instR15\ : main_a2 port map (ren, wen, zi10, arg1, \main_a2_outR3\);
      \instR16\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR2\);
      zi11 <= \main_mkreg_outR2\;
      \instR17\ : \Main_r0\ port map (arg1, \main_r0_outR1\);
      zi12 <= \main_r0_outR1\;
      \instR18\ : main_a3 port map (ren, wen, zi12, arg1, main_a3_out);
      \instR19\ : \Main_r1\ port map (arg1, \main_r1_outR1\);
      zi13 <= \main_r1_outR1\;
      \instR20\ : main_a3 port map (ren, wen, zi13, arg1, \main_a3_outR1\);
      \instR21\ : \Main_r2\ port map (arg1, \main_r2_outR1\);
      zi14 <= \main_r2_outR1\;
      \instR22\ : main_a3 port map (ren, wen, zi14, arg1, \main_a3_outR2\);
      \instR23\ : \Main_r3\ port map (arg1, \main_r3_outR1\);
      zi15 <= \main_r3_outR1\;
      \instR24\ : main_a3 port map (ren, wen, zi15, arg1, \main_a3_outR3\);
      \instR25\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR3\);
      zi16 <= \main_mkreg_outR3\;
      \instR26\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR4\);
      zi17 <= \main_mkreg_outR4\;
      \instR27\ : \Main_r0\ port map (arg1, \main_r0_outR2\);
      \instR28\ : \main_vD\ port map (zi16, zi17, \main_r0_outR2\, arg1, main_vd_out);
      \instR29\ : \Main_r1\ port map (arg1, \main_r1_outR2\);
      \instR30\ : \main_vD\ port map (zi16, zi17, \main_r1_outR2\, arg1, \main_vd_outR1\);
      \instR31\ : \Main_r2\ port map (arg1, \main_r2_outR2\);
      \instR32\ : \main_vD\ port map (zi16, zi17, \main_r2_outR2\, arg1, \main_vd_outR2\);
      \instR33\ : \Main_r3\ port map (arg1, \main_r3_outR2\);
      \instR34\ : \main_vD\ port map (zi16, zi17, \main_r3_outR2\, arg1, \main_vd_outR3\);
      \instR35\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR5\);
      zi18 <= \main_mkreg_outR5\;
      \instR36\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR6\);
      zi19 <= \main_mkreg_outR6\;
      \instR37\ : \Main_r0\ port map (arg1, \main_r0_outR3\);
      \instR38\ : \main_vD2\ port map (zi18, zi19, \main_r0_outR3\, arg1, main_vd2_out);
      \instR39\ : \Main_r1\ port map (arg1, \main_r1_outR3\);
      \instR40\ : \main_vD2\ port map (zi18, zi19, \main_r1_outR3\, arg1, \main_vd2_outR1\);
      \instR41\ : \Main_r2\ port map (arg1, \main_r2_outR3\);
      \instR42\ : \main_vD2\ port map (zi18, zi19, \main_r2_outR3\, arg1, \main_vd2_outR2\);
      \instR43\ : \Main_r3\ port map (arg1, \main_r3_outR3\);
      \instR44\ : \main_vD2\ port map (zi18, zi19, \main_r3_outR3\, arg1, \main_vd2_outR3\);
      \instR45\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR7\);
      zi20 <= \main_mkreg_outR7\;
      \instR46\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR8\);
      zi21 <= \main_mkreg_outR8\;
      \instR47\ : \Main_r0\ port map (arg1, \main_r0_outR4\);
      \instR48\ : \main_vD3\ port map (zi20, zi21, \main_r0_outR4\, arg1, main_vd3_out);
      \instR49\ : \Main_r1\ port map (arg1, \main_r1_outR4\);
      \instR50\ : \main_vD3\ port map (zi20, zi21, \main_r1_outR4\, arg1, \main_vd3_outR1\);
      \instR51\ : \Main_r2\ port map (arg1, \main_r2_outR4\);
      \instR52\ : \main_vD3\ port map (zi20, zi21, \main_r2_outR4\, arg1, \main_vd3_outR2\);
      \instR53\ : \Main_r3\ port map (arg1, \main_r3_outR4\);
      \instR54\ : \main_vD3\ port map (zi20, zi21, \main_r3_outR4\, arg1, \main_vd3_outR3\);
      \instR55\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR9\);
      zi22 <= \main_mkreg_outR9\;
      \instR56\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR10\);
      zi23 <= \main_mkreg_outR10\;
      \instR57\ : \Main_r0\ port map (arg1, \main_r0_outR5\);
      \instR58\ : \main_vD4\ port map (zi22, zi23, \main_r0_outR5\, arg1, main_vd4_out);
      \instR59\ : \Main_r1\ port map (arg1, \main_r1_outR5\);
      \instR60\ : \main_vD4\ port map (zi22, zi23, \main_r1_outR5\, arg1, \main_vd4_outR1\);
      \instR61\ : \Main_r2\ port map (arg1, \main_r2_outR5\);
      \instR62\ : \main_vD4\ port map (zi22, zi23, \main_r2_outR5\, arg1, \main_vd4_outR2\);
      \instR63\ : \Main_r3\ port map (arg1, \main_r3_outR5\);
      \instR64\ : \main_vD4\ port map (zi22, zi23, \main_r3_outR5\, arg1, \main_vd4_outR3\);
      \instR65\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR11\);
      zi24 <= \main_mkreg_outR11\;
      \instR66\ : \Main_r0\ port map (arg1, \main_r0_outR6\);
      zi25 <= \main_r0_outR6\;
      \instR67\ : main_x2 port map (ren, wen, zi25, arg1, main_x2_out);
      \instR68\ : \Main_r1\ port map (arg1, \main_r1_outR6\);
      zi26 <= \main_r1_outR6\;
      \instR69\ : main_x2 port map (ren, wen, zi26, arg1, \main_x2_outR1\);
      \instR70\ : \Main_r2\ port map (arg1, \main_r2_outR6\);
      zi27 <= \main_r2_outR6\;
      \instR71\ : main_x2 port map (ren, wen, zi27, arg1, \main_x2_outR2\);
      \instR72\ : \Main_r3\ port map (arg1, \main_r3_outR6\);
      zi28 <= \main_r3_outR6\;
      \instR73\ : main_x2 port map (ren, wen, zi28, arg1, \main_x2_outR3\);
      \instR74\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR12\);
      zi29 <= \main_mkreg_outR12\;
      \instR75\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR13\);
      zi30 <= \main_mkreg_outR13\;
      \instR76\ : \Main_r0\ port map (arg1, \main_r0_outR7\);
      \instR77\ : \main_vD5\ port map (zi29, zi30, \main_r0_outR7\, arg1, main_vd5_out);
      \instR78\ : \Main_r1\ port map (arg1, \main_r1_outR7\);
      \instR79\ : \main_vD5\ port map (zi29, zi30, \main_r1_outR7\, arg1, \main_vd5_outR1\);
      \instR80\ : \Main_r2\ port map (arg1, \main_r2_outR7\);
      \instR81\ : \main_vD5\ port map (zi29, zi30, \main_r2_outR7\, arg1, \main_vd5_outR2\);
      \instR82\ : \Main_r3\ port map (arg1, \main_r3_outR7\);
      \instR83\ : \main_vD5\ port map (zi29, zi30, \main_r3_outR7\, arg1, \main_vd5_outR3\);
      \instR84\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR14\);
      zi31 <= \main_mkreg_outR14\;
      \instR85\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR15\);
      zi32 <= \main_mkreg_outR15\;
      \instR86\ : \Main_r0\ port map (arg1, \main_r0_outR8\);
      \instR87\ : \main_vD6\ port map (zi31, zi32, \main_r0_outR8\, arg1, main_vd6_out);
      \instR88\ : \Main_r1\ port map (arg1, \main_r1_outR8\);
      \instR89\ : \main_vD6\ port map (zi31, zi32, \main_r1_outR8\, arg1, \main_vd6_outR1\);
      \instR90\ : \Main_r2\ port map (arg1, \main_r2_outR8\);
      \instR91\ : \main_vD6\ port map (zi31, zi32, \main_r2_outR8\, arg1, \main_vd6_outR2\);
      \instR92\ : \Main_r3\ port map (arg1, \main_r3_outR8\);
      \instR93\ : \main_vD6\ port map (zi31, zi32, \main_r3_outR8\, arg1, \main_vd6_outR3\);
      \instR94\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR16\);
      zi33 <= \main_mkreg_outR16\;
      \instR95\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR17\);
      zi34 <= \main_mkreg_outR17\;
      \instR96\ : \Main_r0\ port map (arg1, \main_r0_outR9\);
      \instR97\ : \main_vD7\ port map (zi33, zi34, \main_r0_outR9\, arg1, main_vd7_out);
      \instR98\ : \Main_r1\ port map (arg1, \main_r1_outR9\);
      \instR99\ : \main_vD7\ port map (zi33, zi34, \main_r1_outR9\, arg1, \main_vd7_outR1\);
      \instR100\ : \Main_r2\ port map (arg1, \main_r2_outR9\);
      \instR101\ : \main_vD7\ port map (zi33, zi34, \main_r2_outR9\, arg1, \main_vd7_outR2\);
      \instR102\ : \Main_r3\ port map (arg1, \main_r3_outR9\);
      \instR103\ : \main_vD7\ port map (zi33, zi34, \main_r3_outR9\, arg1, \main_vd7_outR3\);
      \instR104\ : \Main_mkReg\ port map (ren, wen, \main_mkreg_outR18\);
      zi35 <= \main_mkreg_outR18\;
      \instR105\ : \Main_r0\ port map (arg1, \main_r0_outR10\);
      zi36 <= \main_r0_outR10\;
      \instR106\ : \main_vD8\ port map (b0, b1, zi36, arg1, main_vd8_out);
      \instR107\ : \Main_r1\ port map (arg1, \main_r1_outR10\);
      zi37 <= \main_r1_outR10\;
      \instR108\ : \main_vD8\ port map (b0, b1, zi37, arg1, \main_vd8_outR1\);
      \instR109\ : \Main_r2\ port map (arg1, \main_r2_outR10\);
      zi38 <= \main_r2_outR10\;
      \instR110\ : \main_vD8\ port map (b0, b1, zi38, arg1, \main_vd8_outR2\);
      \instR111\ : \Main_r3\ port map (arg1, \main_r3_outR10\);
      zi39 <= \main_r3_outR10\;
      \instR112\ : \main_vD8\ port map (b0, b1, zi39, arg1, \main_vd8_outR3\);
      \instR113\ : \Main_zFlag\ port map (arg1, main_zflag_out);
      zi40 <= main_zflag_out;
      \instR114\ : main_zzz port map (arg1, main_zzz_out);
      \instR115\ : main_arm142 port map (b0, b1, arg1, main_arm142_out);
      \instR116\ : \Main_zFlag\ port map (arg1, \main_zflag_outR1\);
      zi41 <= \main_zflag_outR1\;
      \instR117\ : \Main_notb\ port map (zi41, main_notb_out);
      zi42 <= main_notb_out;
      \instR118\ : main_zzz port map (arg1, \main_zzz_outR1\);
      \instR119\ : main_arm142 port map (b0, b1, arg1, \main_arm142_outR1\);
      \instR120\ : \Main_cFlag\ port map (arg1, main_cflag_out);
      zi43 <= main_cflag_out;
      \instR121\ : main_zzz port map (arg1, \main_zzz_outR2\);
      \instR122\ : main_arm142 port map (b0, b1, arg1, \main_arm142_outR2\);
      \instR123\ : \Main_cFlag\ port map (arg1, \main_cflag_outR1\);
      zi44 <= \main_cflag_outR1\;
      \instR124\ : \Main_notb\ port map (zi44, \main_notb_outR1\);
      zi45 <= \main_notb_outR1\;
      \instR125\ : main_zzz port map (arg1, \main_zzz_outR3\);
      \instR126\ : main_arm142 port map (b0, b1, arg1, \main_arm142_outR3\);
      \instR127\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR19\);
      zi46 <= \main_mkreg_outR19\;
      \instR128\ : \Main_r0\ port map (arg1, \main_r0_outR11\);
      zi47 <= \main_r0_outR11\;
      \instR129\ : main_x3 port map (zi47, arg1, main_x3_out);
      \instR130\ : \Main_r1\ port map (arg1, \main_r1_outR11\);
      zi48 <= \main_r1_outR11\;
      \instR131\ : main_x3 port map (zi48, arg1, \main_x3_outR1\);
      \instR132\ : \Main_r2\ port map (arg1, \main_r2_outR11\);
      zi49 <= \main_r2_outR11\;
      \instR133\ : main_x3 port map (zi49, arg1, \main_x3_outR2\);
      \instR134\ : \Main_r3\ port map (arg1, \main_r3_outR11\);
      zi50 <= \main_r3_outR11\;
      \instR135\ : main_x3 port map (zi50, arg1, \main_x3_outR3\);
      \instR136\ : \Main_setIEFlag\ port map (arg1, b1, main_setieflag_out);
      zi51 <= main_setieflag_out;
      \instR137\ : main_zzz port map (zi51, \main_zzz_outR4\);
      \instR138\ : \Main_outputs\ port map (arg1, \main_outputs_outR2\);
      zi52 <= \main_outputs_outR2\;
      zi53 <= zi52(17 downto 10);
      zi54 <= zi52(9 downto 2);
      zi55 <= zi52(1 downto 1);
      zi56 <= (zi53 & zi54 & zi55 & std_logic_vector'(B"1"));
      \instR139\ : \Main_setOutputs\ port map (arg1, zi56, \main_setoutputs_outR1\);
      zi57 <= \main_setoutputs_outR1\;
      \instR140\ : main_zzz port map (zi57, \main_zzz_outR5\);
      \instR141\ : \Main_setIEFlag\ port map (arg1, std_logic_vector'(B"1"), \main_setieflag_outR1\);
      zi58 <= \main_setieflag_outR1\;
      zi59 <= zi58(39 downto 32);
      zi60 <= zi59;
      \instR142\ : \Main_setPC\ port map (zi58, zi60, main_setpc_out);
      zi61 <= main_setpc_out;
      zi62 <= zi61(41 downto 41);
      zi63 <= zi62;
      \instR143\ : \Main_setZFlag\ port map (zi61, zi63, main_setzflag_out);
      zi64 <= main_setzflag_out;
      zi65 <= zi64(40 downto 40);
      zi66 <= zi65;
      \instR144\ : \Main_setCFlag\ port map (zi64, zi66, main_setcflag_out);
      zi67 <= main_setcflag_out;
      \instR145\ : main_zzz port map (zi67, \main_zzz_outR6\);
      \instR146\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR20\);
      zi68 <= \main_mkreg_outR20\;
      \instR147\ : \Main_r0\ port map (arg1, \main_r0_outR12\);
      zi69 <= \main_r0_outR12\;
      \instR148\ : main_v3 port map (zi68, zi69, arg1, main_v3_out);
      \instR149\ : \Main_r1\ port map (arg1, \main_r1_outR12\);
      zi70 <= \main_r1_outR12\;
      \instR150\ : main_v3 port map (zi68, zi70, arg1, \main_v3_outR1\);
      \instR151\ : \Main_r2\ port map (arg1, \main_r2_outR12\);
      zi71 <= \main_r2_outR12\;
      \instR152\ : main_v3 port map (zi68, zi71, arg1, \main_v3_outR2\);
      \instR153\ : \Main_r3\ port map (arg1, \main_r3_outR12\);
      zi72 <= \main_r3_outR12\;
      \instR154\ : main_v3 port map (zi68, zi72, arg1, \main_v3_outR3\);
      \instR155\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR21\);
      zi73 <= \main_mkreg_outR21\;
      \instR156\ : \Main_setR0\ port map (arg1, std_logic_vector'(B"00000000"), main_setr0_out);
      zi74 <= main_setr0_out;
      \instR157\ : main_zzz port map (zi74, \main_zzz_outR7\);
      \instR158\ : \Main_setR1\ port map (arg1, std_logic_vector'(B"00000000"), main_setr1_out);
      zi75 <= main_setr1_out;
      \instR159\ : main_zzz port map (zi75, \main_zzz_outR8\);
      \instR160\ : \Main_setR2\ port map (arg1, std_logic_vector'(B"00000000"), main_setr2_out);
      zi76 <= main_setr2_out;
      \instR161\ : main_zzz port map (zi76, \main_zzz_outR9\);
      \instR162\ : \Main_setR3\ port map (arg1, std_logic_vector'(B"00000000"), main_setr3_out);
      zi77 <= main_setr3_out;
      \instR163\ : main_zzz port map (zi77, \main_zzz_outR10\);
      \instR164\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR22\);
      zi78 <= \main_mkreg_outR22\;
      \instR165\ : \Main_r0\ port map (arg1, \main_r0_outR13\);
      zi79 <= \main_r0_outR13\;
      \instR166\ : main_v4 port map (zi78, zi79, arg1, main_v4_out);
      \instR167\ : \Main_r1\ port map (arg1, \main_r1_outR13\);
      zi80 <= \main_r1_outR13\;
      \instR168\ : main_v4 port map (zi78, zi80, arg1, \main_v4_outR1\);
      \instR169\ : \Main_r2\ port map (arg1, \main_r2_outR13\);
      zi81 <= \main_r2_outR13\;
      \instR170\ : main_v4 port map (zi78, zi81, arg1, \main_v4_outR2\);
      \instR171\ : \Main_r3\ port map (arg1, \main_r3_outR13\);
      zi82 <= \main_r3_outR13\;
      \instR172\ : main_v4 port map (zi78, zi82, arg1, \main_v4_outR3\);
      \instR173\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR23\);
      zi83 <= \main_mkreg_outR23\;
      \instR174\ : \Main_r0\ port map (arg1, \main_r0_outR14\);
      zi84 <= \main_r0_outR14\;
      \instR175\ : main_v5 port map (zi83, zi84, arg1, main_v5_out);
      \instR176\ : \Main_r1\ port map (arg1, \main_r1_outR14\);
      zi85 <= \main_r1_outR14\;
      \instR177\ : main_v5 port map (zi83, zi85, arg1, \main_v5_outR1\);
      \instR178\ : \Main_r2\ port map (arg1, \main_r2_outR14\);
      zi86 <= \main_r2_outR14\;
      \instR179\ : main_v5 port map (zi83, zi86, arg1, \main_v5_outR2\);
      \instR180\ : \Main_r3\ port map (arg1, \main_r3_outR14\);
      zi87 <= \main_r3_outR14\;
      \instR181\ : main_v5 port map (zi83, zi87, arg1, \main_v5_outR3\);
      \instR182\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR24\);
      zi88 <= \main_mkreg_outR24\;
      \instR183\ : \Main_r0\ port map (arg1, \main_r0_outR15\);
      zi89 <= \main_r0_outR15\;
      \instR184\ : main_v6 port map (zi88, zi89, arg1, main_v6_out);
      \instR185\ : \Main_r1\ port map (arg1, \main_r1_outR15\);
      zi90 <= \main_r1_outR15\;
      \instR186\ : main_v6 port map (zi88, zi90, arg1, \main_v6_outR1\);
      \instR187\ : \Main_r2\ port map (arg1, \main_r2_outR15\);
      zi91 <= \main_r2_outR15\;
      \instR188\ : main_v6 port map (zi88, zi91, arg1, \main_v6_outR2\);
      \instR189\ : \Main_r3\ port map (arg1, \main_r3_outR15\);
      zi92 <= \main_r3_outR15\;
      \instR190\ : main_v6 port map (zi88, zi92, arg1, \main_v6_outR3\);
      \instR191\ : \Main_r0\ port map (arg1, \main_r0_outR16\);
      zi93 <= \main_r0_outR16\;
      \instR192\ : main_v7 port map (zi88, zi93, arg1, main_v7_out);
      \instR193\ : \Main_r1\ port map (arg1, \main_r1_outR16\);
      zi94 <= \main_r1_outR16\;
      \instR194\ : main_v7 port map (zi88, zi94, arg1, \main_v7_outR1\);
      \instR195\ : \Main_r2\ port map (arg1, \main_r2_outR16\);
      zi95 <= \main_r2_outR16\;
      \instR196\ : main_v7 port map (zi88, zi95, arg1, \main_v7_outR2\);
      \instR197\ : \Main_r3\ port map (arg1, \main_r3_outR16\);
      zi96 <= \main_r3_outR16\;
      \instR198\ : main_v7 port map (zi88, zi96, arg1, \main_v7_outR3\);
      \instR199\ : \Main_mkReg\ port map (b0, b1, \main_mkreg_outR25\);
      zi97 <= \main_mkreg_outR25\;
      \instR200\ : \Main_r0\ port map (arg1, \main_r0_outR17\);
      zi98 <= \main_r0_outR17\;
      \instR201\ : main_v8 port map (ren, wen, zi97, zi98, arg1, main_v8_out);
      \instR202\ : \Main_r1\ port map (arg1, \main_r1_outR17\);
      zi99 <= \main_r1_outR17\;
      \instR203\ : main_v8 port map (ren, wen, zi97, zi99, arg1, \main_v8_outR1\);
      \instR204\ : \Main_r2\ port map (arg1, \main_r2_outR17\);
      zi100 <= \main_r2_outR17\;
      \instR205\ : main_v8 port map (ren, wen, zi97, zi100, arg1, \main_v8_outR2\);
      \instR206\ : \Main_r3\ port map (arg1, \main_r3_outR17\);
      zi101 <= \main_r3_outR17\;
      \instR207\ : main_v8 port map (ren, wen, zi97, zi101, arg1, \main_v8_outR3\);
      res <= rw_cond(rw_not(za), rw_cond(rw_not(zds1), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), (zi5 & std_logic_vector'(B"001") & ren & wen & zi0 & zi4), rw_cond(rw_eq(zi6, std_logic_vector'(B"00")), main_a2_out, rw_cond(rw_eq(zi6, std_logic_vector'(B"01")), \main_a2_outR1\, rw_cond(rw_eq(zi6, std_logic_vector'(B"10")), \main_a2_outR2\, \main_a2_outR3\)))), rw_cond(rw_not(zds3), rw_cond(rw_eq(zi11, std_logic_vector'(B"00")), main_a3_out, rw_cond(rw_eq(zi11, std_logic_vector'(B"01")), \main_a3_outR1\, rw_cond(rw_eq(zi11, std_logic_vector'(B"10")), \main_a3_outR2\, \main_a3_outR3\))), rw_cond(rw_eq(zi16, std_logic_vector'(B"00")), main_vd_out, rw_cond(rw_eq(zi16, std_logic_vector'(B"01")), \main_vd_outR1\, rw_cond(rw_eq(zi16, std_logic_vector'(B"10")), \main_vd_outR2\, \main_vd_outR3\))))), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), rw_cond(rw_eq(zi18, std_logic_vector'(B"00")), main_vd2_out, rw_cond(rw_eq(zi18, std_logic_vector'(B"01")), \main_vd2_outR1\, rw_cond(rw_eq(zi18, std_logic_vector'(B"10")), \main_vd2_outR2\, \main_vd2_outR3\))), rw_cond(rw_eq(zi20, std_logic_vector'(B"00")), main_vd3_out, rw_cond(rw_eq(zi20, std_logic_vector'(B"01")), \main_vd3_outR1\, rw_cond(rw_eq(zi20, std_logic_vector'(B"10")), \main_vd3_outR2\, \main_vd3_outR3\)))), rw_cond(rw_not(zds3), rw_cond(rw_eq(zi22, std_logic_vector'(B"00")), main_vd4_out, rw_cond(rw_eq(zi22, std_logic_vector'(B"01")), \main_vd4_outR1\, rw_cond(rw_eq(zi22, std_logic_vector'(B"10")), \main_vd4_outR2\, \main_vd4_outR3\))), rw_cond(rw_eq(zi24, std_logic_vector'(B"00")), main_x2_out, rw_cond(rw_eq(zi24, std_logic_vector'(B"01")), \main_x2_outR1\, rw_cond(rw_eq(zi24, std_logic_vector'(B"10")), \main_x2_outR2\, \main_x2_outR3\)))))), rw_cond(rw_not(zds1), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), rw_cond(rw_eq(zi29, std_logic_vector'(B"00")), main_vd5_out, rw_cond(rw_eq(zi29, std_logic_vector'(B"01")), \main_vd5_outR1\, rw_cond(rw_eq(zi29, std_logic_vector'(B"10")), \main_vd5_outR2\, \main_vd5_outR3\))), rw_cond(rw_eq(zi31, std_logic_vector'(B"00")), main_vd6_out, rw_cond(rw_eq(zi31, std_logic_vector'(B"01")), \main_vd6_outR1\, rw_cond(rw_eq(zi31, std_logic_vector'(B"10")), \main_vd6_outR2\, \main_vd6_outR3\)))), rw_cond(rw_not(zds3), rw_cond(rw_eq(zi33, std_logic_vector'(B"00")), main_vd7_out, rw_cond(rw_eq(zi33, std_logic_vector'(B"01")), \main_vd7_outR1\, rw_cond(rw_eq(zi33, std_logic_vector'(B"10")), \main_vd7_outR2\, \main_vd7_outR3\))), rw_cond(rw_eq(zi35, std_logic_vector'(B"00")), main_vd8_out, rw_cond(rw_eq(zi35, std_logic_vector'(B"01")), \main_vd8_outR1\, rw_cond(rw_eq(zi35, std_logic_vector'(B"10")), \main_vd8_outR2\, \main_vd8_outR3\))))), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), rw_cond(rw_not(ren), rw_cond(rw_not(wen), rw_cond(rw_not(zi40), main_zzz_out, main_arm142_out), rw_cond(rw_not(zi42), \main_zzz_outR1\, \main_arm142_outR1\)), rw_cond(rw_not(wen), rw_cond(rw_not(zi43), \main_zzz_outR2\, \main_arm142_outR2\), rw_cond(rw_not(zi45), \main_zzz_outR3\, \main_arm142_outR3\))), rw_cond(rw_not(ren), rw_cond(rw_not(wen), rw_cond(rw_eq(zi46, std_logic_vector'(B"00")), main_x3_out, rw_cond(rw_eq(zi46, std_logic_vector'(B"01")), \main_x3_outR1\, rw_cond(rw_eq(zi46, std_logic_vector'(B"10")), \main_x3_outR2\, \main_x3_outR3\))), rw_cond(rw_not(b0), \main_zzz_outR4\, rw_cond(rw_not(b1), \main_zzz_outR5\, \main_zzz_outR6\))), rw_cond(rw_not(wen), rw_cond(rw_eq(zi68, std_logic_vector'(B"00")), main_v3_out, rw_cond(rw_eq(zi68, std_logic_vector'(B"01")), \main_v3_outR1\, rw_cond(rw_eq(zi68, std_logic_vector'(B"10")), \main_v3_outR2\, \main_v3_outR3\))), rw_cond(rw_eq(zi73, std_logic_vector'(B"00")), \main_zzz_outR7\, rw_cond(rw_eq(zi73, std_logic_vector'(B"01")), \main_zzz_outR8\, rw_cond(rw_eq(zi73, std_logic_vector'(B"10")), \main_zzz_outR9\, \main_zzz_outR10\)))))), rw_cond(rw_not(zds3), rw_cond(rw_not(ren), rw_cond(rw_not(wen), rw_cond(rw_eq(zi78, std_logic_vector'(B"00")), main_v4_out, rw_cond(rw_eq(zi78, std_logic_vector'(B"01")), \main_v4_outR1\, rw_cond(rw_eq(zi78, std_logic_vector'(B"10")), \main_v4_outR2\, \main_v4_outR3\))), rw_cond(rw_eq(zi83, std_logic_vector'(B"00")), main_v5_out, rw_cond(rw_eq(zi83, std_logic_vector'(B"01")), \main_v5_outR1\, rw_cond(rw_eq(zi83, std_logic_vector'(B"10")), \main_v5_outR2\, \main_v5_outR3\)))), rw_cond(rw_not(wen), rw_cond(rw_eq(zi88, std_logic_vector'(B"00")), main_v6_out, rw_cond(rw_eq(zi88, std_logic_vector'(B"01")), \main_v6_outR1\, rw_cond(rw_eq(zi88, std_logic_vector'(B"10")), \main_v6_outR2\, \main_v6_outR3\))), rw_cond(rw_eq(zi88, std_logic_vector'(B"00")), main_v7_out, rw_cond(rw_eq(zi88, std_logic_vector'(B"01")), \main_v7_outR1\, rw_cond(rw_eq(zi88, std_logic_vector'(B"10")), \main_v7_outR2\, \main_v7_outR3\))))), rw_cond(rw_eq(zi97, std_logic_vector'(B"00")), main_v8_out, rw_cond(rw_eq(zi97, std_logic_vector'(B"01")), \main_v8_outR1\, rw_cond(rw_eq(zi97, std_logic_vector'(B"10")), \main_v8_outR2\, \main_v8_outR3\)))))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_loop is
port (arg0 : in std_logic_vector (80 downto 0);
      res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_loop is
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
      component \main_$fail\ is
      port (arg0 : in std_logic_vector (9 downto 0);
            arg1 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
      end component;
      component main_zzz is
      port (arg0 : in std_logic_vector (80 downto 0);
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
      signal zi49 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
      signal \main_$fail_out\ : std_logic_vector (105 downto 0);
      signal \main_$fail_outR1\ : std_logic_vector (105 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal zi50 : std_logic_vector (80 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal zi51 : std_logic_vector (80 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal zi52 : std_logic_vector (80 downto 0);
      signal \main_zzz_outR1\ : std_logic_vector (105 downto 0);
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
      zi49 <= (zi37 & zi38 & zi39 & zi40 & zi41 & zi42 & zi43 & zi10 & zi44 & zi45 & zi46 & zi47 & zi48);
      \instR5\ : main_zzz port map (zi49, main_zzz_out);
      \instR6\ : \main_$fail\ port map (zi0, arg0, \main_$fail_out\);
      \instR7\ : \main_$fail\ port map (zi0, arg0, \main_$fail_outR1\);
      \instR8\ : \Main_setCFlag\ port map (arg0, std_logic_vector'(B"0"), main_setcflag_out);
      zi50 <= main_setcflag_out;
      \instR9\ : \Main_setZFlag\ port map (zi50, std_logic_vector'(B"0"), main_setzflag_out);
      zi51 <= main_setzflag_out;
      \instR10\ : \Main_setOutputs\ port map (zi51, std_logic_vector'(B"000000000000000000"), main_setoutputs_out);
      zi52 <= main_setoutputs_out;
      \instR11\ : main_zzz port map (zi52, \main_zzz_outR1\);
      res <= rw_cond(rw_not(zi2), rw_cond(zi4, rw_cond(zi6, main_zzz_out, \main_$fail_out\), \main_$fail_outR1\), \main_zzz_outR1\);
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
res <= rw_not(arg0);
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
res <= rw_cond(rw_not(arg0), rw_cond(rw_not(arg1), std_logic_vector'(B"00"), std_logic_vector'(B"01")), rw_cond(rw_not(arg1), std_logic_vector'(B"10"), std_logic_vector'(B"11")));
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
entity \Main_plusCW8$sMain__oneW8__False__Bool\ is
port (arg0 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \Main_plusCW8$sMain__oneW8__False__Bool\ is
signal s : std_logic_vector (8 downto 0);
begin
s <= rw_add(rw_add(rw_resize(arg0, 9), std_logic_vector'(B"000000001")), std_logic_vector'(B"000000000"));
      res <= (s(8 downto 8) & rw_resize(s, 8));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_minusCW8$sFalse__Bool\ is
port (arg0 : in std_logic_vector (7 downto 0);
      arg1 : in std_logic_vector (7 downto 0);
      res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \Main_minusCW8$sFalse__Bool\ is
signal s : std_logic_vector (8 downto 0);
begin
s <= rw_sub(rw_sub(rw_resize(arg0, 9), rw_resize(arg1, 9)), std_logic_vector'(B"000000000"));
      res <= (s(8 downto 8) & rw_resize(s, 8));
end architecture;