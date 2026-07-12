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
            port (\Zds\ : in std_logic_vector (9 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_inputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (9 downto 0));
      end component;
      component \Main_mkReg\ is
            port (\Zds\ : in std_logic_vector (0 downto 0);
                  \Zds1\ : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_setAddrOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  a_o : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setInputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  i : in std_logic_vector (9 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setOutputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  o : in std_logic_vector (17 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r0 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r1 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r2 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r3 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setWeOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  we_o : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \main___unused7\ is
            port (\rEn\ : in std_logic_vector (0 downto 0);
                  r : in std_logic_vector (1 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_d is
            port (\rEn\ : in std_logic_vector (0 downto 0);
                  r : in std_logic_vector (1 downto 0);
                  d : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_loop is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      -- state registers
      -- __resumption_tag: 7 bits, init 0x30
      --   states: 0=i2 1=i4 2=i6 3=i7 4=i8
      -- __st0: 81 bits, init 0x0
      signal \__resumption_tag\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110000");
      signal \__resumption_tag_next\ : std_logic_vector (6 downto 0);
      signal \__st0\ : std_logic_vector (80 downto 0) := (others => '0');
      signal \__st0_next\ : std_logic_vector (80 downto 0);
      signal i : std_logic_vector (9 downto 0);
      signal ren : std_logic_vector (0 downto 0);
      signal r : std_logic_vector (1 downto 0);
      signal main_setinputs_out : std_logic_vector (80 downto 0);
      signal main_loop_out : std_logic_vector (105 downto 0);
      signal main_inputs_out : std_logic_vector (9 downto 0);
      signal main_datain_out : std_logic_vector (7 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal main_loop_out_r1 : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal main_loop_out_r2 : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal main_loop_out_r3 : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal main_loop_out_r4 : std_logic_vector (105 downto 0);
      signal ren_r1 : std_logic_vector (0 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal main_outputs_out_r1 : std_logic_vector (17 downto 0);
      signal main_setweout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out_r1 : std_logic_vector (80 downto 0);
      signal \main__unused7_out\ : std_logic_vector (105 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_d_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_d_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_d_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_d_out_r3 : std_logic_vector (105 downto 0);
      signal ren_r2 : std_logic_vector (0 downto 0);
      signal wen : std_logic_vector (0 downto 0);
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal zres : std_logic_vector (105 downto 0);
begin
      -- combinational logic
      i <= (\__in0\ & \__in1\ & \__in2\);
      ren <= \__resumption_tag\(2 downto 2);
      r <= \__resumption_tag\(1 downto 0);
      setinputs_i : \Main_setInputs\ port map (\__st0\, i, main_setinputs_out);
      loop_i : main_loop port map (main_setinputs_out, main_loop_out);
      inputs_i : \Main_inputs\ port map (main_setinputs_out, main_inputs_out);
      datain_i : \Main_dataIn\ port map (main_inputs_out, main_datain_out);
      setr0_i : \Main_setR0\ port map (main_setinputs_out, main_datain_out, main_setr0_out);
      loop_i_r1 : main_loop port map (main_setr0_out, main_loop_out_r1);
      setr1_i : \Main_setR1\ port map (main_setinputs_out, main_datain_out, main_setr1_out);
      loop_i_r2 : main_loop port map (main_setr1_out, main_loop_out_r2);
      setr2_i : \Main_setR2\ port map (main_setinputs_out, main_datain_out, main_setr2_out);
      loop_i_r3 : main_loop port map (main_setr2_out, main_loop_out_r3);
      setr3_i : \Main_setR3\ port map (main_setinputs_out, main_datain_out, main_setr3_out);
      loop_i_r4 : main_loop port map (main_setr3_out, main_loop_out_r4);
      ren_r1 <= \__resumption_tag\(3 downto 3);
      outputs_i : \Main_outputs\ port map (main_setinputs_out, main_outputs_out);
      setaddrout_i : \Main_setAddrOut\ port map (main_outputs_out, main_datain_out, main_setaddrout_out);
      setoutputs_i : \Main_setOutputs\ port map (main_setinputs_out, main_setaddrout_out, main_setoutputs_out);
      outputs_i_r1 : \Main_outputs\ port map (main_setoutputs_out, main_outputs_out_r1);
      setweout_i : \Main_setWeOut\ port map (main_outputs_out_r1, ren, main_setweout_out);
      setoutputs_i_r1 : \Main_setOutputs\ port map (main_setoutputs_out, main_setweout_out, main_setoutputs_out_r1);
      \_unused7_i\ : \main___unused7\ port map (ren_r1, r, main_setoutputs_out_r1, \main__unused7_out\);
      r0_i : \Main_r0\ port map (main_setoutputs_out_r1, main_r0_out);
      d_i : main_d port map (ren_r1, r, main_r0_out, main_setoutputs_out_r1, main_d_out);
      r1_i : \Main_r1\ port map (main_setoutputs_out_r1, main_r1_out);
      d_i_r1 : main_d port map (ren_r1, r, main_r1_out, main_setoutputs_out_r1, main_d_out_r1);
      r2_i : \Main_r2\ port map (main_setoutputs_out_r1, main_r2_out);
      d_i_r2 : main_d port map (ren_r1, r, main_r2_out, main_setoutputs_out_r1, main_d_out_r2);
      r3_i : \Main_r3\ port map (main_setoutputs_out_r1, main_r3_out);
      d_i_r3 : main_d port map (ren_r1, r, main_r3_out, main_setoutputs_out_r1, main_d_out_r3);
      ren_r2 <= \__resumption_tag\(1 downto 1);
      wen <= \__resumption_tag\(0 downto 0);
      mkreg_i : \Main_mkReg\ port map (ren_r2, wen, main_mkreg_out);
      with \__resumption_tag\(6 downto 4) select zres <=
            rw_cond(rw_not(ren), main_loop_out, rw_cond(rw_eq(r, std_logic_vector'(B"00")), main_loop_out_r1, rw_cond(rw_eq(r, std_logic_vector'(B"01")), main_loop_out_r2, rw_cond(rw_eq(r, std_logic_vector'(B"10")), main_loop_out_r3, main_loop_out_r4)))) when "000",
            rw_cond(rw_not(ren), \main__unused7_out\, rw_cond(rw_eq(r, std_logic_vector'(B"00")), main_d_out, rw_cond(rw_eq(r, std_logic_vector'(B"01")), main_d_out_r1, rw_cond(rw_eq(r, std_logic_vector'(B"10")), main_d_out_r2, main_d_out_r3)))) when "001",
            rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_loop_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_loop_out_r2, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_loop_out_r3, main_loop_out_r4))) when "010",
            main_loop_out when "011",
            main_loop_out when others;
      \__resumption_tag_next\ <= zres(87 downto 81);
      \__st0_next\ <= zres(80 downto 0);
      -- outputs
      \__out0\ <= zres(105 downto 98);
      \__out1\ <= zres(97 downto 90);
      \__out2\ <= zres(89 downto 89);
      \__out3\ <= zres(88 downto 88);
      -- state register update
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

-- main._unused7
-- block '$L._unused7' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused7\ is
      port (\rEn\ : in std_logic_vector (0 downto 0);
            r : in std_logic_vector (1 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused7\ is
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_pc\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_plusCW8$sMain__oneW8__False__Bool\ is
            port (a : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (8 downto 0));
      end component;
      component \Main_setPC\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  pc : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (7 downto 0);
      signal \main_pluscw8$smain_onew8_false_bool_out\ : std_logic_vector (8 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
begin
      pc_i : \Main_pc\ port map (s0, main_pc_out);
      \pluscw8$smain_onew8_false_bool_i\ : \Main_plusCW8$sMain__oneW8__False__Bool\ port map (main_pc_out, \main_pluscw8$smain_onew8_false_bool_out\);
      y <= \main_pluscw8$smain_onew8_false_bool_out\(7 downto 0);
      setpc_i : \Main_setPC\ port map (s0, y, main_setpc_out);
      outputs_i : \Main_outputs\ port map (main_setpc_out, main_outputs_out);
      res <= (main_outputs_out & std_logic_vector'(X"0") & \rEn\ & r & main_setpc_out);
end architecture;

-- main.d
-- block '$L.d' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_d is
      port (\rEn\ : in std_logic_vector (0 downto 0);
            r : in std_logic_vector (1 downto 0);
            d : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_d is
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setDataOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  d_o : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setOutputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  o : in std_logic_vector (17 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused7\ is
            port (\rEn\ : in std_logic_vector (0 downto 0);
                  r : in std_logic_vector (1 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal main_setdataout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal \main__unused7_out\ : std_logic_vector (105 downto 0);
begin
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      setdataout_i : \Main_setDataOut\ port map (main_outputs_out, d, main_setdataout_out);
      setoutputs_i : \Main_setOutputs\ port map (s0, main_setdataout_out, main_setoutputs_out);
      \_unused7_i\ : \main___unused7\ port map (\rEn\, r, main_setoutputs_out, \main__unused7_out\);
      res <= \main__unused7_out\;
end architecture;

-- main.a2
-- block '$L.a2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_a2 is
      port (\rEn\ : in std_logic_vector (0 downto 0);
            \wEn\ : in std_logic_vector (0 downto 0);
            a : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_a2 is
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setAddrOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  a_o : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setOutputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  o : in std_logic_vector (17 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setWeOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  we_o : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal main_setweout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal main_outputs_out_r1 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out_r1 : std_logic_vector (80 downto 0);
      signal main_outputs_out_r2 : std_logic_vector (17 downto 0);
begin
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      setweout_i : \Main_setWeOut\ port map (main_outputs_out, std_logic_vector'(B"0"), main_setweout_out);
      setoutputs_i : \Main_setOutputs\ port map (s0, main_setweout_out, main_setoutputs_out);
      outputs_i_r1 : \Main_outputs\ port map (main_setoutputs_out, main_outputs_out_r1);
      setaddrout_i : \Main_setAddrOut\ port map (main_outputs_out_r1, a, main_setaddrout_out);
      setoutputs_i_r1 : \Main_setOutputs\ port map (main_setoutputs_out, main_setaddrout_out, main_setoutputs_out_r1);
      outputs_i_r2 : \Main_outputs\ port map (main_setoutputs_out_r1, main_outputs_out_r2);
      res <= (main_outputs_out_r2 & std_logic_vector'(B"01000") & \rEn\ & \wEn\ & main_setoutputs_out_r1);
end architecture;

-- main.v2
-- block '$L.v2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v2 is
      port (a : in std_logic_vector (7 downto 0);
            v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v2 is
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setAddrOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  a_o : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setDataOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  d_o : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setOutputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  o : in std_logic_vector (17 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setWeOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  we_o : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal main_setweout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal main_outputs_out_r1 : std_logic_vector (17 downto 0);
      signal main_setdataout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out_r1 : std_logic_vector (80 downto 0);
      signal main_outputs_out_r2 : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out_r2 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      setweout_i : \Main_setWeOut\ port map (main_outputs_out, std_logic_vector'(B"1"), main_setweout_out);
      setoutputs_i : \Main_setOutputs\ port map (s0, main_setweout_out, main_setoutputs_out);
      outputs_i_r1 : \Main_outputs\ port map (main_setoutputs_out, main_outputs_out_r1);
      setdataout_i : \Main_setDataOut\ port map (main_outputs_out_r1, v, main_setdataout_out);
      setoutputs_i_r1 : \Main_setOutputs\ port map (main_setoutputs_out, main_setdataout_out, main_setoutputs_out_r1);
      outputs_i_r2 : \Main_outputs\ port map (main_setoutputs_out_r1, main_outputs_out_r2);
      setaddrout_i : \Main_setAddrOut\ port map (main_outputs_out_r2, a, main_setaddrout_out);
      setoutputs_i_r2 : \Main_setOutputs\ port map (main_setoutputs_out_r1, main_setaddrout_out, main_setoutputs_out_r2);
      zzz_i : main_zzz port map (main_setoutputs_out_r2, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.a3
-- block '$L.a3' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_a3 is
      port (\rEn\ : in std_logic_vector (0 downto 0);
            \wEn\ : in std_logic_vector (0 downto 0);
            a : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_a3 is
      component \Main_mkReg\ is
            port (\Zds\ : in std_logic_vector (0 downto 0);
                  \Zds1\ : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component main_v2 is
            port (a : in std_logic_vector (7 downto 0);
                  v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_v2_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_v2_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_v2_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_v2_out_r3 : std_logic_vector (105 downto 0);
begin
      mkreg_i : \Main_mkReg\ port map (\rEn\, \wEn\, main_mkreg_out);
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      v2_i : main_v2 port map (a, main_r0_out, s0, main_v2_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      v2_i_r1 : main_v2 port map (a, main_r1_out, s0, main_v2_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      v2_i_r2 : main_v2 port map (a, main_r2_out, s0, main_v2_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      v2_i_r3 : main_v2 port map (a, main_r3_out, s0, main_v2_out_r3);
      with main_mkreg_out select res <=
            main_v2_out when "00",
            main_v2_out_r1 when "01",
            main_v2_out_r2 when "10",
            main_v2_out_r3 when others;
end architecture;

-- main._unused17
-- block '$L._unused17' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused17\ is
      port (s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused17\ is
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
begin
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      res <= (main_outputs_out & std_logic_vector'(B"1000000") & s0);
end architecture;

-- main.s63
-- block '$L.s63' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_s63 is
      port (\rD\ : in std_logic_vector (1 downto 0);
            p : in std_logic_vector (8 downto 0);
            s : in std_logic_vector (80 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_s63 is
      component \Main_setCFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  c : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r0 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r1 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r2 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r3 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused17\ is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal x : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \main__unused17_out\ : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \main__unused17_out_r1\ : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \main__unused17_out_r2\ : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \main__unused17_out_r3\ : std_logic_vector (105 downto 0);
begin
      x <= p(8 downto 8);
      setcflag_i : \Main_setCFlag\ port map (s, x, main_setcflag_out);
      y <= p(7 downto 0);
      setr0_i : \Main_setR0\ port map (main_setcflag_out, y, main_setr0_out);
      \_unused17_i\ : \main___unused17\ port map (main_setr0_out, \main__unused17_out\);
      setr1_i : \Main_setR1\ port map (main_setcflag_out, y, main_setr1_out);
      \_unused17_i_r1\ : \main___unused17\ port map (main_setr1_out, \main__unused17_out_r1\);
      setr2_i : \Main_setR2\ port map (main_setcflag_out, y, main_setr2_out);
      \_unused17_i_r2\ : \main___unused17\ port map (main_setr2_out, \main__unused17_out_r2\);
      setr3_i : \Main_setR3\ port map (main_setcflag_out, y, main_setr3_out);
      \_unused17_i_r3\ : \main___unused17\ port map (main_setr3_out, \main__unused17_out_r3\);
      with \rD\ select res <=
            \main__unused17_out\ when "00",
            \main__unused17_out_r1\ when "01",
            \main__unused17_out_r2\ when "10",
            \main__unused17_out_r3\ when others;
end architecture;

-- main.vS
-- block '$L.vS' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS\ is
      component main_s63 is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  p : in std_logic_vector (8 downto 0);
                  s : in std_logic_vector (80 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal s : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
      s <= rw_add(rw_add(rw_resize(\vD\, 9), rw_resize(\vS\, 9)), std_logic_vector'(B"000000000"));
      p <= (s(8 downto 8) & rw_resize(s, 8));
      s63_i : main_s63 port map (\rD\, p, s0, s0, main_s63_out);
      res <= main_s63_out;
end architecture;

-- main.vD
-- block '$L.vD' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \rS\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs_out_r3 : std_logic_vector (105 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs_i : \main_vS\ port map (\rD\, \vD\, main_r0_out, s0, main_vs_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs_i_r1 : \main_vS\ port map (\rD\, \vD\, main_r1_out, s0, main_vs_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs_i_r2 : \main_vS\ port map (\rD\, \vD\, main_r2_out, s0, main_vs_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs_i_r3 : \main_vS\ port map (\rD\, \vD\, main_r3_out, s0, main_vs_out_r3);
      with \rS\ select res <=
            main_vs_out when "00",
            main_vs_out_r1 when "01",
            main_vs_out_r2 when "10",
            main_vs_out_r3 when others;
end architecture;

-- main.vS2
-- block '$L.vS2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS2\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS2\ is
      component \Main_cFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component main_s63 is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  p : in std_logic_vector (8 downto 0);
                  s : in std_logic_vector (80 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal s : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
      cflag_i : \Main_cFlag\ port map (s0, main_cflag_out);
      s <= rw_add(rw_add(rw_resize(\vD\, 9), rw_resize(\vS\, 9)), rw_resize(main_cflag_out, 9));
      p <= (s(8 downto 8) & rw_resize(s, 8));
      s63_i : main_s63 port map (\rD\, p, s0, s0, main_s63_out);
      res <= main_s63_out;
end architecture;

-- main.vD2
-- block '$L.vD2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD2\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \rS\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD2\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS2\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs2_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs2_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs2_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs2_out_r3 : std_logic_vector (105 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs2_i : \main_vS2\ port map (\rD\, \vD\, main_r0_out, s0, main_vs2_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs2_i_r1 : \main_vS2\ port map (\rD\, \vD\, main_r1_out, s0, main_vs2_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs2_i_r2 : \main_vS2\ port map (\rD\, \vD\, main_r2_out, s0, main_vs2_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs2_i_r3 : \main_vS2\ port map (\rD\, \vD\, main_r3_out, s0, main_vs2_out_r3);
      with \rS\ select res <=
            main_vs2_out when "00",
            main_vs2_out_r1 when "01",
            main_vs2_out_r2 when "10",
            main_vs2_out_r3 when others;
end architecture;

-- main.vS3
-- block '$L.vS3' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS3\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS3\ is
      component \Main_minusCW8$sFalse__Bool\ is
            port (a : in std_logic_vector (7 downto 0);
                  b : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (8 downto 0));
      end component;
      component main_s63 is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  p : in std_logic_vector (8 downto 0);
                  s : in std_logic_vector (80 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_minuscw8$sfalse_bool_out\ : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
      \minuscw8$sfalse_bool_i\ : \Main_minusCW8$sFalse__Bool\ port map (\vD\, \vS\, \main_minuscw8$sfalse_bool_out\);
      s63_i : main_s63 port map (\rD\, \main_minuscw8$sfalse_bool_out\, s0, s0, main_s63_out);
      res <= main_s63_out;
end architecture;

-- main.vD3
-- block '$L.vD3' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD3\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \rS\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD3\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS3\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs3_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs3_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs3_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs3_out_r3 : std_logic_vector (105 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs3_i : \main_vS3\ port map (\rD\, \vD\, main_r0_out, s0, main_vs3_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs3_i_r1 : \main_vS3\ port map (\rD\, \vD\, main_r1_out, s0, main_vs3_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs3_i_r2 : \main_vS3\ port map (\rD\, \vD\, main_r2_out, s0, main_vs3_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs3_i_r3 : \main_vS3\ port map (\rD\, \vD\, main_r3_out, s0, main_vs3_out_r3);
      with \rS\ select res <=
            main_vs3_out when "00",
            main_vs3_out_r1 when "01",
            main_vs3_out_r2 when "10",
            main_vs3_out_r3 when others;
end architecture;

-- main.vS4
-- block '$L.vS4' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS4\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS4\ is
      component \Main_cFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component main_s63 is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  p : in std_logic_vector (8 downto 0);
                  s : in std_logic_vector (80 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal s : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal main_s63_out : std_logic_vector (105 downto 0);
begin
      cflag_i : \Main_cFlag\ port map (s0, main_cflag_out);
      s <= rw_sub(rw_sub(rw_resize(\vD\, 9), rw_resize(\vS\, 9)), rw_resize(main_cflag_out, 9));
      p <= (s(8 downto 8) & rw_resize(s, 8));
      s63_i : main_s63 port map (\rD\, p, s0, s0, main_s63_out);
      res <= main_s63_out;
end architecture;

-- main.vD4
-- block '$L.vD4' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD4\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \rS\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD4\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS4\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs4_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs4_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs4_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs4_out_r3 : std_logic_vector (105 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs4_i : \main_vS4\ port map (\rD\, \vD\, main_r0_out, s0, main_vs4_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs4_i_r1 : \main_vS4\ port map (\rD\, \vD\, main_r1_out, s0, main_vs4_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs4_i_r2 : \main_vS4\ port map (\rD\, \vD\, main_r2_out, s0, main_vs4_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs4_i_r3 : \main_vS4\ port map (\rD\, \vD\, main_r3_out, s0, main_vs4_out_r3);
      with \rS\ select res <=
            main_vs4_out when "00",
            main_vs4_out_r1 when "01",
            main_vs4_out_r2 when "10",
            main_vs4_out_r3 when others;
end architecture;

-- main.zzz
-- block '$L.zzz' of process main
-- also: main._unused15
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_zzz is
      port (s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_zzz is
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      signal main_outputs_out : std_logic_vector (17 downto 0);
begin
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      res <= (main_outputs_out & std_logic_vector'(B"0110000") & s0);
end architecture;

-- main.x2
-- block '$L.x2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_x2 is
      port (\rEn\ : in std_logic_vector (0 downto 0);
            \wEn\ : in std_logic_vector (0 downto 0);
            x : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_x2 is
      component \Main_mkReg\ is
            port (\Zds\ : in std_logic_vector (0 downto 0);
                  \Zds1\ : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_setR0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r0 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r1 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r2 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r3 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r1 : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r2 : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r3 : std_logic_vector (105 downto 0);
begin
      mkreg_i : \Main_mkReg\ port map (\rEn\, \wEn\, main_mkreg_out);
      setr0_i : \Main_setR0\ port map (s0, x, main_setr0_out);
      zzz_i : main_zzz port map (main_setr0_out, main_zzz_out);
      setr1_i : \Main_setR1\ port map (s0, x, main_setr1_out);
      zzz_i_r1 : main_zzz port map (main_setr1_out, main_zzz_out_r1);
      setr2_i : \Main_setR2\ port map (s0, x, main_setr2_out);
      zzz_i_r2 : main_zzz port map (main_setr2_out, main_zzz_out_r2);
      setr3_i : \Main_setR3\ port map (s0, x, main_setr3_out);
      zzz_i_r3 : main_zzz port map (main_setr3_out, main_zzz_out_r3);
      with main_mkreg_out select res <=
            main_zzz_out when "00",
            main_zzz_out_r1 when "01",
            main_zzz_out_r2 when "10",
            main_zzz_out_r3 when others;
end architecture;

-- main._unused24
-- block '$L._unused24' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused24\ is
      port (\vD$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused24\ is
      component \Main_setCFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  c : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  z : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused17\ is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal \main__unused17_out\ : std_logic_vector (105 downto 0);
begin
      setcflag_i : \Main_setCFlag\ port map (s0, std_logic_vector'(B"0"), main_setcflag_out);
      conn <= rw_eq(\vD$\, std_logic_vector'(X"00"));
      setzflag_i : \Main_setZFlag\ port map (main_setcflag_out, conn, main_setzflag_out);
      \_unused17_i\ : \main___unused17\ port map (main_setzflag_out, \main__unused17_out\);
      res <= \main__unused17_out\;
end architecture;

-- main.arm90
-- block '$L.arm90' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm90 is
      port (\vD$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm90 is
      component \Main_setR0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r0 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
      setr0_i : \Main_setR0\ port map (s0, \vD$\, main_setr0_out);
      \_unused24_i\ : \main___unused24\ port map (\vD$\, main_setr0_out, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

-- main.arm91
-- block '$L.arm91' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm91 is
      port (\vD$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm91 is
      component \Main_setR1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r1 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
      setr1_i : \Main_setR1\ port map (s0, \vD$\, main_setr1_out);
      \_unused24_i\ : \main___unused24\ port map (\vD$\, main_setr1_out, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

-- main.arm92
-- block '$L.arm92' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm92 is
      port (\vD$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm92 is
      component \Main_setR2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r2 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
      setr2_i : \Main_setR2\ port map (s0, \vD$\, main_setr2_out);
      \_unused24_i\ : \main___unused24\ port map (\vD$\, main_setr2_out, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

-- main.arm93
-- block '$L.arm93' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm93 is
      port (\vD$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm93 is
      component \Main_setR3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r3 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused24\ is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \main__unused24_out\ : std_logic_vector (105 downto 0);
begin
      setr3_i : \Main_setR3\ port map (s0, \vD$\, main_setr3_out);
      \_unused24_i\ : \main___unused24\ port map (\vD$\, main_setr3_out, \main__unused24_out\);
      res <= \main__unused24_out\;
end architecture;

-- main.vS5
-- block '$L.vS5' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS5\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS5\ is
      component main_arm90 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm91 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm92 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm93 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal \vd$\ : std_logic_vector (7 downto 0);
      signal main_arm90_out : std_logic_vector (105 downto 0);
      signal main_arm91_out : std_logic_vector (105 downto 0);
      signal main_arm92_out : std_logic_vector (105 downto 0);
      signal main_arm93_out : std_logic_vector (105 downto 0);
begin
      \vd$\ <= rw_or(\vD\, \vS\);
      arm90_i : main_arm90 port map (\vd$\, s0, main_arm90_out);
      arm91_i : main_arm91 port map (\vd$\, s0, main_arm91_out);
      arm92_i : main_arm92 port map (\vd$\, s0, main_arm92_out);
      arm93_i : main_arm93 port map (\vd$\, s0, main_arm93_out);
      with \rD\ select res <=
            main_arm90_out when "00",
            main_arm91_out when "01",
            main_arm92_out when "10",
            main_arm93_out when others;
end architecture;

-- main.vD5
-- block '$L.vD5' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD5\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \rS\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD5\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS5\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs5_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs5_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs5_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs5_out_r3 : std_logic_vector (105 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs5_i : \main_vS5\ port map (\rD\, \vD\, main_r0_out, s0, main_vs5_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs5_i_r1 : \main_vS5\ port map (\rD\, \vD\, main_r1_out, s0, main_vs5_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs5_i_r2 : \main_vS5\ port map (\rD\, \vD\, main_r2_out, s0, main_vs5_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs5_i_r3 : \main_vS5\ port map (\rD\, \vD\, main_r3_out, s0, main_vs5_out_r3);
      with \rS\ select res <=
            main_vs5_out when "00",
            main_vs5_out_r1 when "01",
            main_vs5_out_r2 when "10",
            main_vs5_out_r3 when others;
end architecture;

-- main.vS6
-- block '$L.vS6' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS6\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS6\ is
      component main_arm90 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm91 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm92 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm93 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal \vd$\ : std_logic_vector (7 downto 0);
      signal main_arm90_out : std_logic_vector (105 downto 0);
      signal main_arm91_out : std_logic_vector (105 downto 0);
      signal main_arm92_out : std_logic_vector (105 downto 0);
      signal main_arm93_out : std_logic_vector (105 downto 0);
begin
      \vd$\ <= rw_and(\vD\, \vS\);
      arm90_i : main_arm90 port map (\vd$\, s0, main_arm90_out);
      arm91_i : main_arm91 port map (\vd$\, s0, main_arm91_out);
      arm92_i : main_arm92 port map (\vd$\, s0, main_arm92_out);
      arm93_i : main_arm93 port map (\vd$\, s0, main_arm93_out);
      with \rD\ select res <=
            main_arm90_out when "00",
            main_arm91_out when "01",
            main_arm92_out when "10",
            main_arm93_out when others;
end architecture;

-- main.vD6
-- block '$L.vD6' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD6\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \rS\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD6\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS6\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs6_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs6_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs6_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs6_out_r3 : std_logic_vector (105 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs6_i : \main_vS6\ port map (\rD\, \vD\, main_r0_out, s0, main_vs6_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs6_i_r1 : \main_vS6\ port map (\rD\, \vD\, main_r1_out, s0, main_vs6_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs6_i_r2 : \main_vS6\ port map (\rD\, \vD\, main_r2_out, s0, main_vs6_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs6_i_r3 : \main_vS6\ port map (\rD\, \vD\, main_r3_out, s0, main_vs6_out_r3);
      with \rS\ select res <=
            main_vs6_out when "00",
            main_vs6_out_r1 when "01",
            main_vs6_out_r2 when "10",
            main_vs6_out_r3 when others;
end architecture;

-- main.vS7
-- block '$L.vS7' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS7\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS7\ is
      component main_arm90 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm91 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm92 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm93 is
            port (\vD$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal \vd$\ : std_logic_vector (7 downto 0);
      signal main_arm90_out : std_logic_vector (105 downto 0);
      signal main_arm91_out : std_logic_vector (105 downto 0);
      signal main_arm92_out : std_logic_vector (105 downto 0);
      signal main_arm93_out : std_logic_vector (105 downto 0);
begin
      \vd$\ <= rw_xor(\vD\, \vS\);
      arm90_i : main_arm90 port map (\vd$\, s0, main_arm90_out);
      arm91_i : main_arm91 port map (\vd$\, s0, main_arm91_out);
      arm92_i : main_arm92 port map (\vd$\, s0, main_arm92_out);
      arm93_i : main_arm93 port map (\vd$\, s0, main_arm93_out);
      with \rD\ select res <=
            main_arm90_out when "00",
            main_arm91_out when "01",
            main_arm92_out when "10",
            main_arm93_out when others;
end architecture;

-- main.vD7
-- block '$L.vD7' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD7\ is
      port (\rD\ : in std_logic_vector (1 downto 0);
            \rS\ : in std_logic_vector (1 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD7\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS7\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs7_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs7_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs7_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs7_out_r3 : std_logic_vector (105 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs7_i : \main_vS7\ port map (\rD\, \vD\, main_r0_out, s0, main_vs7_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs7_i_r1 : \main_vS7\ port map (\rD\, \vD\, main_r1_out, s0, main_vs7_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs7_i_r2 : \main_vS7\ port map (\rD\, \vD\, main_r2_out, s0, main_vs7_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs7_i_r3 : \main_vS7\ port map (\rD\, \vD\, main_r3_out, s0, main_vs7_out_r3);
      with \rS\ select res <=
            main_vs7_out when "00",
            main_vs7_out_r1 when "01",
            main_vs7_out_r2 when "10",
            main_vs7_out_r3 when others;
end architecture;

-- main.vS8
-- block '$L.vS8' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vS8\ is
      port (\vD\ : in std_logic_vector (7 downto 0);
            \vS\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vS8\ is
      component \Main_minusCW8$sFalse__Bool\ is
            port (a : in std_logic_vector (7 downto 0);
                  b : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (8 downto 0));
      end component;
      component \Main_setCFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  c : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  z : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_minuscw8$sfalse_bool_out\ : std_logic_vector (8 downto 0);
      signal x : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      \minuscw8$sfalse_bool_i\ : \Main_minusCW8$sFalse__Bool\ port map (\vD\, \vS\, \main_minuscw8$sfalse_bool_out\);
      x <= \main_minuscw8$sfalse_bool_out\(8 downto 8);
      setcflag_i : \Main_setCFlag\ port map (s0, x, main_setcflag_out);
      y <= \main_minuscw8$sfalse_bool_out\(7 downto 0);
      conn <= rw_eq(y, std_logic_vector'(X"00"));
      setzflag_i : \Main_setZFlag\ port map (main_setcflag_out, conn, main_setzflag_out);
      zzz_i : main_zzz port map (main_setzflag_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.vD8
-- block '$L.vD8' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_vD8\ is
      port (b0 : in std_logic_vector (0 downto 0);
            b1 : in std_logic_vector (0 downto 0);
            \vD\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_vD8\ is
      component \Main_mkReg\ is
            port (\Zds\ : in std_logic_vector (0 downto 0);
                  \Zds1\ : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_vS8\ is
            port (\vD\ : in std_logic_vector (7 downto 0);
                  \vS\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_vs8_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_vs8_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_vs8_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_vs8_out_r3 : std_logic_vector (105 downto 0);
begin
      mkreg_i : \Main_mkReg\ port map (b0, b1, main_mkreg_out);
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      vs8_i : \main_vS8\ port map (\vD\, main_r0_out, s0, main_vs8_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      vs8_i_r1 : \main_vS8\ port map (\vD\, main_r1_out, s0, main_vs8_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      vs8_i_r2 : \main_vS8\ port map (\vD\, main_r2_out, s0, main_vs8_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      vs8_i_r3 : \main_vS8\ port map (\vD\, main_r3_out, s0, main_vs8_out_r3);
      with main_mkreg_out select res <=
            main_vs8_out when "00",
            main_vs8_out_r1 when "01",
            main_vs8_out_r2 when "10",
            main_vs8_out_r3 when others;
end architecture;

-- main.pc3
-- block '$L.pc3' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_pc3 is
      port (pc : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_pc3 is
      component \Main_setPC\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  pc : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      setpc_i : \Main_setPC\ port map (s0, pc, main_setpc_out);
      zzz_i : main_zzz port map (main_setpc_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.arm142
-- block '$L.arm142' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm142 is
      port (b0 : in std_logic_vector (0 downto 0);
            b1 : in std_logic_vector (0 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm142 is
      component \Main_mkReg\ is
            port (\Zds\ : in std_logic_vector (0 downto 0);
                  \Zds1\ : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component main_pc3 is
            port (pc : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_pc3_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_pc3_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_pc3_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_pc3_out_r3 : std_logic_vector (105 downto 0);
begin
      mkreg_i : \Main_mkReg\ port map (b0, b1, main_mkreg_out);
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      pc3_i : main_pc3 port map (main_r0_out, s0, main_pc3_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      pc3_i_r1 : main_pc3 port map (main_r1_out, s0, main_pc3_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      pc3_i_r2 : main_pc3 port map (main_r2_out, s0, main_pc3_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      pc3_i_r3 : main_pc3 port map (main_r3_out, s0, main_pc3_out_r3);
      with main_mkreg_out select res <=
            main_pc3_out when "00",
            main_pc3_out_r1 when "01",
            main_pc3_out_r2 when "10",
            main_pc3_out_r3 when others;
end architecture;

-- main.x3
-- block '$L.x3' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_x3 is
      port (x : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_x3 is
      component \Main_setPC\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  pc : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      setpc_i : \Main_setPC\ port map (s0, x, main_setpc_out);
      zzz_i : main_zzz port map (main_setpc_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.arm170
-- block '$L.arm170' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm170 is
      port (v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm170 is
      component \Main_setR0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r0 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      setr0_i : \Main_setR0\ port map (s0, v, main_setr0_out);
      zzz_i : main_zzz port map (main_setr0_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.arm171
-- block '$L.arm171' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm171 is
      port (v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm171 is
      component \Main_setR1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r1 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      setr1_i : \Main_setR1\ port map (s0, v, main_setr1_out);
      zzz_i : main_zzz port map (main_setr1_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.arm172
-- block '$L.arm172' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm172 is
      port (v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm172 is
      component \Main_setR2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r2 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      setr2_i : \Main_setR2\ port map (s0, v, main_setr2_out);
      zzz_i : main_zzz port map (main_setr2_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.arm173
-- block '$L.arm173' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm173 is
      port (v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm173 is
      component \Main_setR3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r3 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      setr3_i : \Main_setR3\ port map (s0, v, main_setr3_out);
      zzz_i : main_zzz port map (main_setr3_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.v3
-- block '$L.v3' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v3 is
      port (r : in std_logic_vector (1 downto 0);
            v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v3 is
      component main_arm170 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm171 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm172 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm173 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal v1 : std_logic_vector (7 downto 0);
      signal main_arm170_out : std_logic_vector (105 downto 0);
      signal main_arm171_out : std_logic_vector (105 downto 0);
      signal main_arm172_out : std_logic_vector (105 downto 0);
      signal main_arm173_out : std_logic_vector (105 downto 0);
begin
      v1 <= rw_not(v);
      arm170_i : main_arm170 port map (v1, s0, main_arm170_out);
      arm171_i : main_arm171 port map (v1, s0, main_arm171_out);
      arm172_i : main_arm172 port map (v1, s0, main_arm172_out);
      arm173_i : main_arm173 port map (v1, s0, main_arm173_out);
      with r select res <=
            main_arm170_out when "00",
            main_arm171_out when "01",
            main_arm172_out when "10",
            main_arm173_out when others;
end architecture;

-- main._unused44
-- block '$L._unused44' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused44\ is
      port (p : in std_logic_vector (8 downto 0);
            \v$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main___unused44\ is
      component \Main_setCFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  c : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  z : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal x : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal conn : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
begin
      x <= p(8 downto 8);
      setcflag_i : \Main_setCFlag\ port map (s0, x, main_setcflag_out);
      conn <= rw_eq(\v$\, std_logic_vector'(X"00"));
      setzflag_i : \Main_setZFlag\ port map (main_setcflag_out, conn, main_setzflag_out);
      zzz_i : main_zzz port map (main_setzflag_out, main_zzz_out);
      res <= main_zzz_out;
end architecture;

-- main.arm184
-- block '$L.arm184' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm184 is
      port (p : in std_logic_vector (8 downto 0);
            \v$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm184 is
      component \Main_setR0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r0 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
      setr0_i : \Main_setR0\ port map (s0, \v$\, main_setr0_out);
      \_unused44_i\ : \main___unused44\ port map (p, \v$\, main_setr0_out, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

-- main.arm185
-- block '$L.arm185' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm185 is
      port (p : in std_logic_vector (8 downto 0);
            \v$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm185 is
      component \Main_setR1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r1 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
      setr1_i : \Main_setR1\ port map (s0, \v$\, main_setr1_out);
      \_unused44_i\ : \main___unused44\ port map (p, \v$\, main_setr1_out, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

-- main.arm186
-- block '$L.arm186' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm186 is
      port (p : in std_logic_vector (8 downto 0);
            \v$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm186 is
      component \Main_setR2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r2 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
      setr2_i : \Main_setR2\ port map (s0, \v$\, main_setr2_out);
      \_unused44_i\ : \main___unused44\ port map (p, \v$\, main_setr2_out, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

-- main.arm187
-- block '$L.arm187' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm187 is
      port (p : in std_logic_vector (8 downto 0);
            \v$\ : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_arm187 is
      component \Main_setR3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r3 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \main___unused44\ is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal \main__unused44_out\ : std_logic_vector (105 downto 0);
begin
      setr3_i : \Main_setR3\ port map (s0, \v$\, main_setr3_out);
      \_unused44_i\ : \main___unused44\ port map (p, \v$\, main_setr3_out, \main__unused44_out\);
      res <= \main__unused44_out\;
end architecture;

-- main.v4
-- block '$L.v4' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v4 is
      port (r : in std_logic_vector (1 downto 0);
            v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v4 is
      component \Main_plusCW8$sMain__oneW8__False__Bool\ is
            port (a : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (8 downto 0));
      end component;
      component main_arm184 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm185 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm186 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm187 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal \main_pluscw8$smain_onew8_false_bool_out\ : std_logic_vector (8 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal main_arm184_out : std_logic_vector (105 downto 0);
      signal main_arm185_out : std_logic_vector (105 downto 0);
      signal main_arm186_out : std_logic_vector (105 downto 0);
      signal main_arm187_out : std_logic_vector (105 downto 0);
begin
      \pluscw8$smain_onew8_false_bool_i\ : \Main_plusCW8$sMain__oneW8__False__Bool\ port map (v, \main_pluscw8$smain_onew8_false_bool_out\);
      y <= \main_pluscw8$smain_onew8_false_bool_out\(7 downto 0);
      arm184_i : main_arm184 port map (\main_pluscw8$smain_onew8_false_bool_out\, y, s0, main_arm184_out);
      arm185_i : main_arm185 port map (\main_pluscw8$smain_onew8_false_bool_out\, y, s0, main_arm185_out);
      arm186_i : main_arm186 port map (\main_pluscw8$smain_onew8_false_bool_out\, y, s0, main_arm186_out);
      arm187_i : main_arm187 port map (\main_pluscw8$smain_onew8_false_bool_out\, y, s0, main_arm187_out);
      with r select res <=
            main_arm184_out when "00",
            main_arm185_out when "01",
            main_arm186_out when "10",
            main_arm187_out when others;
end architecture;

-- main.v5
-- block '$L.v5' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v5 is
      port (r : in std_logic_vector (1 downto 0);
            v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v5 is
      component main_arm184 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm185 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm186 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm187 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal s : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal main_arm184_out : std_logic_vector (105 downto 0);
      signal main_arm185_out : std_logic_vector (105 downto 0);
      signal main_arm186_out : std_logic_vector (105 downto 0);
      signal main_arm187_out : std_logic_vector (105 downto 0);
begin
      s <= rw_sub(rw_sub(rw_resize(v, 9), std_logic_vector'(B"000000001")), std_logic_vector'(B"000000000"));
      p <= (s(8 downto 8) & rw_resize(s, 8));
      y <= p(7 downto 0);
      arm184_i : main_arm184 port map (p, y, s0, main_arm184_out);
      arm185_i : main_arm185 port map (p, y, s0, main_arm185_out);
      arm186_i : main_arm186 port map (p, y, s0, main_arm186_out);
      arm187_i : main_arm187 port map (p, y, s0, main_arm187_out);
      with r select res <=
            main_arm184_out when "00",
            main_arm185_out when "01",
            main_arm186_out when "10",
            main_arm187_out when others;
end architecture;

-- main.v6
-- block '$L.v6' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v6 is
      port (r : in std_logic_vector (1 downto 0);
            v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v6 is
      component main_arm170 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm171 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm172 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm173 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal v1 : std_logic_vector (7 downto 0);
      signal main_arm170_out : std_logic_vector (105 downto 0);
      signal main_arm171_out : std_logic_vector (105 downto 0);
      signal main_arm172_out : std_logic_vector (105 downto 0);
      signal main_arm173_out : std_logic_vector (105 downto 0);
begin
      v1 <= rw_or(rw_shiftl(v, std_logic_vector'(X"01")), rw_shiftr(v, std_logic_vector'(X"07")));
      arm170_i : main_arm170 port map (v1, s0, main_arm170_out);
      arm171_i : main_arm171 port map (v1, s0, main_arm171_out);
      arm172_i : main_arm172 port map (v1, s0, main_arm172_out);
      arm173_i : main_arm173 port map (v1, s0, main_arm173_out);
      with r select res <=
            main_arm170_out when "00",
            main_arm171_out when "01",
            main_arm172_out when "10",
            main_arm173_out when others;
end architecture;

-- main.v7
-- block '$L.v7' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v7 is
      port (r : in std_logic_vector (1 downto 0);
            v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v7 is
      component main_arm170 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm171 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm172 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm173 is
            port (v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal v1 : std_logic_vector (7 downto 0);
      signal main_arm170_out : std_logic_vector (105 downto 0);
      signal main_arm171_out : std_logic_vector (105 downto 0);
      signal main_arm172_out : std_logic_vector (105 downto 0);
      signal main_arm173_out : std_logic_vector (105 downto 0);
begin
      v1 <= rw_or(rw_shiftr(v, std_logic_vector'(X"01")), rw_shiftl(v, std_logic_vector'(X"07")));
      arm170_i : main_arm170 port map (v1, s0, main_arm170_out);
      arm171_i : main_arm171 port map (v1, s0, main_arm171_out);
      arm172_i : main_arm172 port map (v1, s0, main_arm172_out);
      arm173_i : main_arm173 port map (v1, s0, main_arm173_out);
      with r select res <=
            main_arm170_out when "00",
            main_arm171_out when "01",
            main_arm172_out when "10",
            main_arm173_out when others;
end architecture;

-- main.v8
-- block '$L.v8' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_v8 is
      port (\rEn\ : in std_logic_vector (0 downto 0);
            \wEn\ : in std_logic_vector (0 downto 0);
            r : in std_logic_vector (1 downto 0);
            v : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_v8 is
      component \Main_lsbW8\ is
            port (v : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_msbW8\ is
            port (v : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component main_arm184 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm185 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm186 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm187 is
            port (p : in std_logic_vector (8 downto 0);
                  \v$\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_msbw8_out : std_logic_vector (0 downto 0);
      signal za1 : std_logic_vector (8 downto 0);
      signal za : std_logic_vector (8 downto 0);
      signal za_r1 : std_logic_vector (8 downto 0);
      signal main_lsbw8_out : std_logic_vector (0 downto 0);
      signal za1_r1 : std_logic_vector (8 downto 0);
      signal za_r2 : std_logic_vector (8 downto 0);
      signal za_r3 : std_logic_vector (8 downto 0);
      signal p : std_logic_vector (8 downto 0);
      signal y : std_logic_vector (7 downto 0);
      signal main_arm184_out : std_logic_vector (105 downto 0);
      signal main_arm185_out : std_logic_vector (105 downto 0);
      signal main_arm186_out : std_logic_vector (105 downto 0);
      signal main_arm187_out : std_logic_vector (105 downto 0);
begin
      msbw8_i : \Main_msbW8\ port map (v, main_msbw8_out);
      za1 <= (main_msbw8_out & rw_or(rw_shiftl(v, std_logic_vector'(X"01")), rw_resize(main_msbw8_out, 8)));
      za <= (main_msbw8_out & rw_or(rw_shiftl(v, std_logic_vector'(X"01")), std_logic_vector'(X"00")));
      za_r1 <= rw_cond(rw_not(\rEn\), za1, za);
      lsbw8_i : \Main_lsbW8\ port map (v, main_lsbw8_out);
      za1_r1 <= (main_lsbw8_out & rw_or(rw_shiftr(v, std_logic_vector'(X"01")), rw_shiftl(rw_resize(main_lsbw8_out, 8), std_logic_vector'(X"07"))));
      za_r2 <= (main_lsbw8_out & rw_or(rw_shiftr(v, std_logic_vector'(X"01")), std_logic_vector'(X"00")));
      za_r3 <= rw_cond(rw_not(\rEn\), za1_r1, za_r2);
      p <= rw_cond(rw_not(\wEn\), za_r1, za_r3);
      y <= p(7 downto 0);
      arm184_i : main_arm184 port map (p, y, s0, main_arm184_out);
      arm185_i : main_arm185 port map (p, y, s0, main_arm185_out);
      arm186_i : main_arm186 port map (p, y, s0, main_arm186_out);
      arm187_i : main_arm187 port map (p, y, s0, main_arm187_out);
      with r select res <=
            main_arm184_out when "00",
            main_arm185_out when "01",
            main_arm186_out when "10",
            main_arm187_out when others;
end architecture;

-- main.$fail
-- block '$L.$fail' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_$fail\ is
      port (inp : in std_logic_vector (9 downto 0);
            s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of \main_$fail\ is
      component \Main_cFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_dataIn\ is
            port (\Zds\ : in std_logic_vector (9 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_mkReg\ is
            port (\Zds\ : in std_logic_vector (0 downto 0);
                  \Zds1\ : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_notb\ is
            port (b : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_pc\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_setAddrOut\ is
            port (\Zds\ : in std_logic_vector (17 downto 0);
                  a_o : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (17 downto 0));
      end component;
      component \Main_setCFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  c : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setIEFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  ie : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setOutputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  o : in std_logic_vector (17 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setPC\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  pc : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR0\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r0 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR1\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r1 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR2\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r2 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setR3\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  r3 : in std_logic_vector (7 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  z : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_zFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component main_a2 is
            port (\rEn\ : in std_logic_vector (0 downto 0);
                  \wEn\ : in std_logic_vector (0 downto 0);
                  a : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_a3 is
            port (\rEn\ : in std_logic_vector (0 downto 0);
                  \wEn\ : in std_logic_vector (0 downto 0);
                  a : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_arm142 is
            port (b0 : in std_logic_vector (0 downto 0);
                  b1 : in std_logic_vector (0 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_v3 is
            port (r : in std_logic_vector (1 downto 0);
                  v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_v4 is
            port (r : in std_logic_vector (1 downto 0);
                  v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_v5 is
            port (r : in std_logic_vector (1 downto 0);
                  v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_v6 is
            port (r : in std_logic_vector (1 downto 0);
                  v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_v7 is
            port (r : in std_logic_vector (1 downto 0);
                  v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_v8 is
            port (\rEn\ : in std_logic_vector (0 downto 0);
                  \wEn\ : in std_logic_vector (0 downto 0);
                  r : in std_logic_vector (1 downto 0);
                  v : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \rS\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD2\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \rS\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD3\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \rS\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD4\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \rS\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD5\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \rS\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD6\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \rS\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD7\ is
            port (\rD\ : in std_logic_vector (1 downto 0);
                  \rS\ : in std_logic_vector (1 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component \main_vD8\ is
            port (b0 : in std_logic_vector (0 downto 0);
                  b1 : in std_logic_vector (0 downto 0);
                  \vD\ : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_x2 is
            port (\rEn\ : in std_logic_vector (0 downto 0);
                  \wEn\ : in std_logic_vector (0 downto 0);
                  x : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_x3 is
            port (x : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_datain_out : std_logic_vector (7 downto 0);
      signal zds1 : std_logic_vector (0 downto 0);
      signal zds2 : std_logic_vector (0 downto 0);
      signal zds3 : std_logic_vector (0 downto 0);
      signal ren : std_logic_vector (0 downto 0);
      signal wen : std_logic_vector (0 downto 0);
      signal b0 : std_logic_vector (0 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal za : std_logic_vector (0 downto 0);
      signal main_mkreg_out : std_logic_vector (1 downto 0);
      signal main_pc_out : std_logic_vector (7 downto 0);
      signal main_outputs_out : std_logic_vector (17 downto 0);
      signal main_setaddrout_out : std_logic_vector (17 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal main_outputs_out_r1 : std_logic_vector (17 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal main_a2_out : std_logic_vector (105 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_a2_out_r1 : std_logic_vector (105 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_a2_out_r2 : std_logic_vector (105 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_a2_out_r3 : std_logic_vector (105 downto 0);
      signal main_a3_out : std_logic_vector (105 downto 0);
      signal main_a3_out_r1 : std_logic_vector (105 downto 0);
      signal main_a3_out_r2 : std_logic_vector (105 downto 0);
      signal main_a3_out_r3 : std_logic_vector (105 downto 0);
      signal main_mkreg_out_r1 : std_logic_vector (1 downto 0);
      signal main_vd_out : std_logic_vector (105 downto 0);
      signal main_vd_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd_out_r3 : std_logic_vector (105 downto 0);
      signal main_vd2_out : std_logic_vector (105 downto 0);
      signal main_vd2_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd2_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd2_out_r3 : std_logic_vector (105 downto 0);
      signal main_vd3_out : std_logic_vector (105 downto 0);
      signal main_vd3_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd3_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd3_out_r3 : std_logic_vector (105 downto 0);
      signal main_vd4_out : std_logic_vector (105 downto 0);
      signal main_vd4_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd4_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd4_out_r3 : std_logic_vector (105 downto 0);
      signal main_x2_out : std_logic_vector (105 downto 0);
      signal main_x2_out_r1 : std_logic_vector (105 downto 0);
      signal main_x2_out_r2 : std_logic_vector (105 downto 0);
      signal main_x2_out_r3 : std_logic_vector (105 downto 0);
      signal main_vd5_out : std_logic_vector (105 downto 0);
      signal main_vd5_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd5_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd5_out_r3 : std_logic_vector (105 downto 0);
      signal main_vd6_out : std_logic_vector (105 downto 0);
      signal main_vd6_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd6_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd6_out_r3 : std_logic_vector (105 downto 0);
      signal main_vd7_out : std_logic_vector (105 downto 0);
      signal main_vd7_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd7_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd7_out_r3 : std_logic_vector (105 downto 0);
      signal main_vd8_out : std_logic_vector (105 downto 0);
      signal main_vd8_out_r1 : std_logic_vector (105 downto 0);
      signal main_vd8_out_r2 : std_logic_vector (105 downto 0);
      signal main_vd8_out_r3 : std_logic_vector (105 downto 0);
      signal main_zflag_out : std_logic_vector (0 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
      signal main_arm142_out : std_logic_vector (105 downto 0);
      signal main_notb_out : std_logic_vector (0 downto 0);
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal main_notb_out_r1 : std_logic_vector (0 downto 0);
      signal main_x3_out : std_logic_vector (105 downto 0);
      signal main_x3_out_r1 : std_logic_vector (105 downto 0);
      signal main_x3_out_r2 : std_logic_vector (105 downto 0);
      signal main_x3_out_r3 : std_logic_vector (105 downto 0);
      signal main_setieflag_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r1 : std_logic_vector (105 downto 0);
      signal a_o : std_logic_vector (7 downto 0);
      signal d_o : std_logic_vector (7 downto 0);
      signal we_o : std_logic_vector (0 downto 0);
      signal za_r1 : std_logic_vector (17 downto 0);
      signal main_setoutputs_out_r1 : std_logic_vector (80 downto 0);
      signal main_zzz_out_r2 : std_logic_vector (105 downto 0);
      signal main_setieflag_out_r1 : std_logic_vector (80 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal main_setpc_out : std_logic_vector (80 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r3 : std_logic_vector (105 downto 0);
      signal main_v3_out : std_logic_vector (105 downto 0);
      signal main_v3_out_r1 : std_logic_vector (105 downto 0);
      signal main_v3_out_r2 : std_logic_vector (105 downto 0);
      signal main_v3_out_r3 : std_logic_vector (105 downto 0);
      signal main_setr0_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r4 : std_logic_vector (105 downto 0);
      signal main_setr1_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r5 : std_logic_vector (105 downto 0);
      signal main_setr2_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r6 : std_logic_vector (105 downto 0);
      signal main_setr3_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r7 : std_logic_vector (105 downto 0);
      signal main_v4_out : std_logic_vector (105 downto 0);
      signal main_v4_out_r1 : std_logic_vector (105 downto 0);
      signal main_v4_out_r2 : std_logic_vector (105 downto 0);
      signal main_v4_out_r3 : std_logic_vector (105 downto 0);
      signal main_v5_out : std_logic_vector (105 downto 0);
      signal main_v5_out_r1 : std_logic_vector (105 downto 0);
      signal main_v5_out_r2 : std_logic_vector (105 downto 0);
      signal main_v5_out_r3 : std_logic_vector (105 downto 0);
      signal main_v6_out : std_logic_vector (105 downto 0);
      signal main_v6_out_r1 : std_logic_vector (105 downto 0);
      signal main_v6_out_r2 : std_logic_vector (105 downto 0);
      signal main_v6_out_r3 : std_logic_vector (105 downto 0);
      signal main_v7_out : std_logic_vector (105 downto 0);
      signal main_v7_out_r1 : std_logic_vector (105 downto 0);
      signal main_v7_out_r2 : std_logic_vector (105 downto 0);
      signal main_v7_out_r3 : std_logic_vector (105 downto 0);
      signal main_v8_out : std_logic_vector (105 downto 0);
      signal main_v8_out_r1 : std_logic_vector (105 downto 0);
      signal main_v8_out_r2 : std_logic_vector (105 downto 0);
      signal main_v8_out_r3 : std_logic_vector (105 downto 0);
begin
      datain_i : \Main_dataIn\ port map (inp, main_datain_out);
      zds1 <= main_datain_out(6 downto 6);
      zds2 <= main_datain_out(5 downto 5);
      zds3 <= main_datain_out(4 downto 4);
      ren <= main_datain_out(3 downto 3);
      wen <= main_datain_out(2 downto 2);
      b0 <= main_datain_out(1 downto 1);
      b1 <= main_datain_out(0 downto 0);
      za <= main_datain_out(7 downto 7);
      mkreg_i : \Main_mkReg\ port map (b0, b1, main_mkreg_out);
      pc_i : \Main_pc\ port map (s0, main_pc_out);
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      setaddrout_i : \Main_setAddrOut\ port map (main_outputs_out, main_pc_out, main_setaddrout_out);
      setoutputs_i : \Main_setOutputs\ port map (s0, main_setaddrout_out, main_setoutputs_out);
      outputs_i_r1 : \Main_outputs\ port map (main_setoutputs_out, main_outputs_out_r1);
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      a2_i : main_a2 port map (ren, wen, main_r0_out, s0, main_a2_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      a2_i_r1 : main_a2 port map (ren, wen, main_r1_out, s0, main_a2_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      a2_i_r2 : main_a2 port map (ren, wen, main_r2_out, s0, main_a2_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      a2_i_r3 : main_a2 port map (ren, wen, main_r3_out, s0, main_a2_out_r3);
      a3_i : main_a3 port map (ren, wen, main_r0_out, s0, main_a3_out);
      a3_i_r1 : main_a3 port map (ren, wen, main_r1_out, s0, main_a3_out_r1);
      a3_i_r2 : main_a3 port map (ren, wen, main_r2_out, s0, main_a3_out_r2);
      a3_i_r3 : main_a3 port map (ren, wen, main_r3_out, s0, main_a3_out_r3);
      mkreg_i_r1 : \Main_mkReg\ port map (ren, wen, main_mkreg_out_r1);
      vd_i : \main_vD\ port map (main_mkreg_out_r1, main_mkreg_out, main_r0_out, s0, main_vd_out);
      vd_i_r1 : \main_vD\ port map (main_mkreg_out_r1, main_mkreg_out, main_r1_out, s0, main_vd_out_r1);
      vd_i_r2 : \main_vD\ port map (main_mkreg_out_r1, main_mkreg_out, main_r2_out, s0, main_vd_out_r2);
      vd_i_r3 : \main_vD\ port map (main_mkreg_out_r1, main_mkreg_out, main_r3_out, s0, main_vd_out_r3);
      vd2_i : \main_vD2\ port map (main_mkreg_out_r1, main_mkreg_out, main_r0_out, s0, main_vd2_out);
      vd2_i_r1 : \main_vD2\ port map (main_mkreg_out_r1, main_mkreg_out, main_r1_out, s0, main_vd2_out_r1);
      vd2_i_r2 : \main_vD2\ port map (main_mkreg_out_r1, main_mkreg_out, main_r2_out, s0, main_vd2_out_r2);
      vd2_i_r3 : \main_vD2\ port map (main_mkreg_out_r1, main_mkreg_out, main_r3_out, s0, main_vd2_out_r3);
      vd3_i : \main_vD3\ port map (main_mkreg_out_r1, main_mkreg_out, main_r0_out, s0, main_vd3_out);
      vd3_i_r1 : \main_vD3\ port map (main_mkreg_out_r1, main_mkreg_out, main_r1_out, s0, main_vd3_out_r1);
      vd3_i_r2 : \main_vD3\ port map (main_mkreg_out_r1, main_mkreg_out, main_r2_out, s0, main_vd3_out_r2);
      vd3_i_r3 : \main_vD3\ port map (main_mkreg_out_r1, main_mkreg_out, main_r3_out, s0, main_vd3_out_r3);
      vd4_i : \main_vD4\ port map (main_mkreg_out_r1, main_mkreg_out, main_r0_out, s0, main_vd4_out);
      vd4_i_r1 : \main_vD4\ port map (main_mkreg_out_r1, main_mkreg_out, main_r1_out, s0, main_vd4_out_r1);
      vd4_i_r2 : \main_vD4\ port map (main_mkreg_out_r1, main_mkreg_out, main_r2_out, s0, main_vd4_out_r2);
      vd4_i_r3 : \main_vD4\ port map (main_mkreg_out_r1, main_mkreg_out, main_r3_out, s0, main_vd4_out_r3);
      x2_i : main_x2 port map (ren, wen, main_r0_out, s0, main_x2_out);
      x2_i_r1 : main_x2 port map (ren, wen, main_r1_out, s0, main_x2_out_r1);
      x2_i_r2 : main_x2 port map (ren, wen, main_r2_out, s0, main_x2_out_r2);
      x2_i_r3 : main_x2 port map (ren, wen, main_r3_out, s0, main_x2_out_r3);
      vd5_i : \main_vD5\ port map (main_mkreg_out_r1, main_mkreg_out, main_r0_out, s0, main_vd5_out);
      vd5_i_r1 : \main_vD5\ port map (main_mkreg_out_r1, main_mkreg_out, main_r1_out, s0, main_vd5_out_r1);
      vd5_i_r2 : \main_vD5\ port map (main_mkreg_out_r1, main_mkreg_out, main_r2_out, s0, main_vd5_out_r2);
      vd5_i_r3 : \main_vD5\ port map (main_mkreg_out_r1, main_mkreg_out, main_r3_out, s0, main_vd5_out_r3);
      vd6_i : \main_vD6\ port map (main_mkreg_out_r1, main_mkreg_out, main_r0_out, s0, main_vd6_out);
      vd6_i_r1 : \main_vD6\ port map (main_mkreg_out_r1, main_mkreg_out, main_r1_out, s0, main_vd6_out_r1);
      vd6_i_r2 : \main_vD6\ port map (main_mkreg_out_r1, main_mkreg_out, main_r2_out, s0, main_vd6_out_r2);
      vd6_i_r3 : \main_vD6\ port map (main_mkreg_out_r1, main_mkreg_out, main_r3_out, s0, main_vd6_out_r3);
      vd7_i : \main_vD7\ port map (main_mkreg_out_r1, main_mkreg_out, main_r0_out, s0, main_vd7_out);
      vd7_i_r1 : \main_vD7\ port map (main_mkreg_out_r1, main_mkreg_out, main_r1_out, s0, main_vd7_out_r1);
      vd7_i_r2 : \main_vD7\ port map (main_mkreg_out_r1, main_mkreg_out, main_r2_out, s0, main_vd7_out_r2);
      vd7_i_r3 : \main_vD7\ port map (main_mkreg_out_r1, main_mkreg_out, main_r3_out, s0, main_vd7_out_r3);
      vd8_i : \main_vD8\ port map (b0, b1, main_r0_out, s0, main_vd8_out);
      vd8_i_r1 : \main_vD8\ port map (b0, b1, main_r1_out, s0, main_vd8_out_r1);
      vd8_i_r2 : \main_vD8\ port map (b0, b1, main_r2_out, s0, main_vd8_out_r2);
      vd8_i_r3 : \main_vD8\ port map (b0, b1, main_r3_out, s0, main_vd8_out_r3);
      zflag_i : \Main_zFlag\ port map (s0, main_zflag_out);
      zzz_i : main_zzz port map (s0, main_zzz_out);
      arm142_i : main_arm142 port map (b0, b1, s0, main_arm142_out);
      notb_i : \Main_notb\ port map (main_zflag_out, main_notb_out);
      cflag_i : \Main_cFlag\ port map (s0, main_cflag_out);
      notb_i_r1 : \Main_notb\ port map (main_cflag_out, main_notb_out_r1);
      x3_i : main_x3 port map (main_r0_out, s0, main_x3_out);
      x3_i_r1 : main_x3 port map (main_r1_out, s0, main_x3_out_r1);
      x3_i_r2 : main_x3 port map (main_r2_out, s0, main_x3_out_r2);
      x3_i_r3 : main_x3 port map (main_r3_out, s0, main_x3_out_r3);
      setieflag_i : \Main_setIEFlag\ port map (s0, b1, main_setieflag_out);
      zzz_i_r1 : main_zzz port map (main_setieflag_out, main_zzz_out_r1);
      a_o <= main_outputs_out(17 downto 10);
      d_o <= main_outputs_out(9 downto 2);
      we_o <= main_outputs_out(1 downto 1);
      za_r1 <= (a_o & d_o & we_o & std_logic_vector'(B"1"));
      setoutputs_i_r1 : \Main_setOutputs\ port map (s0, za_r1, main_setoutputs_out_r1);
      zzz_i_r2 : main_zzz port map (main_setoutputs_out_r1, main_zzz_out_r2);
      setieflag_i_r1 : \Main_setIEFlag\ port map (s0, std_logic_vector'(B"1"), main_setieflag_out_r1);
      pcs <= main_setieflag_out_r1(39 downto 32);
      setpc_i : \Main_setPC\ port map (main_setieflag_out_r1, pcs, main_setpc_out);
      zs <= main_setpc_out(41 downto 41);
      setzflag_i : \Main_setZFlag\ port map (main_setpc_out, zs, main_setzflag_out);
      cs <= main_setzflag_out(40 downto 40);
      setcflag_i : \Main_setCFlag\ port map (main_setzflag_out, cs, main_setcflag_out);
      zzz_i_r3 : main_zzz port map (main_setcflag_out, main_zzz_out_r3);
      v3_i : main_v3 port map (main_mkreg_out, main_r0_out, s0, main_v3_out);
      v3_i_r1 : main_v3 port map (main_mkreg_out, main_r1_out, s0, main_v3_out_r1);
      v3_i_r2 : main_v3 port map (main_mkreg_out, main_r2_out, s0, main_v3_out_r2);
      v3_i_r3 : main_v3 port map (main_mkreg_out, main_r3_out, s0, main_v3_out_r3);
      setr0_i : \Main_setR0\ port map (s0, std_logic_vector'(X"00"), main_setr0_out);
      zzz_i_r4 : main_zzz port map (main_setr0_out, main_zzz_out_r4);
      setr1_i : \Main_setR1\ port map (s0, std_logic_vector'(X"00"), main_setr1_out);
      zzz_i_r5 : main_zzz port map (main_setr1_out, main_zzz_out_r5);
      setr2_i : \Main_setR2\ port map (s0, std_logic_vector'(X"00"), main_setr2_out);
      zzz_i_r6 : main_zzz port map (main_setr2_out, main_zzz_out_r6);
      setr3_i : \Main_setR3\ port map (s0, std_logic_vector'(X"00"), main_setr3_out);
      zzz_i_r7 : main_zzz port map (main_setr3_out, main_zzz_out_r7);
      v4_i : main_v4 port map (main_mkreg_out, main_r0_out, s0, main_v4_out);
      v4_i_r1 : main_v4 port map (main_mkreg_out, main_r1_out, s0, main_v4_out_r1);
      v4_i_r2 : main_v4 port map (main_mkreg_out, main_r2_out, s0, main_v4_out_r2);
      v4_i_r3 : main_v4 port map (main_mkreg_out, main_r3_out, s0, main_v4_out_r3);
      v5_i : main_v5 port map (main_mkreg_out, main_r0_out, s0, main_v5_out);
      v5_i_r1 : main_v5 port map (main_mkreg_out, main_r1_out, s0, main_v5_out_r1);
      v5_i_r2 : main_v5 port map (main_mkreg_out, main_r2_out, s0, main_v5_out_r2);
      v5_i_r3 : main_v5 port map (main_mkreg_out, main_r3_out, s0, main_v5_out_r3);
      v6_i : main_v6 port map (main_mkreg_out, main_r0_out, s0, main_v6_out);
      v6_i_r1 : main_v6 port map (main_mkreg_out, main_r1_out, s0, main_v6_out_r1);
      v6_i_r2 : main_v6 port map (main_mkreg_out, main_r2_out, s0, main_v6_out_r2);
      v6_i_r3 : main_v6 port map (main_mkreg_out, main_r3_out, s0, main_v6_out_r3);
      v7_i : main_v7 port map (main_mkreg_out, main_r0_out, s0, main_v7_out);
      v7_i_r1 : main_v7 port map (main_mkreg_out, main_r1_out, s0, main_v7_out_r1);
      v7_i_r2 : main_v7 port map (main_mkreg_out, main_r2_out, s0, main_v7_out_r2);
      v7_i_r3 : main_v7 port map (main_mkreg_out, main_r3_out, s0, main_v7_out_r3);
      v8_i : main_v8 port map (ren, wen, main_mkreg_out, main_r0_out, s0, main_v8_out);
      v8_i_r1 : main_v8 port map (ren, wen, main_mkreg_out, main_r1_out, s0, main_v8_out_r1);
      v8_i_r2 : main_v8 port map (ren, wen, main_mkreg_out, main_r2_out, s0, main_v8_out_r2);
      v8_i_r3 : main_v8 port map (ren, wen, main_mkreg_out, main_r3_out, s0, main_v8_out_r3);
      res <= rw_cond(rw_not(za), rw_cond(rw_not(zds1), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), (main_outputs_out_r1 & std_logic_vector'(B"001") & ren & wen & main_mkreg_out & main_setoutputs_out), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_a2_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_a2_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_a2_out_r2, main_a2_out_r3)))), rw_cond(rw_not(zds3), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_a3_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_a3_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_a3_out_r2, main_a3_out_r3))), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd_out_r2, main_vd_out_r3))))), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd2_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd2_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd2_out_r2, main_vd2_out_r3))), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd3_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd3_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd3_out_r2, main_vd3_out_r3)))), rw_cond(rw_not(zds3), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd4_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd4_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd4_out_r2, main_vd4_out_r3))), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_x2_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_x2_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_x2_out_r2, main_x2_out_r3)))))), rw_cond(rw_not(zds1), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd5_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd5_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd5_out_r2, main_vd5_out_r3))), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd6_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd6_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd6_out_r2, main_vd6_out_r3)))), rw_cond(rw_not(zds3), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd7_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd7_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd7_out_r2, main_vd7_out_r3))), rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"00")), main_vd8_out, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"01")), main_vd8_out_r1, rw_cond(rw_eq(main_mkreg_out_r1, std_logic_vector'(B"10")), main_vd8_out_r2, main_vd8_out_r3))))), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), rw_cond(rw_not(ren), rw_cond(rw_not(wen), rw_cond(rw_not(main_zflag_out), main_zzz_out, main_arm142_out), rw_cond(rw_not(main_notb_out), main_zzz_out, main_arm142_out)), rw_cond(rw_not(wen), rw_cond(rw_not(main_cflag_out), main_zzz_out, main_arm142_out), rw_cond(rw_not(main_notb_out_r1), main_zzz_out, main_arm142_out))), rw_cond(rw_not(ren), rw_cond(rw_not(wen), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_x3_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_x3_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_x3_out_r2, main_x3_out_r3))), rw_cond(rw_not(b0), main_zzz_out_r1, rw_cond(rw_not(b1), main_zzz_out_r2, main_zzz_out_r3))), rw_cond(rw_not(wen), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_v3_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_v3_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_v3_out_r2, main_v3_out_r3))), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_zzz_out_r4, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_zzz_out_r5, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_zzz_out_r6, main_zzz_out_r7)))))), rw_cond(rw_not(zds3), rw_cond(rw_not(ren), rw_cond(rw_not(wen), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_v4_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_v4_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_v4_out_r2, main_v4_out_r3))), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_v5_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_v5_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_v5_out_r2, main_v5_out_r3)))), rw_cond(rw_not(wen), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_v6_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_v6_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_v6_out_r2, main_v6_out_r3))), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_v7_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_v7_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_v7_out_r2, main_v7_out_r3))))), rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"00")), main_v8_out, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"01")), main_v8_out_r1, rw_cond(rw_eq(main_mkreg_out, std_logic_vector'(B"10")), main_v8_out_r2, main_v8_out_r3)))))));
end architecture;

-- main.loop
-- block '$L.Main.loop' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_loop is
      port (s0 : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (105 downto 0));
end entity;

architecture rtl of main_loop is
      component \Main_cFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_inputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (9 downto 0));
      end component;
      component \Main_pc\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_setCFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  c : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setIEFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  ie : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setOutputs\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  o : in std_logic_vector (17 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_setZFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  z : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (80 downto 0));
      end component;
      component \Main_zFlag\ is
            port (\Zds\ : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \main_$fail\ is
            port (inp : in std_logic_vector (9 downto 0);
                  s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      component main_zzz is
            port (s0 : in std_logic_vector (80 downto 0);
                  res : out std_logic_vector (105 downto 0));
      end component;
      signal main_inputs_out : std_logic_vector (9 downto 0);
      signal r_i : std_logic_vector (0 downto 0);
      signal ie : std_logic_vector (0 downto 0);
      signal i_i : std_logic_vector (0 downto 0);
      signal main_setieflag_out : std_logic_vector (80 downto 0);
      signal main_pc_out : std_logic_vector (7 downto 0);
      signal main_zflag_out : std_logic_vector (0 downto 0);
      signal main_cflag_out : std_logic_vector (0 downto 0);
      signal i : std_logic_vector (9 downto 0);
      signal o : std_logic_vector (17 downto 0);
      signal z : std_logic_vector (0 downto 0);
      signal c : std_logic_vector (0 downto 0);
      signal ie_r1 : std_logic_vector (0 downto 0);
      signal pc : std_logic_vector (7 downto 0);
      signal zs : std_logic_vector (0 downto 0);
      signal cs : std_logic_vector (0 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
      signal za : std_logic_vector (80 downto 0);
      signal i_r1 : std_logic_vector (9 downto 0);
      signal o_r1 : std_logic_vector (17 downto 0);
      signal z_r1 : std_logic_vector (0 downto 0);
      signal c_r1 : std_logic_vector (0 downto 0);
      signal ie_r2 : std_logic_vector (0 downto 0);
      signal pc_r1 : std_logic_vector (7 downto 0);
      signal cs_r1 : std_logic_vector (0 downto 0);
      signal pcs : std_logic_vector (7 downto 0);
      signal r0_r1 : std_logic_vector (7 downto 0);
      signal r1_r1 : std_logic_vector (7 downto 0);
      signal r2_r1 : std_logic_vector (7 downto 0);
      signal r3_r1 : std_logic_vector (7 downto 0);
      signal za_r1 : std_logic_vector (80 downto 0);
      signal i_r2 : std_logic_vector (9 downto 0);
      signal o_r2 : std_logic_vector (17 downto 0);
      signal z_r2 : std_logic_vector (0 downto 0);
      signal c_r2 : std_logic_vector (0 downto 0);
      signal ie_r3 : std_logic_vector (0 downto 0);
      signal pc_r2 : std_logic_vector (7 downto 0);
      signal zs_r1 : std_logic_vector (0 downto 0);
      signal pcs_r1 : std_logic_vector (7 downto 0);
      signal r0_r2 : std_logic_vector (7 downto 0);
      signal r1_r2 : std_logic_vector (7 downto 0);
      signal r2_r2 : std_logic_vector (7 downto 0);
      signal r3_r2 : std_logic_vector (7 downto 0);
      signal za_r2 : std_logic_vector (80 downto 0);
      signal main_zzz_out : std_logic_vector (105 downto 0);
      signal \main_$fail_out\ : std_logic_vector (105 downto 0);
      signal main_setcflag_out : std_logic_vector (80 downto 0);
      signal main_setzflag_out : std_logic_vector (80 downto 0);
      signal main_setoutputs_out : std_logic_vector (80 downto 0);
      signal main_zzz_out_r1 : std_logic_vector (105 downto 0);
begin
      inputs_i : \Main_inputs\ port map (s0, main_inputs_out);
      r_i <= main_inputs_out(1 downto 1);
      ie <= s0(50 downto 50);
      i_i <= main_inputs_out(0 downto 0);
      setieflag_i : \Main_setIEFlag\ port map (s0, std_logic_vector'(B"0"), main_setieflag_out);
      pc_i : \Main_pc\ port map (main_setieflag_out, main_pc_out);
      zflag_i : \Main_zFlag\ port map (main_setieflag_out, main_zflag_out);
      cflag_i : \Main_cFlag\ port map (main_setieflag_out, main_cflag_out);
      i <= main_setieflag_out(80 downto 71);
      o <= main_setieflag_out(70 downto 53);
      z <= main_setieflag_out(52 downto 52);
      c <= main_setieflag_out(51 downto 51);
      ie_r1 <= main_setieflag_out(50 downto 50);
      pc <= main_setieflag_out(49 downto 42);
      zs <= main_setieflag_out(41 downto 41);
      cs <= main_setieflag_out(40 downto 40);
      r0 <= main_setieflag_out(31 downto 24);
      r1 <= main_setieflag_out(23 downto 16);
      r2 <= main_setieflag_out(15 downto 8);
      r3 <= main_setieflag_out(7 downto 0);
      za <= (i & o & z & c & ie_r1 & pc & zs & cs & main_pc_out & r0 & r1 & r2 & r3);
      i_r1 <= za(80 downto 71);
      o_r1 <= za(70 downto 53);
      z_r1 <= za(52 downto 52);
      c_r1 <= za(51 downto 51);
      ie_r2 <= za(50 downto 50);
      pc_r1 <= za(49 downto 42);
      cs_r1 <= za(40 downto 40);
      pcs <= za(39 downto 32);
      r0_r1 <= za(31 downto 24);
      r1_r1 <= za(23 downto 16);
      r2_r1 <= za(15 downto 8);
      r3_r1 <= za(7 downto 0);
      za_r1 <= (i_r1 & o_r1 & z_r1 & c_r1 & ie_r2 & pc_r1 & main_zflag_out & cs_r1 & pcs & r0_r1 & r1_r1 & r2_r1 & r3_r1);
      i_r2 <= za_r1(80 downto 71);
      o_r2 <= za_r1(70 downto 53);
      z_r2 <= za_r1(52 downto 52);
      c_r2 <= za_r1(51 downto 51);
      ie_r3 <= za_r1(50 downto 50);
      pc_r2 <= za_r1(49 downto 42);
      zs_r1 <= za_r1(41 downto 41);
      pcs_r1 <= za_r1(39 downto 32);
      r0_r2 <= za_r1(31 downto 24);
      r1_r2 <= za_r1(23 downto 16);
      r2_r2 <= za_r1(15 downto 8);
      r3_r2 <= za_r1(7 downto 0);
      za_r2 <= (i_r2 & o_r2 & z_r2 & c_r2 & ie_r3 & pc_r2 & zs_r1 & main_cflag_out & pcs_r1 & r0_r2 & r1_r2 & r2_r2 & r3_r2);
      zzz_i : main_zzz port map (za_r2, main_zzz_out);
      zfail_i : \main_$fail\ port map (main_inputs_out, s0, \main_$fail_out\);
      setcflag_i : \Main_setCFlag\ port map (s0, std_logic_vector'(B"0"), main_setcflag_out);
      setzflag_i : \Main_setZFlag\ port map (main_setcflag_out, std_logic_vector'(B"0"), main_setzflag_out);
      setoutputs_i : \Main_setOutputs\ port map (main_setzflag_out, std_logic_vector'(B"000000000000000000"), main_setoutputs_out);
      zzz_i_r1 : main_zzz port map (main_setoutputs_out, main_zzz_out_r1);
      res <= rw_cond(rw_not(r_i), rw_cond(ie, rw_cond(i_i, main_zzz_out, \main_$fail_out\), \main_$fail_out\), main_zzz_out_r1);
end architecture;

-- Main.notb
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_notb\ is
      port (b : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_notb\ is
begin
      res <= rw_not(b);
end architecture;

-- Main.mkReg
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_mkReg\ is
      port (\Zds\ : in std_logic_vector (0 downto 0);
            \Zds1\ : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (1 downto 0));
end entity;

architecture rtl of \Main_mkReg\ is
begin
      res <= rw_cond(rw_not(\Zds\), rw_cond(rw_not(\Zds1\), std_logic_vector'(B"00"), std_logic_vector'(B"01")), rw_cond(rw_not(\Zds1\), std_logic_vector'(B"10"), std_logic_vector'(B"11")));
end architecture;

-- Main.outputs
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_outputs\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_outputs\ is
      signal o : std_logic_vector (17 downto 0);
begin
      o <= \Zds\(70 downto 53);
      res <= o;
end architecture;

-- Main.setInputs
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setInputs\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            i : in std_logic_vector (9 downto 0);
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
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.pc
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_pc\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_pc\ is
      signal pc : std_logic_vector (7 downto 0);
begin
      pc <= \Zds\(49 downto 42);
      res <= pc;
end architecture;

-- Main.setPC
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setPC\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            pc : in std_logic_vector (7 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.r0
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r0\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r0\ is
      signal r0 : std_logic_vector (7 downto 0);
begin
      r0 <= \Zds\(31 downto 24);
      res <= r0;
end architecture;

-- Main.r1
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r1\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r1\ is
      signal r1 : std_logic_vector (7 downto 0);
begin
      r1 <= \Zds\(23 downto 16);
      res <= r1;
end architecture;

-- Main.r2
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r2\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r2\ is
      signal r2 : std_logic_vector (7 downto 0);
begin
      r2 <= \Zds\(15 downto 8);
      res <= r2;
end architecture;

-- Main.r3
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r3\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r3\ is
      signal r3 : std_logic_vector (7 downto 0);
begin
      r3 <= \Zds\(7 downto 0);
      res <= r3;
end architecture;

-- Main.setR0
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR0\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            r0 : in std_logic_vector (7 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.setR1
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR1\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            r1 : in std_logic_vector (7 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.setR2
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR2\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            r2 : in std_logic_vector (7 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.setR3
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setR3\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            r3 : in std_logic_vector (7 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.zFlag
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_zFlag\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_zFlag\ is
      signal z : std_logic_vector (0 downto 0);
begin
      z <= \Zds\(52 downto 52);
      res <= z;
end architecture;

-- Main.setZFlag
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setZFlag\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            z : in std_logic_vector (0 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.cFlag
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_cFlag\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_cFlag\ is
      signal c : std_logic_vector (0 downto 0);
begin
      c <= \Zds\(51 downto 51);
      res <= c;
end architecture;

-- Main.setCFlag
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setCFlag\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            c : in std_logic_vector (0 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.setIEFlag
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setIEFlag\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            ie : in std_logic_vector (0 downto 0);
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
      i <= \Zds\(80 downto 71);
      o <= \Zds\(70 downto 53);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.inputs
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_inputs\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            res : out std_logic_vector (9 downto 0));
end entity;

architecture rtl of \Main_inputs\ is
      signal i : std_logic_vector (9 downto 0);
begin
      i <= \Zds\(80 downto 71);
      res <= i;
end architecture;

-- Main.setOutputs
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setOutputs\ is
      port (\Zds\ : in std_logic_vector (80 downto 0);
            o : in std_logic_vector (17 downto 0);
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
      i <= \Zds\(80 downto 71);
      z <= \Zds\(52 downto 52);
      c <= \Zds\(51 downto 51);
      ie <= \Zds\(50 downto 50);
      pc <= \Zds\(49 downto 42);
      zs <= \Zds\(41 downto 41);
      cs <= \Zds\(40 downto 40);
      pcs <= \Zds\(39 downto 32);
      r0 <= \Zds\(31 downto 24);
      r1 <= \Zds\(23 downto 16);
      r2 <= \Zds\(15 downto 8);
      r3 <= \Zds\(7 downto 0);
      res <= (i & o & z & c & ie & pc & zs & cs & pcs & r0 & r1 & r2 & r3);
end architecture;

-- Main.setAddrOut
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setAddrOut\ is
      port (\Zds\ : in std_logic_vector (17 downto 0);
            a_o : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_setAddrOut\ is
      signal d_o : std_logic_vector (7 downto 0);
      signal we_o : std_logic_vector (0 downto 0);
      signal iack_o : std_logic_vector (0 downto 0);
begin
      d_o <= \Zds\(9 downto 2);
      we_o <= \Zds\(1 downto 1);
      iack_o <= \Zds\(0 downto 0);
      res <= (a_o & d_o & we_o & iack_o);
end architecture;

-- Main.setDataOut
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setDataOut\ is
      port (\Zds\ : in std_logic_vector (17 downto 0);
            d_o : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_setDataOut\ is
      signal a_o : std_logic_vector (7 downto 0);
      signal we_o : std_logic_vector (0 downto 0);
      signal iack_o : std_logic_vector (0 downto 0);
begin
      a_o <= \Zds\(17 downto 10);
      we_o <= \Zds\(1 downto 1);
      iack_o <= \Zds\(0 downto 0);
      res <= (a_o & d_o & we_o & iack_o);
end architecture;

-- Main.dataIn
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_dataIn\ is
      port (\Zds\ : in std_logic_vector (9 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_dataIn\ is
      signal d_i : std_logic_vector (7 downto 0);
begin
      d_i <= \Zds\(9 downto 2);
      res <= d_i;
end architecture;

-- Main.setWeOut
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_setWeOut\ is
      port (\Zds\ : in std_logic_vector (17 downto 0);
            we_o : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (17 downto 0));
end entity;

architecture rtl of \Main_setWeOut\ is
      signal a_o : std_logic_vector (7 downto 0);
      signal d_o : std_logic_vector (7 downto 0);
      signal iack_o : std_logic_vector (0 downto 0);
begin
      a_o <= \Zds\(17 downto 10);
      d_o <= \Zds\(9 downto 2);
      iack_o <= \Zds\(0 downto 0);
      res <= (a_o & d_o & we_o & iack_o);
end architecture;

-- Main.lsbW8
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_lsbW8\ is
      port (v : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_lsbW8\ is
begin
      res <= v(0 downto 0);
end architecture;

-- Main.msbW8
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_msbW8\ is
      port (v : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_msbW8\ is
begin
      res <= v(7 downto 7);
end architecture;

-- Main.plusCW8$sMain_oneW8_False_Bool
-- partially applied from 'Main.plusCW8'
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_plusCW8$sMain__oneW8__False__Bool\ is
      port (a : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \Main_plusCW8$sMain__oneW8__False__Bool\ is
      signal s : std_logic_vector (8 downto 0);
begin
      s <= rw_add(rw_add(rw_resize(a, 9), std_logic_vector'(B"000000001")), std_logic_vector'(B"000000000"));
      res <= (s(8 downto 8) & rw_resize(s, 8));
end architecture;

-- Main.minusCW8$sFalse_Bool
-- partially applied from 'Main.minusCW8'
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_minusCW8$sFalse__Bool\ is
      port (a : in std_logic_vector (7 downto 0);
            b : in std_logic_vector (7 downto 0);
            res : out std_logic_vector (8 downto 0));
end entity;

architecture rtl of \Main_minusCW8$sFalse__Bool\ is
      signal s : std_logic_vector (8 downto 0);
begin
      s <= rw_sub(rw_sub(rw_resize(a, 9), rw_resize(b, 9)), std_logic_vector'(B"000000000"));
      res <= (s(8 downto 8) & rw_resize(s, 8));
end architecture;