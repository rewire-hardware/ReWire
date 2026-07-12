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
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (16 downto 0));
      end component;
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (14 downto 0));
      end component;
      component \Main_pc\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (5 downto 0));
      end component;
      component \main_getIns2\ is
            port (s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      component \main_getPC\ is
            port (s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      component \main_getPC2\ is
            port (s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      -- state registers
      -- __resumption_tag: 3 bits, init 0x4
      --   states: 0=i 1=i3 2=i4 3=i5 4=i8
      -- __st0: 70 bits, init 0x0
      constant st_i : std_logic_vector (2 downto 0) := std_logic_vector'(B"000");
      constant st_i3 : std_logic_vector (2 downto 0) := std_logic_vector'(B"001");
      constant st_i4 : std_logic_vector (2 downto 0) := std_logic_vector'(B"010");
      constant st_i5 : std_logic_vector (2 downto 0) := std_logic_vector'(B"011");
      constant st_i8 : std_logic_vector (2 downto 0) := std_logic_vector'(B"100");
      signal \__resumption_tag\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"100");
      signal \__resumption_tag_next\ : std_logic_vector (2 downto 0);
      signal \__st0\ : std_logic_vector (69 downto 0) := (others => '0');
      signal \__st0_next\ : std_logic_vector (69 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
      signal pc : std_logic_vector (5 downto 0);
      signal outputs : std_logic_vector (14 downto 0);
      signal za : std_logic_vector (69 downto 0);
      signal main_getins2_out : std_logic_vector (87 downto 0);
      signal main_inputs_out : std_logic_vector (16 downto 0);
      signal zds2 : std_logic_vector (7 downto 0);
      signal r1_r1 : std_logic_vector (7 downto 0);
      signal r2_r1 : std_logic_vector (7 downto 0);
      signal r3_r1 : std_logic_vector (7 downto 0);
      signal pc_r1 : std_logic_vector (5 downto 0);
      signal inputs : std_logic_vector (16 downto 0);
      signal outputs_r1 : std_logic_vector (14 downto 0);
      signal za_r1 : std_logic_vector (69 downto 0);
      signal main_getins2_out_r1 : std_logic_vector (87 downto 0);
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal a : std_logic_vector (5 downto 0);
      signal r0_r1 : std_logic_vector (7 downto 0);
      signal za_r2 : std_logic_vector (69 downto 0);
      signal main_pc_out_r1 : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal weout : std_logic_vector (0 downto 0);
      signal dataout : std_logic_vector (7 downto 0);
      signal za_r3 : std_logic_vector (14 downto 0);
      signal r0_r2 : std_logic_vector (7 downto 0);
      signal r1_r2 : std_logic_vector (7 downto 0);
      signal r2_r2 : std_logic_vector (7 downto 0);
      signal r3_r2 : std_logic_vector (7 downto 0);
      signal pc_r2 : std_logic_vector (5 downto 0);
      signal inputs_r1 : std_logic_vector (16 downto 0);
      signal za_r4 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r1 : std_logic_vector (14 downto 0);
      signal addrout : std_logic_vector (5 downto 0);
      signal dataout_r1 : std_logic_vector (7 downto 0);
      signal za_r5 : std_logic_vector (14 downto 0);
      signal r0_r3 : std_logic_vector (7 downto 0);
      signal r1_r3 : std_logic_vector (7 downto 0);
      signal r2_r3 : std_logic_vector (7 downto 0);
      signal r3_r3 : std_logic_vector (7 downto 0);
      signal pc_r3 : std_logic_vector (5 downto 0);
      signal inputs_r2 : std_logic_vector (16 downto 0);
      signal za_r6 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r2 : std_logic_vector (14 downto 0);
      signal main_getpc2_out : std_logic_vector (87 downto 0);
      signal main_getpc_out : std_logic_vector (87 downto 0);
      signal zres : std_logic_vector (87 downto 0);
begin
      -- combinational logic
      r0 <= \__st0\(69 downto 62);
      r1 <= \__st0\(61 downto 54);
      r2 <= \__st0\(53 downto 46);
      r3 <= \__st0\(45 downto 38);
      pc <= \__st0\(37 downto 32);
      outputs <= \__st0\(14 downto 0);
      za <= (r0 & r1 & r2 & r3 & pc & \__in0\ & outputs);
      getins2_i : \main_getIns2\ port map (za, main_getins2_out);
      inputs_i : \Main_inputs\ port map (za, main_inputs_out);
      zds2 <= main_inputs_out(7 downto 0);
      r1_r1 <= za(61 downto 54);
      r2_r1 <= za(53 downto 46);
      r3_r1 <= za(45 downto 38);
      pc_r1 <= za(37 downto 32);
      inputs <= za(31 downto 15);
      outputs_r1 <= za(14 downto 0);
      za_r1 <= (zds2 & r1_r1 & r2_r1 & r3_r1 & pc_r1 & inputs & outputs_r1);
      getins2_i_r1 : \main_getIns2\ port map (za_r1, main_getins2_out_r1);
      pc_i : \Main_pc\ port map (za, main_pc_out);
      a <= rw_add(main_pc_out, std_logic_vector'(B"000001"));
      r0_r1 <= za(69 downto 62);
      za_r2 <= (r0_r1 & r1_r1 & r2_r1 & r3_r1 & a & inputs & outputs_r1);
      pc_i_r1 : \Main_pc\ port map (za_r2, main_pc_out_r1);
      outputs_i : \Main_outputs\ port map (za_r2, main_outputs_out);
      weout <= main_outputs_out(14 downto 14);
      dataout <= main_outputs_out(7 downto 0);
      za_r3 <= (weout & main_pc_out_r1 & dataout);
      r0_r2 <= za_r2(69 downto 62);
      r1_r2 <= za_r2(61 downto 54);
      r2_r2 <= za_r2(53 downto 46);
      r3_r2 <= za_r2(45 downto 38);
      pc_r2 <= za_r2(37 downto 32);
      inputs_r1 <= za_r2(31 downto 15);
      za_r4 <= (r0_r2 & r1_r2 & r2_r2 & r3_r2 & pc_r2 & inputs_r1 & za_r3);
      outputs_i_r1 : \Main_outputs\ port map (za_r4, main_outputs_out_r1);
      addrout <= main_outputs_out_r1(13 downto 8);
      dataout_r1 <= main_outputs_out_r1(7 downto 0);
      za_r5 <= (std_logic_vector'(B"0") & addrout & dataout_r1);
      r0_r3 <= za_r4(69 downto 62);
      r1_r3 <= za_r4(61 downto 54);
      r2_r3 <= za_r4(53 downto 46);
      r3_r3 <= za_r4(45 downto 38);
      pc_r3 <= za_r4(37 downto 32);
      inputs_r2 <= za_r4(31 downto 15);
      za_r6 <= (r0_r3 & r1_r3 & r2_r3 & r3_r3 & pc_r3 & inputs_r2 & za_r5);
      outputs_i_r2 : \Main_outputs\ port map (za_r6, main_outputs_out_r2);
      getpc2_i : \main_getPC2\ port map (za, main_getpc2_out);
      getpc_i : \main_getPC\ port map (za, main_getpc_out);
      with \__resumption_tag\ select zres <=
            main_getins2_out when "000",
            main_getins2_out_r1 when "001",
            (main_outputs_out_r2 & std_logic_vector'(B"001") & za_r6) when "010",
            main_getpc2_out when "011",
            main_getpc_out when others;
      \__resumption_tag_next\ <= zres(72 downto 70);
      \__st0_next\ <= zres(69 downto 0);
      -- outputs
      \__out0\ <= zres(87 downto 73);
      -- state register update
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

-- main.getPC
-- block '$L.Main.getPC' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getPC\ is
      port (s0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getPC\ is
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (14 downto 0));
      end component;
      component \Main_pc\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (5 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal weout : std_logic_vector (0 downto 0);
      signal dataout : std_logic_vector (7 downto 0);
      signal za : std_logic_vector (14 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
      signal pc : std_logic_vector (5 downto 0);
      signal inputs : std_logic_vector (16 downto 0);
      signal za_r1 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r1 : std_logic_vector (14 downto 0);
      signal addrout : std_logic_vector (5 downto 0);
      signal dataout_r1 : std_logic_vector (7 downto 0);
      signal za_r2 : std_logic_vector (14 downto 0);
      signal r0_r1 : std_logic_vector (7 downto 0);
      signal r1_r1 : std_logic_vector (7 downto 0);
      signal r2_r1 : std_logic_vector (7 downto 0);
      signal r3_r1 : std_logic_vector (7 downto 0);
      signal pc_r1 : std_logic_vector (5 downto 0);
      signal inputs_r1 : std_logic_vector (16 downto 0);
      signal za_r3 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r2 : std_logic_vector (14 downto 0);
begin
      pc_i : \Main_pc\ port map (s0, main_pc_out);
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      weout <= main_outputs_out(14 downto 14);
      dataout <= main_outputs_out(7 downto 0);
      za <= (weout & main_pc_out & dataout);
      r0 <= s0(69 downto 62);
      r1 <= s0(61 downto 54);
      r2 <= s0(53 downto 46);
      r3 <= s0(45 downto 38);
      pc <= s0(37 downto 32);
      inputs <= s0(31 downto 15);
      za_r1 <= (r0 & r1 & r2 & r3 & pc & inputs & za);
      outputs_i_r1 : \Main_outputs\ port map (za_r1, main_outputs_out_r1);
      addrout <= main_outputs_out_r1(13 downto 8);
      dataout_r1 <= main_outputs_out_r1(7 downto 0);
      za_r2 <= (std_logic_vector'(B"0") & addrout & dataout_r1);
      r0_r1 <= za_r1(69 downto 62);
      r1_r1 <= za_r1(61 downto 54);
      r2_r1 <= za_r1(53 downto 46);
      r3_r1 <= za_r1(45 downto 38);
      pc_r1 <= za_r1(37 downto 32);
      inputs_r1 <= za_r1(31 downto 15);
      za_r3 <= (r0_r1 & r1_r1 & r2_r1 & r3_r1 & pc_r1 & inputs_r1 & za_r2);
      outputs_i_r2 : \Main_outputs\ port map (za_r3, main_outputs_out_r2);
      res <= (main_outputs_out_r2 & std_logic_vector'(B"000") & za_r3);
end architecture;

-- main.putPC
-- block '$L.Main.putPC' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_putPC\ is
      port (a : in std_logic_vector (5 downto 0);
            s0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_putPC\ is
      component \main_getPC\ is
            port (s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
      signal inputs : std_logic_vector (16 downto 0);
      signal outputs : std_logic_vector (14 downto 0);
      signal za : std_logic_vector (69 downto 0);
      signal main_getpc_out : std_logic_vector (87 downto 0);
begin
      r0 <= s0(69 downto 62);
      r1 <= s0(61 downto 54);
      r2 <= s0(53 downto 46);
      r3 <= s0(45 downto 38);
      inputs <= s0(31 downto 15);
      outputs <= s0(14 downto 0);
      za <= (r0 & r1 & r2 & r3 & a & inputs & outputs);
      getpc_i : \main_getPC\ port map (za, main_getpc_out);
      res <= main_getpc_out;
end architecture;

-- main.getPC2
-- block '$L.Main.getPC2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getPC2\ is
      port (s0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getPC2\ is
      component \Main_pc\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (5 downto 0));
      end component;
      component \main_putPC\ is
            port (a : in std_logic_vector (5 downto 0);
                  s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      signal main_pc_out : std_logic_vector (5 downto 0);
      signal conn : std_logic_vector (5 downto 0);
      signal main_putpc_out : std_logic_vector (87 downto 0);
begin
      pc_i : \Main_pc\ port map (s0, main_pc_out);
      conn <= rw_add(main_pc_out, std_logic_vector'(B"000001"));
      putpc_i : \main_putPC\ port map (conn, s0, main_putpc_out);
      res <= main_putpc_out;
end architecture;

-- main.putReg
-- block '$L.Main.putReg' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_putReg\ is
      port (\Zds\ : in std_logic_vector (1 downto 0);
            b : in std_logic_vector (7 downto 0);
            s0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_putReg\ is
      component \main_getPC2\ is
            port (s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
      signal pc : std_logic_vector (5 downto 0);
      signal inputs : std_logic_vector (16 downto 0);
      signal outputs : std_logic_vector (14 downto 0);
      signal za : std_logic_vector (69 downto 0);
      signal main_getpc2_out : std_logic_vector (87 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal za_r1 : std_logic_vector (69 downto 0);
      signal main_getpc2_out_r1 : std_logic_vector (87 downto 0);
      signal za_r2 : std_logic_vector (69 downto 0);
      signal main_getpc2_out_r2 : std_logic_vector (87 downto 0);
      signal za_r3 : std_logic_vector (69 downto 0);
      signal main_getpc2_out_r3 : std_logic_vector (87 downto 0);
begin
      r1 <= s0(61 downto 54);
      r2 <= s0(53 downto 46);
      r3 <= s0(45 downto 38);
      pc <= s0(37 downto 32);
      inputs <= s0(31 downto 15);
      outputs <= s0(14 downto 0);
      za <= (b & r1 & r2 & r3 & pc & inputs & outputs);
      getpc2_i : \main_getPC2\ port map (za, main_getpc2_out);
      r0 <= s0(69 downto 62);
      za_r1 <= (r0 & b & r2 & r3 & pc & inputs & outputs);
      getpc2_i_r1 : \main_getPC2\ port map (za_r1, main_getpc2_out_r1);
      za_r2 <= (r0 & r1 & b & r3 & pc & inputs & outputs);
      getpc2_i_r2 : \main_getPC2\ port map (za_r2, main_getpc2_out_r2);
      za_r3 <= (r0 & r1 & r2 & b & pc & inputs & outputs);
      getpc2_i_r3 : \main_getPC2\ port map (za_r3, main_getpc2_out_r3);
      with \Zds\ select res <=
            main_getpc2_out when "00",
            main_getpc2_out_r1 when "01",
            main_getpc2_out_r2 when "10",
            main_getpc2_out_r3 when others;
end architecture;

-- main.getReg
-- block '$L.Main.getReg' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getReg\ is
      port (rd : in std_logic_vector (1 downto 0);
            a : in std_logic_vector (7 downto 0);
            \Zds\ : in std_logic_vector (1 downto 0);
            s0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getReg\ is
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_putReg\ is
            port (\Zds\ : in std_logic_vector (1 downto 0);
                  b : in std_logic_vector (7 downto 0);
                  s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal conn : std_logic_vector (7 downto 0);
      signal main_putreg_out : std_logic_vector (87 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal conn_r1 : std_logic_vector (7 downto 0);
      signal main_putreg_out_r1 : std_logic_vector (87 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal conn_r2 : std_logic_vector (7 downto 0);
      signal main_putreg_out_r2 : std_logic_vector (87 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal conn_r3 : std_logic_vector (7 downto 0);
      signal main_putreg_out_r3 : std_logic_vector (87 downto 0);
begin
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      conn <= rw_not(rw_and(a, main_r0_out));
      putreg_i : \main_putReg\ port map (rd, conn, s0, main_putreg_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      conn_r1 <= rw_not(rw_and(a, main_r1_out));
      putreg_i_r1 : \main_putReg\ port map (rd, conn_r1, s0, main_putreg_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      conn_r2 <= rw_not(rw_and(a, main_r2_out));
      putreg_i_r2 : \main_putReg\ port map (rd, conn_r2, s0, main_putreg_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      conn_r3 <= rw_not(rw_and(a, main_r3_out));
      putreg_i_r3 : \main_putReg\ port map (rd, conn_r3, s0, main_putreg_out_r3);
      with \Zds\ select res <=
            main_putreg_out when "00",
            main_putreg_out_r1 when "01",
            main_putreg_out_r2 when "10",
            main_putreg_out_r3 when others;
end architecture;

-- main.getIns2
-- block '$L.Main.getIns2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getIns2\ is
      port (s0 : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (87 downto 0));
end entity;

architecture rtl of \main_getIns2\ is
      component \Main_inputs\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (16 downto 0));
      end component;
      component \Main_outputs\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (14 downto 0));
      end component;
      component \Main_r0\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r1\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r2\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \Main_r3\ is
            port (\Zds\ : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (7 downto 0));
      end component;
      component \main_getPC2\ is
            port (s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      component \main_getReg\ is
            port (rd : in std_logic_vector (1 downto 0);
                  a : in std_logic_vector (7 downto 0);
                  \Zds\ : in std_logic_vector (1 downto 0);
                  s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      component \main_putPC\ is
            port (a : in std_logic_vector (5 downto 0);
                  s0 : in std_logic_vector (69 downto 0);
                  res : out std_logic_vector (87 downto 0));
      end component;
      signal main_inputs_out : std_logic_vector (16 downto 0);
      signal zds1 : std_logic_vector (8 downto 0);
      signal main_getpc2_out : std_logic_vector (87 downto 0);
      signal a : std_logic_vector (5 downto 0);
      signal main_outputs_out : std_logic_vector (14 downto 0);
      signal weout : std_logic_vector (0 downto 0);
      signal dataout : std_logic_vector (7 downto 0);
      signal za : std_logic_vector (14 downto 0);
      signal r0 : std_logic_vector (7 downto 0);
      signal r1 : std_logic_vector (7 downto 0);
      signal r2 : std_logic_vector (7 downto 0);
      signal r3 : std_logic_vector (7 downto 0);
      signal pc : std_logic_vector (5 downto 0);
      signal inputs : std_logic_vector (16 downto 0);
      signal za_r1 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r1 : std_logic_vector (14 downto 0);
      signal addrout : std_logic_vector (5 downto 0);
      signal dataout_r1 : std_logic_vector (7 downto 0);
      signal za_r2 : std_logic_vector (14 downto 0);
      signal r0_r1 : std_logic_vector (7 downto 0);
      signal r1_r1 : std_logic_vector (7 downto 0);
      signal r2_r1 : std_logic_vector (7 downto 0);
      signal r3_r1 : std_logic_vector (7 downto 0);
      signal pc_r1 : std_logic_vector (5 downto 0);
      signal inputs_r1 : std_logic_vector (16 downto 0);
      signal za_r3 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r2 : std_logic_vector (14 downto 0);
      signal main_r0_out : std_logic_vector (7 downto 0);
      signal weout_r1 : std_logic_vector (0 downto 0);
      signal za_r4 : std_logic_vector (14 downto 0);
      signal za_r5 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r3 : std_logic_vector (14 downto 0);
      signal addrout_r1 : std_logic_vector (5 downto 0);
      signal dataout_r2 : std_logic_vector (7 downto 0);
      signal za_r6 : std_logic_vector (14 downto 0);
      signal r0_r2 : std_logic_vector (7 downto 0);
      signal r1_r2 : std_logic_vector (7 downto 0);
      signal r2_r2 : std_logic_vector (7 downto 0);
      signal r3_r2 : std_logic_vector (7 downto 0);
      signal pc_r2 : std_logic_vector (5 downto 0);
      signal inputs_r2 : std_logic_vector (16 downto 0);
      signal za_r7 : std_logic_vector (69 downto 0);
      signal main_outputs_out_r4 : std_logic_vector (14 downto 0);
      signal rd : std_logic_vector (1 downto 0);
      signal r1_r3 : std_logic_vector (1 downto 0);
      signal r2_r3 : std_logic_vector (1 downto 0);
      signal main_getreg_out : std_logic_vector (87 downto 0);
      signal main_r1_out : std_logic_vector (7 downto 0);
      signal main_getreg_out_r1 : std_logic_vector (87 downto 0);
      signal main_r2_out : std_logic_vector (7 downto 0);
      signal main_getreg_out_r2 : std_logic_vector (87 downto 0);
      signal main_r3_out : std_logic_vector (7 downto 0);
      signal main_getreg_out_r3 : std_logic_vector (87 downto 0);
      signal za_r8 : std_logic_vector (0 downto 0);
      signal main_putpc_out : std_logic_vector (87 downto 0);
begin
      inputs_i : \Main_inputs\ port map (s0, main_inputs_out);
      zds1 <= main_inputs_out(16 downto 8);
      getpc2_i : \main_getPC2\ port map (s0, main_getpc2_out);
      a <= zds1(5 downto 0);
      outputs_i : \Main_outputs\ port map (s0, main_outputs_out);
      weout <= main_outputs_out(14 downto 14);
      dataout <= main_outputs_out(7 downto 0);
      za <= (weout & a & dataout);
      r0 <= s0(69 downto 62);
      r1 <= s0(61 downto 54);
      r2 <= s0(53 downto 46);
      r3 <= s0(45 downto 38);
      pc <= s0(37 downto 32);
      inputs <= s0(31 downto 15);
      za_r1 <= (r0 & r1 & r2 & r3 & pc & inputs & za);
      outputs_i_r1 : \Main_outputs\ port map (za_r1, main_outputs_out_r1);
      addrout <= main_outputs_out_r1(13 downto 8);
      dataout_r1 <= main_outputs_out_r1(7 downto 0);
      za_r2 <= (std_logic_vector'(B"0") & addrout & dataout_r1);
      r0_r1 <= za_r1(69 downto 62);
      r1_r1 <= za_r1(61 downto 54);
      r2_r1 <= za_r1(53 downto 46);
      r3_r1 <= za_r1(45 downto 38);
      pc_r1 <= za_r1(37 downto 32);
      inputs_r1 <= za_r1(31 downto 15);
      za_r3 <= (r0_r1 & r1_r1 & r2_r1 & r3_r1 & pc_r1 & inputs_r1 & za_r2);
      outputs_i_r2 : \Main_outputs\ port map (za_r3, main_outputs_out_r2);
      r0_i : \Main_r0\ port map (s0, main_r0_out);
      weout_r1 <= main_outputs_out_r1(14 downto 14);
      za_r4 <= (weout_r1 & addrout & main_r0_out);
      za_r5 <= (r0_r1 & r1_r1 & r2_r1 & r3_r1 & pc_r1 & inputs_r1 & za_r4);
      outputs_i_r3 : \Main_outputs\ port map (za_r5, main_outputs_out_r3);
      addrout_r1 <= main_outputs_out_r3(13 downto 8);
      dataout_r2 <= main_outputs_out_r3(7 downto 0);
      za_r6 <= (std_logic_vector'(B"1") & addrout_r1 & dataout_r2);
      r0_r2 <= za_r5(69 downto 62);
      r1_r2 <= za_r5(61 downto 54);
      r2_r2 <= za_r5(53 downto 46);
      r3_r2 <= za_r5(45 downto 38);
      pc_r2 <= za_r5(37 downto 32);
      inputs_r2 <= za_r5(31 downto 15);
      za_r7 <= (r0_r2 & r1_r2 & r2_r2 & r3_r2 & pc_r2 & inputs_r2 & za_r6);
      outputs_i_r4 : \Main_outputs\ port map (za_r7, main_outputs_out_r4);
      rd <= zds1(5 downto 4);
      r1_r3 <= zds1(3 downto 2);
      r2_r3 <= zds1(1 downto 0);
      getreg_i : \main_getReg\ port map (rd, main_r0_out, r2_r3, s0, main_getreg_out);
      r1_i : \Main_r1\ port map (s0, main_r1_out);
      getreg_i_r1 : \main_getReg\ port map (rd, main_r1_out, r2_r3, s0, main_getreg_out_r1);
      r2_i : \Main_r2\ port map (s0, main_r2_out);
      getreg_i_r2 : \main_getReg\ port map (rd, main_r2_out, r2_r3, s0, main_getreg_out_r2);
      r3_i : \Main_r3\ port map (s0, main_r3_out);
      getreg_i_r3 : \main_getReg\ port map (rd, main_r3_out, r2_r3, s0, main_getreg_out_r3);
      za_r8 <= rw_eq(main_r0_out, std_logic_vector'(X"00"));
      putpc_i : \main_putPC\ port map (a, s0, main_putpc_out);
      with zds1(8 downto 6) select res <=
            main_getpc2_out when "000",
            (main_outputs_out_r2 & std_logic_vector'(B"010") & za_r3) when "001",
            (main_outputs_out_r4 & std_logic_vector'(B"011") & za_r7) when "010",
            rw_cond(rw_eq(r1_r3, std_logic_vector'(B"00")), main_getreg_out, rw_cond(rw_eq(r1_r3, std_logic_vector'(B"01")), main_getreg_out_r1, rw_cond(rw_eq(r1_r3, std_logic_vector'(B"10")), main_getreg_out_r2, main_getreg_out_r3))) when "011",
            rw_cond(rw_not(za_r8), main_putpc_out, main_getpc2_out) when others;
end architecture;

-- Main.outputs
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_outputs\ is
      port (\Zds\ : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (14 downto 0));
end entity;

architecture rtl of \Main_outputs\ is
      signal zds7 : std_logic_vector (14 downto 0);
begin
      zds7 <= \Zds\(14 downto 0);
      res <= zds7;
end architecture;

-- Main.inputs
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_inputs\ is
      port (\Zds\ : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (16 downto 0));
end entity;

architecture rtl of \Main_inputs\ is
      signal zds6 : std_logic_vector (16 downto 0);
begin
      zds6 <= \Zds\(31 downto 15);
      res <= zds6;
end architecture;

-- Main.pc
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_pc\ is
      port (\Zds\ : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (5 downto 0));
end entity;

architecture rtl of \Main_pc\ is
      signal zds5 : std_logic_vector (5 downto 0);
begin
      zds5 <= \Zds\(37 downto 32);
      res <= zds5;
end architecture;

-- Main.r3
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r3\ is
      port (\Zds\ : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r3\ is
      signal zds4 : std_logic_vector (7 downto 0);
begin
      zds4 <= \Zds\(45 downto 38);
      res <= zds4;
end architecture;

-- Main.r2
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r2\ is
      port (\Zds\ : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r2\ is
      signal zds3 : std_logic_vector (7 downto 0);
begin
      zds3 <= \Zds\(53 downto 46);
      res <= zds3;
end architecture;

-- Main.r1
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r1\ is
      port (\Zds\ : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r1\ is
      signal zds2 : std_logic_vector (7 downto 0);
begin
      zds2 <= \Zds\(61 downto 54);
      res <= zds2;
end architecture;

-- Main.r0
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_r0\ is
      port (\Zds\ : in std_logic_vector (69 downto 0);
            res : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of \Main_r0\ is
      signal zds1 : std_logic_vector (7 downto 0);
begin
      zds1 <= \Zds\(69 downto 62);
      res <= zds1;
end architecture;