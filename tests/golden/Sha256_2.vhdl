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
            \__in0\ : in std_logic_vector (67 downto 0);
            \__out0\ : out std_logic_vector (64 downto 0));
end entity;

architecture rtl of top_level is
      component \Main_plusW32\ is
            port (\Zeta0\ : in std_logic_vector (31 downto 0);
                  \Zeta1\ : in std_logic_vector (31 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component main_dev is
            port (\Zds\ : in std_logic_vector (67 downto 0);
                  s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1102 downto 0));
      end component;
      component \main_genhash$\ is
            port (s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1102 downto 0));
      end component;
      -- state registers
      -- __resumption_tag: 8 bits, init 0x0
      --   states: 0=$x2 1=i 2=_unused11
      -- __st0: 224 bits, init 0x0
      -- __st1: 32 bits, init 0x0
      -- __st2: 480 bits, init 0x0
      -- __st3: 32 bits, init 0x0
      -- __st4: 224 bits, init 0x0
      -- __st5: 32 bits, init 0x0
      -- __st6: 6 bits, init 0x0
      signal \__resumption_tag\ : std_logic_vector (7 downto 0) := std_logic_vector'(X"00");
      signal \__resumption_tag_next\ : std_logic_vector (7 downto 0);
      signal \__st0\ : std_logic_vector (223 downto 0) := (others => '0');
      signal \__st0_next\ : std_logic_vector (223 downto 0);
      signal \__st1\ : std_logic_vector (31 downto 0) := (others => '0');
      signal \__st1_next\ : std_logic_vector (31 downto 0);
      signal \__st2\ : std_logic_vector (479 downto 0) := (others => '0');
      signal \__st2_next\ : std_logic_vector (479 downto 0);
      signal \__st3\ : std_logic_vector (31 downto 0) := (others => '0');
      signal \__st3_next\ : std_logic_vector (31 downto 0);
      signal \__st4\ : std_logic_vector (223 downto 0) := (others => '0');
      signal \__st4_next\ : std_logic_vector (223 downto 0);
      signal \__st5\ : std_logic_vector (31 downto 0) := (others => '0');
      signal \__st5_next\ : std_logic_vector (31 downto 0);
      signal \__st6\ : std_logic_vector (5 downto 0) := std_logic_vector'(B"000000");
      signal \__st6_next\ : std_logic_vector (5 downto 0);
      signal s0 : std_logic_vector (255 downto 0);
      signal s1 : std_logic_vector (511 downto 0);
      signal s2 : std_logic_vector (255 downto 0);
      signal main_dev_out : std_logic_vector (1102 downto 0);
      signal ctr : std_logic_vector (5 downto 0);
      signal h1 : std_logic_vector (31 downto 0);
      signal h2 : std_logic_vector (31 downto 0);
      signal h3 : std_logic_vector (31 downto 0);
      signal h4 : std_logic_vector (31 downto 0);
      signal h5 : std_logic_vector (31 downto 0);
      signal h6 : std_logic_vector (31 downto 0);
      signal h7 : std_logic_vector (31 downto 0);
      signal a : std_logic_vector (31 downto 0);
      signal b : std_logic_vector (31 downto 0);
      signal c : std_logic_vector (31 downto 0);
      signal d : std_logic_vector (31 downto 0);
      signal e : std_logic_vector (31 downto 0);
      signal f : std_logic_vector (31 downto 0);
      signal g : std_logic_vector (31 downto 0);
      signal main_plusw32_out : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r1 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r2 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r3 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r4 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r5 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r6 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r7 : std_logic_vector (31 downto 0);
      signal za8 : std_logic_vector (255 downto 0);
      signal main_dev_out_r1 : std_logic_vector (1102 downto 0);
      signal \main_genhash$_out\ : std_logic_vector (1102 downto 0);
      signal zres : std_logic_vector (1102 downto 0);
begin
      -- combinational logic
      s0 <= (\__st0\ & \__st1\);
      s1 <= (\__st2\ & \__st3\);
      s2 <= (\__st4\ & \__st5\);
      dev_i : main_dev port map (\__in0\, s0, s1, s2, \__st6\, main_dev_out);
      ctr <= \__resumption_tag\(5 downto 0);
      h1 <= \__st4\(223 downto 192);
      h2 <= \__st4\(191 downto 160);
      h3 <= \__st4\(159 downto 128);
      h4 <= \__st4\(127 downto 96);
      h5 <= \__st4\(95 downto 64);
      h6 <= \__st4\(63 downto 32);
      h7 <= \__st4\(31 downto 0);
      a <= \__st0\(223 downto 192);
      b <= \__st0\(191 downto 160);
      c <= \__st0\(159 downto 128);
      d <= \__st0\(127 downto 96);
      e <= \__st0\(95 downto 64);
      f <= \__st0\(63 downto 32);
      g <= \__st0\(31 downto 0);
      plusw32_i : \Main_plusW32\ port map (a, h1, main_plusw32_out);
      plusw32_i_r1 : \Main_plusW32\ port map (b, h2, main_plusw32_out_r1);
      plusw32_i_r2 : \Main_plusW32\ port map (c, h3, main_plusw32_out_r2);
      plusw32_i_r3 : \Main_plusW32\ port map (d, h4, main_plusw32_out_r3);
      plusw32_i_r4 : \Main_plusW32\ port map (e, h5, main_plusw32_out_r4);
      plusw32_i_r5 : \Main_plusW32\ port map (f, h6, main_plusw32_out_r5);
      plusw32_i_r6 : \Main_plusW32\ port map (g, h7, main_plusw32_out_r6);
      plusw32_i_r7 : \Main_plusW32\ port map (\__st1\, \__st5\, main_plusw32_out_r7);
      za8 <= (main_plusw32_out & main_plusw32_out_r1 & main_plusw32_out_r2 & main_plusw32_out_r3 & main_plusw32_out_r4 & main_plusw32_out_r5 & main_plusw32_out_r6 & main_plusw32_out_r7);
      dev_i_r1 : main_dev port map (\__in0\, s0, s1, za8, \__st6\, main_dev_out_r1);
      \genhash$_i\ : \main_genhash$\ port map (s0, s1, s2, \__st6\, \main_genhash$_out\);
      with \__resumption_tag\(7 downto 6) select zres <=
            main_dev_out when "00",
            rw_cond(rw_eq(ctr, std_logic_vector'(B"111111")), main_dev_out_r1, \main_genhash$_out\) when "01",
            \main_genhash$_out\ when others;
      \__resumption_tag_next\ <= zres(1037 downto 1030);
      \__st0_next\ <= zres(1029 downto 806);
      \__st1_next\ <= zres(805 downto 774);
      \__st2_next\ <= zres(773 downto 294);
      \__st3_next\ <= zres(293 downto 262);
      \__st4_next\ <= zres(261 downto 38);
      \__st5_next\ <= zres(37 downto 6);
      \__st6_next\ <= zres(5 downto 0);
      -- outputs
      \__out0\ <= zres(1102 downto 1038);
      -- state register update
      process (clk, rst)
      begin
            if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(X"00");
                  \__st0\ <= std_logic_vector'(X"00000000000000000000000000000000000000000000000000000000");
                  \__st1\ <= std_logic_vector'(X"00000000");
                  \__st2\ <= std_logic_vector'(X"000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
                  \__st3\ <= std_logic_vector'(X"00000000");
                  \__st4\ <= std_logic_vector'(X"00000000000000000000000000000000000000000000000000000000");
                  \__st5\ <= std_logic_vector'(X"00000000");
                  \__st6\ <= std_logic_vector'(B"000000");
            elsif rising_edge(clk(0)) then
                  \__resumption_tag\ <= \__resumption_tag_next\;
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
                  \__st2\ <= \__st2_next\;
                  \__st3\ <= \__st3_next\;
                  \__st4\ <= \__st4_next\;
                  \__st5\ <= \__st5_next\;
                  \__st6\ <= \__st6_next\;
            end if;
      end process;
end architecture;

-- main._unused
-- block '$L._unused' of process main
-- also: main.devsha256'
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused\ is
      port (s0 : in std_logic_vector (255 downto 0);
            s1 : in std_logic_vector (511 downto 0);
            s2 : in std_logic_vector (255 downto 0);
            s3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1102 downto 0));
end entity;

architecture rtl of \main___unused\ is
begin
      res <= ((std_logic_vector'(B"1") & rw_repl(72, std_logic_vector'(B"0"))) & s0 & s1 & s2 & s3);
end architecture;

-- main.arm2
-- block '$L.arm2' of process main
-- also: main._unused2
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_arm2 is
      port (w1 : in std_logic_vector (31 downto 0);
            w2 : in std_logic_vector (31 downto 0);
            s0 : in std_logic_vector (255 downto 0);
            s1 : in std_logic_vector (511 downto 0);
            s2 : in std_logic_vector (255 downto 0);
            s3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1102 downto 0));
end entity;

architecture rtl of main_arm2 is
      component \main___unused\ is
            port (s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1102 downto 0));
      end component;
      signal x2 : std_logic_vector (31 downto 0);
      signal x3 : std_logic_vector (31 downto 0);
      signal x4 : std_logic_vector (31 downto 0);
      signal x5 : std_logic_vector (31 downto 0);
      signal x6 : std_logic_vector (31 downto 0);
      signal x7 : std_logic_vector (31 downto 0);
      signal x8 : std_logic_vector (31 downto 0);
      signal x9 : std_logic_vector (31 downto 0);
      signal xa : std_logic_vector (31 downto 0);
      signal xb : std_logic_vector (31 downto 0);
      signal xc : std_logic_vector (31 downto 0);
      signal xd : std_logic_vector (31 downto 0);
      signal xe : std_logic_vector (31 downto 0);
      signal xf : std_logic_vector (31 downto 0);
      signal za : std_logic_vector (511 downto 0);
      signal \main__unused_out\ : std_logic_vector (1102 downto 0);
begin
      x2 <= s1(447 downto 416);
      x3 <= s1(415 downto 384);
      x4 <= s1(383 downto 352);
      x5 <= s1(351 downto 320);
      x6 <= s1(319 downto 288);
      x7 <= s1(287 downto 256);
      x8 <= s1(255 downto 224);
      x9 <= s1(223 downto 192);
      xa <= s1(191 downto 160);
      xb <= s1(159 downto 128);
      xc <= s1(127 downto 96);
      xd <= s1(95 downto 64);
      xe <= s1(63 downto 32);
      xf <= s1(31 downto 0);
      za <= (w1 & w2 & x2 & x3 & x4 & x5 & x6 & x7 & x8 & x9 & xa & xb & xc & xd & xe & xf);
      \_unused_i\ : \main___unused\ port map (s0, za, s2, s3, \main__unused_out\);
      res <= \main__unused_out\;
end architecture;

-- main.genhash'
-- block '$L.Main.genhash'' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_genhash$\ is
      port (s0 : in std_logic_vector (255 downto 0);
            s1 : in std_logic_vector (511 downto 0);
            s2 : in std_logic_vector (255 downto 0);
            s3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1102 downto 0));
end entity;

architecture rtl of \main_genhash$\ is
      component \Main_andW32\ is
            port (\Zeta0\ : in std_logic_vector (31 downto 0);
                  \Zeta1\ : in std_logic_vector (31 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_plusW32\ is
            port (\Zeta0\ : in std_logic_vector (31 downto 0);
                  \Zeta1\ : in std_logic_vector (31 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_xorW32\ is
            port (\Zeta0\ : in std_logic_vector (31 downto 0);
                  \Zeta1\ : in std_logic_vector (31 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \notW32\ is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : out std_logic_vector (31 downto 0));
      end component;
      signal w00 : std_logic_vector (31 downto 0);
      signal w01 : std_logic_vector (31 downto 0);
      signal w02 : std_logic_vector (31 downto 0);
      signal w03 : std_logic_vector (31 downto 0);
      signal w04 : std_logic_vector (31 downto 0);
      signal w05 : std_logic_vector (31 downto 0);
      signal w06 : std_logic_vector (31 downto 0);
      signal w07 : std_logic_vector (31 downto 0);
      signal w08 : std_logic_vector (31 downto 0);
      signal w09 : std_logic_vector (31 downto 0);
      signal w10 : std_logic_vector (31 downto 0);
      signal w11 : std_logic_vector (31 downto 0);
      signal w12 : std_logic_vector (31 downto 0);
      signal w13 : std_logic_vector (31 downto 0);
      signal w14 : std_logic_vector (31 downto 0);
      signal w15 : std_logic_vector (31 downto 0);
      signal b0 : std_logic_vector (0 downto 0);
      signal b1 : std_logic_vector (0 downto 0);
      signal b2 : std_logic_vector (0 downto 0);
      signal b3 : std_logic_vector (0 downto 0);
      signal b4 : std_logic_vector (0 downto 0);
      signal b5 : std_logic_vector (0 downto 0);
      signal b6 : std_logic_vector (0 downto 0);
      signal b7 : std_logic_vector (0 downto 0);
      signal b8 : std_logic_vector (0 downto 0);
      signal b9 : std_logic_vector (0 downto 0);
      signal b10 : std_logic_vector (0 downto 0);
      signal b11 : std_logic_vector (0 downto 0);
      signal b12 : std_logic_vector (0 downto 0);
      signal b13 : std_logic_vector (0 downto 0);
      signal b14 : std_logic_vector (0 downto 0);
      signal b15 : std_logic_vector (0 downto 0);
      signal b16 : std_logic_vector (0 downto 0);
      signal b17 : std_logic_vector (0 downto 0);
      signal b18 : std_logic_vector (0 downto 0);
      signal b19 : std_logic_vector (0 downto 0);
      signal b20 : std_logic_vector (0 downto 0);
      signal b21 : std_logic_vector (0 downto 0);
      signal b22 : std_logic_vector (0 downto 0);
      signal b23 : std_logic_vector (0 downto 0);
      signal b24 : std_logic_vector (0 downto 0);
      signal b25 : std_logic_vector (0 downto 0);
      signal b26 : std_logic_vector (0 downto 0);
      signal b27 : std_logic_vector (0 downto 0);
      signal b28 : std_logic_vector (0 downto 0);
      signal b29 : std_logic_vector (0 downto 0);
      signal b30 : std_logic_vector (0 downto 0);
      signal b31 : std_logic_vector (0 downto 0);
      signal conn : std_logic_vector (31 downto 0);
      signal conn_r1 : std_logic_vector (31 downto 0);
      signal main_xorw32_out : std_logic_vector (31 downto 0);
      signal conn_r2 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r1 : std_logic_vector (31 downto 0);
      signal main_plusw32_out : std_logic_vector (31 downto 0);
      signal b0_r1 : std_logic_vector (0 downto 0);
      signal b1_r1 : std_logic_vector (0 downto 0);
      signal b2_r1 : std_logic_vector (0 downto 0);
      signal b3_r1 : std_logic_vector (0 downto 0);
      signal b4_r1 : std_logic_vector (0 downto 0);
      signal b5_r1 : std_logic_vector (0 downto 0);
      signal b6_r1 : std_logic_vector (0 downto 0);
      signal b7_r1 : std_logic_vector (0 downto 0);
      signal b8_r1 : std_logic_vector (0 downto 0);
      signal b9_r1 : std_logic_vector (0 downto 0);
      signal b10_r1 : std_logic_vector (0 downto 0);
      signal b11_r1 : std_logic_vector (0 downto 0);
      signal b12_r1 : std_logic_vector (0 downto 0);
      signal b13_r1 : std_logic_vector (0 downto 0);
      signal b14_r1 : std_logic_vector (0 downto 0);
      signal b15_r1 : std_logic_vector (0 downto 0);
      signal b16_r1 : std_logic_vector (0 downto 0);
      signal b17_r1 : std_logic_vector (0 downto 0);
      signal b18_r1 : std_logic_vector (0 downto 0);
      signal b19_r1 : std_logic_vector (0 downto 0);
      signal b20_r1 : std_logic_vector (0 downto 0);
      signal b21_r1 : std_logic_vector (0 downto 0);
      signal b22_r1 : std_logic_vector (0 downto 0);
      signal b23_r1 : std_logic_vector (0 downto 0);
      signal b24_r1 : std_logic_vector (0 downto 0);
      signal b25_r1 : std_logic_vector (0 downto 0);
      signal b26_r1 : std_logic_vector (0 downto 0);
      signal b27_r1 : std_logic_vector (0 downto 0);
      signal b28_r1 : std_logic_vector (0 downto 0);
      signal b29_r1 : std_logic_vector (0 downto 0);
      signal b30_r1 : std_logic_vector (0 downto 0);
      signal b31_r1 : std_logic_vector (0 downto 0);
      signal conn_r3 : std_logic_vector (31 downto 0);
      signal conn_r4 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r2 : std_logic_vector (31 downto 0);
      signal conn_r5 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r3 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r1 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r2 : std_logic_vector (31 downto 0);
      signal za : std_logic_vector (511 downto 0);
      signal za_r1 : std_logic_vector (31 downto 0);
      signal a : std_logic_vector (31 downto 0);
      signal b : std_logic_vector (31 downto 0);
      signal c : std_logic_vector (31 downto 0);
      signal d : std_logic_vector (31 downto 0);
      signal e : std_logic_vector (31 downto 0);
      signal f : std_logic_vector (31 downto 0);
      signal g : std_logic_vector (31 downto 0);
      signal h : std_logic_vector (31 downto 0);
      signal b0_r2 : std_logic_vector (0 downto 0);
      signal b1_r2 : std_logic_vector (0 downto 0);
      signal b2_r2 : std_logic_vector (0 downto 0);
      signal b3_r2 : std_logic_vector (0 downto 0);
      signal b4_r2 : std_logic_vector (0 downto 0);
      signal b5_r2 : std_logic_vector (0 downto 0);
      signal b6_r2 : std_logic_vector (0 downto 0);
      signal b7_r2 : std_logic_vector (0 downto 0);
      signal b8_r2 : std_logic_vector (0 downto 0);
      signal b9_r2 : std_logic_vector (0 downto 0);
      signal b10_r2 : std_logic_vector (0 downto 0);
      signal b11_r2 : std_logic_vector (0 downto 0);
      signal b12_r2 : std_logic_vector (0 downto 0);
      signal b13_r2 : std_logic_vector (0 downto 0);
      signal b14_r2 : std_logic_vector (0 downto 0);
      signal b15_r2 : std_logic_vector (0 downto 0);
      signal b16_r2 : std_logic_vector (0 downto 0);
      signal b17_r2 : std_logic_vector (0 downto 0);
      signal b18_r2 : std_logic_vector (0 downto 0);
      signal b19_r2 : std_logic_vector (0 downto 0);
      signal b20_r2 : std_logic_vector (0 downto 0);
      signal b21_r2 : std_logic_vector (0 downto 0);
      signal b22_r2 : std_logic_vector (0 downto 0);
      signal b23_r2 : std_logic_vector (0 downto 0);
      signal b24_r2 : std_logic_vector (0 downto 0);
      signal b25_r2 : std_logic_vector (0 downto 0);
      signal b26_r2 : std_logic_vector (0 downto 0);
      signal b27_r2 : std_logic_vector (0 downto 0);
      signal b28_r2 : std_logic_vector (0 downto 0);
      signal b29_r2 : std_logic_vector (0 downto 0);
      signal b30_r2 : std_logic_vector (0 downto 0);
      signal b31_r2 : std_logic_vector (0 downto 0);
      signal conn_r6 : std_logic_vector (31 downto 0);
      signal conn_r7 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r4 : std_logic_vector (31 downto 0);
      signal conn_r8 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r5 : std_logic_vector (31 downto 0);
      signal main_andw32_out : std_logic_vector (31 downto 0);
      signal extres : std_logic_vector (31 downto 0);
      signal main_andw32_out_r1 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r6 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r3 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r4 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r5 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r6 : std_logic_vector (31 downto 0);
      signal b0_r3 : std_logic_vector (0 downto 0);
      signal b1_r3 : std_logic_vector (0 downto 0);
      signal b2_r3 : std_logic_vector (0 downto 0);
      signal b3_r3 : std_logic_vector (0 downto 0);
      signal b4_r3 : std_logic_vector (0 downto 0);
      signal b5_r3 : std_logic_vector (0 downto 0);
      signal b6_r3 : std_logic_vector (0 downto 0);
      signal b7_r3 : std_logic_vector (0 downto 0);
      signal b8_r3 : std_logic_vector (0 downto 0);
      signal b9_r3 : std_logic_vector (0 downto 0);
      signal b10_r3 : std_logic_vector (0 downto 0);
      signal b11_r3 : std_logic_vector (0 downto 0);
      signal b12_r3 : std_logic_vector (0 downto 0);
      signal b13_r3 : std_logic_vector (0 downto 0);
      signal b14_r3 : std_logic_vector (0 downto 0);
      signal b15_r3 : std_logic_vector (0 downto 0);
      signal b16_r3 : std_logic_vector (0 downto 0);
      signal b17_r3 : std_logic_vector (0 downto 0);
      signal b18_r3 : std_logic_vector (0 downto 0);
      signal b19_r3 : std_logic_vector (0 downto 0);
      signal b20_r3 : std_logic_vector (0 downto 0);
      signal b21_r3 : std_logic_vector (0 downto 0);
      signal b22_r3 : std_logic_vector (0 downto 0);
      signal b23_r3 : std_logic_vector (0 downto 0);
      signal b24_r3 : std_logic_vector (0 downto 0);
      signal b25_r3 : std_logic_vector (0 downto 0);
      signal b26_r3 : std_logic_vector (0 downto 0);
      signal b27_r3 : std_logic_vector (0 downto 0);
      signal b28_r3 : std_logic_vector (0 downto 0);
      signal b29_r3 : std_logic_vector (0 downto 0);
      signal b30_r3 : std_logic_vector (0 downto 0);
      signal b31_r3 : std_logic_vector (0 downto 0);
      signal conn_r9 : std_logic_vector (31 downto 0);
      signal conn_r10 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r7 : std_logic_vector (31 downto 0);
      signal conn_r11 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r8 : std_logic_vector (31 downto 0);
      signal main_andw32_out_r2 : std_logic_vector (31 downto 0);
      signal main_andw32_out_r3 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r9 : std_logic_vector (31 downto 0);
      signal main_andw32_out_r4 : std_logic_vector (31 downto 0);
      signal main_xorw32_out_r10 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r7 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r8 : std_logic_vector (31 downto 0);
      signal main_plusw32_out_r9 : std_logic_vector (31 downto 0);
      signal za1 : std_logic_vector (255 downto 0);
      signal za_r2 : std_logic_vector (5 downto 0);
begin
      w00 <= s1(511 downto 480);
      w01 <= s1(479 downto 448);
      w02 <= s1(447 downto 416);
      w03 <= s1(415 downto 384);
      w04 <= s1(383 downto 352);
      w05 <= s1(351 downto 320);
      w06 <= s1(319 downto 288);
      w07 <= s1(287 downto 256);
      w08 <= s1(255 downto 224);
      w09 <= s1(223 downto 192);
      w10 <= s1(191 downto 160);
      w11 <= s1(159 downto 128);
      w12 <= s1(127 downto 96);
      w13 <= s1(95 downto 64);
      w14 <= s1(63 downto 32);
      w15 <= s1(31 downto 0);
      b0 <= s1(63 downto 63);
      b1 <= s1(62 downto 62);
      b2 <= s1(61 downto 61);
      b3 <= s1(60 downto 60);
      b4 <= s1(59 downto 59);
      b5 <= s1(58 downto 58);
      b6 <= s1(57 downto 57);
      b7 <= s1(56 downto 56);
      b8 <= s1(55 downto 55);
      b9 <= s1(54 downto 54);
      b10 <= s1(53 downto 53);
      b11 <= s1(52 downto 52);
      b12 <= s1(51 downto 51);
      b13 <= s1(50 downto 50);
      b14 <= s1(49 downto 49);
      b15 <= s1(48 downto 48);
      b16 <= s1(47 downto 47);
      b17 <= s1(46 downto 46);
      b18 <= s1(45 downto 45);
      b19 <= s1(44 downto 44);
      b20 <= s1(43 downto 43);
      b21 <= s1(42 downto 42);
      b22 <= s1(41 downto 41);
      b23 <= s1(40 downto 40);
      b24 <= s1(39 downto 39);
      b25 <= s1(38 downto 38);
      b26 <= s1(37 downto 37);
      b27 <= s1(36 downto 36);
      b28 <= s1(35 downto 35);
      b29 <= s1(34 downto 34);
      b30 <= s1(33 downto 33);
      b31 <= s1(32 downto 32);
      conn <= (b15 & b16 & b17 & b18 & b19 & b20 & b21 & b22 & b23 & b24 & b25 & b26 & b27 & b28 & b29 & b30 & b31 & b0 & b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 & b9 & b10 & b11 & b12 & b13 & b14);
      conn_r1 <= (b13 & b14 & b15 & b16 & b17 & b18 & b19 & b20 & b21 & b22 & b23 & b24 & b25 & b26 & b27 & b28 & b29 & b30 & b31 & b0 & b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 & b9 & b10 & b11 & b12);
      xorw32_i : \Main_xorW32\ port map (conn, conn_r1, main_xorw32_out);
      conn_r2 <= (std_logic_vector'(B"0000000000") & b0 & b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 & b9 & b10 & b11 & b12 & b13 & b14 & b15 & b16 & b17 & b18 & b19 & b20 & b21);
      xorw32_i_r1 : \Main_xorW32\ port map (main_xorw32_out, conn_r2, main_xorw32_out_r1);
      plusw32_i : \Main_plusW32\ port map (main_xorw32_out_r1, w09, main_plusw32_out);
      b0_r1 <= s1(479 downto 479);
      b1_r1 <= s1(478 downto 478);
      b2_r1 <= s1(477 downto 477);
      b3_r1 <= s1(476 downto 476);
      b4_r1 <= s1(475 downto 475);
      b5_r1 <= s1(474 downto 474);
      b6_r1 <= s1(473 downto 473);
      b7_r1 <= s1(472 downto 472);
      b8_r1 <= s1(471 downto 471);
      b9_r1 <= s1(470 downto 470);
      b10_r1 <= s1(469 downto 469);
      b11_r1 <= s1(468 downto 468);
      b12_r1 <= s1(467 downto 467);
      b13_r1 <= s1(466 downto 466);
      b14_r1 <= s1(465 downto 465);
      b15_r1 <= s1(464 downto 464);
      b16_r1 <= s1(463 downto 463);
      b17_r1 <= s1(462 downto 462);
      b18_r1 <= s1(461 downto 461);
      b19_r1 <= s1(460 downto 460);
      b20_r1 <= s1(459 downto 459);
      b21_r1 <= s1(458 downto 458);
      b22_r1 <= s1(457 downto 457);
      b23_r1 <= s1(456 downto 456);
      b24_r1 <= s1(455 downto 455);
      b25_r1 <= s1(454 downto 454);
      b26_r1 <= s1(453 downto 453);
      b27_r1 <= s1(452 downto 452);
      b28_r1 <= s1(451 downto 451);
      b29_r1 <= s1(450 downto 450);
      b30_r1 <= s1(449 downto 449);
      b31_r1 <= s1(448 downto 448);
      conn_r3 <= (b25_r1 & b26_r1 & b27_r1 & b28_r1 & b29_r1 & b30_r1 & b31_r1 & b0_r1 & b1_r1 & b2_r1 & b3_r1 & b4_r1 & b5_r1 & b6_r1 & b7_r1 & b8_r1 & b9_r1 & b10_r1 & b11_r1 & b12_r1 & b13_r1 & b14_r1 & b15_r1 & b16_r1 & b17_r1 & b18_r1 & b19_r1 & b20_r1 & b21_r1 & b22_r1 & b23_r1 & b24_r1);
      conn_r4 <= (b14_r1 & b15_r1 & b16_r1 & b17_r1 & b18_r1 & b19_r1 & b20_r1 & b21_r1 & b22_r1 & b23_r1 & b24_r1 & b25_r1 & b26_r1 & b27_r1 & b28_r1 & b29_r1 & b30_r1 & b31_r1 & b0_r1 & b1_r1 & b2_r1 & b3_r1 & b4_r1 & b5_r1 & b6_r1 & b7_r1 & b8_r1 & b9_r1 & b10_r1 & b11_r1 & b12_r1 & b13_r1);
      xorw32_i_r2 : \Main_xorW32\ port map (conn_r3, conn_r4, main_xorw32_out_r2);
      conn_r5 <= (std_logic_vector'(B"000") & b0_r1 & b1_r1 & b2_r1 & b3_r1 & b4_r1 & b5_r1 & b6_r1 & b7_r1 & b8_r1 & b9_r1 & b10_r1 & b11_r1 & b12_r1 & b13_r1 & b14_r1 & b15_r1 & b16_r1 & b17_r1 & b18_r1 & b19_r1 & b20_r1 & b21_r1 & b22_r1 & b23_r1 & b24_r1 & b25_r1 & b26_r1 & b27_r1 & b28_r1);
      xorw32_i_r3 : \Main_xorW32\ port map (main_xorw32_out_r2, conn_r5, main_xorw32_out_r3);
      plusw32_i_r1 : \Main_plusW32\ port map (main_xorw32_out_r3, w00, main_plusw32_out_r1);
      plusw32_i_r2 : \Main_plusW32\ port map (main_plusw32_out, main_plusw32_out_r1, main_plusw32_out_r2);
      za <= (w01 & w02 & w03 & w04 & w05 & w06 & w07 & w08 & w09 & w10 & w11 & w12 & w13 & w14 & w15 & main_plusw32_out_r2);
      with s3 select za_r1 <=
            std_logic_vector'(X"428a2f98") when "000000",
            std_logic_vector'(X"71374491") when "000001",
            std_logic_vector'(X"b5c0fbcf") when "000010",
            std_logic_vector'(X"e9b5dba5") when "000011",
            std_logic_vector'(X"3956c25b") when "000100",
            std_logic_vector'(X"59f111f1") when "000101",
            std_logic_vector'(X"923f82a4") when "000110",
            std_logic_vector'(X"ab1c5ed5") when "000111",
            std_logic_vector'(X"d807aa98") when "001000",
            std_logic_vector'(X"12835b01") when "001001",
            std_logic_vector'(X"243185be") when "001010",
            std_logic_vector'(X"550c7dc3") when "001011",
            std_logic_vector'(X"72be5d74") when "001100",
            std_logic_vector'(X"80deb1fe") when "001101",
            std_logic_vector'(X"9bdc06a7") when "001110",
            std_logic_vector'(X"c19bf174") when "001111",
            std_logic_vector'(X"e49b69c1") when "010000",
            std_logic_vector'(X"efbe4786") when "010001",
            std_logic_vector'(X"0fc19dc6") when "010010",
            std_logic_vector'(X"240ca1cc") when "010011",
            std_logic_vector'(X"2de92c6f") when "010100",
            std_logic_vector'(X"4a7484aa") when "010101",
            std_logic_vector'(X"5cb0a9dc") when "010110",
            std_logic_vector'(X"76f988da") when "010111",
            std_logic_vector'(X"983e5152") when "011000",
            std_logic_vector'(X"a831c66d") when "011001",
            std_logic_vector'(X"b00327c8") when "011010",
            std_logic_vector'(X"bf597fc7") when "011011",
            std_logic_vector'(X"c6e00bf3") when "011100",
            std_logic_vector'(X"d5a79147") when "011101",
            std_logic_vector'(X"06ca6351") when "011110",
            std_logic_vector'(X"14292967") when "011111",
            std_logic_vector'(X"27b70a85") when "100000",
            std_logic_vector'(X"2e1b2138") when "100001",
            std_logic_vector'(X"4d2c6dfc") when "100010",
            std_logic_vector'(X"53380d13") when "100011",
            std_logic_vector'(X"650a7354") when "100100",
            std_logic_vector'(X"766a0abb") when "100101",
            std_logic_vector'(X"81c2c92e") when "100110",
            std_logic_vector'(X"92722c85") when "100111",
            std_logic_vector'(X"a2bfe8a1") when "101000",
            std_logic_vector'(X"a81a664b") when "101001",
            std_logic_vector'(X"c24b8b70") when "101010",
            std_logic_vector'(X"c76c51a3") when "101011",
            std_logic_vector'(X"d192e819") when "101100",
            std_logic_vector'(X"d6990624") when "101101",
            std_logic_vector'(X"f40e3585") when "101110",
            std_logic_vector'(X"106aa070") when "101111",
            std_logic_vector'(X"19a4c116") when "110000",
            std_logic_vector'(X"1e376c08") when "110001",
            std_logic_vector'(X"2748774c") when "110010",
            std_logic_vector'(X"34b0bcb5") when "110011",
            std_logic_vector'(X"391c0cb3") when "110100",
            std_logic_vector'(X"4ed8aa4a") when "110101",
            std_logic_vector'(X"5b9cca4f") when "110110",
            std_logic_vector'(X"682e6ff3") when "110111",
            std_logic_vector'(X"748f82ee") when "111000",
            std_logic_vector'(X"78a5636f") when "111001",
            std_logic_vector'(X"84c87814") when "111010",
            std_logic_vector'(X"8cc70208") when "111011",
            std_logic_vector'(X"90befffa") when "111100",
            std_logic_vector'(X"a4506ceb") when "111101",
            std_logic_vector'(X"bef9a3f7") when "111110",
            std_logic_vector'(X"c67178f2") when others;
      a <= s0(255 downto 224);
      b <= s0(223 downto 192);
      c <= s0(191 downto 160);
      d <= s0(159 downto 128);
      e <= s0(127 downto 96);
      f <= s0(95 downto 64);
      g <= s0(63 downto 32);
      h <= s0(31 downto 0);
      b0_r2 <= s0(127 downto 127);
      b1_r2 <= s0(126 downto 126);
      b2_r2 <= s0(125 downto 125);
      b3_r2 <= s0(124 downto 124);
      b4_r2 <= s0(123 downto 123);
      b5_r2 <= s0(122 downto 122);
      b6_r2 <= s0(121 downto 121);
      b7_r2 <= s0(120 downto 120);
      b8_r2 <= s0(119 downto 119);
      b9_r2 <= s0(118 downto 118);
      b10_r2 <= s0(117 downto 117);
      b11_r2 <= s0(116 downto 116);
      b12_r2 <= s0(115 downto 115);
      b13_r2 <= s0(114 downto 114);
      b14_r2 <= s0(113 downto 113);
      b15_r2 <= s0(112 downto 112);
      b16_r2 <= s0(111 downto 111);
      b17_r2 <= s0(110 downto 110);
      b18_r2 <= s0(109 downto 109);
      b19_r2 <= s0(108 downto 108);
      b20_r2 <= s0(107 downto 107);
      b21_r2 <= s0(106 downto 106);
      b22_r2 <= s0(105 downto 105);
      b23_r2 <= s0(104 downto 104);
      b24_r2 <= s0(103 downto 103);
      b25_r2 <= s0(102 downto 102);
      b26_r2 <= s0(101 downto 101);
      b27_r2 <= s0(100 downto 100);
      b28_r2 <= s0(99 downto 99);
      b29_r2 <= s0(98 downto 98);
      b30_r2 <= s0(97 downto 97);
      b31_r2 <= s0(96 downto 96);
      conn_r6 <= (b26_r2 & b27_r2 & b28_r2 & b29_r2 & b30_r2 & b31_r2 & b0_r2 & b1_r2 & b2_r2 & b3_r2 & b4_r2 & b5_r2 & b6_r2 & b7_r2 & b8_r2 & b9_r2 & b10_r2 & b11_r2 & b12_r2 & b13_r2 & b14_r2 & b15_r2 & b16_r2 & b17_r2 & b18_r2 & b19_r2 & b20_r2 & b21_r2 & b22_r2 & b23_r2 & b24_r2 & b25_r2);
      conn_r7 <= (b21_r2 & b22_r2 & b23_r2 & b24_r2 & b25_r2 & b26_r2 & b27_r2 & b28_r2 & b29_r2 & b30_r2 & b31_r2 & b0_r2 & b1_r2 & b2_r2 & b3_r2 & b4_r2 & b5_r2 & b6_r2 & b7_r2 & b8_r2 & b9_r2 & b10_r2 & b11_r2 & b12_r2 & b13_r2 & b14_r2 & b15_r2 & b16_r2 & b17_r2 & b18_r2 & b19_r2 & b20_r2);
      xorw32_i_r4 : \Main_xorW32\ port map (conn_r6, conn_r7, main_xorw32_out_r4);
      conn_r8 <= (b7_r2 & b8_r2 & b9_r2 & b10_r2 & b11_r2 & b12_r2 & b13_r2 & b14_r2 & b15_r2 & b16_r2 & b17_r2 & b18_r2 & b19_r2 & b20_r2 & b21_r2 & b22_r2 & b23_r2 & b24_r2 & b25_r2 & b26_r2 & b27_r2 & b28_r2 & b29_r2 & b30_r2 & b31_r2 & b0_r2 & b1_r2 & b2_r2 & b3_r2 & b4_r2 & b5_r2 & b6_r2);
      xorw32_i_r5 : \Main_xorW32\ port map (main_xorw32_out_r4, conn_r8, main_xorw32_out_r5);
      andw32_i : \Main_andW32\ port map (e, f, main_andw32_out);
      notw32_i : \notW32\ port map (p0 => e, p1 => extres(31 downto 0));
      andw32_i_r1 : \Main_andW32\ port map (extres, g, main_andw32_out_r1);
      xorw32_i_r6 : \Main_xorW32\ port map (main_andw32_out, main_andw32_out_r1, main_xorw32_out_r6);
      plusw32_i_r3 : \Main_plusW32\ port map (main_xorw32_out_r5, main_xorw32_out_r6, main_plusw32_out_r3);
      plusw32_i_r4 : \Main_plusW32\ port map (za_r1, w00, main_plusw32_out_r4);
      plusw32_i_r5 : \Main_plusW32\ port map (main_plusw32_out_r3, main_plusw32_out_r4, main_plusw32_out_r5);
      plusw32_i_r6 : \Main_plusW32\ port map (h, main_plusw32_out_r5, main_plusw32_out_r6);
      b0_r3 <= s0(255 downto 255);
      b1_r3 <= s0(254 downto 254);
      b2_r3 <= s0(253 downto 253);
      b3_r3 <= s0(252 downto 252);
      b4_r3 <= s0(251 downto 251);
      b5_r3 <= s0(250 downto 250);
      b6_r3 <= s0(249 downto 249);
      b7_r3 <= s0(248 downto 248);
      b8_r3 <= s0(247 downto 247);
      b9_r3 <= s0(246 downto 246);
      b10_r3 <= s0(245 downto 245);
      b11_r3 <= s0(244 downto 244);
      b12_r3 <= s0(243 downto 243);
      b13_r3 <= s0(242 downto 242);
      b14_r3 <= s0(241 downto 241);
      b15_r3 <= s0(240 downto 240);
      b16_r3 <= s0(239 downto 239);
      b17_r3 <= s0(238 downto 238);
      b18_r3 <= s0(237 downto 237);
      b19_r3 <= s0(236 downto 236);
      b20_r3 <= s0(235 downto 235);
      b21_r3 <= s0(234 downto 234);
      b22_r3 <= s0(233 downto 233);
      b23_r3 <= s0(232 downto 232);
      b24_r3 <= s0(231 downto 231);
      b25_r3 <= s0(230 downto 230);
      b26_r3 <= s0(229 downto 229);
      b27_r3 <= s0(228 downto 228);
      b28_r3 <= s0(227 downto 227);
      b29_r3 <= s0(226 downto 226);
      b30_r3 <= s0(225 downto 225);
      b31_r3 <= s0(224 downto 224);
      conn_r9 <= (b30_r3 & b31_r3 & b0_r3 & b1_r3 & b2_r3 & b3_r3 & b4_r3 & b5_r3 & b6_r3 & b7_r3 & b8_r3 & b9_r3 & b10_r3 & b11_r3 & b12_r3 & b13_r3 & b14_r3 & b15_r3 & b16_r3 & b17_r3 & b18_r3 & b19_r3 & b20_r3 & b21_r3 & b22_r3 & b23_r3 & b24_r3 & b25_r3 & b26_r3 & b27_r3 & b28_r3 & b29_r3);
      conn_r10 <= (b19_r3 & b20_r3 & b21_r3 & b22_r3 & b23_r3 & b24_r3 & b25_r3 & b26_r3 & b27_r3 & b28_r3 & b29_r3 & b30_r3 & b31_r3 & b0_r3 & b1_r3 & b2_r3 & b3_r3 & b4_r3 & b5_r3 & b6_r3 & b7_r3 & b8_r3 & b9_r3 & b10_r3 & b11_r3 & b12_r3 & b13_r3 & b14_r3 & b15_r3 & b16_r3 & b17_r3 & b18_r3);
      xorw32_i_r7 : \Main_xorW32\ port map (conn_r9, conn_r10, main_xorw32_out_r7);
      conn_r11 <= (b10_r3 & b11_r3 & b12_r3 & b13_r3 & b14_r3 & b15_r3 & b16_r3 & b17_r3 & b18_r3 & b19_r3 & b20_r3 & b21_r3 & b22_r3 & b23_r3 & b24_r3 & b25_r3 & b26_r3 & b27_r3 & b28_r3 & b29_r3 & b30_r3 & b31_r3 & b0_r3 & b1_r3 & b2_r3 & b3_r3 & b4_r3 & b5_r3 & b6_r3 & b7_r3 & b8_r3 & b9_r3);
      xorw32_i_r8 : \Main_xorW32\ port map (main_xorw32_out_r7, conn_r11, main_xorw32_out_r8);
      andw32_i_r2 : \Main_andW32\ port map (a, b, main_andw32_out_r2);
      andw32_i_r3 : \Main_andW32\ port map (a, c, main_andw32_out_r3);
      xorw32_i_r9 : \Main_xorW32\ port map (main_andw32_out_r2, main_andw32_out_r3, main_xorw32_out_r9);
      andw32_i_r4 : \Main_andW32\ port map (b, c, main_andw32_out_r4);
      xorw32_i_r10 : \Main_xorW32\ port map (main_xorw32_out_r9, main_andw32_out_r4, main_xorw32_out_r10);
      plusw32_i_r7 : \Main_plusW32\ port map (main_xorw32_out_r8, main_xorw32_out_r10, main_plusw32_out_r7);
      plusw32_i_r8 : \Main_plusW32\ port map (main_plusw32_out_r6, main_plusw32_out_r7, main_plusw32_out_r8);
      plusw32_i_r9 : \Main_plusW32\ port map (d, main_plusw32_out_r6, main_plusw32_out_r9);
      za1 <= (main_plusw32_out_r8 & a & b & c & main_plusw32_out_r9 & e & f & g);
      with s3 select za_r2 <=
            std_logic_vector'(B"000001") when "000000",
            std_logic_vector'(B"000010") when "000001",
            std_logic_vector'(B"000011") when "000010",
            std_logic_vector'(B"000100") when "000011",
            std_logic_vector'(B"000101") when "000100",
            std_logic_vector'(B"000110") when "000101",
            std_logic_vector'(B"000111") when "000110",
            std_logic_vector'(B"001000") when "000111",
            std_logic_vector'(B"001001") when "001000",
            std_logic_vector'(B"001010") when "001001",
            std_logic_vector'(B"001011") when "001010",
            std_logic_vector'(B"001100") when "001011",
            std_logic_vector'(B"001101") when "001100",
            std_logic_vector'(B"001110") when "001101",
            std_logic_vector'(B"001111") when "001110",
            std_logic_vector'(B"010000") when "001111",
            std_logic_vector'(B"010001") when "010000",
            std_logic_vector'(B"010010") when "010001",
            std_logic_vector'(B"010011") when "010010",
            std_logic_vector'(B"010100") when "010011",
            std_logic_vector'(B"010101") when "010100",
            std_logic_vector'(B"010110") when "010101",
            std_logic_vector'(B"010111") when "010110",
            std_logic_vector'(B"011000") when "010111",
            std_logic_vector'(B"011001") when "011000",
            std_logic_vector'(B"011010") when "011001",
            std_logic_vector'(B"011011") when "011010",
            std_logic_vector'(B"011100") when "011011",
            std_logic_vector'(B"011101") when "011100",
            std_logic_vector'(B"011110") when "011101",
            std_logic_vector'(B"011111") when "011110",
            std_logic_vector'(B"100000") when "011111",
            std_logic_vector'(B"100001") when "100000",
            std_logic_vector'(B"100010") when "100001",
            std_logic_vector'(B"100011") when "100010",
            std_logic_vector'(B"100100") when "100011",
            std_logic_vector'(B"100101") when "100100",
            std_logic_vector'(B"100110") when "100101",
            std_logic_vector'(B"100111") when "100110",
            std_logic_vector'(B"101000") when "100111",
            std_logic_vector'(B"101001") when "101000",
            std_logic_vector'(B"101010") when "101001",
            std_logic_vector'(B"101011") when "101010",
            std_logic_vector'(B"101100") when "101011",
            std_logic_vector'(B"101101") when "101100",
            std_logic_vector'(B"101110") when "101101",
            std_logic_vector'(B"101111") when "101110",
            std_logic_vector'(B"110000") when "101111",
            std_logic_vector'(B"110001") when "110000",
            std_logic_vector'(B"110010") when "110001",
            std_logic_vector'(B"110011") when "110010",
            std_logic_vector'(B"110100") when "110011",
            std_logic_vector'(B"110101") when "110100",
            std_logic_vector'(B"110110") when "110101",
            std_logic_vector'(B"110111") when "110110",
            std_logic_vector'(B"111000") when "110111",
            std_logic_vector'(B"111001") when "111000",
            std_logic_vector'(B"111010") when "111001",
            std_logic_vector'(B"111011") when "111010",
            std_logic_vector'(B"111100") when "111011",
            std_logic_vector'(B"111101") when "111100",
            std_logic_vector'(B"111110") when "111101",
            std_logic_vector'(B"111111") when "111110",
            std_logic_vector'(B"000000") when others;
      res <= (std_logic_vector'(B"1000000000000000000000000000000000000000000000000000000000000000001") & s3 & za1 & za & s2 & za_r2);
end architecture;

-- main.dev
-- block '$L.Main.dev' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_dev is
      port (\Zds\ : in std_logic_vector (67 downto 0);
            s0 : in std_logic_vector (255 downto 0);
            s1 : in std_logic_vector (511 downto 0);
            s2 : in std_logic_vector (255 downto 0);
            s3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1102 downto 0));
end entity;

architecture rtl of main_dev is
      component \main___unused\ is
            port (s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1102 downto 0));
      end component;
      component main_arm2 is
            port (w1 : in std_logic_vector (31 downto 0);
                  w2 : in std_logic_vector (31 downto 0);
                  s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1102 downto 0));
      end component;
      signal w1 : std_logic_vector (31 downto 0);
      signal w2 : std_logic_vector (31 downto 0);
      signal main_arm2_out : std_logic_vector (1102 downto 0);
      signal main_arm2_out_r1 : std_logic_vector (1102 downto 0);
      signal x0 : std_logic_vector (31 downto 0);
      signal x1 : std_logic_vector (31 downto 0);
      signal x4 : std_logic_vector (31 downto 0);
      signal x5 : std_logic_vector (31 downto 0);
      signal x6 : std_logic_vector (31 downto 0);
      signal x7 : std_logic_vector (31 downto 0);
      signal x8 : std_logic_vector (31 downto 0);
      signal x9 : std_logic_vector (31 downto 0);
      signal xa : std_logic_vector (31 downto 0);
      signal xb : std_logic_vector (31 downto 0);
      signal xc : std_logic_vector (31 downto 0);
      signal xd : std_logic_vector (31 downto 0);
      signal xe : std_logic_vector (31 downto 0);
      signal xf : std_logic_vector (31 downto 0);
      signal za : std_logic_vector (511 downto 0);
      signal \main__unused_out\ : std_logic_vector (1102 downto 0);
      signal x2 : std_logic_vector (31 downto 0);
      signal x3 : std_logic_vector (31 downto 0);
      signal za_r1 : std_logic_vector (511 downto 0);
      signal \main__unused_out_r1\ : std_logic_vector (1102 downto 0);
      signal za_r2 : std_logic_vector (511 downto 0);
      signal \main__unused_out_r2\ : std_logic_vector (1102 downto 0);
      signal za_r3 : std_logic_vector (511 downto 0);
      signal \main__unused_out_r3\ : std_logic_vector (1102 downto 0);
      signal za_r4 : std_logic_vector (511 downto 0);
      signal \main__unused_out_r4\ : std_logic_vector (1102 downto 0);
      signal za_r5 : std_logic_vector (511 downto 0);
      signal \main__unused_out_r5\ : std_logic_vector (1102 downto 0);
      signal za_r6 : std_logic_vector (511 downto 0);
      signal x0_r1 : std_logic_vector (31 downto 0);
      signal x1_r1 : std_logic_vector (31 downto 0);
      signal za_r7 : std_logic_vector (64 downto 0);
      signal x2_r1 : std_logic_vector (31 downto 0);
      signal x3_r1 : std_logic_vector (31 downto 0);
      signal za_r8 : std_logic_vector (64 downto 0);
      signal x4_r1 : std_logic_vector (31 downto 0);
      signal x5_r1 : std_logic_vector (31 downto 0);
      signal za_r9 : std_logic_vector (64 downto 0);
      signal x6_r1 : std_logic_vector (31 downto 0);
      signal x7_r1 : std_logic_vector (31 downto 0);
      signal za_r10 : std_logic_vector (64 downto 0);
begin
      w1 <= \Zds\(63 downto 32);
      w2 <= \Zds\(31 downto 0);
      arm2_i : main_arm2 port map (w1, w2, s0, s1, std_logic_vector'(X"6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19"), s3, main_arm2_out);
      arm2_i_r1 : main_arm2 port map (w1, w2, s0, s1, s2, s3, main_arm2_out_r1);
      x0 <= s1(511 downto 480);
      x1 <= s1(479 downto 448);
      x4 <= s1(383 downto 352);
      x5 <= s1(351 downto 320);
      x6 <= s1(319 downto 288);
      x7 <= s1(287 downto 256);
      x8 <= s1(255 downto 224);
      x9 <= s1(223 downto 192);
      xa <= s1(191 downto 160);
      xb <= s1(159 downto 128);
      xc <= s1(127 downto 96);
      xd <= s1(95 downto 64);
      xe <= s1(63 downto 32);
      xf <= s1(31 downto 0);
      za <= (x0 & x1 & w1 & w2 & x4 & x5 & x6 & x7 & x8 & x9 & xa & xb & xc & xd & xe & xf);
      \_unused_i\ : \main___unused\ port map (s0, za, s2, s3, \main__unused_out\);
      x2 <= s1(447 downto 416);
      x3 <= s1(415 downto 384);
      za_r1 <= (x0 & x1 & x2 & x3 & w1 & w2 & x6 & x7 & x8 & x9 & xa & xb & xc & xd & xe & xf);
      \_unused_i_r1\ : \main___unused\ port map (s0, za_r1, s2, s3, \main__unused_out_r1\);
      za_r2 <= (x0 & x1 & x2 & x3 & x4 & x5 & w1 & w2 & x8 & x9 & xa & xb & xc & xd & xe & xf);
      \_unused_i_r2\ : \main___unused\ port map (s0, za_r2, s2, s3, \main__unused_out_r2\);
      za_r3 <= (x0 & x1 & x2 & x3 & x4 & x5 & x6 & x7 & w1 & w2 & xa & xb & xc & xd & xe & xf);
      \_unused_i_r3\ : \main___unused\ port map (s0, za_r3, s2, s3, \main__unused_out_r3\);
      za_r4 <= (x0 & x1 & x2 & x3 & x4 & x5 & x6 & x7 & x8 & x9 & w1 & w2 & xc & xd & xe & xf);
      \_unused_i_r4\ : \main___unused\ port map (s0, za_r4, s2, s3, \main__unused_out_r4\);
      za_r5 <= (x0 & x1 & x2 & x3 & x4 & x5 & x6 & x7 & x8 & x9 & xa & xb & w1 & w2 & xe & xf);
      \_unused_i_r5\ : \main___unused\ port map (s0, za_r5, s2, s3, \main__unused_out_r5\);
      za_r6 <= (x0 & x1 & x2 & x3 & x4 & x5 & x6 & x7 & x8 & x9 & xa & xb & xc & xd & w1 & w2);
      x0_r1 <= s2(255 downto 224);
      x1_r1 <= s2(223 downto 192);
      za_r7 <= (std_logic_vector'(B"0") & x0_r1 & x1_r1);
      x2_r1 <= s2(191 downto 160);
      x3_r1 <= s2(159 downto 128);
      za_r8 <= (std_logic_vector'(B"0") & x2_r1 & x3_r1);
      x4_r1 <= s2(127 downto 96);
      x5_r1 <= s2(95 downto 64);
      za_r9 <= (std_logic_vector'(B"0") & x4_r1 & x5_r1);
      x6_r1 <= s2(63 downto 32);
      x7_r1 <= s2(31 downto 0);
      za_r10 <= (std_logic_vector'(B"0") & x6_r1 & x7_r1);
      with \Zds\(67 downto 64) select res <=
            main_arm2_out when "0000",
            main_arm2_out_r1 when "0001",
            \main__unused_out\ when "0010",
            \main__unused_out_r1\ when "0011",
            \main__unused_out_r2\ when "0100",
            \main__unused_out_r3\ when "0101",
            \main__unused_out_r4\ when "0110",
            \main__unused_out_r5\ when "0111",
            (std_logic_vector'(B"1000000000000000000000000000000000000000000000000000000000000000010000000") & s2 & za_r6 & s2 & std_logic_vector'(B"000000")) when "1000",
            (za_r7 & std_logic_vector'(X"00") & s0 & s1 & s2 & s3) when "1001",
            (za_r8 & std_logic_vector'(X"00") & s0 & s1 & s2 & s3) when "1010",
            (za_r9 & std_logic_vector'(X"00") & s0 & s1 & s2 & s3) when "1011",
            (za_r10 & std_logic_vector'(X"00") & s0 & s1 & s2 & s3) when others;
end architecture;

-- Main.plusW32
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_plusW32\ is
      port (\Zeta0\ : in std_logic_vector (31 downto 0);
            \Zeta1\ : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_plusW32\ is
      component \plusW32\ is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : in std_logic_vector (31 downto 0);
                  p2 : out std_logic_vector (31 downto 0));
      end component;
      signal extres : std_logic_vector (31 downto 0);
begin
      plusw32_i : \plusW32\ port map (p0 => \Zeta0\, p1 => \Zeta1\, p2 => extres(31 downto 0));
      res <= extres;
end architecture;

-- Main.andW32
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_andW32\ is
      port (\Zeta0\ : in std_logic_vector (31 downto 0);
            \Zeta1\ : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_andW32\ is
      component \andW32\ is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : in std_logic_vector (31 downto 0);
                  p2 : out std_logic_vector (31 downto 0));
      end component;
      signal extres : std_logic_vector (31 downto 0);
begin
      andw32_i : \andW32\ port map (p0 => \Zeta0\, p1 => \Zeta1\, p2 => extres(31 downto 0));
      res <= extres;
end architecture;

-- Main.xorW32
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_xorW32\ is
      port (\Zeta0\ : in std_logic_vector (31 downto 0);
            \Zeta1\ : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_xorW32\ is
      component \xorW32\ is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : in std_logic_vector (31 downto 0);
                  p2 : out std_logic_vector (31 downto 0));
      end component;
      signal extres : std_logic_vector (31 downto 0);
begin
      xorw32_i : \xorW32\ port map (p0 => \Zeta0\, p1 => \Zeta1\, p2 => extres(31 downto 0));
      res <= extres;
end architecture;