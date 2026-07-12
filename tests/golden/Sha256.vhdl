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
            \__in0\ : in std_logic_vector (513 downto 0);
            \__out0\ : out std_logic_vector (257 downto 0));
end entity;

architecture rtl of top_level is
      component main_dev is
            port (\Zds\ : in std_logic_vector (513 downto 0);
                  s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1295 downto 0));
      end component;
      component main_loop is
            port (s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1295 downto 0));
      end component;
      -- state registers
      -- __resumption_tag: 8 bits, init 0x80
      --   states: 0=i 1=_unused2 2=$x2
      -- __st0: 224 bits, init 0x0
      -- __st1: 32 bits, init 0x0
      -- __st2: 480 bits, init 0x0
      -- __st3: 32 bits, init 0x0
      -- __st4: 224 bits, init 0x0
      -- __st5: 32 bits, init 0x0
      -- __st6: 6 bits, init 0x0
      signal \__resumption_tag\ : std_logic_vector (7 downto 0) := std_logic_vector'(X"80");
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
      signal ctr : std_logic_vector (5 downto 0);
      signal s0 : std_logic_vector (255 downto 0);
      signal s1 : std_logic_vector (511 downto 0);
      signal s2 : std_logic_vector (255 downto 0);
      signal za : std_logic_vector (0 downto 0);
      signal main_loop_out : std_logic_vector (1295 downto 0);
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
      signal za_r1 : std_logic_vector (255 downto 0);
      signal main_dev_out : std_logic_vector (1295 downto 0);
      signal main_dev_out_r1 : std_logic_vector (1295 downto 0);
      signal zres : std_logic_vector (1295 downto 0);
begin
      -- combinational logic
      ctr <= \__resumption_tag\(5 downto 0);
      s0 <= (\__st0\ & \__st1\);
      s1 <= (\__st2\ & \__st3\);
      s2 <= (\__st4\ & \__st5\);
      za <= rw_eq(ctr, std_logic_vector'(B"111111"));
      loop_i : main_loop port map (s0, s1, s2, \__st6\, main_loop_out);
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
      za_r1 <= (rw_add(a, h1) & rw_add(b, h2) & rw_add(c, h3) & rw_add(d, h4) & rw_add(e, h5) & rw_add(f, h6) & rw_add(g, h7) & rw_add(\__st1\, \__st5\));
      dev_i : main_dev port map (\__in0\, s0, s1, za_r1, \__st6\, main_dev_out);
      dev_i_r1 : main_dev port map (\__in0\, s0, s1, s2, \__st6\, main_dev_out_r1);
      with \__resumption_tag\(7 downto 6) select zres <=
            rw_cond(rw_not(za), main_loop_out, main_dev_out) when "00",
            main_loop_out when "01",
            main_dev_out_r1 when others;
      \__resumption_tag_next\ <= zres(1037 downto 1030);
      \__st0_next\ <= zres(1029 downto 806);
      \__st1_next\ <= zres(805 downto 774);
      \__st2_next\ <= zres(773 downto 294);
      \__st3_next\ <= zres(293 downto 262);
      \__st4_next\ <= zres(261 downto 38);
      \__st5_next\ <= zres(37 downto 6);
      \__st6_next\ <= zres(5 downto 0);
      -- outputs
      \__out0\ <= zres(1295 downto 1038);
      -- state register update
      process (clk, rst)
      begin
            if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(X"80");
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

-- main.loop
-- block '$L.Main.loop' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_loop is
      port (s0 : in std_logic_vector (255 downto 0);
            s1 : in std_logic_vector (511 downto 0);
            s2 : in std_logic_vector (255 downto 0);
            s3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1295 downto 0));
end entity;

architecture rtl of main_loop is
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
      signal t1 : std_logic_vector (31 downto 0);
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
      za <= (w01 & w02 & w03 & w04 & w05 & w06 & w07 & w08 & w09 & w10 & w11 & w12 & w13 & w14 & w15 & rw_add(rw_add(rw_xor(rw_xor(rw_or(rw_shiftr(w14, std_logic_vector'(X"00000011")), rw_shiftl(w14, std_logic_vector'(X"0000000f"))), rw_or(rw_shiftr(w14, std_logic_vector'(X"00000013")), rw_shiftl(w14, std_logic_vector'(X"0000000d")))), rw_shiftr(w14, std_logic_vector'(X"0000000a"))), w09), rw_add(rw_xor(rw_xor(rw_or(rw_shiftr(w01, std_logic_vector'(X"00000007")), rw_shiftl(w01, std_logic_vector'(X"00000019"))), rw_or(rw_shiftr(w01, std_logic_vector'(X"00000012")), rw_shiftl(w01, std_logic_vector'(X"0000000e")))), rw_shiftr(w01, std_logic_vector'(X"00000003"))), w00)));
      za_r1 <= rw_resize(rw_shiftr(std_logic_vector'(X"428a2f9871374491b5c0fbcfe9b5dba53956c25b59f111f1923f82a4ab1c5ed5d807aa9812835b01243185be550c7dc372be5d7480deb1fe9bdc06a7c19bf174e49b69c1efbe47860fc19dc6240ca1cc2de92c6f4a7484aa5cb0a9dc76f988da983e5152a831c66db00327c8bf597fc7c6e00bf3d5a7914706ca63511429296727b70a852e1b21384d2c6dfc53380d13650a7354766a0abb81c2c92e92722c85a2bfe8a1a81a664bc24b8b70c76c51a3d192e819d6990624f40e3585106aa07019a4c1161e376c082748774c34b0bcb5391c0cb34ed8aa4a5b9cca4f682e6ff3748f82ee78a5636f84c878148cc7020890befffaa4506cebbef9a3f7c67178f2"), rw_mul(rw_sub(rw_sub(std_logic_vector'(X"00000000000000000000000000000040"), rw_resize(s3, 128)), std_logic_vector'(X"00000000000000000000000000000001")), std_logic_vector'(X"00000000000000000000000000000020"))), 32);
      a <= s0(255 downto 224);
      b <= s0(223 downto 192);
      c <= s0(191 downto 160);
      d <= s0(159 downto 128);
      e <= s0(127 downto 96);
      f <= s0(95 downto 64);
      g <= s0(63 downto 32);
      h <= s0(31 downto 0);
      t1 <= rw_add(h, rw_add(rw_add(rw_xor(rw_xor(rw_or(rw_shiftr(e, std_logic_vector'(X"00000006")), rw_shiftl(e, std_logic_vector'(X"0000001a"))), rw_or(rw_shiftr(e, std_logic_vector'(X"0000000b")), rw_shiftl(e, std_logic_vector'(X"00000015")))), rw_or(rw_shiftr(e, std_logic_vector'(X"00000019")), rw_shiftl(e, std_logic_vector'(X"00000007")))), rw_xor(rw_and(e, f), rw_and(rw_not(e), g))), rw_add(za_r1, w00)));
      za1 <= (rw_add(t1, rw_add(rw_xor(rw_xor(rw_or(rw_shiftr(a, std_logic_vector'(X"00000002")), rw_shiftl(a, std_logic_vector'(X"0000001e"))), rw_or(rw_shiftr(a, std_logic_vector'(X"0000000d")), rw_shiftl(a, std_logic_vector'(X"00000013")))), rw_or(rw_shiftr(a, std_logic_vector'(X"00000016")), rw_shiftl(a, std_logic_vector'(X"0000000a")))), rw_xor(rw_xor(rw_and(a, b), rw_and(a, c)), rw_and(b, c)))) & a & b & c & rw_add(d, t1) & e & f & g);
      za_r2 <= rw_add(s3, std_logic_vector'(B"000001"));
      res <= ((std_logic_vector'(B"1") & rw_repl(259, std_logic_vector'(B"0"))) & s3 & za1 & za & s2 & za_r2);
end architecture;

-- main._unused7
-- block '$L._unused7' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused7\ is
      port (hw32 : in std_logic_vector (511 downto 0);
            s0 : in std_logic_vector (255 downto 0);
            s1 : in std_logic_vector (511 downto 0);
            s2 : in std_logic_vector (255 downto 0);
            s3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1295 downto 0));
end entity;

architecture rtl of \main___unused7\ is
begin
      res <= (std_logic_vector'(B"01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000") & s0 & hw32 & s2 & s3);
end architecture;

-- main.dev
-- block '$L.Main.dev' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity main_dev is
      port (\Zds\ : in std_logic_vector (513 downto 0);
            s0 : in std_logic_vector (255 downto 0);
            s1 : in std_logic_vector (511 downto 0);
            s2 : in std_logic_vector (255 downto 0);
            s3 : in std_logic_vector (5 downto 0);
            res : out std_logic_vector (1295 downto 0));
end entity;

architecture rtl of main_dev is
      component \main___unused7\ is
            port (hw32 : in std_logic_vector (511 downto 0);
                  s0 : in std_logic_vector (255 downto 0);
                  s1 : in std_logic_vector (511 downto 0);
                  s2 : in std_logic_vector (255 downto 0);
                  s3 : in std_logic_vector (5 downto 0);
                  res : out std_logic_vector (1295 downto 0));
      end component;
      signal hw32 : std_logic_vector (511 downto 0);
      signal \main__unused7_out\ : std_logic_vector (1295 downto 0);
      signal \main__unused7_out_r1\ : std_logic_vector (1295 downto 0);
      signal za : std_logic_vector (257 downto 0);
begin
      hw32 <= \Zds\(511 downto 0);
      \_unused7_i\ : \main___unused7\ port map (hw32, std_logic_vector'(X"6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19"), s1, std_logic_vector'(X"6a09e667bb67ae853c6ef372a54ff53a510e527f9b05688c1f83d9ab5be0cd19"), std_logic_vector'(B"000000"), \main__unused7_out\);
      \_unused7_i_r1\ : \main___unused7\ port map (hw32, s2, s1, s2, std_logic_vector'(B"000000"), \main__unused7_out_r1\);
      za <= (std_logic_vector'(B"00") & s2);
      with \Zds\(513 downto 512) select res <=
            \main__unused7_out\ when "00",
            \main__unused7_out_r1\ when "01",
            (za & std_logic_vector'(X"80") & s0 & s1 & s2 & s3) when others;
end architecture;