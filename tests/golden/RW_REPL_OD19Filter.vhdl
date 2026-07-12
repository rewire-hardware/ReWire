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
            \__in0\ : in std_logic_vector (0 downto 0);
            \__in1\ : in std_logic_vector (31 downto 0);
            \__out0\ : out std_logic_vector (0 downto 0);
            \__out1\ : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of top_level is
      component \main_getReg\ is
            port (w32 : in std_logic_vector (31 downto 0);
                  s0 : in std_logic_vector (38 downto 0);
                  res : out std_logic_vector (40 downto 0));
      end component;
      component \main_nextPC\ is
            port (s0 : in std_logic_vector (38 downto 0);
                  res : out std_logic_vector (40 downto 0));
      end component;
      component \main_putReg\ is
            port (w32 : in std_logic_vector (31 downto 0);
                  s0 : in std_logic_vector (38 downto 0);
                  res : out std_logic_vector (40 downto 0));
      end component;
      component test2 is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : in std_logic_vector (31 downto 0);
                  p2 : out std_logic_vector (0 downto 0));
      end component;
      component test3 is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : in std_logic_vector (31 downto 0);
                  p2 : out std_logic_vector (0 downto 0));
      end component;
      component test4 is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : in std_logic_vector (31 downto 0);
                  p2 : out std_logic_vector (0 downto 0));
      end component;
      -- state registers
      -- __st0: 32 bits, init 0x0
      -- __st1: 7 bits, init 0x0
      signal \__st0\ : std_logic_vector (31 downto 0) := (others => '0');
      signal \__st0_next\ : std_logic_vector (31 downto 0);
      signal \__st1\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000000");
      signal \__st1_next\ : std_logic_vector (6 downto 0);
      signal disc : std_logic_vector (38 downto 0);
      signal main_putreg_out : std_logic_vector (40 downto 0);
      signal main_getreg_out : std_logic_vector (40 downto 0);
      signal extres : std_logic_vector (0 downto 0);
      signal za1 : std_logic_vector (1 downto 0);
      signal extres_r1 : std_logic_vector (0 downto 0);
      signal za1_r1 : std_logic_vector (1 downto 0);
      signal extres_r2 : std_logic_vector (0 downto 0);
      signal za1_r2 : std_logic_vector (1 downto 0);
      signal main_nextpc_out : std_logic_vector (40 downto 0);
      signal zres : std_logic_vector (40 downto 0);
begin
      -- combinational logic
      disc <= (\__st0\ & \__st1\);
      putreg_i : \main_putReg\ port map (\__in1\, disc, main_putreg_out);
      getreg_i : \main_getReg\ port map (\__in1\, disc, main_getreg_out);
      test2_i : test2 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres(0 downto 0));
      za1 <= (std_logic_vector'(B"0") & extres);
      test3_i : test3 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres_r1(0 downto 0));
      za1_r1 <= (std_logic_vector'(B"0") & extres_r1);
      test4_i : test4 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres_r2(0 downto 0));
      za1_r2 <= (std_logic_vector'(B"0") & extres_r2);
      nextpc_i : \main_nextPC\ port map (disc, main_nextpc_out);
      zres <= rw_cond(rw_not(\__in0\), rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000000")), main_putreg_out, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000001")), main_getreg_out, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000010")), main_putreg_out, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000011")), main_getreg_out, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000100")), (za1 & disc), rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000101")), (za1_r1 & disc), rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000110")), (za1_r2 & disc), main_nextpc_out))))))), (std_logic_vector'(B"10") & disc));
      \__st0_next\ <= zres(38 downto 7);
      \__st1_next\ <= zres(6 downto 0);
      -- outputs
      \__out0\ <= zres(40 downto 40);
      \__out1\ <= zres(39 downto 39);
      -- state register update
      process (clk, rst)
      begin
            if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(X"00000000");
                  \__st1\ <= std_logic_vector'(B"0000000");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
            end if;
      end process;
end architecture;

-- main.nextPC
-- block '$L.Main.nextPC' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_nextPC\ is
      port (s0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (40 downto 0));
end entity;

architecture rtl of \main_nextPC\ is
      signal r : std_logic_vector (31 downto 0);
      signal pc : std_logic_vector (6 downto 0);
      signal za : std_logic_vector (6 downto 0);
      signal za1 : std_logic_vector (38 downto 0);
begin
      r <= s0(38 downto 7);
      pc <= s0(6 downto 0);
      with pc select za <=
            std_logic_vector'(B"0000001") when "0000000",
            std_logic_vector'(B"0000010") when "0000001",
            std_logic_vector'(B"0000011") when "0000010",
            std_logic_vector'(B"0000100") when "0000011",
            std_logic_vector'(B"0000101") when "0000100",
            std_logic_vector'(B"0000110") when "0000101",
            std_logic_vector'(B"0000111") when "0000110",
            std_logic_vector'(B"0001000") when "0000111",
            std_logic_vector'(B"0001001") when "0001000",
            std_logic_vector'(B"0001010") when "0001001",
            std_logic_vector'(B"0001011") when "0001010",
            std_logic_vector'(B"0001100") when "0001011",
            std_logic_vector'(B"0001101") when "0001100",
            std_logic_vector'(B"0001110") when "0001101",
            std_logic_vector'(B"0001111") when "0001110",
            std_logic_vector'(B"0010000") when "0001111",
            std_logic_vector'(B"0010001") when "0010000",
            std_logic_vector'(B"0010010") when "0010001",
            std_logic_vector'(B"0010011") when "0010010",
            std_logic_vector'(B"0010100") when "0010011",
            std_logic_vector'(B"0010101") when "0010100",
            std_logic_vector'(B"0010110") when "0010101",
            std_logic_vector'(B"0010111") when "0010110",
            std_logic_vector'(B"0011000") when "0010111",
            std_logic_vector'(B"0011001") when "0011000",
            std_logic_vector'(B"0011010") when "0011001",
            std_logic_vector'(B"0011011") when "0011010",
            std_logic_vector'(B"0011100") when "0011011",
            std_logic_vector'(B"0011101") when "0011100",
            std_logic_vector'(B"0011110") when "0011101",
            std_logic_vector'(B"0011111") when "0011110",
            std_logic_vector'(B"0100000") when "0011111",
            std_logic_vector'(B"0100001") when "0100000",
            std_logic_vector'(B"0100010") when "0100001",
            std_logic_vector'(B"0100011") when "0100010",
            std_logic_vector'(B"0100100") when "0100011",
            std_logic_vector'(B"0100101") when "0100100",
            std_logic_vector'(B"0100110") when "0100101",
            std_logic_vector'(B"0100111") when "0100110",
            std_logic_vector'(B"0101000") when "0100111",
            std_logic_vector'(B"0101001") when "0101000",
            std_logic_vector'(B"0101010") when "0101001",
            std_logic_vector'(B"0101011") when "0101010",
            std_logic_vector'(B"0101100") when "0101011",
            std_logic_vector'(B"0101101") when "0101100",
            std_logic_vector'(B"0101110") when "0101101",
            std_logic_vector'(B"0101111") when "0101110",
            std_logic_vector'(B"0110000") when "0101111",
            std_logic_vector'(B"0110001") when "0110000",
            std_logic_vector'(B"0110010") when "0110001",
            std_logic_vector'(B"0110011") when "0110010",
            std_logic_vector'(B"0110100") when "0110011",
            std_logic_vector'(B"0110101") when "0110100",
            std_logic_vector'(B"0110110") when "0110101",
            std_logic_vector'(B"0110111") when "0110110",
            std_logic_vector'(B"0111000") when "0110111",
            std_logic_vector'(B"0111001") when "0111000",
            std_logic_vector'(B"0111010") when "0111001",
            std_logic_vector'(B"0111011") when "0111010",
            std_logic_vector'(B"0111100") when "0111011",
            std_logic_vector'(B"0111101") when "0111100",
            std_logic_vector'(B"0111110") when "0111101",
            std_logic_vector'(B"0111111") when "0111110",
            std_logic_vector'(B"1000000") when "0111111",
            std_logic_vector'(B"1000001") when "1000000",
            std_logic_vector'(B"1000010") when "1000001",
            std_logic_vector'(B"1000011") when "1000010",
            std_logic_vector'(B"1000100") when "1000011",
            std_logic_vector'(B"1000101") when "1000100",
            std_logic_vector'(B"1000110") when "1000101",
            std_logic_vector'(B"1000111") when "1000110",
            std_logic_vector'(B"1001000") when "1000111",
            std_logic_vector'(B"1001001") when "1001000",
            std_logic_vector'(B"1001010") when "1001001",
            std_logic_vector'(B"1001011") when "1001010",
            std_logic_vector'(B"1001100") when "1001011",
            std_logic_vector'(B"1001101") when "1001100",
            std_logic_vector'(B"1001110") when "1001101",
            std_logic_vector'(B"1001111") when "1001110",
            std_logic_vector'(B"1010000") when "1001111",
            std_logic_vector'(B"1010001") when "1010000",
            std_logic_vector'(B"1010010") when "1010001",
            std_logic_vector'(B"1010011") when "1010010",
            std_logic_vector'(B"1010100") when "1010011",
            std_logic_vector'(B"1010101") when "1010100",
            std_logic_vector'(B"1010110") when "1010101",
            std_logic_vector'(B"1010111") when "1010110",
            std_logic_vector'(B"1011000") when "1010111",
            std_logic_vector'(B"1011001") when "1011000",
            std_logic_vector'(B"1011010") when "1011001",
            std_logic_vector'(B"1011011") when "1011010",
            std_logic_vector'(B"1011100") when "1011011",
            std_logic_vector'(B"1011101") when "1011100",
            std_logic_vector'(B"1011110") when "1011101",
            std_logic_vector'(B"1011111") when "1011110",
            std_logic_vector'(B"1100000") when "1011111",
            std_logic_vector'(B"1100001") when "1100000",
            std_logic_vector'(B"1100010") when "1100001",
            std_logic_vector'(B"1100011") when "1100010",
            std_logic_vector'(B"1100100") when "1100011",
            std_logic_vector'(B"1100101") when "1100100",
            std_logic_vector'(B"1100110") when "1100101",
            std_logic_vector'(B"1100111") when "1100110",
            std_logic_vector'(B"1101000") when "1100111",
            std_logic_vector'(B"1101001") when "1101000",
            std_logic_vector'(B"1101010") when "1101001",
            std_logic_vector'(B"1101011") when "1101010",
            std_logic_vector'(B"1101100") when "1101011",
            std_logic_vector'(B"1101101") when "1101100",
            std_logic_vector'(B"1101110") when "1101101",
            std_logic_vector'(B"1101111") when "1101110",
            std_logic_vector'(B"1110000") when "1101111",
            std_logic_vector'(B"1110001") when "1110000",
            std_logic_vector'(B"1110010") when "1110001",
            std_logic_vector'(B"1110011") when "1110010",
            std_logic_vector'(B"1110100") when "1110011",
            std_logic_vector'(B"1110101") when "1110100",
            std_logic_vector'(B"1110110") when "1110101",
            std_logic_vector'(B"1110111") when "1110110",
            std_logic_vector'(B"1111000") when "1110111",
            std_logic_vector'(B"1111001") when "1111000",
            std_logic_vector'(B"1111010") when "1111001",
            std_logic_vector'(B"1111011") when "1111010",
            std_logic_vector'(B"1111100") when "1111011",
            std_logic_vector'(B"1111101") when "1111100",
            std_logic_vector'(B"1111110") when "1111101",
            std_logic_vector'(B"0000000") when others;
      za1 <= (r & za);
      res <= (std_logic_vector'(B"10") & za1);
end architecture;

-- main.putReg
-- block '$L.Main.putReg' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_putReg\ is
      port (w32 : in std_logic_vector (31 downto 0);
            s0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (40 downto 0));
end entity;

architecture rtl of \main_putReg\ is
      component \main_nextPC\ is
            port (s0 : in std_logic_vector (38 downto 0);
                  res : out std_logic_vector (40 downto 0));
      end component;
      signal pc : std_logic_vector (6 downto 0);
      signal za : std_logic_vector (38 downto 0);
      signal main_nextpc_out : std_logic_vector (40 downto 0);
begin
      pc <= s0(6 downto 0);
      za <= (w32 & pc);
      nextpc_i : \main_nextPC\ port map (za, main_nextpc_out);
      res <= main_nextpc_out;
end architecture;

-- main.getReg
-- block '$L.Main.getReg' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getReg\ is
      port (w32 : in std_logic_vector (31 downto 0);
            s0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (40 downto 0));
end entity;

architecture rtl of \main_getReg\ is
      component test1 is
            port (p0 : in std_logic_vector (31 downto 0);
                  p1 : in std_logic_vector (31 downto 0);
                  p2 : out std_logic_vector (0 downto 0));
      end component;
      signal r : std_logic_vector (31 downto 0);
      signal extres : std_logic_vector (0 downto 0);
      signal za1 : std_logic_vector (1 downto 0);
begin
      r <= s0(38 downto 7);
      test1_i : test1 port map (p0 => w32, p1 => r, p2 => extres(0 downto 0));
      za1 <= (std_logic_vector'(B"0") & extres);
      res <= (za1 & s0);
end architecture;