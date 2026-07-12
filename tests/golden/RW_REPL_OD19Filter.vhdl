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
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (40 downto 0));
      end component;
      component \main_nextPC\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (40 downto 0));
      end component;
      component \main_putReg\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (38 downto 0);
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
      signal \__st0\ : std_logic_vector (31 downto 0) := std_logic_vector'(B"00000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (31 downto 0);
      signal \__st1\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000000");
      signal \__st1_next\ : std_logic_vector (6 downto 0);
      signal zi0 : std_logic_vector (38 downto 0);
      signal main_putreg_out : std_logic_vector (40 downto 0);
      signal main_getreg_out : std_logic_vector (40 downto 0);
      signal \main_putreg_outR1\ : std_logic_vector (40 downto 0);
      signal \main_getreg_outR1\ : std_logic_vector (40 downto 0);
      signal extres : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (1 downto 0);
      signal \extresR1\ : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (1 downto 0);
      signal \extresR2\ : std_logic_vector (0 downto 0);
      signal zi11 : std_logic_vector (0 downto 0);
      signal zi12 : std_logic_vector (1 downto 0);
      signal main_nextpc_out : std_logic_vector (40 downto 0);
      signal zres : std_logic_vector (40 downto 0);
begin
zi0 <= (\__st0\ & \__st1\);
      inst : \main_putReg\ port map (\__in1\, zi0, main_putreg_out);
      \instR1\ : \main_getReg\ port map (\__in1\, zi0, main_getreg_out);
      \instR2\ : \main_putReg\ port map (\__in1\, zi0, \main_putreg_outR1\);
      \instR3\ : \main_getReg\ port map (\__in1\, zi0, \main_getreg_outR1\);
      \instR4\ : test2 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres(0 downto 0));
      zi5 <= extres;
      zi6 <= (std_logic_vector'(B"0") & zi5);
      \instR5\ : test3 port map (p0 => \__in1\, p1 => \__st0\, p2 => \extresR1\(0 downto 0));
      zi8 <= \extresR1\;
      zi9 <= (std_logic_vector'(B"0") & zi8);
      \instR6\ : test4 port map (p0 => \__in1\, p1 => \__st0\, p2 => \extresR2\(0 downto 0));
      zi11 <= \extresR2\;
      zi12 <= (std_logic_vector'(B"0") & zi11);
      \instR7\ : \main_nextPC\ port map (zi0, main_nextpc_out);
      zres <= rw_cond(rw_not(\__in0\), rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000000")), main_putreg_out, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000001")), main_getreg_out, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000010")), \main_putreg_outR1\, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000011")), \main_getreg_outR1\, rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000100")), (zi6 & zi0), rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000101")), (zi9 & zi0), rw_cond(rw_eq(\__st1\, std_logic_vector'(B"0000110")), (zi12 & zi0), main_nextpc_out))))))), (std_logic_vector'(B"10") & zi0));
      \__st0_next\ <= zres(38 downto 7);
      \__st1_next\ <= zres(6 downto 0);
      \__out0\ <= zres(40 downto 40);
      \__out1\ <= zres(39 downto 39);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"00000000000000000000000000000000");
                  \__st1\ <= std_logic_vector'(B"0000000");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_nextPC\ is
port (arg0 : in std_logic_vector (38 downto 0);
      res : out std_logic_vector (40 downto 0));
end entity;

architecture rtl of \main_nextPC\ is
signal zi0 : std_logic_vector (31 downto 0);
      signal zi1 : std_logic_vector (6 downto 0);
      signal zi2 : std_logic_vector (6 downto 0);
      signal zi3 : std_logic_vector (38 downto 0);
begin
zi0 <= arg0(38 downto 7);
      zi1 <= arg0(6 downto 0);
      zi2 <= rw_cond(rw_eq(zi1, std_logic_vector'(B"0000000")), std_logic_vector'(B"0000001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0000001")), std_logic_vector'(B"0000010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0000010")), std_logic_vector'(B"0000011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0000011")), std_logic_vector'(B"0000100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0000100")), std_logic_vector'(B"0000101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0000101")), std_logic_vector'(B"0000110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0000110")), std_logic_vector'(B"0000111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0000111")), std_logic_vector'(B"0001000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001000")), std_logic_vector'(B"0001001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001001")), std_logic_vector'(B"0001010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001010")), std_logic_vector'(B"0001011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001011")), std_logic_vector'(B"0001100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001100")), std_logic_vector'(B"0001101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001101")), std_logic_vector'(B"0001110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001110")), std_logic_vector'(B"0001111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0001111")), std_logic_vector'(B"0010000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010000")), std_logic_vector'(B"0010001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010001")), std_logic_vector'(B"0010010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010010")), std_logic_vector'(B"0010011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010011")), std_logic_vector'(B"0010100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010100")), std_logic_vector'(B"0010101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010101")), std_logic_vector'(B"0010110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010110")), std_logic_vector'(B"0010111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0010111")), std_logic_vector'(B"0011000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011000")), std_logic_vector'(B"0011001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011001")), std_logic_vector'(B"0011010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011010")), std_logic_vector'(B"0011011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011011")), std_logic_vector'(B"0011100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011100")), std_logic_vector'(B"0011101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011101")), std_logic_vector'(B"0011110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011110")), std_logic_vector'(B"0011111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0011111")), std_logic_vector'(B"0100000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100000")), std_logic_vector'(B"0100001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100001")), std_logic_vector'(B"0100010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100010")), std_logic_vector'(B"0100011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100011")), std_logic_vector'(B"0100100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100100")), std_logic_vector'(B"0100101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100101")), std_logic_vector'(B"0100110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100110")), std_logic_vector'(B"0100111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0100111")), std_logic_vector'(B"0101000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101000")), std_logic_vector'(B"0101001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101001")), std_logic_vector'(B"0101010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101010")), std_logic_vector'(B"0101011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101011")), std_logic_vector'(B"0101100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101100")), std_logic_vector'(B"0101101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101101")), std_logic_vector'(B"0101110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101110")), std_logic_vector'(B"0101111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0101111")), std_logic_vector'(B"0110000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110000")), std_logic_vector'(B"0110001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110001")), std_logic_vector'(B"0110010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110010")), std_logic_vector'(B"0110011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110011")), std_logic_vector'(B"0110100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110100")), std_logic_vector'(B"0110101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110101")), std_logic_vector'(B"0110110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110110")), std_logic_vector'(B"0110111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0110111")), std_logic_vector'(B"0111000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111000")), std_logic_vector'(B"0111001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111001")), std_logic_vector'(B"0111010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111010")), std_logic_vector'(B"0111011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111011")), std_logic_vector'(B"0111100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111100")), std_logic_vector'(B"0111101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111101")), std_logic_vector'(B"0111110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111110")), std_logic_vector'(B"0111111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"0111111")), std_logic_vector'(B"1000000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000000")), std_logic_vector'(B"1000001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000001")), std_logic_vector'(B"1000010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000010")), std_logic_vector'(B"1000011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000011")), std_logic_vector'(B"1000100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000100")), std_logic_vector'(B"1000101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000101")), std_logic_vector'(B"1000110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000110")), std_logic_vector'(B"1000111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1000111")), std_logic_vector'(B"1001000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001000")), std_logic_vector'(B"1001001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001001")), std_logic_vector'(B"1001010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001010")), std_logic_vector'(B"1001011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001011")), std_logic_vector'(B"1001100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001100")), std_logic_vector'(B"1001101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001101")), std_logic_vector'(B"1001110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001110")), std_logic_vector'(B"1001111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1001111")), std_logic_vector'(B"1010000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010000")), std_logic_vector'(B"1010001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010001")), std_logic_vector'(B"1010010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010010")), std_logic_vector'(B"1010011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010011")), std_logic_vector'(B"1010100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010100")), std_logic_vector'(B"1010101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010101")), std_logic_vector'(B"1010110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010110")), std_logic_vector'(B"1010111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1010111")), std_logic_vector'(B"1011000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011000")), std_logic_vector'(B"1011001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011001")), std_logic_vector'(B"1011010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011010")), std_logic_vector'(B"1011011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011011")), std_logic_vector'(B"1011100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011100")), std_logic_vector'(B"1011101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011101")), std_logic_vector'(B"1011110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011110")), std_logic_vector'(B"1011111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1011111")), std_logic_vector'(B"1100000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100000")), std_logic_vector'(B"1100001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100001")), std_logic_vector'(B"1100010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100010")), std_logic_vector'(B"1100011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100011")), std_logic_vector'(B"1100100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100100")), std_logic_vector'(B"1100101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100101")), std_logic_vector'(B"1100110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100110")), std_logic_vector'(B"1100111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1100111")), std_logic_vector'(B"1101000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101000")), std_logic_vector'(B"1101001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101001")), std_logic_vector'(B"1101010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101010")), std_logic_vector'(B"1101011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101011")), std_logic_vector'(B"1101100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101100")), std_logic_vector'(B"1101101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101101")), std_logic_vector'(B"1101110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101110")), std_logic_vector'(B"1101111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1101111")), std_logic_vector'(B"1110000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110000")), std_logic_vector'(B"1110001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110001")), std_logic_vector'(B"1110010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110010")), std_logic_vector'(B"1110011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110011")), std_logic_vector'(B"1110100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110100")), std_logic_vector'(B"1110101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110101")), std_logic_vector'(B"1110110"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110110")), std_logic_vector'(B"1110111"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1110111")), std_logic_vector'(B"1111000"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1111000")), std_logic_vector'(B"1111001"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1111001")), std_logic_vector'(B"1111010"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1111010")), std_logic_vector'(B"1111011"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1111011")), std_logic_vector'(B"1111100"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1111100")), std_logic_vector'(B"1111101"), rw_cond(rw_eq(zi1, std_logic_vector'(B"1111101")), std_logic_vector'(B"1111110"), std_logic_vector'(B"0000000")))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      zi3 <= (zi0 & zi2);
      res <= (std_logic_vector'(B"10") & zi3);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_putReg\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (38 downto 0);
      res : out std_logic_vector (40 downto 0));
end entity;

architecture rtl of \main_putReg\ is
component \main_nextPC\ is
      port (arg0 : in std_logic_vector (38 downto 0);
            res : out std_logic_vector (40 downto 0));
      end component;
      signal zi1 : std_logic_vector (6 downto 0);
      signal zi2 : std_logic_vector (38 downto 0);
      signal main_nextpc_out : std_logic_vector (40 downto 0);
begin
zi1 <= arg1(6 downto 0);
      zi2 <= (arg0 & zi1);
      inst : \main_nextPC\ port map (zi2, main_nextpc_out);
      res <= main_nextpc_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_getReg\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (38 downto 0);
      res : out std_logic_vector (40 downto 0));
end entity;

architecture rtl of \main_getReg\ is
component test1 is
      port (p0 : in std_logic_vector (31 downto 0);
            p1 : in std_logic_vector (31 downto 0);
            p2 : out std_logic_vector (0 downto 0));
      end component;
      signal zi0 : std_logic_vector (31 downto 0);
      signal extres : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (1 downto 0);
begin
zi0 <= arg1(38 downto 7);
      inst : test1 port map (p0 => arg0, p1 => zi0, p2 => extres(0 downto 0));
      zi1 <= extres;
      zi2 <= (std_logic_vector'(B"0") & zi1);
      res <= (zi2 & arg1);
end architecture;