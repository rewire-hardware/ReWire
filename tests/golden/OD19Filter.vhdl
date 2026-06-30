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
component \Main_test1\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (31 downto 0);
            res : out std_logic_vector (0 downto 0));
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
      signal \__resumption_tag\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011011");
      signal \__resumption_tag_next\ : std_logic_vector (6 downto 0);
      signal \__st0\ : std_logic_vector (31 downto 0) := std_logic_vector'(B"00000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (31 downto 0);
      signal extres : std_logic_vector (0 downto 0);
      signal main_test1_out : std_logic_vector (0 downto 0);
      signal \extresR1\ : std_logic_vector (0 downto 0);
      signal \main_test1_outR1\ : std_logic_vector (0 downto 0);
      signal \extresR2\ : std_logic_vector (0 downto 0);
      signal zres : std_logic_vector (40 downto 0);
begin
inst : test4 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres(0 downto 0));
      \instR1\ : \Main_test1\ port map (\__in1\, \__st0\, main_test1_out);
      \instR2\ : test3 port map (p0 => \__in1\, p1 => \__st0\, p2 => \extresR1\(0 downto 0));
      \instR3\ : \Main_test1\ port map (\__in1\, \__st0\, \main_test1_outR1\);
      \instR4\ : test2 port map (p0 => \__in1\, p1 => \__st0\, p2 => \extresR2\(0 downto 0));
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0000001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101111110") & \__st0\), (std_logic_vector'(B"100000001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0000010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100111") & \__st0\), (std_logic_vector'(B"100000010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0000011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110100") & \__st0\), (std_logic_vector'(B"100000011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0000100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101111001") & \__st0\), (std_logic_vector'(B"100000100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0000101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101010") & \__st0\), (std_logic_vector'(B"100000101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0000110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101111011") & \__st0\), (std_logic_vector'(B"100000110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0000111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110000") & \__st0\), (std_logic_vector'(B"100000111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110011") & \__st0\), (std_logic_vector'(B"100001000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100011100") & \__st0\), (std_logic_vector'(B"100001001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101001") & \__st0\), (std_logic_vector'(B"100001010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100100") & \__st0\), (std_logic_vector'(B"100001011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110011") & \__st0\), (std_logic_vector'(B"100001100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010001") & \__st0\), (std_logic_vector'(B"100001101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101110") & \__st0\), (std_logic_vector'(B"100001110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0001111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101001111") & \__st0\), (std_logic_vector'(B"100001111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100111001") & \__st0\), (std_logic_vector'(B"100010000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000110") & \__st0\), (std_logic_vector'(B"100010001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101100") & \__st0\), (std_logic_vector'(B"100010010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101000011") & \__st0\), (std_logic_vector'(B"100010011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010110") & \__st0\), (std_logic_vector'(B"100010100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011000") & \__st0\), (std_logic_vector'(B"100010101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100010") & \__st0\), (std_logic_vector'(B"100010110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0010111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101001001") & \__st0\), (std_logic_vector'(B"100010111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101000") & \__st0\), (std_logic_vector'(B"100011000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101000100") & \__in1\), (std_logic_vector'(B"100011001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101101") & \__st0\), (std_logic_vector'(B"100011010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011101") & \__in1\), (std_logic_vector'(B"100011011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000011") & \__st0\), (std_logic_vector'(B"100011100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100111110") & \__st0\), (std_logic_vector'(B"100011101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011001") & \__st0\), (std_logic_vector'(B"100011110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0011111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011111") & \__st0\), (std_logic_vector'(B"100011111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110001") & \__st0\), (std_logic_vector'(B"100100000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101001010") & \__st0\), (std_logic_vector'(B"100100001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101111101") & \__st0\), (std_logic_vector'(B"100100010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101010") & \__st0\), (std_logic_vector'(B"100100011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010111") & \__st0\), (std_logic_vector'(B"100100100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000001") & \__st0\), (std_logic_vector'(B"100100101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010101") & \__st0\), (std_logic_vector'(B"100100110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0100111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101011") & \__st0\), (std_logic_vector'(B"100100111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011100") & \__st0\), (std_logic_vector'(B"100101000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110111") & \__st0\), (std_logic_vector'(B"100101001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101001") & \__st0\), (std_logic_vector'(B"100101010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100111011") & \__st0\), (std_logic_vector'(B"100101011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000010") & \__st0\), (std_logic_vector'(B"100101100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010010") & \__st0\), (std_logic_vector'(B"100101101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001010") & \__st0\), (std_logic_vector'(B"100101110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0101111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101011") & \__st0\), (std_logic_vector'(B"100101111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011011") & \__st0\), (std_logic_vector'(B"100110000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100011000") & \__st0\), (std_logic_vector'(B"100110001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110110") & \__st0\), (std_logic_vector'(B"100110010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101111100") & \__st0\), (std_logic_vector'(B"100110011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100010") & \__st0\), (std_logic_vector'(B"100110100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101111") & \__st0\), (std_logic_vector'(B"100110101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100001") & \__st0\), (std_logic_vector'(B"100110110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0110111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100011") & \__st0\), (std_logic_vector'(B"100110111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110001") & \__st0\), (std_logic_vector'(B"100111000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101000111") & \__st0\), (std_logic_vector'(B"100111001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"0") & extres & std_logic_vector'(B"1000101") & \__st0\), (std_logic_vector'(B"100111010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100100") & \__st0\), (std_logic_vector'(B"100111011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010000") & \__st0\), (std_logic_vector'(B"100111100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110010") & \__st0\), (std_logic_vector'(B"100111101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001101") & \__st0\), (std_logic_vector'(B"100111110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"0111111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001111") & \__st0\), (std_logic_vector'(B"100111111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010110") & \__st0\), (std_logic_vector'(B"101000000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001100") & \__st0\), (std_logic_vector'(B"101000001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010011") & \__st0\), (std_logic_vector'(B"101000010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100011101") & \__st0\), (std_logic_vector'(B"101000011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"0") & main_test1_out & std_logic_vector'(B"1111010") & \__st0\), (std_logic_vector'(B"101000100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001000") & \__st0\), (std_logic_vector'(B"101000101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101001110") & \__st0\), (std_logic_vector'(B"101000110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1000111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101001011") & \__st0\), (std_logic_vector'(B"101000111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"0") & \extresR1\ & std_logic_vector'(B"0111010") & \__st0\), (std_logic_vector'(B"101001000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001011") & \__st0\), (std_logic_vector'(B"101001001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101000110") & \__st0\), (std_logic_vector'(B"101001010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101111") & \__st0\), (std_logic_vector'(B"101001011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110100") & \__st0\), (std_logic_vector'(B"101001100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010101") & \__st0\), (std_logic_vector'(B"101001101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100011110") & \__st0\), (std_logic_vector'(B"101001110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1001111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101000001") & \__st0\), (std_logic_vector'(B"101001111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000100") & \__st0\), (std_logic_vector'(B"101010000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110010") & \__st0\), (std_logic_vector'(B"101010001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100001") & \__st0\), (std_logic_vector'(B"101010010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100111000") & \__st0\), (std_logic_vector'(B"101010011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001110") & \__st0\), (std_logic_vector'(B"101010100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010011") & \__st0\), (std_logic_vector'(B"101010101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000101") & \__st0\), (std_logic_vector'(B"101010110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1010111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110000") & \__st0\), (std_logic_vector'(B"101010111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100011") & \__st0\), (std_logic_vector'(B"101011000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010111") & \__st0\), (std_logic_vector'(B"101011001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101000000") & \__st0\), (std_logic_vector'(B"101011010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100000") & \__st0\), (std_logic_vector'(B"101011011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011110") & \__st0\), (std_logic_vector'(B"101011100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"0") & \main_test1_outR1\ & std_logic_vector'(B"0011001") & \__st0\), (std_logic_vector'(B"101011101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101000010") & \__st0\), (std_logic_vector'(B"101011110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1011111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010001") & \__st0\), (std_logic_vector'(B"101011111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100111111") & \__st0\), (std_logic_vector'(B"101100000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000111") & \__st0\), (std_logic_vector'(B"101100001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100101") & \__st0\), (std_logic_vector'(B"101100010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101000") & \__st0\), (std_logic_vector'(B"101100011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110101") & \__st0\), (std_logic_vector'(B"101100100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100000000") & \__st0\), (std_logic_vector'(B"101100101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101011010") & \__st0\), (std_logic_vector'(B"101100110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1100111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010010") & \__st0\), (std_logic_vector'(B"101100111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101100") & \__st0\), (std_logic_vector'(B"101101000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101101101") & \__st0\), (std_logic_vector'(B"101101001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110111") & \__st0\), (std_logic_vector'(B"101101010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010000") & \__st0\), (std_logic_vector'(B"101101011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100100110") & \__st0\), (std_logic_vector'(B"101101100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100111101") & \__st0\), (std_logic_vector'(B"101101101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101110110") & \__st0\), (std_logic_vector'(B"101101110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1101111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100101") & \__st0\), (std_logic_vector'(B"101101111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101001100") & \__st0\), (std_logic_vector'(B"101110000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100110101") & \__st0\), (std_logic_vector'(B"101110001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100011010") & \__st0\), (std_logic_vector'(B"101110010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100101110") & \__st0\), (std_logic_vector'(B"101110011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100010100") & \__st0\), (std_logic_vector'(B"101110100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100111") & \__st0\), (std_logic_vector'(B"101110101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100011011") & \__st0\), (std_logic_vector'(B"101110110") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1110111")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101010100") & \__st0\), (std_logic_vector'(B"101110111") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1111000")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100001001") & \__st0\), (std_logic_vector'(B"101111000") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1111001")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100111100") & \__st0\), (std_logic_vector'(B"101111001") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1111010")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"0") & \extresR2\ & std_logic_vector'(B"1001000") & \__st0\), (std_logic_vector'(B"101111010") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1111011")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100110") & \__st0\), (std_logic_vector'(B"101111011") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1111100")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"100011111") & \__st0\), (std_logic_vector'(B"101111100") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1111101")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101100000") & \__st0\), (std_logic_vector'(B"101111101") & \__st0\)), rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"1111110")), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101001101") & \__st0\), (std_logic_vector'(B"101111110") & \__st0\)), rw_cond(rw_eq(\__in0\, std_logic_vector'(B"0")), (std_logic_vector'(B"101111000") & \__st0\), (std_logic_vector'(B"100000000") & \__st0\))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      \__resumption_tag_next\ <= zres(38 downto 32);
      \__st0_next\ <= zres(31 downto 0);
      \__out0\ <= zres(40 downto 40);
      \__out1\ <= zres(39 downto 39);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"0011011");
                  \__st0\ <= std_logic_vector'(B"00000000000000000000000000000000");
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
entity \Main_test1\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (31 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_test1\ is
component test1 is
      port (p0 : in std_logic_vector (31 downto 0);
            p1 : in std_logic_vector (31 downto 0);
            p2 : out std_logic_vector (0 downto 0));
      end component;
      signal extres : std_logic_vector (0 downto 0);
begin
inst : test1 port map (p0 => arg0, p1 => arg1, p2 => extres(0 downto 0));
      res <= extres;
end architecture;