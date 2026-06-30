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
      \__in0\ : in std_logic_vector (9 downto 0);
      \__out0\ : out std_logic_vector (15 downto 0));
end entity;

architecture rtl of top_level is
signal \__st0\ : std_logic_vector (29 downto 0) := std_logic_vector'(B"000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (29 downto 0);
      signal zi5 : std_logic_vector (3 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (7 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal zi10 : std_logic_vector (4 downto 0);
      signal zi12 : std_logic_vector (3 downto 0);
      signal zi14 : std_logic_vector (8 downto 0);
      signal zi16 : std_logic_vector (8 downto 0);
      signal zi17 : std_logic_vector (44 downto 0);
      signal zi18 : std_logic_vector (14 downto 0);
      signal zi19 : std_logic_vector (29 downto 0);
      signal zi20 : std_logic_vector (14 downto 0);
      signal zi24 : std_logic_vector (44 downto 0);
      signal zi25 : std_logic_vector (29 downto 0);
      signal zi27 : std_logic_vector (4 downto 0);
      signal zi28 : std_logic_vector (3 downto 0);
      signal zi29 : std_logic_vector (4 downto 0);
      signal zi30 : std_logic_vector (7 downto 0);
      signal zi31 : std_logic_vector (8 downto 0);
      signal zi32 : std_logic_vector (75 downto 0);
      signal zi33 : std_logic_vector (45 downto 0);
      signal zi34 : std_logic_vector (29 downto 0);
      signal zi35 : std_logic_vector (76 downto 0);
      signal zi38 : std_logic_vector (15 downto 0);
      signal zi39 : std_logic_vector (29 downto 0);
      signal zres : std_logic_vector (76 downto 0);
begin
zi5 <= \__in0\(3 downto 0);
      zi6 <= \__st0\(29 downto 29);
      zi8 <= \__in0\(7 downto 0);
      zi9 <= \__st0\(29 downto 29);
      zi10 <= \__st0\(28 downto 24);
      zi12 <= \__st0\(27 downto 24);
      zi14 <= \__st0\(23 downto 15);
      zi16 <= \__st0\(23 downto 15);
      zi17 <= (rw_cond(rw_eq(\__in0\(9 downto 8), std_logic_vector'(B"00")), (zi6 & std_logic_vector'(B"1") & zi5 & std_logic_vector'(B"00000") & zi5), rw_cond(rw_eq(\__in0\(9 downto 8), std_logic_vector'(B"01")), (zi9 & zi10 & std_logic_vector'(B"1") & zi8), rw_cond(rw_and(rw_eq(\__in0\(9 downto 8), std_logic_vector'(B"10")), rw_and(rw_eq(\__in0\(0 downto 0), std_logic_vector'(B"1")), rw_eq(\__st0\(28 downto 28), std_logic_vector'(B"1")))), (std_logic_vector'(B"10000000000") & zi12), rw_cond(rw_and(rw_eq(\__in0\(9 downto 8), std_logic_vector'(B"10")), rw_and(rw_eq(\__in0\(0 downto 0), std_logic_vector'(B"1")), rw_eq(\__st0\(28 downto 28), std_logic_vector'(B"0")))), (std_logic_vector'(B"100000") & zi14), (std_logic_vector'(B"000000") & zi16))))) & \__st0\);
      zi18 <= zi17(44 downto 30);
      zi19 <= zi17(29 downto 0);
      zi20 <= zi17(29 downto 15);
      zi24 <= ((zi18 & zi20) & rw_resize(rw_shiftr(zi19, rw_repl(128, std_logic_vector'(B"0"))), 15));
      zi25 <= zi24(44 downto 15);
      zi27 <= zi24(13 downto 9);
      zi28 <= zi24(3 downto 0);
      zi29 <= zi24(13 downto 9);
      zi30 <= zi24(7 downto 0);
      zi31 <= zi24(8 downto 0);
      zi32 <= ((rw_cond(rw_and(rw_eq(zi24(14 downto 14), std_logic_vector'(B"1")), rw_eq(zi24(8 downto 8), std_logic_vector'(B"0"))), (std_logic_vector'(B"11") & zi27 & std_logic_vector'(B"00000") & zi28), rw_cond(rw_and(rw_eq(zi24(14 downto 14), std_logic_vector'(B"1")), rw_eq(zi24(8 downto 8), std_logic_vector'(B"1"))), (std_logic_vector'(B"01") & zi29 & std_logic_vector'(B"1") & zi30), (std_logic_vector'(B"0000000") & zi31))) & zi25) & \__st0\);
      zi33 <= zi32(75 downto 30);
      zi34 <= zi32(29 downto 0);
      zi35 <= (std_logic_vector'(B"0") & zi33 & zi34);
      zi38 <= zi35(75 downto 60);
      zi39 <= zi35(59 downto 30);
      zres <= (std_logic_vector'(B"1000000000000000000000000000000") & zi38 & zi39);
      \__st0_next\ <= zres(29 downto 0);
      \__out0\ <= zres(45 downto 30);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__st0\ <= std_logic_vector'(B"000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;