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
      port (\__in0\ : in std_logic_vector (7 downto 0);
            \__out0\ : out std_logic_vector (7 downto 0));
end entity;

architecture rtl of top_level is
      component \Main_$cfrob$Bool$Bool$s32ddedfb\ is
            port (\Zds\ : in std_logic_vector (1 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_$cunfrob$Bool$Bool$s32ddedfb\ is
            port (\Zds\ : in std_logic_vector (1 downto 0);
                  res : out std_logic_vector (1 downto 0));
      end component;
      signal conn : std_logic_vector (1 downto 0);
      signal \main_$cunfrob$bool$bool$s32ddedfb_out\ : std_logic_vector (1 downto 0);
      signal conn_r1 : std_logic_vector (1 downto 0);
      signal \main_$cunfrob$bool$bool$s32ddedfb_out_r1\ : std_logic_vector (1 downto 0);
      signal conn_r2 : std_logic_vector (1 downto 0);
      signal \main_$cunfrob$bool$bool$s32ddedfb_out_r2\ : std_logic_vector (1 downto 0);
      signal conn_r3 : std_logic_vector (1 downto 0);
      signal \main_$cunfrob$bool$bool$s32ddedfb_out_r3\ : std_logic_vector (1 downto 0);
      signal zeta0 : std_logic_vector (7 downto 0);
      signal \main_$cfrob$bool$bool$s32ddedfb_out\ : std_logic_vector (1 downto 0);
      signal \main_$cfrob$bool$bool$s32ddedfb_out_r1\ : std_logic_vector (1 downto 0);
      signal \main_$cfrob$bool$bool$s32ddedfb_out_r2\ : std_logic_vector (1 downto 0);
      signal \main_$cfrob$bool$bool$s32ddedfb_out_r3\ : std_logic_vector (1 downto 0);
      signal zt0 : std_logic_vector (7 downto 0);
      signal zt1 : std_logic_vector (1 downto 0);
      signal x : std_logic_vector (0 downto 0);
      signal x_r1 : std_logic_vector (1 downto 0);
      signal \main_$cunfrob$bool$bool$s32ddedfb_out_r4\ : std_logic_vector (1 downto 0);
      signal \main_$cfrob$bool$bool$s32ddedfb_out_r4\ : std_logic_vector (1 downto 0);
      signal x_r2 : std_logic_vector (0 downto 0);
      signal conn_r4 : std_logic_vector (1 downto 0);
      signal \main_$cfrob$bool$bool$s32ddedfb_out_r5\ : std_logic_vector (1 downto 0);
      signal y : std_logic_vector (0 downto 0);
      signal zt3 : std_logic_vector (0 downto 0);
      signal za : std_logic_vector (7 downto 0);
begin
      -- combinational logic
      conn <= (\__in0\(0 downto 0) & \__in0\(1 downto 1));
      \zcunfrob$bool$bool$s32ddedfb_i\ : \Main_$cunfrob$Bool$Bool$s32ddedfb\ port map (conn, \main_$cunfrob$bool$bool$s32ddedfb_out\);
      conn_r1 <= (\__in0\(2 downto 2) & \__in0\(3 downto 3));
      \zcunfrob$bool$bool$s32ddedfb_i_r1\ : \Main_$cunfrob$Bool$Bool$s32ddedfb\ port map (conn_r1, \main_$cunfrob$bool$bool$s32ddedfb_out_r1\);
      conn_r2 <= (\__in0\(4 downto 4) & \__in0\(5 downto 5));
      \zcunfrob$bool$bool$s32ddedfb_i_r2\ : \Main_$cunfrob$Bool$Bool$s32ddedfb\ port map (conn_r2, \main_$cunfrob$bool$bool$s32ddedfb_out_r2\);
      conn_r3 <= (\__in0\(6 downto 6) & \__in0\(7 downto 7));
      \zcunfrob$bool$bool$s32ddedfb_i_r3\ : \Main_$cunfrob$Bool$Bool$s32ddedfb\ port map (conn_r3, \main_$cunfrob$bool$bool$s32ddedfb_out_r3\);
      zeta0 <= (\main_$cunfrob$bool$bool$s32ddedfb_out\ & \main_$cunfrob$bool$bool$s32ddedfb_out_r1\ & \main_$cunfrob$bool$bool$s32ddedfb_out_r2\ & \main_$cunfrob$bool$bool$s32ddedfb_out_r3\);
      \zcfrob$bool$bool$s32ddedfb_i\ : \Main_$cfrob$Bool$Bool$s32ddedfb\ port map (zeta0(7 downto 6), \main_$cfrob$bool$bool$s32ddedfb_out\);
      \zcfrob$bool$bool$s32ddedfb_i_r1\ : \Main_$cfrob$Bool$Bool$s32ddedfb\ port map (zeta0(5 downto 4), \main_$cfrob$bool$bool$s32ddedfb_out_r1\);
      \zcfrob$bool$bool$s32ddedfb_i_r2\ : \Main_$cfrob$Bool$Bool$s32ddedfb\ port map (zeta0(3 downto 2), \main_$cfrob$bool$bool$s32ddedfb_out_r2\);
      \zcfrob$bool$bool$s32ddedfb_i_r3\ : \Main_$cfrob$Bool$Bool$s32ddedfb\ port map (zeta0(1 downto 0), \main_$cfrob$bool$bool$s32ddedfb_out_r3\);
      zt0 <= (\main_$cfrob$bool$bool$s32ddedfb_out\ & \main_$cfrob$bool$bool$s32ddedfb_out_r1\ & \main_$cfrob$bool$bool$s32ddedfb_out_r2\ & \main_$cfrob$bool$bool$s32ddedfb_out_r3\);
      zt1 <= zt0(7 downto 6);
      x <= zt1(1 downto 1);
      x_r1 <= (\__in0\(0 downto 0) & x);
      \zcunfrob$bool$bool$s32ddedfb_i_r4\ : \Main_$cunfrob$Bool$Bool$s32ddedfb\ port map (x_r1, \main_$cunfrob$bool$bool$s32ddedfb_out_r4\);
      \zcfrob$bool$bool$s32ddedfb_i_r4\ : \Main_$cfrob$Bool$Bool$s32ddedfb\ port map (\main_$cunfrob$bool$bool$s32ddedfb_out_r4\, \main_$cfrob$bool$bool$s32ddedfb_out_r4\);
      x_r2 <= \main_$cfrob$bool$bool$s32ddedfb_out_r4\(1 downto 1);
      conn_r4 <= (\__in0\(1 downto 1) & \__in0\(7 downto 7));
      \zcfrob$bool$bool$s32ddedfb_i_r5\ : \Main_$cfrob$Bool$Bool$s32ddedfb\ port map (conn_r4, \main_$cfrob$bool$bool$s32ddedfb_out_r5\);
      y <= \main_$cfrob$bool$bool$s32ddedfb_out_r5\(0 downto 0);
      zt3 <= rw_cond(x_r2, std_logic_vector'(B"1"), y);
      za <= rw_cond(rw_not(zt3), \__in0\, rw_xor(\__in0\, std_logic_vector'(X"55")));
      -- outputs
      \__out0\ <= za;
end architecture;

-- Main.$cunfrob
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_$cunfrob\ is
      port (b : in std_logic_vector (0 downto 0);
            res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_$cunfrob\ is
begin
      res <= b;
end architecture;

-- Main.$cfrob$Bool$Bool$s32ddedfb
-- specialized from 'Main.$cfrob' at Bool, Bool
-- also: Main.$cfrob$Bool$Bool$sda47482e, Main.$csole$Bool$Bool$sa0d0e4b4
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_$cfrob$Bool$Bool$s32ddedfb\ is
      port (\Zds\ : in std_logic_vector (1 downto 0);
            res : out std_logic_vector (1 downto 0));
end entity;

architecture rtl of \Main_$cfrob$Bool$Bool$s32ddedfb\ is
      signal a : std_logic_vector (0 downto 0);
      signal b : std_logic_vector (0 downto 0);
begin
      a <= \Zds\(1 downto 1);
      b <= \Zds\(0 downto 0);
      res <= (rw_not(a) & rw_not(b));
end architecture;

-- Main.$cunfrob$Bool$Bool$s32ddedfb
-- specialized from 'Main.$cunfrob' at Bool, Bool
-- also: Main.$cunfrob$Bool$Bool$sda47482e
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_$cunfrob$Bool$Bool$s32ddedfb\ is
      port (\Zds\ : in std_logic_vector (1 downto 0);
            res : out std_logic_vector (1 downto 0));
end entity;

architecture rtl of \Main_$cunfrob$Bool$Bool$s32ddedfb\ is
      component \Main_$cunfrob\ is
            port (b : in std_logic_vector (0 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      signal a : std_logic_vector (0 downto 0);
      signal b : std_logic_vector (0 downto 0);
      signal \main_$cunfrob_out\ : std_logic_vector (0 downto 0);
      signal \main_$cunfrob_out_r1\ : std_logic_vector (0 downto 0);
begin
      a <= \Zds\(1 downto 1);
      b <= \Zds\(0 downto 0);
      zcunfrob_i : \Main_$cunfrob\ port map (a, \main_$cunfrob_out\);
      zcunfrob_i_r1 : \Main_$cunfrob\ port map (b, \main_$cunfrob_out_r1\);
      res <= (\main_$cunfrob_out\ & \main_$cunfrob_out_r1\);
end architecture;