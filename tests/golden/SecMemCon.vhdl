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
            \__in0\ : in std_logic_vector (45 downto 0);
            \__out0\ : out std_logic_vector (32 downto 0));
end entity;

architecture rtl of top_level is
      component \Main_ack__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_addr__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (9 downto 0));
      end component;
      component \Main_data__out__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_memLookup_$fail1\ is
            port (\Zeta0\ : in std_logic_vector (127 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_memTweak\ is
            port (\Zds\ : in std_logic_vector (9 downto 0);
                  v : in std_logic_vector (31 downto 0);
                  m : in std_logic_vector (127 downto 0);
                  res : out std_logic_vector (127 downto 0));
      end component;
      component \Mainzuzlzazg\ is
            port (\Zds\ : in std_logic_vector (1 downto 0);
                  \Zds1\ : in std_logic_vector (9 downto 0);
                  res : out std_logic_vector (9 downto 0));
      end component;
      component \main___unused2\ is
            port (s0 : in std_logic_vector (204 downto 0);
                  res : out std_logic_vector (240 downto 0));
      end component;
      component \main_get__addr__reg4\ is
            port (x : in std_logic_vector (204 downto 0);
                  s0 : in std_logic_vector (204 downto 0);
                  res : out std_logic_vector (240 downto 0));
      end component;
      component \main_set__ack__reg$sMain__S__Main__Bit\ is
            port (s0 : in std_logic_vector (204 downto 0);
                  res : out std_logic_vector (240 downto 0));
      end component;
      -- state registers
      -- __resumption_tag: 3 bits, init 0x4
      --   states: 0=$x4 1=$x6 2=$x10 3=_unused22 4=$x
      -- __st0: 77 bits, init 0x60000000000000000
      -- __st1: 128 bits, init 0x0
      constant \st_$x4\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"000");
      constant \st_$x6\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"001");
      constant \st_$x10\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"010");
      constant \st__unused22\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"011");
      constant \st_$x\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"100");
      signal \__resumption_tag\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"100");
      signal \__resumption_tag_next\ : std_logic_vector (2 downto 0);
      signal \__st0\ : std_logic_vector (76 downto 0) := std_logic_vector'(B"00000000001100000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (76 downto 0);
      signal \__st1\ : std_logic_vector (127 downto 0) := (others => '0');
      signal \__st1_next\ : std_logic_vector (127 downto 0);
      signal zds2 : std_logic_vector (9 downto 0);
      signal zds5 : std_logic_vector (1 downto 0);
      signal zds3 : std_logic_vector (0 downto 0);
      signal zds4 : std_logic_vector (0 downto 0);
      signal mainzuzlzazgzuout : std_logic_vector (9 downto 0);
      signal partition_reg : std_logic_vector (1 downto 0);
      signal data_reg : std_logic_vector (31 downto 0);
      signal ack_reg : std_logic_vector (0 downto 0);
      signal data_out_reg : std_logic_vector (31 downto 0);
      signal za : std_logic_vector (76 downto 0);
      signal za1 : std_logic_vector (204 downto 0);
      signal zds1 : std_logic_vector (31 downto 0);
      signal m : std_logic_vector (127 downto 0);
      signal partition_reg_r1 : std_logic_vector (1 downto 0);
      signal addr_reg : std_logic_vector (9 downto 0);
      signal ack_reg_r1 : std_logic_vector (0 downto 0);
      signal data_out_reg_r1 : std_logic_vector (31 downto 0);
      signal za_r1 : std_logic_vector (76 downto 0);
      signal za1_r1 : std_logic_vector (204 downto 0);
      signal m_r1 : std_logic_vector (127 downto 0);
      signal addr_reg_r1 : std_logic_vector (9 downto 0);
      signal data_reg_r1 : std_logic_vector (31 downto 0);
      signal ack_reg_r2 : std_logic_vector (0 downto 0);
      signal data_out_reg_r2 : std_logic_vector (31 downto 0);
      signal za_r2 : std_logic_vector (76 downto 0);
      signal za1_r2 : std_logic_vector (204 downto 0);
      signal m_r2 : std_logic_vector (127 downto 0);
      signal partition_reg_r2 : std_logic_vector (1 downto 0);
      signal addr_reg_r2 : std_logic_vector (9 downto 0);
      signal data_reg_r2 : std_logic_vector (31 downto 0);
      signal data_out_reg_r3 : std_logic_vector (31 downto 0);
      signal za_r3 : std_logic_vector (76 downto 0);
      signal za1_r3 : std_logic_vector (204 downto 0);
      signal m_r3 : std_logic_vector (127 downto 0);
      signal partition_reg_r3 : std_logic_vector (1 downto 0);
      signal addr_reg_r3 : std_logic_vector (9 downto 0);
      signal data_reg_r3 : std_logic_vector (31 downto 0);
      signal ack_reg_r3 : std_logic_vector (0 downto 0);
      signal za_r4 : std_logic_vector (76 downto 0);
      signal za1_r4 : std_logic_vector (204 downto 0);
      signal x1 : std_logic_vector (76 downto 0);
      signal main_ack_reg_out : std_logic_vector (0 downto 0);
      signal main_data_out_reg_out : std_logic_vector (31 downto 0);
      signal za2 : std_logic_vector (32 downto 0);
      signal data_reg_r4 : std_logic_vector (31 downto 0);
      signal za_r5 : std_logic_vector (76 downto 0);
      signal za1_r5 : std_logic_vector (204 downto 0);
      signal m_r4 : std_logic_vector (127 downto 0);
      signal partition_reg_r4 : std_logic_vector (1 downto 0);
      signal addr_reg_r4 : std_logic_vector (9 downto 0);
      signal data_reg_r5 : std_logic_vector (31 downto 0);
      signal data_out_reg_r4 : std_logic_vector (31 downto 0);
      signal za_r6 : std_logic_vector (76 downto 0);
      signal za1_r6 : std_logic_vector (204 downto 0);
      signal m_r5 : std_logic_vector (127 downto 0);
      signal partition_reg_r5 : std_logic_vector (1 downto 0);
      signal addr_reg_r5 : std_logic_vector (9 downto 0);
      signal data_reg_r6 : std_logic_vector (31 downto 0);
      signal ack_reg_r4 : std_logic_vector (0 downto 0);
      signal za_r7 : std_logic_vector (76 downto 0);
      signal za1_r7 : std_logic_vector (204 downto 0);
      signal x1_r1 : std_logic_vector (76 downto 0);
      signal main_ack_reg_out_r1 : std_logic_vector (0 downto 0);
      signal main_data_out_reg_out_r1 : std_logic_vector (31 downto 0);
      signal za2_r1 : std_logic_vector (32 downto 0);
      signal addr_reg_r6 : std_logic_vector (9 downto 0);
      signal za_r8 : std_logic_vector (76 downto 0);
      signal za1_r8 : std_logic_vector (204 downto 0);
      signal m_r6 : std_logic_vector (127 downto 0);
      signal partition_reg_r6 : std_logic_vector (1 downto 0);
      signal addr_reg_r7 : std_logic_vector (9 downto 0);
      signal data_reg_r7 : std_logic_vector (31 downto 0);
      signal ack_reg_r5 : std_logic_vector (0 downto 0);
      signal za_r9 : std_logic_vector (76 downto 0);
      signal za1_r9 : std_logic_vector (204 downto 0);
      signal \main__unused2_out\ : std_logic_vector (240 downto 0);
      signal za_r10 : std_logic_vector (76 downto 0);
      signal za1_r10 : std_logic_vector (204 downto 0);
      signal s : std_logic_vector (76 downto 0);
      signal main_addr_reg_out : std_logic_vector (9 downto 0);
      signal zds3_r1 : std_logic_vector (31 downto 0);
      signal m_r7 : std_logic_vector (127 downto 0);
      signal main_memtweak_out : std_logic_vector (127 downto 0);
      signal za1_r11 : std_logic_vector (204 downto 0);
      signal \main_set_ack_reg$smain_s_main_bit_out\ : std_logic_vector (240 downto 0);
      signal main_addr_reg_out_r1 : std_logic_vector (9 downto 0);
      signal zds1_r1 : std_logic_vector (0 downto 0);
      signal zds2_r1 : std_logic_vector (0 downto 0);
      signal zds3_r2 : std_logic_vector (0 downto 0);
      signal zds4_r1 : std_logic_vector (0 downto 0);
      signal zds5_r1 : std_logic_vector (0 downto 0);
      signal zds6 : std_logic_vector (0 downto 0);
      signal zds7 : std_logic_vector (0 downto 0);
      signal zds8 : std_logic_vector (0 downto 0);
      signal zds9 : std_logic_vector (0 downto 0);
      signal zds10 : std_logic_vector (0 downto 0);
      signal zds1_r2 : std_logic_vector (31 downto 0);
      signal zds2_r2 : std_logic_vector (31 downto 0);
      signal zds3_r3 : std_logic_vector (31 downto 0);
      signal zds4_r2 : std_logic_vector (31 downto 0);
      signal \main_memlookup_$fail1_out\ : std_logic_vector (31 downto 0);
      signal za_r11 : std_logic_vector (31 downto 0);
      signal za_r12 : std_logic_vector (76 downto 0);
      signal za1_r12 : std_logic_vector (204 downto 0);
      signal \main_set_ack_reg$smain_s_main_bit_out_r1\ : std_logic_vector (240 downto 0);
      signal conn : std_logic_vector (31 downto 0);
      signal main_memtweak_out_r1 : std_logic_vector (127 downto 0);
      signal za1_r13 : std_logic_vector (204 downto 0);
      signal za_r13 : std_logic_vector (9 downto 0);
      signal m_r8 : std_logic_vector (127 downto 0);
      signal partition_reg_r7 : std_logic_vector (1 downto 0);
      signal data_reg_r8 : std_logic_vector (31 downto 0);
      signal ack_reg_r6 : std_logic_vector (0 downto 0);
      signal data_out_reg_r5 : std_logic_vector (31 downto 0);
      signal za_r14 : std_logic_vector (76 downto 0);
      signal za1_r14 : std_logic_vector (204 downto 0);
      signal main_get_addr_reg4_out : std_logic_vector (240 downto 0);
      signal s0 : std_logic_vector (204 downto 0);
      signal main_get_addr_reg4_out_r1 : std_logic_vector (240 downto 0);
      signal zres : std_logic_vector (240 downto 0);
begin
      -- combinational logic
      zds2 <= \__in0\(13 downto 4);
      zds5 <= \__in0\(1 downto 0);
      zds3 <= \__in0\(3 downto 3);
      zds4 <= \__in0\(2 downto 2);
      zlzazgzui : \Mainzuzlzazg\ port map (zds5, zds2, mainzuzlzazgzuout);
      partition_reg <= \__st0\(76 downto 75);
      data_reg <= \__st0\(64 downto 33);
      ack_reg <= \__st0\(32 downto 32);
      data_out_reg <= \__st0\(31 downto 0);
      za <= (partition_reg & mainzuzlzazgzuout & data_reg & ack_reg & data_out_reg);
      za1 <= (za & \__st1\);
      zds1 <= \__in0\(45 downto 14);
      m <= za1(127 downto 0);
      partition_reg_r1 <= za1(204 downto 203);
      addr_reg <= za1(202 downto 193);
      ack_reg_r1 <= za1(160 downto 160);
      data_out_reg_r1 <= za1(159 downto 128);
      za_r1 <= (partition_reg_r1 & addr_reg & zds1 & ack_reg_r1 & data_out_reg_r1);
      za1_r1 <= (za_r1 & m);
      m_r1 <= za1_r1(127 downto 0);
      addr_reg_r1 <= za1_r1(202 downto 193);
      data_reg_r1 <= za1_r1(192 downto 161);
      ack_reg_r2 <= za1_r1(160 downto 160);
      data_out_reg_r2 <= za1_r1(159 downto 128);
      za_r2 <= (zds5 & addr_reg_r1 & data_reg_r1 & ack_reg_r2 & data_out_reg_r2);
      za1_r2 <= (za_r2 & m_r1);
      m_r2 <= za1_r2(127 downto 0);
      partition_reg_r2 <= za1_r2(204 downto 203);
      addr_reg_r2 <= za1_r2(202 downto 193);
      data_reg_r2 <= za1_r2(192 downto 161);
      data_out_reg_r3 <= za1_r2(159 downto 128);
      za_r3 <= (partition_reg_r2 & addr_reg_r2 & data_reg_r2 & std_logic_vector'(B"0") & data_out_reg_r3);
      za1_r3 <= (za_r3 & m_r2);
      m_r3 <= za1_r3(127 downto 0);
      partition_reg_r3 <= za1_r3(204 downto 203);
      addr_reg_r3 <= za1_r3(202 downto 193);
      data_reg_r3 <= za1_r3(192 downto 161);
      ack_reg_r3 <= za1_r3(160 downto 160);
      za_r4 <= (partition_reg_r3 & addr_reg_r3 & data_reg_r3 & ack_reg_r3 & rw_repl(32, std_logic_vector'(B"0")));
      za1_r4 <= (za_r4 & m_r3);
      x1 <= za1_r4(204 downto 128);
      ack_reg_i : \Main_ack__reg\ port map (x1, main_ack_reg_out);
      data_out_reg_i : \Main_data__out__reg\ port map (x1, main_data_out_reg_out);
      za2 <= (main_ack_reg_out & main_data_out_reg_out);
      data_reg_r4 <= za1(192 downto 161);
      za_r5 <= (zds5 & addr_reg & data_reg_r4 & ack_reg_r1 & data_out_reg_r1);
      za1_r5 <= (za_r5 & m);
      m_r4 <= za1_r5(127 downto 0);
      partition_reg_r4 <= za1_r5(204 downto 203);
      addr_reg_r4 <= za1_r5(202 downto 193);
      data_reg_r5 <= za1_r5(192 downto 161);
      data_out_reg_r4 <= za1_r5(159 downto 128);
      za_r6 <= (partition_reg_r4 & addr_reg_r4 & data_reg_r5 & std_logic_vector'(B"0") & data_out_reg_r4);
      za1_r6 <= (za_r6 & m_r4);
      m_r5 <= za1_r6(127 downto 0);
      partition_reg_r5 <= za1_r6(204 downto 203);
      addr_reg_r5 <= za1_r6(202 downto 193);
      data_reg_r6 <= za1_r6(192 downto 161);
      ack_reg_r4 <= za1_r6(160 downto 160);
      za_r7 <= (partition_reg_r5 & addr_reg_r5 & data_reg_r6 & ack_reg_r4 & rw_repl(32, std_logic_vector'(B"0")));
      za1_r7 <= (za_r7 & m_r5);
      x1_r1 <= za1_r7(204 downto 128);
      ack_reg_i_r1 : \Main_ack__reg\ port map (x1_r1, main_ack_reg_out_r1);
      data_out_reg_i_r1 : \Main_data__out__reg\ port map (x1_r1, main_data_out_reg_out_r1);
      za2_r1 <= (main_ack_reg_out_r1 & main_data_out_reg_out_r1);
      addr_reg_r6 <= \__st0\(74 downto 65);
      za_r8 <= (partition_reg & addr_reg_r6 & data_reg & std_logic_vector'(B"0") & data_out_reg);
      za1_r8 <= (za_r8 & \__st1\);
      m_r6 <= za1_r8(127 downto 0);
      partition_reg_r6 <= za1_r8(204 downto 203);
      addr_reg_r7 <= za1_r8(202 downto 193);
      data_reg_r7 <= za1_r8(192 downto 161);
      ack_reg_r5 <= za1_r8(160 downto 160);
      za_r9 <= (partition_reg_r6 & addr_reg_r7 & data_reg_r7 & ack_reg_r5 & rw_repl(32, std_logic_vector'(B"0")));
      za1_r9 <= (za_r9 & m_r6);
      \_unused2_i\ : \main___unused2\ port map (za1_r9, \main__unused2_out\);
      za_r10 <= (partition_reg & addr_reg_r6 & data_reg & ack_reg & rw_repl(32, std_logic_vector'(B"0")));
      za1_r10 <= (za_r10 & \__st1\);
      s <= za1_r10(204 downto 128);
      addr_reg_i : \Main_addr__reg\ port map (s, main_addr_reg_out);
      zds3_r1 <= za1_r10(192 downto 161);
      m_r7 <= za1_r10(127 downto 0);
      memtweak_i : \Main_memTweak\ port map (main_addr_reg_out, zds3_r1, m_r7, main_memtweak_out);
      za1_r11 <= (s & main_memtweak_out);
      \set_ack_reg$smain_s_main_bit_i\ : \main_set__ack__reg$sMain__S__Main__Bit\ port map (za1_r11, \main_set_ack_reg$smain_s_main_bit_out\);
      addr_reg_i_r1 : \Main_addr__reg\ port map (\__st0\, main_addr_reg_out_r1);
      zds1_r1 <= main_addr_reg_out_r1(9 downto 9);
      zds2_r1 <= main_addr_reg_out_r1(8 downto 8);
      zds3_r2 <= main_addr_reg_out_r1(7 downto 7);
      zds4_r1 <= main_addr_reg_out_r1(6 downto 6);
      zds5_r1 <= main_addr_reg_out_r1(5 downto 5);
      zds6 <= main_addr_reg_out_r1(4 downto 4);
      zds7 <= main_addr_reg_out_r1(3 downto 3);
      zds8 <= main_addr_reg_out_r1(2 downto 2);
      zds9 <= main_addr_reg_out_r1(1 downto 1);
      zds10 <= main_addr_reg_out_r1(0 downto 0);
      zds1_r2 <= \__st1\(127 downto 96);
      zds2_r2 <= \__st1\(95 downto 64);
      zds3_r3 <= \__st1\(63 downto 32);
      zds4_r2 <= \__st1\(31 downto 0);
      zfail1_i : \Main_memLookup_$fail1\ port map (\__st1\, \main_memlookup_$fail1_out\);
      za_r11 <= rw_cond(rw_not(zds1_r1), rw_cond(rw_not(zds2_r1), rw_cond(rw_not(zds3_r2), rw_cond(rw_not(zds4_r1), rw_cond(rw_not(zds5_r1), rw_cond(rw_not(zds6), rw_cond(rw_not(zds7), rw_cond(rw_not(zds8), rw_cond(rw_not(zds9), rw_cond(rw_not(zds10), zds1_r2, zds2_r2), rw_cond(rw_not(zds10), zds3_r3, zds4_r2)), \main_memlookup_$fail1_out\), \main_memlookup_$fail1_out\), \main_memlookup_$fail1_out\), \main_memlookup_$fail1_out\), \main_memlookup_$fail1_out\), \main_memlookup_$fail1_out\), \main_memlookup_$fail1_out\), \main_memlookup_$fail1_out\);
      za_r12 <= (partition_reg & addr_reg_r6 & data_reg & ack_reg & za_r11);
      za1_r12 <= (za_r12 & \__st1\);
      \set_ack_reg$smain_s_main_bit_i_r1\ : \main_set__ack__reg$sMain__S__Main__Bit\ port map (za1_r12, \main_set_ack_reg$smain_s_main_bit_out_r1\);
      conn <= rw_repl(32, std_logic_vector'(B"0"));
      memtweak_i_r1 : \Main_memTweak\ port map (main_addr_reg_out_r1, conn, \__st1\, main_memtweak_out_r1);
      za1_r13 <= (\__st0\ & main_memtweak_out_r1);
      za_r13 <= rw_cond(rw_not(zds1_r1), rw_cond(rw_not(zds2_r1), rw_cond(rw_not(zds3_r2), rw_cond(rw_not(zds4_r1), rw_cond(rw_not(zds5_r1), rw_cond(rw_not(zds6), rw_cond(rw_not(zds7), rw_cond(rw_not(zds8), rw_cond(rw_not(zds9), rw_cond(zds10, std_logic_vector'(B"0000000000"), std_logic_vector'(B"1111111111")), rw_cond(rw_not(zds10), std_logic_vector'(B"0000000001"), std_logic_vector'(B"0000000010"))), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111"));
      m_r8 <= za1_r13(127 downto 0);
      partition_reg_r7 <= za1_r13(204 downto 203);
      data_reg_r8 <= za1_r13(192 downto 161);
      ack_reg_r6 <= za1_r13(160 downto 160);
      data_out_reg_r5 <= za1_r13(159 downto 128);
      za_r14 <= (partition_reg_r7 & za_r13 & data_reg_r8 & ack_reg_r6 & data_out_reg_r5);
      za1_r14 <= (za_r14 & m_r8);
      get_addr_reg4_i : \main_get__addr__reg4\ port map (za1_r14, za1_r14, main_get_addr_reg4_out);
      s0 <= (\__st0\ & \__st1\);
      get_addr_reg4_i_r1 : \main_get__addr__reg4\ port map (s0, s0, main_get_addr_reg4_out_r1);
      with \__resumption_tag\ select zres <=
            rw_cond(zds3, rw_cond(rw_not(zds4), (za2 & std_logic_vector'(B"001") & za1_r4), (za2_r1 & std_logic_vector'(B"010") & za1_r7)), \main__unused2_out\) when "000",
            \main_set_ack_reg$smain_s_main_bit_out\ when "001",
            \main_set_ack_reg$smain_s_main_bit_out_r1\ when "010",
            main_get_addr_reg4_out when "011",
            main_get_addr_reg4_out_r1 when others;
      \__resumption_tag_next\ <= zres(207 downto 205);
      \__st0_next\ <= zres(204 downto 128);
      \__st1_next\ <= zres(127 downto 0);
      -- outputs
      \__out0\ <= zres(240 downto 208);
      -- state register update
      process (clk, rst)
      begin
            if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"100");
                  \__st0\ <= std_logic_vector'(B"00000000001100000000000000000000000000000000000000000000000000000000000000000");
                  \__st1\ <= std_logic_vector'(X"00000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__resumption_tag\ <= \__resumption_tag_next\;
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
            end if;
      end process;
end architecture;

-- main._unused2
-- block '$L._unused2' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main___unused2\ is
      port (s0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (240 downto 0));
end entity;

architecture rtl of \main___unused2\ is
      component \Main_ack__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_data__out__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      signal x1 : std_logic_vector (76 downto 0);
      signal main_ack_reg_out : std_logic_vector (0 downto 0);
      signal main_data_out_reg_out : std_logic_vector (31 downto 0);
      signal za2 : std_logic_vector (32 downto 0);
begin
      x1 <= s0(204 downto 128);
      ack_reg_i : \Main_ack__reg\ port map (x1, main_ack_reg_out);
      data_out_reg_i : \Main_data__out__reg\ port map (x1, main_data_out_reg_out);
      za2 <= (main_ack_reg_out & main_data_out_reg_out);
      res <= (za2 & std_logic_vector'(B"000") & s0);
end architecture;

-- main.set_ack_reg$sMain_S_Main_Bit
-- block '$L.Main.set_ack_reg$sMain_S_Main_Bit' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_set__ack__reg$sMain__S__Main__Bit\ is
      port (s0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (240 downto 0));
end entity;

architecture rtl of \main_set__ack__reg$sMain__S__Main__Bit\ is
      component \main___unused2\ is
            port (s0 : in std_logic_vector (204 downto 0);
                  res : out std_logic_vector (240 downto 0));
      end component;
      signal m : std_logic_vector (127 downto 0);
      signal partition_reg : std_logic_vector (1 downto 0);
      signal addr_reg : std_logic_vector (9 downto 0);
      signal data_reg : std_logic_vector (31 downto 0);
      signal data_out_reg : std_logic_vector (31 downto 0);
      signal za : std_logic_vector (76 downto 0);
      signal za1 : std_logic_vector (204 downto 0);
      signal \main__unused2_out\ : std_logic_vector (240 downto 0);
begin
      m <= s0(127 downto 0);
      partition_reg <= s0(204 downto 203);
      addr_reg <= s0(202 downto 193);
      data_reg <= s0(192 downto 161);
      data_out_reg <= s0(159 downto 128);
      za <= (partition_reg & addr_reg & data_reg & std_logic_vector'(B"1") & data_out_reg);
      za1 <= (za & m);
      \_unused2_i\ : \main___unused2\ port map (za1, \main__unused2_out\);
      res <= \main__unused2_out\;
end architecture;

-- main.get_addr_reg4
-- block '$L.Main.get_addr_reg4' of process main
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \main_get__addr__reg4\ is
      port (x : in std_logic_vector (204 downto 0);
            s0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (240 downto 0));
end entity;

architecture rtl of \main_get__addr__reg4\ is
      component \Main_ack__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_addr__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (9 downto 0));
      end component;
      component \Main_data__out__reg\ is
            port (\Zds\ : in std_logic_vector (76 downto 0);
                  res : out std_logic_vector (31 downto 0));
      end component;
      signal s : std_logic_vector (76 downto 0);
      signal main_addr_reg_out : std_logic_vector (9 downto 0);
      signal zds1 : std_logic_vector (0 downto 0);
      signal zds2 : std_logic_vector (0 downto 0);
      signal zds3 : std_logic_vector (0 downto 0);
      signal zds4 : std_logic_vector (0 downto 0);
      signal zds5 : std_logic_vector (0 downto 0);
      signal zds6 : std_logic_vector (0 downto 0);
      signal zds7 : std_logic_vector (0 downto 0);
      signal zds8 : std_logic_vector (0 downto 0);
      signal zds9 : std_logic_vector (0 downto 0);
      signal zds10 : std_logic_vector (0 downto 0);
      signal za : std_logic_vector (0 downto 0);
      signal main_ack_reg_out : std_logic_vector (0 downto 0);
      signal main_data_out_reg_out : std_logic_vector (31 downto 0);
      signal za2 : std_logic_vector (32 downto 0);
      signal x1 : std_logic_vector (76 downto 0);
      signal main_ack_reg_out_r1 : std_logic_vector (0 downto 0);
      signal main_data_out_reg_out_r1 : std_logic_vector (31 downto 0);
      signal za2_r1 : std_logic_vector (32 downto 0);
begin
      s <= s0(204 downto 128);
      addr_reg_i : \Main_addr__reg\ port map (s, main_addr_reg_out);
      zds1 <= main_addr_reg_out(9 downto 9);
      zds2 <= main_addr_reg_out(8 downto 8);
      zds3 <= main_addr_reg_out(7 downto 7);
      zds4 <= main_addr_reg_out(6 downto 6);
      zds5 <= main_addr_reg_out(5 downto 5);
      zds6 <= main_addr_reg_out(4 downto 4);
      zds7 <= main_addr_reg_out(3 downto 3);
      zds8 <= main_addr_reg_out(2 downto 2);
      zds9 <= main_addr_reg_out(1 downto 1);
      zds10 <= main_addr_reg_out(0 downto 0);
      za <= rw_cond(zds1, rw_cond(zds2, rw_cond(zds3, rw_cond(zds4, rw_cond(zds5, rw_cond(zds6, rw_cond(zds7, rw_cond(zds8, rw_cond(zds9, rw_not(zds10), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1"));
      ack_reg_i : \Main_ack__reg\ port map (s, main_ack_reg_out);
      data_out_reg_i : \Main_data__out__reg\ port map (s, main_data_out_reg_out);
      za2 <= (main_ack_reg_out & main_data_out_reg_out);
      x1 <= x(204 downto 128);
      ack_reg_i_r1 : \Main_ack__reg\ port map (x1, main_ack_reg_out_r1);
      data_out_reg_i_r1 : \Main_data__out__reg\ port map (x1, main_data_out_reg_out_r1);
      za2_r1 <= (main_ack_reg_out_r1 & main_data_out_reg_out_r1);
      res <= rw_cond(rw_not(za), (za2 & std_logic_vector'(B"000") & s0), (za2_r1 & std_logic_vector'(B"011") & x));
end architecture;

-- Main.data_out_reg
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_data__out__reg\ is
      port (\Zds\ : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_data__out__reg\ is
      signal zds5 : std_logic_vector (31 downto 0);
begin
      zds5 <= \Zds\(31 downto 0);
      res <= zds5;
end architecture;

-- Main.ack_reg
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_ack__reg\ is
      port (\Zds\ : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_ack__reg\ is
      signal zds4 : std_logic_vector (0 downto 0);
begin
      zds4 <= \Zds\(32 downto 32);
      res <= zds4;
end architecture;

-- Main.addr_reg
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_addr__reg\ is
      port (\Zds\ : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (9 downto 0));
end entity;

architecture rtl of \Main_addr__reg\ is
      signal zds2 : std_logic_vector (9 downto 0);
begin
      zds2 <= \Zds\(74 downto 65);
      res <= zds2;
end architecture;

-- Main.memLookup.$fail1
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_memLookup_$fail1\ is
      port (\Zeta0\ : in std_logic_vector (127 downto 0);
            res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_memLookup_$fail1\ is
begin
      res <= rw_repl(32, std_logic_vector'(B"0"));
end architecture;

-- Main.memTweak
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_memTweak\ is
      port (\Zds\ : in std_logic_vector (9 downto 0);
            v : in std_logic_vector (31 downto 0);
            m : in std_logic_vector (127 downto 0);
            res : out std_logic_vector (127 downto 0));
end entity;

architecture rtl of \Main_memTweak\ is
      signal zds1 : std_logic_vector (0 downto 0);
      signal zds2 : std_logic_vector (0 downto 0);
      signal zds3 : std_logic_vector (0 downto 0);
      signal zds4 : std_logic_vector (0 downto 0);
      signal zds5 : std_logic_vector (0 downto 0);
      signal zds6 : std_logic_vector (0 downto 0);
      signal zds7 : std_logic_vector (0 downto 0);
      signal zds8 : std_logic_vector (0 downto 0);
      signal zds9 : std_logic_vector (0 downto 0);
      signal zds10 : std_logic_vector (0 downto 0);
      signal mem1 : std_logic_vector (31 downto 0);
      signal mem2 : std_logic_vector (31 downto 0);
      signal mem3 : std_logic_vector (31 downto 0);
      signal mem0 : std_logic_vector (31 downto 0);
begin
      zds1 <= \Zds\(9 downto 9);
      zds2 <= \Zds\(8 downto 8);
      zds3 <= \Zds\(7 downto 7);
      zds4 <= \Zds\(6 downto 6);
      zds5 <= \Zds\(5 downto 5);
      zds6 <= \Zds\(4 downto 4);
      zds7 <= \Zds\(3 downto 3);
      zds8 <= \Zds\(2 downto 2);
      zds9 <= \Zds\(1 downto 1);
      zds10 <= \Zds\(0 downto 0);
      mem1 <= m(95 downto 64);
      mem2 <= m(63 downto 32);
      mem3 <= m(31 downto 0);
      mem0 <= m(127 downto 96);
      res <= rw_cond(rw_not(zds1), rw_cond(rw_not(zds2), rw_cond(rw_not(zds3), rw_cond(rw_not(zds4), rw_cond(rw_not(zds5), rw_cond(rw_not(zds6), rw_cond(rw_not(zds7), rw_cond(rw_not(zds8), rw_cond(rw_not(zds9), rw_cond(rw_not(zds10), (v & mem1 & mem2 & mem3), (mem0 & v & mem2 & mem3)), rw_cond(rw_not(zds10), (mem0 & mem1 & v & mem3), (mem0 & mem1 & mem2 & v))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0")));
end architecture;

-- Main.<&>
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Mainzuzlzazg\ is
      port (\Zds\ : in std_logic_vector (1 downto 0);
            \Zds1\ : in std_logic_vector (9 downto 0);
            res : out std_logic_vector (9 downto 0));
end entity;

architecture rtl of \Mainzuzlzazg\ is
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
begin
      b0 <= \Zds\(1 downto 1);
      b1 <= \Zds\(0 downto 0);
      b2 <= \Zds1\(7 downto 7);
      b3 <= \Zds1\(6 downto 6);
      b4 <= \Zds1\(5 downto 5);
      b5 <= \Zds1\(4 downto 4);
      b6 <= \Zds1\(3 downto 3);
      b7 <= \Zds1\(2 downto 2);
      b8 <= \Zds1\(1 downto 1);
      b9 <= \Zds1\(0 downto 0);
      res <= (b0 & b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 & b9);
end architecture;