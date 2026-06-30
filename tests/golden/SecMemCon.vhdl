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
component \Main_addr__in\ is
      port (arg0 : in std_logic_vector (45 downto 0);
            res : out std_logic_vector (9 downto 0));
      end component;
      component \Main_get__addr__reg\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (214 downto 0));
      end component;
      component \Main_go\ is
      port (arg0 : in std_logic_vector (45 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_partition__in\ is
      port (arg0 : in std_logic_vector (45 downto 0);
            res : out std_logic_vector (1 downto 0));
      end component;
      component \Main_rnw\ is
      port (arg0 : in std_logic_vector (45 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_set__ack__reg1\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \Main_set__addr__reg1\ is
      port (arg0 : in std_logic_vector (9 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \Main_set__data__out__reg\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \Main_while__addr__reg__0\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (411 downto 0));
      end component;
      component \Mainzuzlzazg\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (9 downto 0);
            res : out std_logic_vector (9 downto 0));
      end component;
      component \ZLL_Main_connect\ is
      port (arg0 : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (32 downto 0));
      end component;
      component \ZLL_Main_connect1\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (76 downto 0));
      end component;
      component \ZLL_Main_idle15\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_idle22\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_idle27\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (409 downto 0));
      end component;
      component \ZLL_Main_perform__read\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (411 downto 0));
      end component;
      component \ZLL_Main_perform__write9\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_set__data__out__reg3\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_setloc2\ is
      port (arg0 : in std_logic_vector (41 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal \__resumption_tag\ : std_logic_vector (2 downto 0) := std_logic_vector'(B"000");
      signal \__resumption_tag_next\ : std_logic_vector (2 downto 0);
      signal \__st0\ : std_logic_vector (76 downto 0) := std_logic_vector'(B"00000000001100000000000000000000000000000000000000000000000000000000000000000");
      signal \__st0_next\ : std_logic_vector (76 downto 0);
      signal \__st1\ : std_logic_vector (127 downto 0) := std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
      signal \__st1_next\ : std_logic_vector (127 downto 0);
      signal zi1 : std_logic_vector (204 downto 0);
      signal main_set_data_out_reg_out : std_logic_vector (204 downto 0);
      signal zi2 : std_logic_vector (204 downto 0);
      signal main_get_addr_reg_out : std_logic_vector (214 downto 0);
      signal zi3 : std_logic_vector (214 downto 0);
      signal zi4 : std_logic_vector (9 downto 0);
      signal zi5 : std_logic_vector (204 downto 0);
      signal zi7 : std_logic_vector (31 downto 0);
      signal zi8 : std_logic_vector (236 downto 0);
      signal zi9 : std_logic_vector (31 downto 0);
      signal zi10 : std_logic_vector (204 downto 0);
      signal conn : std_logic_vector (41 downto 0);
      signal zll_main_setloc2_out : std_logic_vector (204 downto 0);
      signal zi11 : std_logic_vector (204 downto 0);
      signal zll_main_perform_write9_out : std_logic_vector (204 downto 0);
      signal zi12 : std_logic_vector (204 downto 0);
      signal zll_main_idle27_out : std_logic_vector (409 downto 0);
      signal zi13 : std_logic_vector (409 downto 0);
      signal zi14 : std_logic_vector (204 downto 0);
      signal zi15 : std_logic_vector (204 downto 0);
      signal zi16 : std_logic_vector (411 downto 0);
      signal zi17 : std_logic_vector (204 downto 0);
      signal zi18 : std_logic_vector (204 downto 0);
      signal zll_main_perform_read_out : std_logic_vector (411 downto 0);
      signal zi19 : std_logic_vector (204 downto 0);
      signal \main_get_addr_reg_outR1\ : std_logic_vector (214 downto 0);
      signal zi20 : std_logic_vector (214 downto 0);
      signal zi21 : std_logic_vector (9 downto 0);
      signal zi22 : std_logic_vector (204 downto 0);
      signal \connR1\ : std_logic_vector (41 downto 0);
      signal \zll_main_setloc2_outR1\ : std_logic_vector (204 downto 0);
      signal zi23 : std_logic_vector (204 downto 0);
      signal \connR2\ : std_logic_vector (9 downto 0);
      signal main_set_addr_reg1_out : std_logic_vector (204 downto 0);
      signal zi24 : std_logic_vector (204 downto 0);
      signal \zll_main_idle27_outR1\ : std_logic_vector (409 downto 0);
      signal zi25 : std_logic_vector (409 downto 0);
      signal zi26 : std_logic_vector (204 downto 0);
      signal zi27 : std_logic_vector (204 downto 0);
      signal zi28 : std_logic_vector (411 downto 0);
      signal zi29 : std_logic_vector (204 downto 0);
      signal zi30 : std_logic_vector (204 downto 0);
      signal main_while_addr_reg_0_out : std_logic_vector (411 downto 0);
      signal zi31 : std_logic_vector (204 downto 0);
      signal \main_get_addr_reg_outR2\ : std_logic_vector (214 downto 0);
      signal zi32 : std_logic_vector (214 downto 0);
      signal zi34 : std_logic_vector (204 downto 0);
      signal zi36 : std_logic_vector (31 downto 0);
      signal zi37 : std_logic_vector (31 downto 0);
      signal zi38 : std_logic_vector (31 downto 0);
      signal zi39 : std_logic_vector (31 downto 0);
      signal zi40 : std_logic_vector (236 downto 0);
      signal zi41 : std_logic_vector (31 downto 0);
      signal zi42 : std_logic_vector (204 downto 0);
      signal zll_main_set_data_out_reg3_out : std_logic_vector (204 downto 0);
      signal zi43 : std_logic_vector (204 downto 0);
      signal \zll_main_perform_write9_outR1\ : std_logic_vector (204 downto 0);
      signal zi44 : std_logic_vector (204 downto 0);
      signal \zll_main_idle27_outR2\ : std_logic_vector (409 downto 0);
      signal zi45 : std_logic_vector (409 downto 0);
      signal zi46 : std_logic_vector (204 downto 0);
      signal zi47 : std_logic_vector (204 downto 0);
      signal zi48 : std_logic_vector (411 downto 0);
      signal zi49 : std_logic_vector (204 downto 0);
      signal zi50 : std_logic_vector (204 downto 0);
      signal \zll_main_perform_read_outR1\ : std_logic_vector (411 downto 0);
      signal zi51 : std_logic_vector (204 downto 0);
      signal main_go_out : std_logic_vector (0 downto 0);
      signal main_rnw_out : std_logic_vector (0 downto 0);
      signal zi52 : std_logic_vector (1 downto 0);
      signal main_partition_in_out : std_logic_vector (1 downto 0);
      signal zi53 : std_logic_vector (1 downto 0);
      signal main_addr_in_out : std_logic_vector (9 downto 0);
      signal zi54 : std_logic_vector (9 downto 0);
      signal mainzuzlzazgzuout : std_logic_vector (9 downto 0);
      signal \main_set_addr_reg1_outR1\ : std_logic_vector (204 downto 0);
      signal zi58 : std_logic_vector (204 downto 0);
      signal zll_main_idle15_out : std_logic_vector (204 downto 0);
      signal zi59 : std_logic_vector (204 downto 0);
      signal \zll_main_idle27_outR3\ : std_logic_vector (409 downto 0);
      signal zi60 : std_logic_vector (409 downto 0);
      signal zi61 : std_logic_vector (204 downto 0);
      signal zi62 : std_logic_vector (204 downto 0);
      signal zi63 : std_logic_vector (411 downto 0);
      signal zi64 : std_logic_vector (204 downto 0);
      signal zll_main_connect1_out : std_logic_vector (76 downto 0);
      signal zll_main_connect_out : std_logic_vector (32 downto 0);
      signal \main_go_outR1\ : std_logic_vector (0 downto 0);
      signal \main_rnw_outR1\ : std_logic_vector (0 downto 0);
      signal zi66 : std_logic_vector (1 downto 0);
      signal \main_partition_in_outR1\ : std_logic_vector (1 downto 0);
      signal zi67 : std_logic_vector (1 downto 0);
      signal \main_addr_in_outR1\ : std_logic_vector (9 downto 0);
      signal zi68 : std_logic_vector (9 downto 0);
      signal zi69 : std_logic_vector (31 downto 0);
      signal zi70 : std_logic_vector (31 downto 0);
      signal \mainzuzlzazgzuoutR1\ : std_logic_vector (9 downto 0);
      signal \main_set_addr_reg1_outR2\ : std_logic_vector (204 downto 0);
      signal zi75 : std_logic_vector (204 downto 0);
      signal zi77 : std_logic_vector (127 downto 0);
      signal zi78 : std_logic_vector (1 downto 0);
      signal zi79 : std_logic_vector (9 downto 0);
      signal zi80 : std_logic_vector (0 downto 0);
      signal zi81 : std_logic_vector (31 downto 0);
      signal zi82 : std_logic_vector (204 downto 0);
      signal \zll_main_idle15_outR1\ : std_logic_vector (204 downto 0);
      signal zi83 : std_logic_vector (204 downto 0);
      signal \zll_main_idle27_outR4\ : std_logic_vector (409 downto 0);
      signal zi84 : std_logic_vector (409 downto 0);
      signal zi85 : std_logic_vector (204 downto 0);
      signal zi86 : std_logic_vector (204 downto 0);
      signal zi87 : std_logic_vector (411 downto 0);
      signal zi88 : std_logic_vector (204 downto 0);
      signal \zll_main_connect1_outR1\ : std_logic_vector (76 downto 0);
      signal \zll_main_connect_outR1\ : std_logic_vector (32 downto 0);
      signal main_set_ack_reg1_out : std_logic_vector (204 downto 0);
      signal zi90 : std_logic_vector (204 downto 0);
      signal zll_main_idle22_out : std_logic_vector (204 downto 0);
      signal zi91 : std_logic_vector (204 downto 0);
      signal \zll_main_idle27_outR5\ : std_logic_vector (409 downto 0);
      signal zi92 : std_logic_vector (409 downto 0);
      signal zi93 : std_logic_vector (204 downto 0);
      signal zi94 : std_logic_vector (204 downto 0);
      signal zi95 : std_logic_vector (411 downto 0);
      signal zi96 : std_logic_vector (204 downto 0);
      signal zi97 : std_logic_vector (204 downto 0);
      signal \zll_main_perform_read_outR2\ : std_logic_vector (411 downto 0);
      signal zi98 : std_logic_vector (204 downto 0);
      signal \main_while_addr_reg_0_outR1\ : std_logic_vector (411 downto 0);
      signal zres : std_logic_vector (411 downto 0);
begin
zi1 <= (\__st0\ & \__st1\);
      inst : \Main_set__data__out__reg\ port map (zi1, main_set_data_out_reg_out);
      zi2 <= main_set_data_out_reg_out;
      \instR1\ : \Main_get__addr__reg\ port map (zi2, main_get_addr_reg_out);
      zi3 <= main_get_addr_reg_out;
      zi4 <= zi3(214 downto 205);
      zi5 <= zi3(204 downto 0);
      zi7 <= zi3(192 downto 161);
      zi8 <= (zi7 & zi5);
      zi9 <= zi8(236 downto 205);
      zi10 <= zi8(204 downto 0);
      conn <= (zi4 & zi9);
      \instR2\ : \ZLL_Main_setloc2\ port map (conn, zi10, zll_main_setloc2_out);
      zi11 <= zll_main_setloc2_out;
      \instR3\ : \ZLL_Main_perform__write9\ port map (zi11, zll_main_perform_write9_out);
      zi12 <= zll_main_perform_write9_out;
      \instR4\ : \ZLL_Main_idle27\ port map (zi12, zll_main_idle27_out);
      zi13 <= zll_main_idle27_out;
      zi14 <= zi13(409 downto 205);
      zi15 <= zi13(204 downto 0);
      zi16 <= (std_logic_vector'(B"01") & zi14 & zi15);
      zi17 <= zi16(409 downto 205);
      zi18 <= zi16(204 downto 0);
      \instR5\ : \ZLL_Main_perform__read\ port map (zi17, zi18, zll_main_perform_read_out);
      zi19 <= (\__st0\ & \__st1\);
      \instR6\ : \Main_get__addr__reg\ port map (zi19, \main_get_addr_reg_outR1\);
      zi20 <= \main_get_addr_reg_outR1\;
      zi21 <= zi20(214 downto 205);
      zi22 <= zi20(204 downto 0);
      \connR1\ <= (zi21 & rw_repl(32, std_logic_vector'(B"0")));
      \instR7\ : \ZLL_Main_setloc2\ port map (\connR1\, zi22, \zll_main_setloc2_outR1\);
      zi23 <= \zll_main_setloc2_outR1\;
      \connR2\ <= rw_cond(rw_and(rw_eq(zi20(214 downto 214), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(213 downto 213), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(212 downto 212), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(211 downto 211), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(210 downto 210), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(209 downto 209), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(208 downto 208), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(207 downto 207), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(206 downto 206), std_logic_vector'(B"1")), rw_eq(zi20(205 downto 205), std_logic_vector'(B"1"))))))))))), std_logic_vector'(B"0000000010"), rw_cond(rw_and(rw_eq(zi20(214 downto 214), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(213 downto 213), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(212 downto 212), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(211 downto 211), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(210 downto 210), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(209 downto 209), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(208 downto 208), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(207 downto 207), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(206 downto 206), std_logic_vector'(B"1")), rw_eq(zi20(205 downto 205), std_logic_vector'(B"0"))))))))))), std_logic_vector'(B"0000000001"), rw_cond(rw_and(rw_eq(zi20(214 downto 214), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(213 downto 213), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(212 downto 212), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(211 downto 211), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(210 downto 210), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(209 downto 209), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(208 downto 208), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(207 downto 207), std_logic_vector'(B"0")), rw_and(rw_eq(zi20(206 downto 206), std_logic_vector'(B"0")), rw_eq(zi20(205 downto 205), std_logic_vector'(B"1"))))))))))), std_logic_vector'(B"0000000000"), std_logic_vector'(B"1111111111"))));
      \instR8\ : \Main_set__addr__reg1\ port map (\connR2\, zi23, main_set_addr_reg1_out);
      zi24 <= main_set_addr_reg1_out;
      \instR9\ : \ZLL_Main_idle27\ port map (zi24, \zll_main_idle27_outR1\);
      zi25 <= \zll_main_idle27_outR1\;
      zi26 <= zi25(409 downto 205);
      zi27 <= zi25(204 downto 0);
      zi28 <= (std_logic_vector'(B"01") & zi26 & zi27);
      zi29 <= zi28(409 downto 205);
      zi30 <= zi28(204 downto 0);
      \instR10\ : \Main_while__addr__reg__0\ port map (zi29, zi30, main_while_addr_reg_0_out);
      zi31 <= (\__st0\ & \__st1\);
      \instR11\ : \Main_get__addr__reg\ port map (zi31, \main_get_addr_reg_outR2\);
      zi32 <= \main_get_addr_reg_outR2\;
      zi34 <= zi32(204 downto 0);
      zi36 <= zi32(127 downto 96);
      zi37 <= zi32(95 downto 64);
      zi38 <= zi32(63 downto 32);
      zi39 <= zi32(31 downto 0);
      zi40 <= (rw_cond(rw_and(rw_eq(zi32(214 downto 214), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(213 downto 213), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(212 downto 212), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(211 downto 211), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(210 downto 210), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(209 downto 209), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(208 downto 208), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(207 downto 207), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(206 downto 206), std_logic_vector'(B"0")), rw_eq(zi32(205 downto 205), std_logic_vector'(B"0"))))))))))), zi36, rw_cond(rw_and(rw_eq(zi32(214 downto 214), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(213 downto 213), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(212 downto 212), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(211 downto 211), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(210 downto 210), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(209 downto 209), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(208 downto 208), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(207 downto 207), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(206 downto 206), std_logic_vector'(B"0")), rw_eq(zi32(205 downto 205), std_logic_vector'(B"1"))))))))))), zi37, rw_cond(rw_and(rw_eq(zi32(214 downto 214), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(213 downto 213), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(212 downto 212), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(211 downto 211), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(210 downto 210), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(209 downto 209), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(208 downto 208), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(207 downto 207), std_logic_vector'(B"0")), rw_and(rw_eq(zi32(206 downto 206), std_logic_vector'(B"1")), rw_eq(zi32(205 downto 205), std_logic_vector'(B"0"))))))))))), zi38, zi39))) & zi34);
      zi41 <= zi40(236 downto 205);
      zi42 <= zi40(204 downto 0);
      \instR12\ : \ZLL_Main_set__data__out__reg3\ port map (zi41, zi42, zll_main_set_data_out_reg3_out);
      zi43 <= zll_main_set_data_out_reg3_out;
      \instR13\ : \ZLL_Main_perform__write9\ port map (zi43, \zll_main_perform_write9_outR1\);
      zi44 <= \zll_main_perform_write9_outR1\;
      \instR14\ : \ZLL_Main_idle27\ port map (zi44, \zll_main_idle27_outR2\);
      zi45 <= \zll_main_idle27_outR2\;
      zi46 <= zi45(409 downto 205);
      zi47 <= zi45(204 downto 0);
      zi48 <= (std_logic_vector'(B"01") & zi46 & zi47);
      zi49 <= zi48(409 downto 205);
      zi50 <= zi48(204 downto 0);
      \instR15\ : \ZLL_Main_perform__read\ port map (zi49, zi50, \zll_main_perform_read_outR1\);
      zi51 <= (\__st0\ & \__st1\);
      \instR16\ : \Main_go\ port map (\__in0\, main_go_out);
      \instR17\ : \Main_rnw\ port map (\__in0\, main_rnw_out);
      zi52 <= (main_go_out & main_rnw_out);
      \instR18\ : \Main_partition__in\ port map (\__in0\, main_partition_in_out);
      zi53 <= main_partition_in_out;
      \instR19\ : \Main_addr__in\ port map (\__in0\, main_addr_in_out);
      zi54 <= main_addr_in_out;
      \instR20\ : \Mainzuzlzazg\ port map (zi53, zi54, mainzuzlzazgzuout);
      \instR21\ : \Main_set__addr__reg1\ port map (mainzuzlzazgzuout, zi51, \main_set_addr_reg1_outR1\);
      zi58 <= \main_set_addr_reg1_outR1\;
      \instR22\ : \ZLL_Main_idle15\ port map (zi53, zi58, zll_main_idle15_out);
      zi59 <= zll_main_idle15_out;
      \instR23\ : \ZLL_Main_idle27\ port map (zi59, \zll_main_idle27_outR3\);
      zi60 <= \zll_main_idle27_outR3\;
      zi61 <= zi60(409 downto 205);
      zi62 <= zi60(204 downto 0);
      zi63 <= (std_logic_vector'(B"01") & zi61 & zi62);
      zi64 <= zi63(409 downto 205);
      \instR24\ : \ZLL_Main_connect1\ port map (zi64, zll_main_connect1_out);
      \instR25\ : \ZLL_Main_connect\ port map (zll_main_connect1_out, zll_main_connect_out);
      \instR26\ : \Main_go\ port map (\__in0\, \main_go_outR1\);
      \instR27\ : \Main_rnw\ port map (\__in0\, \main_rnw_outR1\);
      zi66 <= (\main_go_outR1\ & \main_rnw_outR1\);
      \instR28\ : \Main_partition__in\ port map (\__in0\, \main_partition_in_outR1\);
      zi67 <= \main_partition_in_outR1\;
      \instR29\ : \Main_addr__in\ port map (\__in0\, \main_addr_in_outR1\);
      zi68 <= \main_addr_in_outR1\;
      zi69 <= \__in0\(45 downto 14);
      zi70 <= zi69;
      \instR30\ : \Mainzuzlzazg\ port map (zi67, zi68, \mainzuzlzazgzuoutR1\);
      \instR31\ : \Main_set__addr__reg1\ port map (\mainzuzlzazgzuoutR1\, zi51, \main_set_addr_reg1_outR2\);
      zi75 <= \main_set_addr_reg1_outR2\;
      zi77 <= zi75(127 downto 0);
      zi78 <= zi75(204 downto 203);
      zi79 <= zi75(202 downto 193);
      zi80 <= zi75(160 downto 160);
      zi81 <= zi75(159 downto 128);
      zi82 <= ((zi78 & zi79 & zi70 & zi80 & zi81) & zi77);
      \instR32\ : \ZLL_Main_idle15\ port map (zi67, zi82, \zll_main_idle15_outR1\);
      zi83 <= \zll_main_idle15_outR1\;
      \instR33\ : \ZLL_Main_idle27\ port map (zi83, \zll_main_idle27_outR4\);
      zi84 <= \zll_main_idle27_outR4\;
      zi85 <= zi84(409 downto 205);
      zi86 <= zi84(204 downto 0);
      zi87 <= (std_logic_vector'(B"01") & zi85 & zi86);
      zi88 <= zi87(409 downto 205);
      \instR34\ : \ZLL_Main_connect1\ port map (zi88, \zll_main_connect1_outR1\);
      \instR35\ : \ZLL_Main_connect\ port map (\zll_main_connect1_outR1\, \zll_main_connect_outR1\);
      \instR36\ : \Main_set__ack__reg1\ port map (zi51, main_set_ack_reg1_out);
      zi90 <= main_set_ack_reg1_out;
      \instR37\ : \ZLL_Main_idle22\ port map (zi90, zll_main_idle22_out);
      zi91 <= zll_main_idle22_out;
      \instR38\ : \ZLL_Main_idle27\ port map (zi91, \zll_main_idle27_outR5\);
      zi92 <= \zll_main_idle27_outR5\;
      zi93 <= zi92(409 downto 205);
      zi94 <= zi92(204 downto 0);
      zi95 <= (std_logic_vector'(B"01") & zi93 & zi94);
      zi96 <= zi95(409 downto 205);
      zi97 <= zi95(204 downto 0);
      \instR39\ : \ZLL_Main_perform__read\ port map (zi96, zi97, \zll_main_perform_read_outR2\);
      zi98 <= (\__st0\ & \__st1\);
      \instR40\ : \Main_while__addr__reg__0\ port map (zi98, zi98, \main_while_addr_reg_0_outR1\);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"001")), zll_main_perform_read_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"010")), main_while_addr_reg_0_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"011")), \zll_main_perform_read_outR1\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"100")), rw_cond(rw_and(rw_eq(zi52(1 downto 1), std_logic_vector'(B"1")), rw_eq(zi52(0 downto 0), std_logic_vector'(B"1"))), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & zll_main_connect_out & std_logic_vector'(B"011") & zi64), rw_cond(rw_and(rw_eq(zi66(1 downto 1), std_logic_vector'(B"1")), rw_eq(zi66(0 downto 0), std_logic_vector'(B"0"))), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & \zll_main_connect_outR1\ & std_logic_vector'(B"001") & zi88), \zll_main_perform_read_outR2\)), \main_while_addr_reg_0_outR1\))));
      \__resumption_tag_next\ <= zres(207 downto 205);
      \__st0_next\ <= zres(204 downto 128);
      \__st1_next\ <= zres(127 downto 0);
      \__out0\ <= zres(240 downto 208);
      process (clk, rst)
      begin
      if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"000");
                  \__st0\ <= std_logic_vector'(B"00000000001100000000000000000000000000000000000000000000000000000000000000000");
                  \__st1\ <= std_logic_vector'(B"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
            elsif rising_edge(clk(0)) then
                  \__resumption_tag\ <= \__resumption_tag_next\;
                  \__st0\ <= \__st0_next\;
                  \__st1\ <= \__st1_next\;
            end if;
      end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_data__out__reg\ is
port (arg0 : in std_logic_vector (76 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \Main_data__out__reg\ is
signal zi0 : std_logic_vector (31 downto 0);
begin
zi0 <= arg0(31 downto 0);
      res <= zi0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_idle27\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (409 downto 0));
end entity;

architecture rtl of \ZLL_Main_idle27\ is

begin
res <= (arg0 & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_perform__write9\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_perform__write9\ is
component \ZLL_Main_set__ack__reg3\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal zll_main_set_ack_reg3_out : std_logic_vector (204 downto 0);
begin
inst : \ZLL_Main_set__ack__reg3\ port map (std_logic_vector'(B"1"), arg0, zll_main_set_ack_reg3_out);
      res <= zll_main_set_ack_reg3_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_set__ack__reg3\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_set__ack__reg3\ is
signal zi1 : std_logic_vector (127 downto 0);
      signal zi2 : std_logic_vector (1 downto 0);
      signal zi3 : std_logic_vector (9 downto 0);
      signal zi4 : std_logic_vector (31 downto 0);
      signal zi5 : std_logic_vector (31 downto 0);
begin
zi1 <= arg1(127 downto 0);
      zi2 <= arg1(204 downto 203);
      zi3 <= arg1(202 downto 193);
      zi4 <= arg1(192 downto 161);
      zi5 <= arg1(159 downto 128);
      res <= ((zi2 & zi3 & zi4 & arg0 & zi5) & zi1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_connect1\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (76 downto 0));
end entity;

architecture rtl of \ZLL_Main_connect1\ is
signal x : std_logic_vector (76 downto 0);
begin
x <= arg0(204 downto 128);
      res <= x;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_set__addr__reg1\ is
port (arg0 : in std_logic_vector (9 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \Main_set__addr__reg1\ is
signal zi1 : std_logic_vector (127 downto 0);
      signal zi2 : std_logic_vector (1 downto 0);
      signal zi3 : std_logic_vector (31 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (31 downto 0);
begin
zi1 <= arg1(127 downto 0);
      zi2 <= arg1(204 downto 203);
      zi3 <= arg1(192 downto 161);
      zi4 <= arg1(160 downto 160);
      zi5 <= arg1(159 downto 128);
      res <= ((zi2 & arg0 & zi3 & zi4 & zi5) & zi1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_idle22\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_idle22\ is
component \Main_set__data__out__reg\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal main_set_data_out_reg_out : std_logic_vector (204 downto 0);
begin
inst : \Main_set__data__out__reg\ port map (arg0, main_set_data_out_reg_out);
      res <= main_set_data_out_reg_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_set__data__out__reg3\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_set__data__out__reg3\ is
signal zi1 : std_logic_vector (127 downto 0);
      signal zi2 : std_logic_vector (1 downto 0);
      signal zi3 : std_logic_vector (9 downto 0);
      signal zi4 : std_logic_vector (31 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
begin
zi1 <= arg1(127 downto 0);
      zi2 <= arg1(204 downto 203);
      zi3 <= arg1(202 downto 193);
      zi4 <= arg1(192 downto 161);
      zi5 <= arg1(160 downto 160);
      res <= ((zi2 & zi3 & zi4 & zi5 & arg0) & zi1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_get__addr__reg\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (214 downto 0));
end entity;

architecture rtl of \Main_get__addr__reg\ is
signal zi1 : std_logic_vector (9 downto 0);
begin
zi1 <= arg0(202 downto 193);
      res <= (zi1 & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_idle15\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_idle15\ is
component \Main_set__ack__reg1\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_idle22\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal zi1 : std_logic_vector (127 downto 0);
      signal zi2 : std_logic_vector (9 downto 0);
      signal zi3 : std_logic_vector (31 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (31 downto 0);
      signal zi6 : std_logic_vector (204 downto 0);
      signal main_set_ack_reg1_out : std_logic_vector (204 downto 0);
      signal zi7 : std_logic_vector (204 downto 0);
      signal zll_main_idle22_out : std_logic_vector (204 downto 0);
begin
zi1 <= arg1(127 downto 0);
      zi2 <= arg1(202 downto 193);
      zi3 <= arg1(192 downto 161);
      zi4 <= arg1(160 downto 160);
      zi5 <= arg1(159 downto 128);
      zi6 <= ((arg0 & zi2 & zi3 & zi4 & zi5) & zi1);
      inst : \Main_set__ack__reg1\ port map (zi6, main_set_ack_reg1_out);
      zi7 <= main_set_ack_reg1_out;
      \instR1\ : \ZLL_Main_idle22\ port map (zi7, zll_main_idle22_out);
      res <= zll_main_idle22_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_connect\ is
port (arg0 : in std_logic_vector (76 downto 0);
      res : out std_logic_vector (32 downto 0));
end entity;

architecture rtl of \ZLL_Main_connect\ is
component \Main_ack__reg\ is
      port (arg0 : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_data__out__reg\ is
      port (arg0 : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      signal main_ack_reg_out : std_logic_vector (0 downto 0);
      signal main_data_out_reg_out : std_logic_vector (31 downto 0);
begin
inst : \Main_ack__reg\ port map (arg0, main_ack_reg_out);
      \instR1\ : \Main_data__out__reg\ port map (arg0, main_data_out_reg_out);
      res <= (main_ack_reg_out & main_data_out_reg_out);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_partition__in\ is
port (arg0 : in std_logic_vector (45 downto 0);
      res : out std_logic_vector (1 downto 0));
end entity;

architecture rtl of \Main_partition__in\ is
signal zi0 : std_logic_vector (1 downto 0);
begin
zi0 <= arg0(1 downto 0);
      res <= zi0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_set__data__out__reg\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \Main_set__data__out__reg\ is
component \ZLL_Main_set__data__out__reg3\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal conn : std_logic_vector (31 downto 0);
      signal zll_main_set_data_out_reg3_out : std_logic_vector (204 downto 0);
begin
conn <= rw_repl(32, std_logic_vector'(B"0"));
      inst : \ZLL_Main_set__data__out__reg3\ port map (conn, arg0, zll_main_set_data_out_reg3_out);
      res <= zll_main_set_data_out_reg3_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_rnw\ is
port (arg0 : in std_logic_vector (45 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_rnw\ is
signal zi0 : std_logic_vector (0 downto 0);
begin
zi0 <= arg0(2 downto 2);
      res <= zi0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_while__addr__reg__0\ is
port (arg0 : in std_logic_vector (204 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (411 downto 0));
end entity;

architecture rtl of \Main_while__addr__reg__0\ is
component \Main_ack__reg\ is
      port (arg0 : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (0 downto 0));
      end component;
      component \Main_data__out__reg\ is
      port (arg0 : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      component \Main_get__addr__reg\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (214 downto 0));
      end component;
      component \ZLL_Main_connect\ is
      port (arg0 : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (32 downto 0));
      end component;
      component \ZLL_Main_connect1\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (76 downto 0));
      end component;
      signal main_get_addr_reg_out : std_logic_vector (214 downto 0);
      signal zi0 : std_logic_vector (214 downto 0);
      signal zi1 : std_logic_vector (9 downto 0);
      signal zi2 : std_logic_vector (204 downto 0);
      signal zi3 : std_logic_vector (411 downto 0);
      signal zi5 : std_logic_vector (204 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zll_main_connect1_out : std_logic_vector (76 downto 0);
      signal zll_main_connect_out : std_logic_vector (32 downto 0);
      signal zi7 : std_logic_vector (76 downto 0);
      signal main_ack_reg_out : std_logic_vector (0 downto 0);
      signal main_data_out_reg_out : std_logic_vector (31 downto 0);
begin
inst : \Main_get__addr__reg\ port map (arg1, main_get_addr_reg_out);
      zi0 <= main_get_addr_reg_out;
      zi1 <= zi0(214 downto 205);
      zi2 <= zi0(204 downto 0);
      zi3 <= (rw_repl(197, std_logic_vector'(B"0")) & zi1 & zi2);
      zi5 <= zi3(204 downto 0);
      zi6 <= rw_cond(rw_and(rw_eq(zi3(214 downto 214), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(213 downto 213), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(212 downto 212), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(211 downto 211), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(210 downto 210), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(209 downto 209), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(208 downto 208), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(207 downto 207), std_logic_vector'(B"1")), rw_and(rw_eq(zi3(206 downto 206), std_logic_vector'(B"1")), rw_eq(zi3(205 downto 205), std_logic_vector'(B"1"))))))))))), std_logic_vector'(B"0"), std_logic_vector'(B"1"));
      \instR1\ : \ZLL_Main_connect1\ port map (arg0, zll_main_connect1_out);
      \instR2\ : \ZLL_Main_connect\ port map (zll_main_connect1_out, zll_main_connect_out);
      zi7 <= zi3(204 downto 128);
      \instR3\ : \Main_ack__reg\ port map (zi7, main_ack_reg_out);
      \instR4\ : \Main_data__out__reg\ port map (zi7, main_data_out_reg_out);
      res <= rw_cond(rw_eq(zi6, std_logic_vector'(B"1")), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & zll_main_connect_out & std_logic_vector'(B"010") & arg0), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & main_ack_reg_out & main_data_out_reg_out & std_logic_vector'(B"100") & zi5));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_setloc2\ is
port (arg0 : in std_logic_vector (41 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_setloc2\ is
signal e : std_logic_vector (31 downto 0);
      signal zi0 : std_logic_vector (76 downto 0);
      signal zi5 : std_logic_vector (31 downto 0);
      signal zi6 : std_logic_vector (31 downto 0);
      signal zi7 : std_logic_vector (31 downto 0);
      signal zi11 : std_logic_vector (31 downto 0);
      signal zi12 : std_logic_vector (31 downto 0);
      signal zi13 : std_logic_vector (31 downto 0);
      signal zi17 : std_logic_vector (31 downto 0);
      signal zi18 : std_logic_vector (31 downto 0);
      signal zi19 : std_logic_vector (31 downto 0);
      signal zi23 : std_logic_vector (31 downto 0);
      signal zi24 : std_logic_vector (31 downto 0);
      signal zi25 : std_logic_vector (31 downto 0);
begin
e <= arg0(31 downto 0);
      zi0 <= arg1(204 downto 128);
      zi5 <= arg1(95 downto 64);
      zi6 <= arg1(63 downto 32);
      zi7 <= arg1(31 downto 0);
      zi11 <= arg1(127 downto 96);
      zi12 <= arg1(63 downto 32);
      zi13 <= arg1(31 downto 0);
      zi17 <= arg1(127 downto 96);
      zi18 <= arg1(95 downto 64);
      zi19 <= arg1(31 downto 0);
      zi23 <= arg1(127 downto 96);
      zi24 <= arg1(95 downto 64);
      zi25 <= arg1(63 downto 32);
      res <= (zi0 & rw_cond(rw_and(rw_eq(arg0(41 downto 41), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(40 downto 40), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(39 downto 39), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(38 downto 38), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(37 downto 37), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(36 downto 36), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(35 downto 35), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(34 downto 34), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(33 downto 33), std_logic_vector'(B"0")), rw_eq(arg0(32 downto 32), std_logic_vector'(B"0"))))))))))), (e & zi5 & zi6 & zi7), rw_cond(rw_and(rw_eq(arg0(41 downto 41), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(40 downto 40), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(39 downto 39), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(38 downto 38), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(37 downto 37), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(36 downto 36), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(35 downto 35), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(34 downto 34), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(33 downto 33), std_logic_vector'(B"0")), rw_eq(arg0(32 downto 32), std_logic_vector'(B"1"))))))))))), (zi11 & e & zi12 & zi13), rw_cond(rw_and(rw_eq(arg0(41 downto 41), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(40 downto 40), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(39 downto 39), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(38 downto 38), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(37 downto 37), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(36 downto 36), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(35 downto 35), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(34 downto 34), std_logic_vector'(B"0")), rw_and(rw_eq(arg0(33 downto 33), std_logic_vector'(B"1")), rw_eq(arg0(32 downto 32), std_logic_vector'(B"0"))))))))))), (zi17 & zi18 & e & zi19), (zi23 & zi24 & zi25 & e)))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Mainzuzlzazg\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (9 downto 0);
      res : out std_logic_vector (9 downto 0));
end entity;

architecture rtl of \Mainzuzlzazg\ is
signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal zi10 : std_logic_vector (0 downto 0);
begin
zi1 <= arg0(1 downto 1);
      zi2 <= arg0(0 downto 0);
      zi3 <= arg1(7 downto 7);
      zi4 <= arg1(6 downto 6);
      zi5 <= arg1(5 downto 5);
      zi6 <= arg1(4 downto 4);
      zi7 <= arg1(3 downto 3);
      zi8 <= arg1(2 downto 2);
      zi9 <= arg1(1 downto 1);
      zi10 <= arg1(0 downto 0);
      res <= (zi1 & zi2 & zi3 & zi4 & zi5 & zi6 & zi7 & zi8 & zi9 & zi10);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_set__ack__reg1\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \Main_set__ack__reg1\ is
component \ZLL_Main_set__ack__reg3\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal zll_main_set_ack_reg3_out : std_logic_vector (204 downto 0);
begin
inst : \ZLL_Main_set__ack__reg3\ port map (std_logic_vector'(B"0"), arg0, zll_main_set_ack_reg3_out);
      res <= zll_main_set_ack_reg3_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_perform__read\ is
port (arg0 : in std_logic_vector (204 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (411 downto 0));
end entity;

architecture rtl of \ZLL_Main_perform__read\ is
component \ZLL_Main_connect\ is
      port (arg0 : in std_logic_vector (76 downto 0);
            res : out std_logic_vector (32 downto 0));
      end component;
      component \ZLL_Main_connect1\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (76 downto 0));
      end component;
      signal zll_main_connect1_out : std_logic_vector (76 downto 0);
      signal zll_main_connect_out : std_logic_vector (32 downto 0);
begin
inst : \ZLL_Main_connect1\ port map (arg0, zll_main_connect1_out);
      \instR1\ : \ZLL_Main_connect\ port map (zll_main_connect1_out, zll_main_connect_out);
      res <= ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & zll_main_connect_out & std_logic_vector'(B"100") & arg0);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_ack__reg\ is
port (arg0 : in std_logic_vector (76 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_ack__reg\ is
signal zi0 : std_logic_vector (0 downto 0);
begin
zi0 <= arg0(32 downto 32);
      res <= zi0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_go\ is
port (arg0 : in std_logic_vector (45 downto 0);
      res : out std_logic_vector (0 downto 0));
end entity;

architecture rtl of \Main_go\ is
signal zi0 : std_logic_vector (0 downto 0);
begin
zi0 <= arg0(3 downto 3);
      res <= zi0;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_addr__in\ is
port (arg0 : in std_logic_vector (45 downto 0);
      res : out std_logic_vector (9 downto 0));
end entity;

architecture rtl of \Main_addr__in\ is
signal zi0 : std_logic_vector (9 downto 0);
begin
zi0 <= arg0(13 downto 4);
      res <= zi0;
end architecture;