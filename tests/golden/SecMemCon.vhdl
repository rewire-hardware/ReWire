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
component \Main_get__addr__reg\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (214 downto 0));
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
      component \ZLL_Main_idle1\ is
      port (arg0 : in std_logic_vector (1 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_idle17\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (411 downto 0));
      end component;
      component \ZLL_Main_idle6\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_idle8\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (409 downto 0));
      end component;
      component \ZLL_Main_memLookup2\ is
      port (arg0 : in std_logic_vector (127 downto 0);
            res : out std_logic_vector (31 downto 0));
      end component;
      component \ZLL_Main_perform__read3\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_set__data__out__reg2\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_setloc1\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (9 downto 0);
            arg2 : in std_logic_vector (204 downto 0);
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
      signal zll_main_setloc1_out : std_logic_vector (204 downto 0);
      signal zi11 : std_logic_vector (204 downto 0);
      signal zll_main_perform_read3_out : std_logic_vector (204 downto 0);
      signal zi12 : std_logic_vector (204 downto 0);
      signal zll_main_idle8_out : std_logic_vector (409 downto 0);
      signal zi13 : std_logic_vector (409 downto 0);
      signal zi14 : std_logic_vector (204 downto 0);
      signal zi15 : std_logic_vector (204 downto 0);
      signal zi16 : std_logic_vector (411 downto 0);
      signal zi17 : std_logic_vector (204 downto 0);
      signal zi18 : std_logic_vector (204 downto 0);
      signal zll_main_idle17_out : std_logic_vector (411 downto 0);
      signal zi19 : std_logic_vector (204 downto 0);
      signal \main_get_addr_reg_outR1\ : std_logic_vector (214 downto 0);
      signal zi20 : std_logic_vector (214 downto 0);
      signal zi22 : std_logic_vector (204 downto 0);
      signal zi23 : std_logic_vector (127 downto 0);
      signal zi24 : std_logic_vector (0 downto 0);
      signal zi25 : std_logic_vector (0 downto 0);
      signal zi26 : std_logic_vector (0 downto 0);
      signal zi27 : std_logic_vector (0 downto 0);
      signal zi28 : std_logic_vector (0 downto 0);
      signal zi29 : std_logic_vector (0 downto 0);
      signal zi30 : std_logic_vector (0 downto 0);
      signal zi31 : std_logic_vector (0 downto 0);
      signal zi32 : std_logic_vector (0 downto 0);
      signal zi33 : std_logic_vector (0 downto 0);
      signal zi34 : std_logic_vector (31 downto 0);
      signal zi35 : std_logic_vector (31 downto 0);
      signal zi36 : std_logic_vector (31 downto 0);
      signal zi37 : std_logic_vector (31 downto 0);
      signal zll_main_memlookup2_out : std_logic_vector (31 downto 0);
      signal \zll_main_memlookup2_outR1\ : std_logic_vector (31 downto 0);
      signal \zll_main_memlookup2_outR2\ : std_logic_vector (31 downto 0);
      signal \zll_main_memlookup2_outR3\ : std_logic_vector (31 downto 0);
      signal \zll_main_memlookup2_outR4\ : std_logic_vector (31 downto 0);
      signal \zll_main_memlookup2_outR5\ : std_logic_vector (31 downto 0);
      signal \zll_main_memlookup2_outR6\ : std_logic_vector (31 downto 0);
      signal \zll_main_memlookup2_outR7\ : std_logic_vector (31 downto 0);
      signal zi38 : std_logic_vector (236 downto 0);
      signal zi39 : std_logic_vector (31 downto 0);
      signal zi40 : std_logic_vector (204 downto 0);
      signal zll_main_set_data_out_reg2_out : std_logic_vector (204 downto 0);
      signal zi41 : std_logic_vector (204 downto 0);
      signal \zll_main_perform_read3_outR1\ : std_logic_vector (204 downto 0);
      signal zi42 : std_logic_vector (204 downto 0);
      signal \zll_main_idle8_outR1\ : std_logic_vector (409 downto 0);
      signal zi43 : std_logic_vector (409 downto 0);
      signal zi44 : std_logic_vector (204 downto 0);
      signal zi45 : std_logic_vector (204 downto 0);
      signal zi46 : std_logic_vector (411 downto 0);
      signal zi47 : std_logic_vector (204 downto 0);
      signal zi48 : std_logic_vector (204 downto 0);
      signal \zll_main_idle17_outR1\ : std_logic_vector (411 downto 0);
      signal zi49 : std_logic_vector (204 downto 0);
      signal \main_get_addr_reg_outR2\ : std_logic_vector (214 downto 0);
      signal zi50 : std_logic_vector (214 downto 0);
      signal zi51 : std_logic_vector (9 downto 0);
      signal zi52 : std_logic_vector (204 downto 0);
      signal conn : std_logic_vector (31 downto 0);
      signal \zll_main_setloc1_outR1\ : std_logic_vector (204 downto 0);
      signal zi53 : std_logic_vector (204 downto 0);
      signal zi54 : std_logic_vector (0 downto 0);
      signal zi55 : std_logic_vector (0 downto 0);
      signal zi56 : std_logic_vector (0 downto 0);
      signal zi57 : std_logic_vector (0 downto 0);
      signal zi58 : std_logic_vector (0 downto 0);
      signal zi59 : std_logic_vector (0 downto 0);
      signal zi60 : std_logic_vector (0 downto 0);
      signal zi61 : std_logic_vector (0 downto 0);
      signal zi62 : std_logic_vector (0 downto 0);
      signal zi63 : std_logic_vector (0 downto 0);
      signal \connR1\ : std_logic_vector (9 downto 0);
      signal main_set_addr_reg1_out : std_logic_vector (204 downto 0);
      signal zi64 : std_logic_vector (204 downto 0);
      signal \zll_main_idle8_outR2\ : std_logic_vector (409 downto 0);
      signal zi65 : std_logic_vector (409 downto 0);
      signal zi66 : std_logic_vector (204 downto 0);
      signal zi67 : std_logic_vector (204 downto 0);
      signal zi68 : std_logic_vector (411 downto 0);
      signal zi69 : std_logic_vector (204 downto 0);
      signal zi70 : std_logic_vector (204 downto 0);
      signal main_while_addr_reg_0_out : std_logic_vector (411 downto 0);
      signal zi71 : std_logic_vector (204 downto 0);
      signal zi72 : std_logic_vector (0 downto 0);
      signal zi73 : std_logic_vector (0 downto 0);
      signal zi74 : std_logic_vector (0 downto 0);
      signal zi75 : std_logic_vector (0 downto 0);
      signal zi76 : std_logic_vector (1 downto 0);
      signal zi77 : std_logic_vector (1 downto 0);
      signal zi78 : std_logic_vector (9 downto 0);
      signal zi79 : std_logic_vector (9 downto 0);
      signal mainzuzlzazgzuout : std_logic_vector (9 downto 0);
      signal \main_set_addr_reg1_outR1\ : std_logic_vector (204 downto 0);
      signal zi80 : std_logic_vector (204 downto 0);
      signal zi81 : std_logic_vector (31 downto 0);
      signal zi82 : std_logic_vector (31 downto 0);
      signal zi83 : std_logic_vector (127 downto 0);
      signal zi84 : std_logic_vector (1 downto 0);
      signal zi85 : std_logic_vector (9 downto 0);
      signal zi86 : std_logic_vector (0 downto 0);
      signal zi87 : std_logic_vector (31 downto 0);
      signal zi88 : std_logic_vector (204 downto 0);
      signal zll_main_idle1_out : std_logic_vector (204 downto 0);
      signal zi89 : std_logic_vector (204 downto 0);
      signal \zll_main_idle8_outR3\ : std_logic_vector (409 downto 0);
      signal zi90 : std_logic_vector (409 downto 0);
      signal zi91 : std_logic_vector (204 downto 0);
      signal zi92 : std_logic_vector (204 downto 0);
      signal zi93 : std_logic_vector (411 downto 0);
      signal zi94 : std_logic_vector (204 downto 0);
      signal zll_main_connect1_out : std_logic_vector (76 downto 0);
      signal zll_main_connect_out : std_logic_vector (32 downto 0);
      signal \mainzuzlzazgzuoutR1\ : std_logic_vector (9 downto 0);
      signal \main_set_addr_reg1_outR2\ : std_logic_vector (204 downto 0);
      signal zi96 : std_logic_vector (204 downto 0);
      signal \zll_main_idle1_outR1\ : std_logic_vector (204 downto 0);
      signal zi97 : std_logic_vector (204 downto 0);
      signal \zll_main_idle8_outR4\ : std_logic_vector (409 downto 0);
      signal zi98 : std_logic_vector (409 downto 0);
      signal zi99 : std_logic_vector (204 downto 0);
      signal zi100 : std_logic_vector (204 downto 0);
      signal zi101 : std_logic_vector (411 downto 0);
      signal zi102 : std_logic_vector (204 downto 0);
      signal \zll_main_connect1_outR1\ : std_logic_vector (76 downto 0);
      signal \zll_main_connect_outR1\ : std_logic_vector (32 downto 0);
      signal main_set_ack_reg1_out : std_logic_vector (204 downto 0);
      signal zi104 : std_logic_vector (204 downto 0);
      signal zll_main_idle6_out : std_logic_vector (204 downto 0);
      signal zi105 : std_logic_vector (204 downto 0);
      signal \zll_main_idle8_outR5\ : std_logic_vector (409 downto 0);
      signal zi106 : std_logic_vector (409 downto 0);
      signal zi107 : std_logic_vector (204 downto 0);
      signal zi108 : std_logic_vector (204 downto 0);
      signal zi109 : std_logic_vector (411 downto 0);
      signal zi110 : std_logic_vector (204 downto 0);
      signal zi111 : std_logic_vector (204 downto 0);
      signal \zll_main_idle17_outR2\ : std_logic_vector (411 downto 0);
      signal zi112 : std_logic_vector (204 downto 0);
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
      \instR2\ : \ZLL_Main_setloc1\ port map (zi9, zi4, zi10, zll_main_setloc1_out);
      zi11 <= zll_main_setloc1_out;
      \instR3\ : \ZLL_Main_perform__read3\ port map (zi11, zll_main_perform_read3_out);
      zi12 <= zll_main_perform_read3_out;
      \instR4\ : \ZLL_Main_idle8\ port map (zi12, zll_main_idle8_out);
      zi13 <= zll_main_idle8_out;
      zi14 <= zi13(409 downto 205);
      zi15 <= zi13(204 downto 0);
      zi16 <= (std_logic_vector'(B"01") & zi14 & zi15);
      zi17 <= zi16(409 downto 205);
      zi18 <= zi16(204 downto 0);
      \instR5\ : \ZLL_Main_idle17\ port map (zi17, zi18, zll_main_idle17_out);
      zi19 <= (\__st0\ & \__st1\);
      \instR6\ : \Main_get__addr__reg\ port map (zi19, \main_get_addr_reg_outR1\);
      zi20 <= \main_get_addr_reg_outR1\;
      zi22 <= zi20(204 downto 0);
      zi23 <= zi20(127 downto 0);
      zi24 <= zi20(214 downto 214);
      zi25 <= zi20(213 downto 213);
      zi26 <= zi20(212 downto 212);
      zi27 <= zi20(211 downto 211);
      zi28 <= zi20(210 downto 210);
      zi29 <= zi20(209 downto 209);
      zi30 <= zi20(208 downto 208);
      zi31 <= zi20(207 downto 207);
      zi32 <= zi20(206 downto 206);
      zi33 <= zi20(205 downto 205);
      zi34 <= zi20(127 downto 96);
      zi35 <= zi20(95 downto 64);
      zi36 <= zi20(63 downto 32);
      zi37 <= zi20(31 downto 0);
      \instR7\ : \ZLL_Main_memLookup2\ port map (zi23, zll_main_memlookup2_out);
      \instR8\ : \ZLL_Main_memLookup2\ port map (zi23, \zll_main_memlookup2_outR1\);
      \instR9\ : \ZLL_Main_memLookup2\ port map (zi23, \zll_main_memlookup2_outR2\);
      \instR10\ : \ZLL_Main_memLookup2\ port map (zi23, \zll_main_memlookup2_outR3\);
      \instR11\ : \ZLL_Main_memLookup2\ port map (zi23, \zll_main_memlookup2_outR4\);
      \instR12\ : \ZLL_Main_memLookup2\ port map (zi23, \zll_main_memlookup2_outR5\);
      \instR13\ : \ZLL_Main_memLookup2\ port map (zi23, \zll_main_memlookup2_outR6\);
      \instR14\ : \ZLL_Main_memLookup2\ port map (zi23, \zll_main_memlookup2_outR7\);
      zi38 <= (rw_cond(rw_eq(zi24, std_logic_vector'(B"0")), rw_cond(rw_eq(zi25, std_logic_vector'(B"0")), rw_cond(rw_eq(zi26, std_logic_vector'(B"0")), rw_cond(rw_eq(zi27, std_logic_vector'(B"0")), rw_cond(rw_eq(zi28, std_logic_vector'(B"0")), rw_cond(rw_eq(zi29, std_logic_vector'(B"0")), rw_cond(rw_eq(zi30, std_logic_vector'(B"0")), rw_cond(rw_eq(zi31, std_logic_vector'(B"0")), rw_cond(rw_eq(zi32, std_logic_vector'(B"0")), rw_cond(rw_eq(zi33, std_logic_vector'(B"0")), zi34, zi35), rw_cond(rw_eq(zi33, std_logic_vector'(B"0")), zi36, zi37)), zll_main_memlookup2_out), \zll_main_memlookup2_outR1\), \zll_main_memlookup2_outR2\), \zll_main_memlookup2_outR3\), \zll_main_memlookup2_outR4\), \zll_main_memlookup2_outR5\), \zll_main_memlookup2_outR6\), \zll_main_memlookup2_outR7\) & zi22);
      zi39 <= zi38(236 downto 205);
      zi40 <= zi38(204 downto 0);
      \instR15\ : \ZLL_Main_set__data__out__reg2\ port map (zi39, zi40, zll_main_set_data_out_reg2_out);
      zi41 <= zll_main_set_data_out_reg2_out;
      \instR16\ : \ZLL_Main_perform__read3\ port map (zi41, \zll_main_perform_read3_outR1\);
      zi42 <= \zll_main_perform_read3_outR1\;
      \instR17\ : \ZLL_Main_idle8\ port map (zi42, \zll_main_idle8_outR1\);
      zi43 <= \zll_main_idle8_outR1\;
      zi44 <= zi43(409 downto 205);
      zi45 <= zi43(204 downto 0);
      zi46 <= (std_logic_vector'(B"01") & zi44 & zi45);
      zi47 <= zi46(409 downto 205);
      zi48 <= zi46(204 downto 0);
      \instR18\ : \ZLL_Main_idle17\ port map (zi47, zi48, \zll_main_idle17_outR1\);
      zi49 <= (\__st0\ & \__st1\);
      \instR19\ : \Main_get__addr__reg\ port map (zi49, \main_get_addr_reg_outR2\);
      zi50 <= \main_get_addr_reg_outR2\;
      zi51 <= zi50(214 downto 205);
      zi52 <= zi50(204 downto 0);
      conn <= rw_repl(32, std_logic_vector'(B"0"));
      \instR20\ : \ZLL_Main_setloc1\ port map (conn, zi51, zi52, \zll_main_setloc1_outR1\);
      zi53 <= \zll_main_setloc1_outR1\;
      zi54 <= zi50(214 downto 214);
      zi55 <= zi50(213 downto 213);
      zi56 <= zi50(212 downto 212);
      zi57 <= zi50(211 downto 211);
      zi58 <= zi50(210 downto 210);
      zi59 <= zi50(209 downto 209);
      zi60 <= zi50(208 downto 208);
      zi61 <= zi50(207 downto 207);
      zi62 <= zi50(206 downto 206);
      zi63 <= zi50(205 downto 205);
      \connR1\ <= rw_cond(rw_eq(zi54, std_logic_vector'(B"0")), rw_cond(rw_eq(zi55, std_logic_vector'(B"0")), rw_cond(rw_eq(zi56, std_logic_vector'(B"0")), rw_cond(rw_eq(zi57, std_logic_vector'(B"0")), rw_cond(rw_eq(zi58, std_logic_vector'(B"0")), rw_cond(rw_eq(zi59, std_logic_vector'(B"0")), rw_cond(rw_eq(zi60, std_logic_vector'(B"0")), rw_cond(rw_eq(zi61, std_logic_vector'(B"0")), rw_cond(rw_eq(zi62, std_logic_vector'(B"0")), rw_cond(rw_eq(zi63, std_logic_vector'(B"1")), std_logic_vector'(B"0000000000"), std_logic_vector'(B"1111111111")), rw_cond(rw_eq(zi63, std_logic_vector'(B"0")), std_logic_vector'(B"0000000001"), std_logic_vector'(B"0000000010"))), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111")), std_logic_vector'(B"1111111111"));
      \instR21\ : \Main_set__addr__reg1\ port map (\connR1\, zi53, main_set_addr_reg1_out);
      zi64 <= main_set_addr_reg1_out;
      \instR22\ : \ZLL_Main_idle8\ port map (zi64, \zll_main_idle8_outR2\);
      zi65 <= \zll_main_idle8_outR2\;
      zi66 <= zi65(409 downto 205);
      zi67 <= zi65(204 downto 0);
      zi68 <= (std_logic_vector'(B"01") & zi66 & zi67);
      zi69 <= zi68(409 downto 205);
      zi70 <= zi68(204 downto 0);
      \instR23\ : \Main_while__addr__reg__0\ port map (zi69, zi70, main_while_addr_reg_0_out);
      zi71 <= (\__st0\ & \__st1\);
      zi72 <= \__in0\(2 downto 2);
      zi73 <= zi72;
      zi74 <= \__in0\(3 downto 3);
      zi75 <= zi74;
      zi76 <= \__in0\(1 downto 0);
      zi77 <= zi76;
      zi78 <= \__in0\(13 downto 4);
      zi79 <= zi78;
      \instR24\ : \Mainzuzlzazg\ port map (zi77, zi79, mainzuzlzazgzuout);
      \instR25\ : \Main_set__addr__reg1\ port map (mainzuzlzazgzuout, zi71, \main_set_addr_reg1_outR1\);
      zi80 <= \main_set_addr_reg1_outR1\;
      zi81 <= \__in0\(45 downto 14);
      zi82 <= zi81;
      zi83 <= zi80(127 downto 0);
      zi84 <= zi80(204 downto 203);
      zi85 <= zi80(202 downto 193);
      zi86 <= zi80(160 downto 160);
      zi87 <= zi80(159 downto 128);
      zi88 <= ((zi84 & zi85 & zi82 & zi86 & zi87) & zi83);
      \instR26\ : \ZLL_Main_idle1\ port map (zi77, zi88, zll_main_idle1_out);
      zi89 <= zll_main_idle1_out;
      \instR27\ : \ZLL_Main_idle8\ port map (zi89, \zll_main_idle8_outR3\);
      zi90 <= \zll_main_idle8_outR3\;
      zi91 <= zi90(409 downto 205);
      zi92 <= zi90(204 downto 0);
      zi93 <= (std_logic_vector'(B"01") & zi91 & zi92);
      zi94 <= zi93(409 downto 205);
      \instR28\ : \ZLL_Main_connect1\ port map (zi94, zll_main_connect1_out);
      \instR29\ : \ZLL_Main_connect\ port map (zll_main_connect1_out, zll_main_connect_out);
      \instR30\ : \Mainzuzlzazg\ port map (zi77, zi79, \mainzuzlzazgzuoutR1\);
      \instR31\ : \Main_set__addr__reg1\ port map (\mainzuzlzazgzuoutR1\, zi71, \main_set_addr_reg1_outR2\);
      zi96 <= \main_set_addr_reg1_outR2\;
      \instR32\ : \ZLL_Main_idle1\ port map (zi77, zi96, \zll_main_idle1_outR1\);
      zi97 <= \zll_main_idle1_outR1\;
      \instR33\ : \ZLL_Main_idle8\ port map (zi97, \zll_main_idle8_outR4\);
      zi98 <= \zll_main_idle8_outR4\;
      zi99 <= zi98(409 downto 205);
      zi100 <= zi98(204 downto 0);
      zi101 <= (std_logic_vector'(B"01") & zi99 & zi100);
      zi102 <= zi101(409 downto 205);
      \instR34\ : \ZLL_Main_connect1\ port map (zi102, \zll_main_connect1_outR1\);
      \instR35\ : \ZLL_Main_connect\ port map (\zll_main_connect1_outR1\, \zll_main_connect_outR1\);
      \instR36\ : \Main_set__ack__reg1\ port map (zi71, main_set_ack_reg1_out);
      zi104 <= main_set_ack_reg1_out;
      \instR37\ : \ZLL_Main_idle6\ port map (zi104, zll_main_idle6_out);
      zi105 <= zll_main_idle6_out;
      \instR38\ : \ZLL_Main_idle8\ port map (zi105, \zll_main_idle8_outR5\);
      zi106 <= \zll_main_idle8_outR5\;
      zi107 <= zi106(409 downto 205);
      zi108 <= zi106(204 downto 0);
      zi109 <= (std_logic_vector'(B"01") & zi107 & zi108);
      zi110 <= zi109(409 downto 205);
      zi111 <= zi109(204 downto 0);
      \instR39\ : \ZLL_Main_idle17\ port map (zi110, zi111, \zll_main_idle17_outR2\);
      zi112 <= (\__st0\ & \__st1\);
      \instR40\ : \Main_while__addr__reg__0\ port map (zi112, zi112, \main_while_addr_reg_0_outR1\);
      zres <= rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"001")), zll_main_idle17_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"010")), \zll_main_idle17_outR1\, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"011")), main_while_addr_reg_0_out, rw_cond(rw_eq(\__resumption_tag\, std_logic_vector'(B"100")), rw_cond(rw_eq(zi75, std_logic_vector'(B"1")), rw_cond(rw_eq(zi73, std_logic_vector'(B"0")), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & zll_main_connect_out & std_logic_vector'(B"001") & zi94), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & \zll_main_connect_outR1\ & std_logic_vector'(B"010") & zi102)), \zll_main_idle17_outR2\), \main_while_addr_reg_0_outR1\))));
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
entity \ZLL_Main_set__ack__reg2\ is
port (arg0 : in std_logic_vector (0 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_set__ack__reg2\ is
signal m : std_logic_vector (127 downto 0);
      signal partition_reg : std_logic_vector (1 downto 0);
      signal addr_reg : std_logic_vector (9 downto 0);
      signal data_reg : std_logic_vector (31 downto 0);
      signal data_out_reg : std_logic_vector (31 downto 0);
begin
m <= arg1(127 downto 0);
      partition_reg <= arg1(204 downto 203);
      addr_reg <= arg1(202 downto 193);
      data_reg <= arg1(192 downto 161);
      data_out_reg <= arg1(159 downto 128);
      res <= ((partition_reg & addr_reg & data_reg & arg0 & data_out_reg) & m);
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
component \ZLL_Main_set__ack__reg2\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal zll_main_set_ack_reg2_out : std_logic_vector (204 downto 0);
begin
inst : \ZLL_Main_set__ack__reg2\ port map (std_logic_vector'(B"0"), arg0, zll_main_set_ack_reg2_out);
      res <= zll_main_set_ack_reg2_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_perform__read3\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_perform__read3\ is
component \ZLL_Main_set__ack__reg2\ is
      port (arg0 : in std_logic_vector (0 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal zll_main_set_ack_reg2_out : std_logic_vector (204 downto 0);
begin
inst : \ZLL_Main_set__ack__reg2\ port map (std_logic_vector'(B"1"), arg0, zll_main_set_ack_reg2_out);
      res <= zll_main_set_ack_reg2_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_memLookup2\ is
port (arg0 : in std_logic_vector (127 downto 0);
      res : out std_logic_vector (31 downto 0));
end entity;

architecture rtl of \ZLL_Main_memLookup2\ is

begin
res <= rw_repl(32, std_logic_vector'(B"0"));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_idle17\ is
port (arg0 : in std_logic_vector (204 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (411 downto 0));
end entity;

architecture rtl of \ZLL_Main_idle17\ is
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
entity \Main_set__addr__reg1\ is
port (arg0 : in std_logic_vector (9 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \Main_set__addr__reg1\ is
signal zi0 : std_logic_vector (127 downto 0);
      signal zi1 : std_logic_vector (1 downto 0);
      signal zi2 : std_logic_vector (31 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (31 downto 0);
begin
zi0 <= arg1(127 downto 0);
      zi1 <= arg1(204 downto 203);
      zi2 <= arg1(192 downto 161);
      zi3 <= arg1(160 downto 160);
      zi4 <= arg1(159 downto 128);
      res <= ((zi1 & arg0 & zi2 & zi3 & zi4) & zi0);
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
entity \Main_set__data__out__reg\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \Main_set__data__out__reg\ is
component \ZLL_Main_set__data__out__reg2\ is
      port (arg0 : in std_logic_vector (31 downto 0);
            arg1 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal conn : std_logic_vector (31 downto 0);
      signal zll_main_set_data_out_reg2_out : std_logic_vector (204 downto 0);
begin
conn <= rw_repl(32, std_logic_vector'(B"0"));
      inst : \ZLL_Main_set__data__out__reg2\ port map (conn, arg0, zll_main_set_data_out_reg2_out);
      res <= zll_main_set_data_out_reg2_out;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_setloc1\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (9 downto 0);
      arg2 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_setloc1\ is
signal s : std_logic_vector (76 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (0 downto 0);
      signal zi2 : std_logic_vector (0 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (0 downto 0);
      signal zi5 : std_logic_vector (0 downto 0);
      signal zi6 : std_logic_vector (0 downto 0);
      signal zi7 : std_logic_vector (0 downto 0);
      signal zi8 : std_logic_vector (0 downto 0);
      signal zi9 : std_logic_vector (0 downto 0);
      signal zi10 : std_logic_vector (31 downto 0);
      signal zi11 : std_logic_vector (31 downto 0);
      signal zi12 : std_logic_vector (31 downto 0);
      signal zi13 : std_logic_vector (31 downto 0);
      signal zi14 : std_logic_vector (31 downto 0);
      signal zi15 : std_logic_vector (31 downto 0);
      signal zi16 : std_logic_vector (31 downto 0);
      signal zi17 : std_logic_vector (31 downto 0);
      signal zi18 : std_logic_vector (31 downto 0);
      signal zi19 : std_logic_vector (31 downto 0);
      signal zi20 : std_logic_vector (31 downto 0);
      signal zi21 : std_logic_vector (31 downto 0);
begin
s <= arg2(204 downto 128);
      zi0 <= arg1(9 downto 9);
      zi1 <= arg1(8 downto 8);
      zi2 <= arg1(7 downto 7);
      zi3 <= arg1(6 downto 6);
      zi4 <= arg1(5 downto 5);
      zi5 <= arg1(4 downto 4);
      zi6 <= arg1(3 downto 3);
      zi7 <= arg1(2 downto 2);
      zi8 <= arg1(1 downto 1);
      zi9 <= arg1(0 downto 0);
      zi10 <= arg2(95 downto 64);
      zi11 <= arg2(63 downto 32);
      zi12 <= arg2(31 downto 0);
      zi13 <= arg2(127 downto 96);
      zi14 <= arg2(63 downto 32);
      zi15 <= arg2(31 downto 0);
      zi16 <= arg2(127 downto 96);
      zi17 <= arg2(95 downto 64);
      zi18 <= arg2(31 downto 0);
      zi19 <= arg2(127 downto 96);
      zi20 <= arg2(95 downto 64);
      zi21 <= arg2(63 downto 32);
      res <= (s & rw_cond(rw_eq(zi0, std_logic_vector'(B"0")), rw_cond(rw_eq(zi1, std_logic_vector'(B"0")), rw_cond(rw_eq(zi2, std_logic_vector'(B"0")), rw_cond(rw_eq(zi3, std_logic_vector'(B"0")), rw_cond(rw_eq(zi4, std_logic_vector'(B"0")), rw_cond(rw_eq(zi5, std_logic_vector'(B"0")), rw_cond(rw_eq(zi6, std_logic_vector'(B"0")), rw_cond(rw_eq(zi7, std_logic_vector'(B"0")), rw_cond(rw_eq(zi8, std_logic_vector'(B"0")), rw_cond(rw_eq(zi9, std_logic_vector'(B"0")), (arg0 & zi10 & zi11 & zi12), (zi13 & arg0 & zi14 & zi15)), rw_cond(rw_eq(zi9, std_logic_vector'(B"0")), (zi16 & zi17 & arg0 & zi18), (zi19 & zi20 & zi21 & arg0))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))), rw_repl(128, std_logic_vector'(B"0"))));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_idle8\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (409 downto 0));
end entity;

architecture rtl of \ZLL_Main_idle8\ is

begin
res <= (arg0 & arg0);
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
      signal zt0 : std_logic_vector (214 downto 0);
      signal v : std_logic_vector (9 downto 0);
      signal s0 : std_logic_vector (204 downto 0);
      signal zt1 : std_logic_vector (411 downto 0);
      signal \s0R1\ : std_logic_vector (204 downto 0);
      signal zi0 : std_logic_vector (0 downto 0);
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
      signal zi11 : std_logic_vector (76 downto 0);
      signal zll_main_connect_out : std_logic_vector (32 downto 0);
      signal zll_main_connect1_out : std_logic_vector (76 downto 0);
      signal \zll_main_connect_outR1\ : std_logic_vector (32 downto 0);
begin
inst : \Main_get__addr__reg\ port map (arg1, main_get_addr_reg_out);
      zt0 <= main_get_addr_reg_out;
      v <= zt0(214 downto 205);
      s0 <= zt0(204 downto 0);
      zt1 <= (rw_repl(197, std_logic_vector'(B"0")) & v & s0);
      \s0R1\ <= zt1(204 downto 0);
      zi0 <= zt1(214 downto 214);
      zi1 <= zt1(213 downto 213);
      zi2 <= zt1(212 downto 212);
      zi3 <= zt1(211 downto 211);
      zi4 <= zt1(210 downto 210);
      zi5 <= zt1(209 downto 209);
      zi6 <= zt1(208 downto 208);
      zi7 <= zt1(207 downto 207);
      zi8 <= zt1(206 downto 206);
      zi9 <= zt1(205 downto 205);
      zi10 <= rw_cond(rw_eq(zi0, std_logic_vector'(B"1")), rw_cond(rw_eq(zi1, std_logic_vector'(B"1")), rw_cond(rw_eq(zi2, std_logic_vector'(B"1")), rw_cond(rw_eq(zi3, std_logic_vector'(B"1")), rw_cond(rw_eq(zi4, std_logic_vector'(B"1")), rw_cond(rw_eq(zi5, std_logic_vector'(B"1")), rw_cond(rw_eq(zi6, std_logic_vector'(B"1")), rw_cond(rw_eq(zi7, std_logic_vector'(B"1")), rw_cond(rw_eq(zi8, std_logic_vector'(B"1")), rw_cond(rw_eq(zi9, std_logic_vector'(B"1")), std_logic_vector'(B"0"), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1")), std_logic_vector'(B"1"));
      zi11 <= zt1(204 downto 128);
      \instR1\ : \ZLL_Main_connect\ port map (zi11, zll_main_connect_out);
      \instR2\ : \ZLL_Main_connect1\ port map (arg0, zll_main_connect1_out);
      \instR3\ : \ZLL_Main_connect\ port map (zll_main_connect1_out, \zll_main_connect_outR1\);
      res <= rw_cond(rw_eq(zi10, std_logic_vector'(B"0")), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & zll_main_connect_out & std_logic_vector'(B"100") & \s0R1\), ((std_logic_vector'(B"1") & rw_repl(170, std_logic_vector'(B"0"))) & \zll_main_connect_outR1\ & std_logic_vector'(B"011") & arg0));
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
entity \ZLL_Main_set__data__out__reg2\ is
port (arg0 : in std_logic_vector (31 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_set__data__out__reg2\ is
signal m : std_logic_vector (127 downto 0);
      signal partition_reg : std_logic_vector (1 downto 0);
      signal addr_reg : std_logic_vector (9 downto 0);
      signal data_reg : std_logic_vector (31 downto 0);
      signal ack_reg : std_logic_vector (0 downto 0);
begin
m <= arg1(127 downto 0);
      partition_reg <= arg1(204 downto 203);
      addr_reg <= arg1(202 downto 193);
      data_reg <= arg1(192 downto 161);
      ack_reg <= arg1(160 downto 160);
      res <= ((partition_reg & addr_reg & data_reg & ack_reg & arg0) & m);
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
b0 <= arg0(1 downto 1);
      b1 <= arg0(0 downto 0);
      b2 <= arg1(7 downto 7);
      b3 <= arg1(6 downto 6);
      b4 <= arg1(5 downto 5);
      b5 <= arg1(4 downto 4);
      b6 <= arg1(3 downto 3);
      b7 <= arg1(2 downto 2);
      b8 <= arg1(1 downto 1);
      b9 <= arg1(0 downto 0);
      res <= (b0 & b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 & b9);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_idle6\ is
port (arg0 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_idle6\ is
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
entity \ZLL_Main_connect\ is
port (arg0 : in std_logic_vector (76 downto 0);
      res : out std_logic_vector (32 downto 0));
end entity;

architecture rtl of \ZLL_Main_connect\ is
signal zi0 : std_logic_vector (0 downto 0);
      signal zi1 : std_logic_vector (31 downto 0);
begin
zi0 <= arg0(32 downto 32);
      zi1 <= arg0(31 downto 0);
      res <= (zi0 & zi1);
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \ZLL_Main_idle1\ is
port (arg0 : in std_logic_vector (1 downto 0);
      arg1 : in std_logic_vector (204 downto 0);
      res : out std_logic_vector (204 downto 0));
end entity;

architecture rtl of \ZLL_Main_idle1\ is
component \Main_set__ack__reg1\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      component \ZLL_Main_idle6\ is
      port (arg0 : in std_logic_vector (204 downto 0);
            res : out std_logic_vector (204 downto 0));
      end component;
      signal zi0 : std_logic_vector (127 downto 0);
      signal zi1 : std_logic_vector (9 downto 0);
      signal zi2 : std_logic_vector (31 downto 0);
      signal zi3 : std_logic_vector (0 downto 0);
      signal zi4 : std_logic_vector (31 downto 0);
      signal zt0 : std_logic_vector (204 downto 0);
      signal main_set_ack_reg1_out : std_logic_vector (204 downto 0);
      signal zi5 : std_logic_vector (204 downto 0);
      signal zll_main_idle6_out : std_logic_vector (204 downto 0);
begin
zi0 <= arg1(127 downto 0);
      zi1 <= arg1(202 downto 193);
      zi2 <= arg1(192 downto 161);
      zi3 <= arg1(160 downto 160);
      zi4 <= arg1(159 downto 128);
      zt0 <= ((arg0 & zi1 & zi2 & zi3 & zi4) & zi0);
      inst : \Main_set__ack__reg1\ port map (zt0, main_set_ack_reg1_out);
      zi5 <= main_set_ack_reg1_out;
      \instR1\ : \ZLL_Main_idle6\ port map (zi5, zll_main_idle6_out);
      res <= zll_main_idle6_out;
end architecture;