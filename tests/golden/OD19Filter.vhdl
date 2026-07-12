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
            port (\Zeta0\ : in std_logic_vector (31 downto 0);
                  \Zeta1\ : in std_logic_vector (31 downto 0);
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
      -- state registers
      -- __resumption_tag: 7 bits, init 0x0
      --   states: 0=$x128 1=$x129 2=$x130 3=$x131 4=$x132 5=$x133 6=$x134 7=$x135 8=$x136 9=$x137 10=$x138 11=$x139 12=$x140 13=$x141 14=$x142 15=$x143 16=$x144 17=$x145 18=$x146 19=$x147 20=$x148 21=$x149 22=$x150 23=$x151 24=$x152 25=$x153 26=$x154 27=$x155 28=$x156 29=$x157 30=$x158 31=$x159 32=$x160 33=$x161 34=$x162 35=$x163 36=$x164 37=$x165 38=$x166 39=$x167 40=$x168 41=$x169 42=$x170 43=$x171 44=$x172 45=$x173 46=$x174 47=$x175 48=$x176 49=$x177 50=$x178 51=$x179 52=$x180 53=$x181 54=$x182 55=$x183 56=$x184 57=$x185 58=$x186 59=$x187 60=$x188 61=$x189 62=$x190 63=$x191 64=$x192 65=$x193 66=$x194 67=$x195 68=$x196 69=$x197 70=$x198 71=$x199 72=$x200 73=$x201 74=$x202 75=$x203 76=$x204 77=$x205 78=$x206 79=$x207 80=$x208 81=$x209 82=$x210 83=$x211 84=$x212 85=$x213 86=$x214 87=$x215 88=$x216 89=$x217 90=$x218 91=$x219 92=$x220 93=$x221 94=$x222 95=$x223 96=$x224 97=$x225 98=$x226 99=$x227 100=$x228 101=$x229 102=$x230 103=$x231 104=$x232 105=$x233 106=$x234 107=$x235 108=$x236 109=$x237 110=$x238 111=$x239 112=$x240 113=$x241 114=$x242 115=$x243 116=$x244 117=$x245 118=$x246 119=$x247 120=$x248 121=$x249 122=$x250 123=$x251 124=$x252 125=$x253 126=$x254
      -- __st0: 32 bits, init 0x0
      constant \st_$x128\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000000");
      constant \st_$x129\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000001");
      constant \st_$x130\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000010");
      constant \st_$x131\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000011");
      constant \st_$x132\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000100");
      constant \st_$x133\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000101");
      constant \st_$x134\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000110");
      constant \st_$x135\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000111");
      constant \st_$x136\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001000");
      constant \st_$x137\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001001");
      constant \st_$x138\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001010");
      constant \st_$x139\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001011");
      constant \st_$x140\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001100");
      constant \st_$x141\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001101");
      constant \st_$x142\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001110");
      constant \st_$x143\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0001111");
      constant \st_$x144\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010000");
      constant \st_$x145\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010001");
      constant \st_$x146\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010010");
      constant \st_$x147\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010011");
      constant \st_$x148\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010100");
      constant \st_$x149\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010101");
      constant \st_$x150\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010110");
      constant \st_$x151\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0010111");
      constant \st_$x152\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011000");
      constant \st_$x153\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011001");
      constant \st_$x154\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011010");
      constant \st_$x155\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011011");
      constant \st_$x156\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011100");
      constant \st_$x157\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011101");
      constant \st_$x158\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011110");
      constant \st_$x159\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0011111");
      constant \st_$x160\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100000");
      constant \st_$x161\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100001");
      constant \st_$x162\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100010");
      constant \st_$x163\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100011");
      constant \st_$x164\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100100");
      constant \st_$x165\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100101");
      constant \st_$x166\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100110");
      constant \st_$x167\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0100111");
      constant \st_$x168\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101000");
      constant \st_$x169\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101001");
      constant \st_$x170\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101010");
      constant \st_$x171\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101011");
      constant \st_$x172\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101100");
      constant \st_$x173\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101101");
      constant \st_$x174\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101110");
      constant \st_$x175\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0101111");
      constant \st_$x176\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110000");
      constant \st_$x177\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110001");
      constant \st_$x178\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110010");
      constant \st_$x179\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110011");
      constant \st_$x180\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110100");
      constant \st_$x181\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110101");
      constant \st_$x182\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110110");
      constant \st_$x183\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0110111");
      constant \st_$x184\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111000");
      constant \st_$x185\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111001");
      constant \st_$x186\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111010");
      constant \st_$x187\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111011");
      constant \st_$x188\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111100");
      constant \st_$x189\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111101");
      constant \st_$x190\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111110");
      constant \st_$x191\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0111111");
      constant \st_$x192\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000000");
      constant \st_$x193\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000001");
      constant \st_$x194\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000010");
      constant \st_$x195\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000011");
      constant \st_$x196\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000100");
      constant \st_$x197\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000101");
      constant \st_$x198\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000110");
      constant \st_$x199\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1000111");
      constant \st_$x200\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001000");
      constant \st_$x201\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001001");
      constant \st_$x202\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001010");
      constant \st_$x203\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001011");
      constant \st_$x204\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001100");
      constant \st_$x205\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001101");
      constant \st_$x206\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001110");
      constant \st_$x207\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1001111");
      constant \st_$x208\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010000");
      constant \st_$x209\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010001");
      constant \st_$x210\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010010");
      constant \st_$x211\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010011");
      constant \st_$x212\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010100");
      constant \st_$x213\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010101");
      constant \st_$x214\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010110");
      constant \st_$x215\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1010111");
      constant \st_$x216\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011000");
      constant \st_$x217\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011001");
      constant \st_$x218\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011010");
      constant \st_$x219\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011011");
      constant \st_$x220\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011100");
      constant \st_$x221\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011101");
      constant \st_$x222\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011110");
      constant \st_$x223\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1011111");
      constant \st_$x224\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100000");
      constant \st_$x225\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100001");
      constant \st_$x226\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100010");
      constant \st_$x227\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100011");
      constant \st_$x228\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100100");
      constant \st_$x229\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100101");
      constant \st_$x230\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100110");
      constant \st_$x231\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1100111");
      constant \st_$x232\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101000");
      constant \st_$x233\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101001");
      constant \st_$x234\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101010");
      constant \st_$x235\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101011");
      constant \st_$x236\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101100");
      constant \st_$x237\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101101");
      constant \st_$x238\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101110");
      constant \st_$x239\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1101111");
      constant \st_$x240\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110000");
      constant \st_$x241\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110001");
      constant \st_$x242\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110010");
      constant \st_$x243\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110011");
      constant \st_$x244\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110100");
      constant \st_$x245\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110101");
      constant \st_$x246\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110110");
      constant \st_$x247\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1110111");
      constant \st_$x248\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1111000");
      constant \st_$x249\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1111001");
      constant \st_$x250\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1111010");
      constant \st_$x251\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1111011");
      constant \st_$x252\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1111100");
      constant \st_$x253\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1111101");
      constant \st_$x254\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"1111110");
      signal \__resumption_tag\ : std_logic_vector (6 downto 0) := std_logic_vector'(B"0000000");
      signal \__resumption_tag_next\ : std_logic_vector (6 downto 0);
      signal \__st0\ : std_logic_vector (31 downto 0) := (others => '0');
      signal \__st0_next\ : std_logic_vector (31 downto 0);
      signal extres : std_logic_vector (0 downto 0);
      signal za1 : std_logic_vector (1 downto 0);
      signal extres_r1 : std_logic_vector (0 downto 0);
      signal za1_r1 : std_logic_vector (1 downto 0);
      signal extres_r2 : std_logic_vector (0 downto 0);
      signal za1_r2 : std_logic_vector (1 downto 0);
      signal main_test1_out : std_logic_vector (0 downto 0);
      signal za1_r3 : std_logic_vector (1 downto 0);
      signal zres : std_logic_vector (40 downto 0);
begin
      -- combinational logic
      test4_i : test4 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres(0 downto 0));
      za1 <= (std_logic_vector'(B"0") & extres);
      test3_i : test3 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres_r1(0 downto 0));
      za1_r1 <= (std_logic_vector'(B"0") & extres_r1);
      test2_i : test2 port map (p0 => \__in1\, p1 => \__st0\, p2 => extres_r2(0 downto 0));
      za1_r2 <= (std_logic_vector'(B"0") & extres_r2);
      test1_i : \Main_test1\ port map (\__in1\, \__st0\, main_test1_out);
      za1_r3 <= (std_logic_vector'(B"0") & main_test1_out);
      with \__resumption_tag\ select zres <=
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101111110") & \__in1\), (std_logic_vector'(B"100000000") & \__st0\)) when "0000000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000000") & \__st0\), (std_logic_vector'(B"100000001") & \__st0\)) when "0000001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000001") & \__st0\), (std_logic_vector'(B"100000010") & \__st0\)) when "0000010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000010") & \__st0\), (std_logic_vector'(B"100000011") & \__st0\)) when "0000011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000011") & \__st0\), (std_logic_vector'(B"100000100") & \__st0\)) when "0000100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000100") & \__st0\), (std_logic_vector'(B"100000101") & \__st0\)) when "0000101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000101") & \__st0\), (std_logic_vector'(B"100000110") & \__st0\)) when "0000110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000110") & \__st0\), (std_logic_vector'(B"100000111") & \__st0\)) when "0000111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100000111") & \__st0\), (std_logic_vector'(B"100001000") & \__st0\)) when "0001000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001000") & \__st0\), (std_logic_vector'(B"100001001") & \__st0\)) when "0001001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001001") & \__st0\), (std_logic_vector'(B"100001010") & \__st0\)) when "0001010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001010") & \__st0\), (std_logic_vector'(B"100001011") & \__st0\)) when "0001011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001011") & \__st0\), (std_logic_vector'(B"100001100") & \__st0\)) when "0001100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001100") & \__st0\), (std_logic_vector'(B"100001101") & \__st0\)) when "0001101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001101") & \__st0\), (std_logic_vector'(B"100001110") & \__st0\)) when "0001110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001110") & \__st0\), (std_logic_vector'(B"100001111") & \__st0\)) when "0001111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100001111") & \__st0\), (std_logic_vector'(B"100010000") & \__st0\)) when "0010000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010000") & \__st0\), (std_logic_vector'(B"100010001") & \__st0\)) when "0010001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010001") & \__st0\), (std_logic_vector'(B"100010010") & \__st0\)) when "0010010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010010") & \__st0\), (std_logic_vector'(B"100010011") & \__st0\)) when "0010011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010011") & \__st0\), (std_logic_vector'(B"100010100") & \__st0\)) when "0010100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010100") & \__st0\), (std_logic_vector'(B"100010101") & \__st0\)) when "0010101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010101") & \__st0\), (std_logic_vector'(B"100010110") & \__st0\)) when "0010110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010110") & \__st0\), (std_logic_vector'(B"100010111") & \__st0\)) when "0010111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100010111") & \__st0\), (std_logic_vector'(B"100011000") & \__st0\)) when "0011000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011000") & \__st0\), (std_logic_vector'(B"100011001") & \__st0\)) when "0011001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011001") & \__st0\), (std_logic_vector'(B"100011010") & \__st0\)) when "0011010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011010") & \__st0\), (std_logic_vector'(B"100011011") & \__st0\)) when "0011011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011011") & \__st0\), (std_logic_vector'(B"100011100") & \__st0\)) when "0011100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011100") & \__st0\), (std_logic_vector'(B"100011101") & \__st0\)) when "0011101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011101") & \__st0\), (std_logic_vector'(B"100011110") & \__st0\)) when "0011110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011110") & \__st0\), (std_logic_vector'(B"100011111") & \__st0\)) when "0011111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100011111") & \__st0\), (std_logic_vector'(B"100100000") & \__st0\)) when "0100000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100000") & \__st0\), (std_logic_vector'(B"100100001") & \__st0\)) when "0100001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100001") & \__st0\), (std_logic_vector'(B"100100010") & \__st0\)) when "0100010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100010") & \__st0\), (std_logic_vector'(B"100100011") & \__st0\)) when "0100011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100011") & \__st0\), (std_logic_vector'(B"100100100") & \__st0\)) when "0100100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100100") & \__st0\), (std_logic_vector'(B"100100101") & \__st0\)) when "0100101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100101") & \__st0\), (std_logic_vector'(B"100100110") & \__st0\)) when "0100110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100110") & \__st0\), (std_logic_vector'(B"100100111") & \__st0\)) when "0100111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100100111") & \__st0\), (std_logic_vector'(B"100101000") & \__st0\)) when "0101000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101000") & \__st0\), (std_logic_vector'(B"100101001") & \__st0\)) when "0101001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101001") & \__st0\), (std_logic_vector'(B"100101010") & \__st0\)) when "0101010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101010") & \__st0\), (std_logic_vector'(B"100101011") & \__st0\)) when "0101011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101011") & \__st0\), (std_logic_vector'(B"100101100") & \__st0\)) when "0101100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101100") & \__st0\), (std_logic_vector'(B"100101101") & \__st0\)) when "0101101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101101") & \__st0\), (std_logic_vector'(B"100101110") & \__st0\)) when "0101110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101110") & \__st0\), (std_logic_vector'(B"100101111") & \__st0\)) when "0101111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100101111") & \__st0\), (std_logic_vector'(B"100110000") & \__st0\)) when "0110000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110000") & \__st0\), (std_logic_vector'(B"100110001") & \__st0\)) when "0110001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110001") & \__st0\), (std_logic_vector'(B"100110010") & \__st0\)) when "0110010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110010") & \__st0\), (std_logic_vector'(B"100110011") & \__st0\)) when "0110011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110011") & \__st0\), (std_logic_vector'(B"100110100") & \__st0\)) when "0110100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110100") & \__st0\), (std_logic_vector'(B"100110101") & \__st0\)) when "0110101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110101") & \__st0\), (std_logic_vector'(B"100110110") & \__st0\)) when "0110110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110110") & \__st0\), (std_logic_vector'(B"100110111") & \__st0\)) when "0110111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100110111") & \__st0\), (std_logic_vector'(B"100111000") & \__st0\)) when "0111000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111000") & \__st0\), (std_logic_vector'(B"100111001") & \__st0\)) when "0111001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111001") & \__st0\), (std_logic_vector'(B"100111010") & \__st0\)) when "0111010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111010") & \__st0\), (std_logic_vector'(B"100111011") & \__st0\)) when "0111011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111011") & \__st0\), (std_logic_vector'(B"100111100") & \__st0\)) when "0111100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111100") & \__st0\), (std_logic_vector'(B"100111101") & \__st0\)) when "0111101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111101") & \__st0\), (std_logic_vector'(B"100111110") & \__st0\)) when "0111110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111110") & \__st0\), (std_logic_vector'(B"100111111") & \__st0\)) when "0111111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"100111111") & \__st0\), (std_logic_vector'(B"101000000") & \__st0\)) when "1000000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000000") & \__st0\), (std_logic_vector'(B"101000001") & \__st0\)) when "1000001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000001") & \__st0\), (std_logic_vector'(B"101000010") & \__st0\)) when "1000010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000010") & \__st0\), (std_logic_vector'(B"101000011") & \__st0\)) when "1000011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000011") & \__st0\), (std_logic_vector'(B"101000100") & \__st0\)) when "1000100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000100") & \__st0\), (std_logic_vector'(B"101000101") & \__st0\)) when "1000101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000101") & \__st0\), (std_logic_vector'(B"101000110") & \__st0\)) when "1000110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000110") & \__st0\), (std_logic_vector'(B"101000111") & \__st0\)) when "1000111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101000111") & \__st0\), (std_logic_vector'(B"101001000") & \__st0\)) when "1001000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001000") & \__st0\), (std_logic_vector'(B"101001001") & \__st0\)) when "1001001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001001") & \__st0\), (std_logic_vector'(B"101001010") & \__st0\)) when "1001010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001010") & \__st0\), (std_logic_vector'(B"101001011") & \__st0\)) when "1001011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001011") & \__st0\), (std_logic_vector'(B"101001100") & \__st0\)) when "1001100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001100") & \__st0\), (std_logic_vector'(B"101001101") & \__st0\)) when "1001101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001101") & \__st0\), (std_logic_vector'(B"101001110") & \__st0\)) when "1001110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001110") & \__st0\), (std_logic_vector'(B"101001111") & \__st0\)) when "1001111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101001111") & \__st0\), (std_logic_vector'(B"101010000") & \__st0\)) when "1010000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010000") & \__st0\), (std_logic_vector'(B"101010001") & \__st0\)) when "1010001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010001") & \__st0\), (std_logic_vector'(B"101010010") & \__st0\)) when "1010010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010010") & \__st0\), (std_logic_vector'(B"101010011") & \__st0\)) when "1010011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010011") & \__st0\), (std_logic_vector'(B"101010100") & \__st0\)) when "1010100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010100") & \__st0\), (std_logic_vector'(B"101010101") & \__st0\)) when "1010101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010101") & \__st0\), (std_logic_vector'(B"101010110") & \__st0\)) when "1010110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010110") & \__st0\), (std_logic_vector'(B"101010111") & \__st0\)) when "1010111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101010111") & \__st0\), (std_logic_vector'(B"101011000") & \__st0\)) when "1011000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011000") & \__st0\), (std_logic_vector'(B"101011001") & \__st0\)) when "1011001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011001") & \__st0\), (std_logic_vector'(B"101011010") & \__st0\)) when "1011010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011010") & \__st0\), (std_logic_vector'(B"101011011") & \__st0\)) when "1011011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011011") & \__st0\), (std_logic_vector'(B"101011100") & \__st0\)) when "1011100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011100") & \__st0\), (std_logic_vector'(B"101011101") & \__st0\)) when "1011101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011101") & \__st0\), (std_logic_vector'(B"101011110") & \__st0\)) when "1011110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011110") & \__st0\), (std_logic_vector'(B"101011111") & \__st0\)) when "1011111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101011111") & \__st0\), (std_logic_vector'(B"101100000") & \__st0\)) when "1100000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100000") & \__st0\), (std_logic_vector'(B"101100001") & \__st0\)) when "1100001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100001") & \__st0\), (std_logic_vector'(B"101100010") & \__st0\)) when "1100010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100010") & \__st0\), (std_logic_vector'(B"101100011") & \__st0\)) when "1100011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100011") & \__st0\), (std_logic_vector'(B"101100100") & \__st0\)) when "1100100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100100") & \__st0\), (std_logic_vector'(B"101100101") & \__st0\)) when "1100101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100101") & \__st0\), (std_logic_vector'(B"101100110") & \__st0\)) when "1100110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100110") & \__st0\), (std_logic_vector'(B"101100111") & \__st0\)) when "1100111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101100111") & \__st0\), (std_logic_vector'(B"101101000") & \__st0\)) when "1101000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101000") & \__st0\), (std_logic_vector'(B"101101001") & \__st0\)) when "1101001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101001") & \__st0\), (std_logic_vector'(B"101101010") & \__st0\)) when "1101010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101010") & \__st0\), (std_logic_vector'(B"101101011") & \__st0\)) when "1101011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101011") & \__st0\), (std_logic_vector'(B"101101100") & \__st0\)) when "1101100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101100") & \__st0\), (std_logic_vector'(B"101101101") & \__st0\)) when "1101101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101101") & \__st0\), (std_logic_vector'(B"101101110") & \__st0\)) when "1101110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101110") & \__st0\), (std_logic_vector'(B"101101111") & \__st0\)) when "1101111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101101111") & \__st0\), (std_logic_vector'(B"101110000") & \__st0\)) when "1110000",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110000") & \__st0\), (std_logic_vector'(B"101110001") & \__st0\)) when "1110001",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110001") & \__st0\), (std_logic_vector'(B"101110010") & \__st0\)) when "1110010",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110010") & \__st0\), (std_logic_vector'(B"101110011") & \__st0\)) when "1110011",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110011") & \__st0\), (std_logic_vector'(B"101110100") & \__st0\)) when "1110100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110100") & \__st0\), (std_logic_vector'(B"101110101") & \__st0\)) when "1110101",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110101") & \__st0\), (std_logic_vector'(B"101110110") & \__st0\)) when "1110110",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110110") & \__st0\), (std_logic_vector'(B"101110111") & \__st0\)) when "1110111",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101110111") & \__st0\), (std_logic_vector'(B"101111000") & \__st0\)) when "1111000",
            rw_cond(rw_not(\__in0\), (za1 & std_logic_vector'(B"1111000") & \__st0\), (std_logic_vector'(B"101111001") & \__st0\)) when "1111001",
            rw_cond(rw_not(\__in0\), (za1_r1 & std_logic_vector'(B"1111001") & \__st0\), (std_logic_vector'(B"101111010") & \__st0\)) when "1111010",
            rw_cond(rw_not(\__in0\), (za1_r2 & std_logic_vector'(B"1111010") & \__st0\), (std_logic_vector'(B"101111011") & \__st0\)) when "1111011",
            rw_cond(rw_not(\__in0\), (za1_r3 & std_logic_vector'(B"1111011") & \__st0\), (std_logic_vector'(B"101111100") & \__st0\)) when "1111100",
            rw_cond(rw_not(\__in0\), (std_logic_vector'(B"101111100") & \__in1\), (std_logic_vector'(B"101111101") & \__st0\)) when "1111101",
            rw_cond(rw_not(\__in0\), (za1_r3 & std_logic_vector'(B"1111101") & \__st0\), (std_logic_vector'(B"101111110") & \__st0\)) when others;
      \__resumption_tag_next\ <= zres(38 downto 32);
      \__st0_next\ <= zres(31 downto 0);
      -- outputs
      \__out0\ <= zres(40 downto 40);
      \__out1\ <= zres(39 downto 39);
      -- state register update
      process (clk, rst)
      begin
            if rst = std_logic_vector'(B"1") then
                  \__resumption_tag\ <= std_logic_vector'(B"0000000");
                  \__st0\ <= std_logic_vector'(X"00000000");
            elsif rising_edge(clk(0)) then
                  \__resumption_tag\ <= \__resumption_tag_next\;
                  \__st0\ <= \__st0_next\;
            end if;
      end process;
end architecture;

-- Main.test1
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rw_helpers.all;
entity \Main_test1\ is
      port (\Zeta0\ : in std_logic_vector (31 downto 0);
            \Zeta1\ : in std_logic_vector (31 downto 0);
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
      test1_i : test1 port map (p0 => \Zeta0\, p1 => \Zeta1\, p2 => extres(0 downto 0));
      res <= extres;
end architecture;