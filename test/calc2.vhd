library ieee;
use ieee.std_logic_1164.all;
-- Uncomment the following line if VHDL primitives are in use.
-- use prims.all;
entity rewire is
  Port ( clk : in std_logic ;
         input : in std_logic_vector (0 to 10);
         output : out std_logic_vector (0 to 7));
end rewire;

architecture behavioral of rewire is
  type control_state is (STATE0,STATE11);
  
  
begin
  process (clk)
    variable goto_L1 : boolean := false;
    variable goto_L0 : boolean := false;
    variable goto_L149 : boolean := false;
    variable goto_L26 : boolean := false;
    variable goto_L44 : boolean := false;
    variable goto_L62 : boolean := false;
    variable goto_L79 : boolean := false;
    variable goto_L96 : boolean := false;
    variable goto_L114 : boolean := false;
    variable goto_L132 : boolean := false;
    variable goto_L137 : boolean := false;
    variable goto_L119 : boolean := false;
    variable goto_L101 : boolean := false;
    variable goto_L83 : boolean := false;
    variable goto_L66 : boolean := false;
    variable goto_L49 : boolean := false;
    variable goto_L31 : boolean := false;
    variable goto_L11 : boolean := false;
    variable goto_L151 : boolean := false;
    variable r146 : std_logic_vector(0 to 7) := (others => '0');
    variable b143 : boolean := false;
    variable r141 : std_logic_vector(0 to 2) := (others => '0');
    variable r139 : std_logic_vector(0 to 7) := (others => '0');
    variable r134 : std_logic_vector(0 to 7) := (others => '0');
    variable b130 : boolean := false;
    variable b128 : boolean := false;
    variable b126 : boolean := false;
    variable r124 : std_logic_vector(0 to 2) := (others => '0');
    variable r122 : std_logic_vector(0 to 7) := (others => '0');
    variable b120 : boolean := false;
    variable r116 : std_logic_vector(0 to 7) := (others => '0');
    variable b112 : boolean := false;
    variable b110 : boolean := false;
    variable b108 : boolean := false;
    variable r106 : std_logic_vector(0 to 2) := (others => '0');
    variable r104 : std_logic_vector(0 to 7) := (others => '0');
    variable b102 : boolean := false;
    variable r98 : std_logic_vector(0 to 7) := (others => '0');
    variable b94 : boolean := false;
    variable b92 : boolean := false;
    variable b90 : boolean := false;
    variable r88 : std_logic_vector(0 to 2) := (others => '0');
    variable r86 : std_logic_vector(0 to 7) := (others => '0');
    variable b84 : boolean := false;
    variable r80 : std_logic_vector(0 to 7) := (others => '0');
    variable b77 : boolean := false;
    variable b75 : boolean := false;
    variable b73 : boolean := false;
    variable r71 : std_logic_vector(0 to 2) := (others => '0');
    variable r69 : std_logic_vector(0 to 7) := (others => '0');
    variable b67 : boolean := false;
    variable r63 : std_logic_vector(0 to 7) := (others => '0');
    variable b60 : boolean := false;
    variable b58 : boolean := false;
    variable b56 : boolean := false;
    variable r54 : std_logic_vector(0 to 2) := (others => '0');
    variable r52 : std_logic_vector(0 to 7) := (others => '0');
    variable b50 : boolean := false;
    variable r46 : std_logic_vector(0 to 7) := (others => '0');
    variable b42 : boolean := false;
    variable b40 : boolean := false;
    variable b38 : boolean := false;
    variable r36 : std_logic_vector(0 to 2) := (others => '0');
    variable r34 : std_logic_vector(0 to 7) := (others => '0');
    variable b32 : boolean := false;
    variable r28 : std_logic_vector(0 to 7) := (others => '0');
    variable b24 : boolean := false;
    variable b22 : boolean := false;
    variable b20 : boolean := false;
    variable r18 : std_logic_vector(0 to 2) := (others => '0');
    variable r16 : std_logic_vector(0 to 7) := (others => '0');
    variable b14 : boolean := false;
    variable r13 : std_logic_vector(0 to 7) := (others => '0');
    variable r10 : std_logic_vector(0 to 10) := (others => '0');
    variable r7 : std_logic_vector(0 to 7) := (others => '0');
    variable r3 : std_logic_vector(0 to 7) := (others => '0');
    variable statevar0 : std_logic_vector(0 to 7) := (others => '0');
    variable state : control_state := STATE0;
  begin
    if clk'event and clk='1' then
      null; -- label L151
      goto_L151 := false;
      -- ENTER
      goto_L0 := (state = STATE0);
      if (NOT goto_L0) then
        goto_L11 := (state = STATE11);
        null; -- label L11
        goto_L11 := false;
        r10 := input;
        -- got inp@1 in r10
        b14 := true;
        r16 := r10(0 to 7);
        r18 := r10(8 to 10);
        b20 := true;
        b22 := ("000" = r18(0 to 2));
        b24 := (b14 AND (b20 AND b22));
        goto_L26 := b24;
        if (NOT goto_L26) then
          goto_L31 := (NOT b24);
          null; -- label L31
          goto_L31 := false;
          -- alt exit (no match)
          b32 := true;
          r34 := r10(0 to 7);
          r36 := r10(8 to 10);
          b38 := true;
          b40 := ("001" = r36(0 to 2));
          b42 := (b32 AND (b38 AND b40));
          goto_L44 := b42;
          if (NOT goto_L44) then
            goto_L49 := (NOT b42);
            null; -- label L49
            goto_L49 := false;
            -- alt exit (no match)
            b50 := true;
            r52 := r10(0 to 7);
            r54 := r10(8 to 10);
            b56 := true;
            b58 := ("010" = r54(0 to 2));
            b60 := (b50 AND (b56 AND b58));
            goto_L62 := b60;
            if (NOT goto_L62) then
              goto_L66 := (NOT b60);
              null; -- label L66
              goto_L66 := false;
              -- alt exit (no match)
              b67 := true;
              r69 := r10(0 to 7);
              r71 := r10(8 to 10);
              b73 := true;
              b75 := ("011" = r71(0 to 2));
              b77 := (b67 AND (b73 AND b75));
              goto_L79 := b77;
              if (NOT goto_L79) then
                goto_L83 := (NOT b77);
                null; -- label L83
                goto_L83 := false;
                -- alt exit (no match)
                b84 := true;
                r86 := r10(0 to 7);
                r88 := r10(8 to 10);
                b90 := true;
                b92 := ("100" = r88(0 to 2));
                b94 := (b84 AND (b90 AND b92));
                goto_L96 := b94;
                if (NOT goto_L96) then
                  goto_L101 := (NOT b94);
                  null; -- label L101
                  goto_L101 := false;
                  -- alt exit (no match)
                  b102 := true;
                  r104 := r10(0 to 7);
                  r106 := r10(8 to 10);
                  b108 := true;
                  b110 := ("101" = r106(0 to 2));
                  b112 := (b102 AND (b108 AND b110));
                  goto_L114 := b112;
                  if (NOT goto_L114) then
                    goto_L119 := (NOT b112);
                    null; -- label L119
                    goto_L119 := false;
                    -- alt exit (no match)
                    b120 := true;
                    r122 := r10(0 to 7);
                    r124 := r10(8 to 10);
                    b126 := true;
                    b128 := ("110" = r124(0 to 2));
                    b130 := (b120 AND (b126 AND b128));
                    goto_L132 := b130;
                    if (NOT goto_L132) then
                      goto_L137 := (NOT b130);
                      null; -- label L137
                      goto_L137 := false;
                      -- alt exit (no match)
                      -- final pat
                      r139 := r10(0 to 7);
                      r141 := r10(8 to 10);
                      b143 := true;
                      -- final pat
                      r146 := zeroW8;
                      r13 := r146;
                      goto_L149 := true;
                    end if;
                    goto_L149 := goto_L149;
                    if (NOT goto_L149) then
                      null; -- label L132
                      goto_L132 := false;
                      -- got reg@0 in r3
                      -- got rand@6 in r122
                      r134 := xorW8(r3,r122);
                      r13 := r134;
                      goto_L149 := true;
                    end if;
                    goto_L149 := goto_L149;
                  end if;
                  goto_L149 := goto_L149;
                  if (NOT goto_L149) then
                    null; -- label L114
                    goto_L114 := false;
                    -- got reg@0 in r3
                    -- got rand@5 in r104
                    r116 := orW8(r3,r104);
                    r13 := r116;
                    goto_L149 := true;
                  end if;
                  goto_L149 := goto_L149;
                end if;
                goto_L149 := goto_L149;
                if (NOT goto_L149) then
                  null; -- label L96
                  goto_L96 := false;
                  -- got reg@0 in r3
                  -- got rand@4 in r86
                  r98 := andW8(r3,r86);
                  r13 := r98;
                  goto_L149 := true;
                end if;
                goto_L149 := goto_L149;
              end if;
              goto_L149 := goto_L149;
              if (NOT goto_L149) then
                null; -- label L79
                goto_L79 := false;
                -- got reg@0 in r3
                r80 := shrW8(r3);
                r13 := r80;
                goto_L149 := true;
              end if;
              goto_L149 := goto_L149;
            end if;
            goto_L149 := goto_L149;
            if (NOT goto_L149) then
              null; -- label L62
              goto_L62 := false;
              -- got reg@0 in r3
              r63 := shlW8(r3);
              r13 := r63;
              goto_L149 := true;
            end if;
            goto_L149 := goto_L149;
          end if;
          goto_L149 := goto_L149;
          if (NOT goto_L149) then
            null; -- label L44
            goto_L44 := false;
            -- got reg@0 in r3
            -- got rand@3 in r34
            r46 := minusW8(r3,r34);
            r13 := r46;
            goto_L149 := true;
          end if;
          goto_L149 := goto_L149;
        end if;
        goto_L149 := goto_L149;
        if (NOT goto_L149) then
          null; -- label L26
          goto_L26 := false;
          -- got reg@0 in r3
          -- got rand@2 in r16
          r28 := plusW8(r3,r16);
          r13 := r28;
          goto_L149 := true;
        end if;
        goto_L149 := goto_L149;
        null; -- label L149
        goto_L149 := false;
        -- end case
        statevar0 := r13;
        goto_L1 := true;
      end if;
      goto_L1 := goto_L1;
      if (NOT goto_L1) then
        null; -- label L0
        goto_L0 := false;
        -- START
        goto_L1 := true;
      end if;
      goto_L1 := goto_L1;
      null; -- label L1
      goto_L1 := false;
      -- loop in
      r3 := statevar0;
      null;
      -- got reg@0 in r3
      r7 := (r3);
      output <= r7;
      state := STATE11;
      -- EXIT
    end if;
  end process;
end behavioral;
