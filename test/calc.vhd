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
    variable goto_L13 : boolean := false;
    variable goto_L26 : boolean := false;
    variable goto_L45 : boolean := false;
    variable goto_L64 : boolean := false;
    variable goto_L82 : boolean := false;
    variable goto_L100 : boolean := false;
    variable goto_L119 : boolean := false;
    variable goto_L138 : boolean := false;
    variable goto_L144 : boolean := false;
    variable goto_L125 : boolean := false;
    variable goto_L106 : boolean := false;
    variable goto_L87 : boolean := false;
    variable goto_L69 : boolean := false;
    variable goto_L51 : boolean := false;
    variable goto_L32 : boolean := false;
    variable goto_L11 : boolean := false;
    variable goto_L157 : boolean := false;
    variable r153 : std_logic_vector(0 to 7) := (others => '0');
    variable b150 : boolean := false;
    variable r148 : std_logic_vector(0 to 2) := (others => '0');
    variable r146 : std_logic_vector(0 to 7) := (others => '0');
    variable r140 : std_logic_vector(0 to 7) := (others => '0');
    variable b136 : boolean := false;
    variable b134 : boolean := false;
    variable b132 : boolean := false;
    variable r130 : std_logic_vector(0 to 2) := (others => '0');
    variable r128 : std_logic_vector(0 to 7) := (others => '0');
    variable b126 : boolean := false;
    variable r121 : std_logic_vector(0 to 7) := (others => '0');
    variable b117 : boolean := false;
    variable b115 : boolean := false;
    variable b113 : boolean := false;
    variable r111 : std_logic_vector(0 to 2) := (others => '0');
    variable r109 : std_logic_vector(0 to 7) := (others => '0');
    variable b107 : boolean := false;
    variable r102 : std_logic_vector(0 to 7) := (others => '0');
    variable b98 : boolean := false;
    variable b96 : boolean := false;
    variable b94 : boolean := false;
    variable r92 : std_logic_vector(0 to 2) := (others => '0');
    variable r90 : std_logic_vector(0 to 7) := (others => '0');
    variable b88 : boolean := false;
    variable r83 : std_logic_vector(0 to 7) := (others => '0');
    variable b80 : boolean := false;
    variable b78 : boolean := false;
    variable b76 : boolean := false;
    variable r74 : std_logic_vector(0 to 2) := (others => '0');
    variable r72 : std_logic_vector(0 to 7) := (others => '0');
    variable b70 : boolean := false;
    variable r65 : std_logic_vector(0 to 7) := (others => '0');
    variable b62 : boolean := false;
    variable b60 : boolean := false;
    variable b58 : boolean := false;
    variable r56 : std_logic_vector(0 to 2) := (others => '0');
    variable r54 : std_logic_vector(0 to 7) := (others => '0');
    variable b52 : boolean := false;
    variable r47 : std_logic_vector(0 to 7) := (others => '0');
    variable b43 : boolean := false;
    variable b41 : boolean := false;
    variable b39 : boolean := false;
    variable r37 : std_logic_vector(0 to 2) := (others => '0');
    variable r35 : std_logic_vector(0 to 7) := (others => '0');
    variable b33 : boolean := false;
    variable r28 : std_logic_vector(0 to 7) := (others => '0');
    variable b24 : boolean := false;
    variable b22 : boolean := false;
    variable b20 : boolean := false;
    variable r18 : std_logic_vector(0 to 2) := (others => '0');
    variable r16 : std_logic_vector(0 to 7) := (others => '0');
    variable b14 : boolean := false;
    variable r10 : std_logic_vector(0 to 10) := (others => '0');
    variable r7 : std_logic_vector(0 to 7) := (others => '0');
    variable r3 : std_logic_vector(0 to 7) := (others => '0');
    variable statevar0 : std_logic_vector(0 to 7) := (others => '0');
    variable state : control_state := STATE0;
  begin
    if clk'event and clk='1' then
      null; -- label L157
      goto_L157 := false;
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
          goto_L32 := (NOT b24);
          null; -- label L32
          goto_L32 := false;
          -- alt exit (no match)
          b33 := true;
          r35 := r10(0 to 7);
          r37 := r10(8 to 10);
          b39 := true;
          b41 := ("001" = r37(0 to 2));
          b43 := (b33 AND (b39 AND b41));
          goto_L45 := b43;
          if (NOT goto_L45) then
            goto_L51 := (NOT b43);
            null; -- label L51
            goto_L51 := false;
            -- alt exit (no match)
            b52 := true;
            r54 := r10(0 to 7);
            r56 := r10(8 to 10);
            b58 := true;
            b60 := ("010" = r56(0 to 2));
            b62 := (b52 AND (b58 AND b60));
            goto_L64 := b62;
            if (NOT goto_L64) then
              goto_L69 := (NOT b62);
              null; -- label L69
              goto_L69 := false;
              -- alt exit (no match)
              b70 := true;
              r72 := r10(0 to 7);
              r74 := r10(8 to 10);
              b76 := true;
              b78 := ("011" = r74(0 to 2));
              b80 := (b70 AND (b76 AND b78));
              goto_L82 := b80;
              if (NOT goto_L82) then
                goto_L87 := (NOT b80);
                null; -- label L87
                goto_L87 := false;
                -- alt exit (no match)
                b88 := true;
                r90 := r10(0 to 7);
                r92 := r10(8 to 10);
                b94 := true;
                b96 := ("100" = r92(0 to 2));
                b98 := (b88 AND (b94 AND b96));
                goto_L100 := b98;
                if (NOT goto_L100) then
                  goto_L106 := (NOT b98);
                  null; -- label L106
                  goto_L106 := false;
                  -- alt exit (no match)
                  b107 := true;
                  r109 := r10(0 to 7);
                  r111 := r10(8 to 10);
                  b113 := true;
                  b115 := ("101" = r111(0 to 2));
                  b117 := (b107 AND (b113 AND b115));
                  goto_L119 := b117;
                  if (NOT goto_L119) then
                    goto_L125 := (NOT b117);
                    null; -- label L125
                    goto_L125 := false;
                    -- alt exit (no match)
                    b126 := true;
                    r128 := r10(0 to 7);
                    r130 := r10(8 to 10);
                    b132 := true;
                    b134 := ("110" = r130(0 to 2));
                    b136 := (b126 AND (b132 AND b134));
                    goto_L138 := b136;
                    if (NOT goto_L138) then
                      goto_L144 := (NOT b136);
                      null; -- label L144
                      goto_L144 := false;
                      -- alt exit (no match)
                      -- final pat
                      r146 := r10(0 to 7);
                      r148 := r10(8 to 10);
                      b150 := true;
                      -- final pat
                      r153 := zeroW8;
                      statevar0 := r153;
                      null;
                      goto_L13 := true;
                    end if;
                    goto_L13 := goto_L13;
                    if (NOT goto_L13) then
                      null; -- label L138
                      goto_L138 := false;
                      -- got reg@0 in r3
                      -- got rand@6 in r128
                      r140 := xorW8(r3,r128);
                      statevar0 := r140;
                      null;
                      goto_L13 := true;
                    end if;
                    goto_L13 := goto_L13;
                  end if;
                  goto_L13 := goto_L13;
                  if (NOT goto_L13) then
                    null; -- label L119
                    goto_L119 := false;
                    -- got reg@0 in r3
                    -- got rand@5 in r109
                    r121 := orW8(r3,r109);
                    statevar0 := r121;
                    null;
                    goto_L13 := true;
                  end if;
                  goto_L13 := goto_L13;
                end if;
                goto_L13 := goto_L13;
                if (NOT goto_L13) then
                  null; -- label L100
                  goto_L100 := false;
                  -- got reg@0 in r3
                  -- got rand@4 in r90
                  r102 := andW8(r3,r90);
                  statevar0 := r102;
                  null;
                  goto_L13 := true;
                end if;
                goto_L13 := goto_L13;
              end if;
              goto_L13 := goto_L13;
              if (NOT goto_L13) then
                null; -- label L82
                goto_L82 := false;
                -- got reg@0 in r3
                r83 := shrW8(r3);
                statevar0 := r83;
                null;
                goto_L13 := true;
              end if;
              goto_L13 := goto_L13;
            end if;
            goto_L13 := goto_L13;
            if (NOT goto_L13) then
              null; -- label L64
              goto_L64 := false;
              -- got reg@0 in r3
              r65 := shlW8(r3);
              statevar0 := r65;
              null;
              goto_L13 := true;
            end if;
            goto_L13 := goto_L13;
          end if;
          goto_L13 := goto_L13;
          if (NOT goto_L13) then
            null; -- label L45
            goto_L45 := false;
            -- got reg@0 in r3
            -- got rand@3 in r35
            r47 := minusW8(r3,r35);
            statevar0 := r47;
            null;
            goto_L13 := true;
          end if;
          goto_L13 := goto_L13;
        end if;
        goto_L13 := goto_L13;
        if (NOT goto_L13) then
          null; -- label L26
          goto_L26 := false;
          -- got reg@0 in r3
          -- got rand@2 in r16
          r28 := plusW8(r3,r16);
          statevar0 := r28;
          null;
          goto_L13 := true;
        end if;
        goto_L13 := goto_L13;
        null; -- label L13
        goto_L13 := false;
        -- end case
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
