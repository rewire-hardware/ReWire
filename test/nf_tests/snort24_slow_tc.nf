data React i o a is D a | P o (i -> React i o a) end
data Bit is Zero | One end
data Nil is end
data Char is Char Bit Bit Bit Bit Bit Bit Bit Bit end

not :: Bit -> Bit
is
  \ b -> case b of
         { One -> Zero
         ; Zero -> One
         }
end

and :: Bit -> Bit -> Bit
is
  \a -> \b -> case a of
          {
            Zero -> Zero
            ; One  -> case b of
                      {
                        Zero -> Zero
                        ; One -> One
                      }
          }
end

or :: Bit -> Bit -> Bit
is
  \a -> \b -> case a of
          {
            One    -> One
            ; Zero -> case b of
                      {
                        Zero -> Zero
                        ; One -> One
                      }
          }
end

beq :: Bit  -> Bit  -> Bit
is
 \a -> \b -> case a of
          {
            One -> case b of
                    {
                      One -> One
                      ; Zero -> Zero
                    }
            ; Zero -> case b of
                    {
                      One -> Zero
                      ; Zero -> One
                    }
           }
end

ceq :: Char -> Char -> Bit
is
  \c -> \d -> case c of
                {
                  (Char c0 c1 c2 c3 c4 c5 c6 c7) -> case d of
                                                    {
                                                      (Char d0 d1 d2 d3 d4 d5 d6 d7) -> and  (beq c0 d0)
                                                                                        (and (beq c1 d1)
                                                                                        (and (beq c2 d2)
                                                                                        (and (beq c3 d3)
                                                                                        (and (beq c4 d4)
                                                                                        (and (beq c5 d5)
                                                                                        (and (beq c6 d6)
                                                                                        (beq c7 d7)))))))
                                                    }
                }

end

failstate :: Char -> React Char Bit Nil
is
  \ xxx -> P Zero failstate
end


--End Templating

state0 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state6
 ; (Char  Zero One Zero Zero Zero One One Zero) -> P Zero state175
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state358
 ; (Char  One One Zero Zero Zero Zero One Zero) -> P Zero state370
 ; (Char  One One Zero Zero Zero Zero One Zero) -> P Zero state440
 ; (Char  One One Zero Zero Zero Zero One Zero) -> P Zero state519
 ; (Char  Zero Zero One Zero Zero Zero One Zero) -> P Zero state631
 ; (Char  Zero Zero One Zero Zero Zero One Zero) -> P Zero state689
 ; (Char  Zero One One Zero Zero Zero One Zero) -> P Zero state707
 ; (Char  Zero One One Zero Zero Zero One Zero) -> P Zero state748
 ; (Char  One One One Zero Zero Zero One Zero) -> P Zero state810
 ; (Char  Zero Zero Zero One Zero Zero One Zero) -> P Zero state897
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1004
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1147
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1190
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1297
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1359
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1414
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1528
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1615
 ; (Char  Zero Zero Zero One Zero Zero One Zero) -> P Zero state1798
 ; (Char  Zero Zero Zero One Zero Zero One Zero) -> P Zero state1862
 ; (Char  Zero Zero Zero One Zero Zero One Zero) -> P Zero state1932
 ; (Char  Zero Zero Zero One Zero Zero One Zero) -> P Zero state2006
 ; fail -> P Zero failstate
}
end
state6 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state9
 ; fail -> P Zero failstate
}
end
state9 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state12
 ; fail -> P Zero failstate
}
end
state12 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state15
 ; fail -> P Zero failstate
}
end
state15 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One One One Zero Zero) -> P Zero state18
 ; fail -> P Zero failstate
}
end
state18 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero Zero Zero One Zero) -> P Zero state21
 ; fail -> P Zero failstate
}
end
state21 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state24
 ; fail -> P Zero failstate
}
end
state24 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state27
 ; fail -> P Zero failstate
}
end
state27 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One Zero One Zero) -> P Zero state30
 ; fail -> P Zero failstate
}
end
state30 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state33
 ; fail -> P Zero failstate
}
end
state33 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state36
 ; fail -> P Zero failstate
}
end
state36 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One Zero Zero) -> P Zero state39
 ; fail -> P Zero failstate
}
end
state39 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One Zero Zero) -> P Zero state42
 ; fail -> P Zero failstate
}
end
state42 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state45
 ; fail -> P Zero failstate
}
end
state45 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state48
 ; fail -> P Zero failstate
}
end
state48 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state51
 ; fail -> P Zero failstate
}
end
state51 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One Zero Zero) -> P Zero state54
 ; fail -> P Zero failstate
}
end
state54 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state57
 ; fail -> P Zero failstate
}
end
state57 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state60
 ; fail -> P Zero failstate
}
end
state60 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One Zero Zero) -> P Zero state63
 ; fail -> P Zero failstate
}
end
state63 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero Zero One Zero) -> P Zero state66
 ; fail -> P Zero failstate
}
end
state66 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state69
 ; fail -> P Zero failstate
}
end
state69 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state72
 ; fail -> P Zero failstate
}
end
state72 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state75
 ; fail -> P Zero failstate
}
end
state75 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state78
 ; fail -> P Zero failstate
}
end
state78 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state81
 ; fail -> P Zero failstate
}
end
state81 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state84
 ; fail -> P Zero failstate
}
end
state84 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state87
 ; fail -> P Zero failstate
}
end
state87 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state90
 ; fail -> P Zero failstate
}
end
state90 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One Zero Zero) -> P Zero state93
 ; fail -> P Zero failstate
}
end
state93 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One One Zero One Zero) -> P Zero state96
 ; fail -> P Zero failstate
}
end
state96 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero Zero One Zero) -> P Zero state99
 ; fail -> P Zero failstate
}
end
state99 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One Zero One Zero) -> P Zero state102
 ; fail -> P Zero failstate
}
end
state102 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One One Zero One Zero) -> P Zero state105
 ; fail -> P Zero failstate
}
end
state105 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state110
 ; fail -> P Zero failstate
}
end
state110 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state112
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state115
 ; fail -> P Zero failstate
}
end
state112 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state112
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state115
 ; fail -> P Zero failstate
}
end
state115 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state120
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state120
 ; fail -> P Zero failstate
}
end
state120 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state122
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state125
 ; fail -> P Zero failstate
}
end
state122 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state122
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state125
 ; fail -> P Zero failstate
}
end
state125 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state130
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state130
 ; fail -> P Zero failstate
}
end
state130 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state132
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state135
 ; fail -> P Zero failstate
}
end
state132 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state132
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state135
 ; fail -> P Zero failstate
}
end
state135 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state140
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state140
 ; fail -> P Zero failstate
}
end
state140 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state142
 ; (Char  One Zero One One One Zero One Zero) -> P Zero state145
 ; fail -> P Zero failstate
}
end
state142 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state142
 ; (Char  One Zero One One One Zero One Zero) -> P Zero state145
 ; fail -> P Zero failstate
}
end
state145 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state148
 ; (Char  One Zero One Zero Zero One Zero Zero) -> P Zero state148
 ; fail -> P Zero failstate
}
end
state148 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state151
 ; fail -> P Zero failstate
}
end
state151 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One Zero Zero) -> P Zero state154
 ; fail -> P Zero failstate
}
end
state154 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One One Zero One Zero) -> P Zero state157
 ; fail -> P Zero failstate
}
end
state157 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One Zero One Zero) -> P Zero state160
 ; fail -> P Zero failstate
}
end
state160 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state163
 ; fail -> P Zero failstate
}
end
state163 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state166
 ; fail -> P Zero failstate
}
end
state166 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state169
 ; fail -> P Zero failstate
}
end
state169 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One One Zero One Zero) -> P Zero state170
 ; fail -> P Zero failstate
}
end
state170 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state145 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state148
 ; (Char  One Zero One Zero Zero One Zero Zero) -> P Zero state148
 ; fail -> P Zero failstate
}
end
state135 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state140
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state140
 ; fail -> P Zero failstate
}
end
state125 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state130
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state130
 ; fail -> P Zero failstate
}
end
state115 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state120
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state120
 ; fail -> P Zero failstate
}
end
state175 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state178
 ; fail -> P Zero failstate
}
end
state178 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state181
 ; fail -> P Zero failstate
}
end
state181 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state184
 ; fail -> P Zero failstate
}
end
state184 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One One One Zero Zero) -> P Zero state187
 ; fail -> P Zero failstate
}
end
state187 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state190
 ; fail -> P Zero failstate
}
end
state190 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state193
 ; fail -> P Zero failstate
}
end
state193 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero One One Zero Zero) -> P Zero state196
 ; fail -> P Zero failstate
}
end
state196 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state199
 ; fail -> P Zero failstate
}
end
state199 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state202
 ; fail -> P Zero failstate
}
end
state202 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero One One Zero Zero) -> P Zero state205
 ; fail -> P Zero failstate
}
end
state205 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state208
 ; fail -> P Zero failstate
}
end
state208 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state211
 ; fail -> P Zero failstate
}
end
state211 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero One One Zero Zero) -> P Zero state214
 ; fail -> P Zero failstate
}
end
state214 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero Zero One Zero) -> P Zero state217
 ; fail -> P Zero failstate
}
end
state217 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state220
 ; fail -> P Zero failstate
}
end
state220 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state223
 ; fail -> P Zero failstate
}
end
state223 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state226
 ; fail -> P Zero failstate
}
end
state226 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One One One One Zero) -> P Zero state229
 ; fail -> P Zero failstate
}
end
state229 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state234
 ; fail -> P Zero failstate
}
end
state234 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state236
 ; (Char  Zero Zero Zero Zero One Zero One Zero) -> P Zero state239
 ; fail -> P Zero failstate
}
end
state236 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state236
 ; (Char  Zero Zero Zero Zero One Zero One Zero) -> P Zero state239
 ; fail -> P Zero failstate
}
end
state239 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state242
 ; (Char  Zero One Zero Zero One One One Zero) -> P Zero state242
 ; fail -> P Zero failstate
}
end
state242 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state245
 ; fail -> P Zero failstate
}
end
state245 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state250
 ; fail -> P Zero failstate
}
end
state250 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state252
 ; (Char  Zero One One Zero One One One Zero) -> P Zero state255
 ; fail -> P Zero failstate
}
end
state252 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state252
 ; (Char  Zero One One Zero One One One Zero) -> P Zero state255
 ; fail -> P Zero failstate
}
end
state255 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state260
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state260
 ; fail -> P Zero failstate
}
end
state260 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state262
 ; (Char  One Zero One Zero Zero One Zero Zero) -> P Zero state265
 ; fail -> P Zero failstate
}
end
state262 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state262
 ; (Char  One Zero One Zero Zero One Zero Zero) -> P Zero state265
 ; fail -> P Zero failstate
}
end
state265 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state268
 ; (Char  Zero One Zero Zero One One Zero Zero) -> P Zero state268
 ; fail -> P Zero failstate
}
end
state268 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero Zero One Zero) -> P Zero state271
 ; fail -> P Zero failstate
}
end
state271 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state276
 ; fail -> P Zero failstate
}
end
state276 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state278
 ; (Char  One One Zero Zero One Zero One Zero) -> P Zero state283
 ; fail -> P Zero failstate
}
end
state278 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state278
 ; (Char  One One Zero Zero One Zero One Zero) -> P Zero state283
 ; fail -> P Zero failstate
}
end
state283 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One Zero One Zero) -> P Zero state285
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state288
 ; (Char  One One Zero Zero One Zero One Zero) -> P Zero state285
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state288
 ; fail -> P Zero failstate
}
end
state285 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One Zero One Zero) -> P Zero state285
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state288
 ; fail -> P Zero failstate
}
end
state288 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero Zero One Zero) -> P Zero state291
 ; (Char  One Zero One Zero Zero Zero One Zero) -> P Zero state291
 ; fail -> P Zero failstate
}
end
state291 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state294
 ; fail -> P Zero failstate
}
end
state294 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero One One One Zero) -> P Zero state297
 ; fail -> P Zero failstate
}
end
state297 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state300
 ; fail -> P Zero failstate
}
end
state300 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state303
 ; fail -> P Zero failstate
}
end
state303 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state308
 ; fail -> P Zero failstate
}
end
state308 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state310
 ; (Char  One One One One Zero Zero One Zero) -> P Zero state313
 ; fail -> P Zero failstate
}
end
state310 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state310
 ; (Char  One One One One Zero Zero One Zero) -> P Zero state313
 ; fail -> P Zero failstate
}
end
state313 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state316
 ; (Char  Zero One One One Zero One One Zero) -> P Zero state316
 ; fail -> P Zero failstate
}
end
state316 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state319
 ; fail -> P Zero failstate
}
end
state319 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state322
 ; fail -> P Zero failstate
}
end
state322 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state325
 ; fail -> P Zero failstate
}
end
state325 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state328
 ; fail -> P Zero failstate
}
end
state328 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state331
 ; fail -> P Zero failstate
}
end
state331 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state334
 ; fail -> P Zero failstate
}
end
state334 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero One One Zero Zero) -> P Zero state337
 ; fail -> P Zero failstate
}
end
state337 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state340
 ; fail -> P Zero failstate
}
end
state340 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state343
 ; fail -> P Zero failstate
}
end
state343 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero One One Zero Zero) -> P Zero state346
 ; fail -> P Zero failstate
}
end
state346 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One Zero Zero) -> P Zero state349
 ; fail -> P Zero failstate
}
end
state349 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state352
 ; fail -> P Zero failstate
}
end
state352 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero One One Zero Zero) -> P Zero state353
 ; fail -> P Zero failstate
}
end
state353 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state313 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state316
 ; (Char  Zero One One One Zero One One Zero) -> P Zero state316
 ; fail -> P Zero failstate
}
end
state288 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero Zero One Zero) -> P Zero state291
 ; (Char  One Zero One Zero Zero Zero One Zero) -> P Zero state291
 ; fail -> P Zero failstate
}
end
state283 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One Zero One Zero) -> P Zero state285
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state288
 ; (Char  One One Zero Zero One Zero One Zero) -> P Zero state285
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state288
 ; fail -> P Zero failstate
}
end
state265 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state268
 ; (Char  Zero One Zero Zero One One Zero Zero) -> P Zero state268
 ; fail -> P Zero failstate
}
end
state255 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state260
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state260
 ; fail -> P Zero failstate
}
end
state239 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state242
 ; (Char  Zero One Zero Zero One One One Zero) -> P Zero state242
 ; fail -> P Zero failstate
}
end
state358 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state361
 ; fail -> P Zero failstate
}
end
state361 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero Zero One One Zero) -> P Zero state364
 ; fail -> P Zero failstate
}
end
state364 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state365
 ; fail -> P Zero failstate
}
end
state365 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state370 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state373
 ; fail -> P Zero failstate
}
end
state373 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state376
 ; fail -> P Zero failstate
}
end
state376 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero One One Zero) -> P Zero state379
 ; fail -> P Zero failstate
}
end
state379 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state382
 ; fail -> P Zero failstate
}
end
state382 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state385
 ; fail -> P Zero failstate
}
end
state385 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state388
 ; fail -> P Zero failstate
}
end
state388 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state392
}
 ; (Char  One One One One Zero One One Zero) -> P Zero state395
 ; fail -> P Zero failstate
}
end
state392 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state392
}
 ; (Char  One One One One Zero One One Zero) -> P Zero state395
 ; fail -> P Zero failstate
}
end
state395 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state398
 ; (Char  Zero One Zero Zero One One One Zero) -> P Zero state398
 ; fail -> P Zero failstate
}
end
state398 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state401
 ; fail -> P Zero failstate
}
end
state401 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero Zero One One Zero) -> P Zero state404
 ; fail -> P Zero failstate
}
end
state404 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state407
 ; fail -> P Zero failstate
}
end
state407 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state410
 ; fail -> P Zero failstate
}
end
state410 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One One One Zero Zero) -> P Zero state413
 ; fail -> P Zero failstate
}
end
state413 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state416
 ; fail -> P Zero failstate
}
end
state416 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state419
 ; fail -> P Zero failstate
}
end
state419 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state422
 ; fail -> P Zero failstate
}
end
state422 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state425
 ; fail -> P Zero failstate
}
end
state425 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero Zero One One Zero) -> P Zero state428
 ; fail -> P Zero failstate
}
end
state428 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state431
 ; fail -> P Zero failstate
}
end
state431 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state434
 ; fail -> P Zero failstate
}
end
state434 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state435
 ; fail -> P Zero failstate
}
end
state435 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state395 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state398
 ; (Char  Zero One Zero Zero One One One Zero) -> P Zero state398
 ; fail -> P Zero failstate
}
end
state440 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state443
 ; fail -> P Zero failstate
}
end
state443 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state446
 ; fail -> P Zero failstate
}
end
state446 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero One One Zero) -> P Zero state449
 ; fail -> P Zero failstate
}
end
state449 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state452
 ; fail -> P Zero failstate
}
end
state452 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state455
 ; fail -> P Zero failstate
}
end
state455 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state458
 ; fail -> P Zero failstate
}
end
state458 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state462
}
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state465
 ; fail -> P Zero failstate
}
end
state462 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state462
}
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state465
 ; fail -> P Zero failstate
}
end
state465 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state468
 ; (Char  One One One One Zero One One Zero) -> P Zero state468
 ; fail -> P Zero failstate
}
end
state468 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero One One One Zero) -> P Zero state471
 ; fail -> P Zero failstate
}
end
state471 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state474
 ; fail -> P Zero failstate
}
end
state474 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state477
 ; fail -> P Zero failstate
}
end
state477 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state480
 ; fail -> P Zero failstate
}
end
state480 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One One One Zero Zero) -> P Zero state483
 ; fail -> P Zero failstate
}
end
state483 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero Zero One Zero) -> P Zero state486
 ; fail -> P Zero failstate
}
end
state486 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state489
 ; fail -> P Zero failstate
}
end
state489 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state492
 ; fail -> P Zero failstate
}
end
state492 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state495
 ; fail -> P Zero failstate
}
end
state495 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state498
 ; fail -> P Zero failstate
}
end
state498 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state501
 ; fail -> P Zero failstate
}
end
state501 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state504
 ; fail -> P Zero failstate
}
end
state504 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero Zero Zero One Zero) -> P Zero state507
 ; fail -> P Zero failstate
}
end
state507 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state510
 ; fail -> P Zero failstate
}
end
state510 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state513
 ; fail -> P Zero failstate
}
end
state513 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state514
 ; fail -> P Zero failstate
}
end
state514 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state465 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state468
 ; (Char  One One One One Zero One One Zero) -> P Zero state468
 ; fail -> P Zero failstate
}
end
state519 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state522
 ; fail -> P Zero failstate
}
end
state522 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state525
 ; fail -> P Zero failstate
}
end
state525 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state528
 ; fail -> P Zero failstate
}
end
state528 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state531
 ; fail -> P Zero failstate
}
end
state531 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state534
 ; fail -> P Zero failstate
}
end
state534 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state537
 ; fail -> P Zero failstate
}
end
state537 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state542
 ; fail -> P Zero failstate
}
end
state542 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state544
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state547
 ; fail -> P Zero failstate
}
end
state544 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state544
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state547
 ; fail -> P Zero failstate
}
end
state547 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state552
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state552
 ; fail -> P Zero failstate
}
end
state552 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state554
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state557
 ; fail -> P Zero failstate
}
end
state554 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state554
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state557
 ; fail -> P Zero failstate
}
end
state557 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state560
 ; (Char  One One One One Zero One One Zero) -> P Zero state560
 ; fail -> P Zero failstate
}
end
state560 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state563
 ; fail -> P Zero failstate
}
end
state563 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state566
 ; fail -> P Zero failstate
}
end
state566 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state569
 ; fail -> P Zero failstate
}
end
state569 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state572
 ; fail -> P Zero failstate
}
end
state572 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state575
 ; fail -> P Zero failstate
}
end
state575 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state578
 ; fail -> P Zero failstate
}
end
state578 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state581
 ; fail -> P Zero failstate
}
end
state581 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state584
 ; fail -> P Zero failstate
}
end
state584 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state589
 ; fail -> P Zero failstate
}
end
state589 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state591
 ; (Char  Zero One Zero Zero One One One Zero) -> P Zero state594
 ; fail -> P Zero failstate
}
end
state591 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state591
 ; (Char  Zero One Zero Zero One One One Zero) -> P Zero state594
 ; fail -> P Zero failstate
}
end
state594 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state597
 ; (Char  One Zero One Zero Zero One One Zero) -> P Zero state597
 ; fail -> P Zero failstate
}
end
state597 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state600
 ; fail -> P Zero failstate
}
end
state600 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state603
 ; fail -> P Zero failstate
}
end
state603 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state606
 ; fail -> P Zero failstate
}
end
state606 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state609
 ; fail -> P Zero failstate
}
end
state609 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state612
 ; fail -> P Zero failstate
}
end
state612 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state615
 ; fail -> P Zero failstate
}
end
state615 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state620
 ; fail -> P Zero failstate
}
end
state620 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state622
 ; (Char  Zero Zero One Zero One One One Zero) -> P Zero state625
 ; fail -> P Zero failstate
}
end
state622 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state622
 ; (Char  Zero Zero One Zero One One One Zero) -> P Zero state625
 ; fail -> P Zero failstate
}
end
state625 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state626
 ; (Char  One One One One Zero One One Zero) -> P Zero state626
 ; fail -> P Zero failstate
}
end
state626 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state625 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state626
 ; (Char  One One One One Zero One One Zero) -> P Zero state626
 ; fail -> P Zero failstate
}
end
state594 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state597
 ; (Char  One Zero One Zero Zero One One Zero) -> P Zero state597
 ; fail -> P Zero failstate
}
end
state557 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state560
 ; (Char  One One One One Zero One One Zero) -> P Zero state560
 ; fail -> P Zero failstate
}
end
state547 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state552
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state552
 ; fail -> P Zero failstate
}
end
state631 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state634
 ; fail -> P Zero failstate
}
end
state634 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state637
 ; fail -> P Zero failstate
}
end
state637 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state640
 ; fail -> P Zero failstate
}
end
state640 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state643
 ; fail -> P Zero failstate
}
end
state643 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state646
 ; fail -> P Zero failstate
}
end
state646 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state649
 ; fail -> P Zero failstate
}
end
state649 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero Zero One One Zero) -> P Zero state652
 ; fail -> P Zero failstate
}
end
state652 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state656
}
 ; (Char  One Zero Zero Zero Zero Zero One Zero) -> P Zero state659
 ; fail -> P Zero failstate
}
end
state656 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state656
}
 ; (Char  One Zero Zero Zero Zero Zero One Zero) -> P Zero state659
 ; fail -> P Zero failstate
}
end
state659 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One Zero One Zero) -> P Zero state662
 ; (Char  Zero Zero One Zero One Zero One Zero) -> P Zero state662
 ; fail -> P Zero failstate
}
end
state662 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One Zero One Zero) -> P Zero state665
 ; fail -> P Zero failstate
}
end
state665 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero Zero One Zero) -> P Zero state668
 ; fail -> P Zero failstate
}
end
state668 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero Zero One Zero) -> P Zero state671
 ; fail -> P Zero failstate
}
end
state671 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One Zero One Zero) -> P Zero state674
 ; fail -> P Zero failstate
}
end
state674 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero Zero One Zero) -> P Zero state677
 ; fail -> P Zero failstate
}
end
state677 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero Zero One Zero) -> P Zero state680
 ; fail -> P Zero failstate
}
end
state680 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero Zero One Zero) -> P Zero state683
 ; fail -> P Zero failstate
}
end
state683 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state684
 ; fail -> P Zero failstate
}
end
state684 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state659 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One Zero One Zero) -> P Zero state662
 ; (Char  Zero Zero One Zero One Zero One Zero) -> P Zero state662
 ; fail -> P Zero failstate
}
end
state689 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero Zero One Zero) -> P Zero state692
 ; fail -> P Zero failstate
}
end
state692 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One Zero One Zero) -> P Zero state695
 ; fail -> P Zero failstate
}
end
state695 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero Zero One Zero) -> P Zero state698
 ; fail -> P Zero failstate
}
end
state698 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case (or (ceq (Char  Zero One Zero One One One One Zero) c) (or (ceq (Char  One Zero Zero One One One One Zero) c) (or (ceq (Char  Zero Zero Zero One One One One Zero) c) (or (ceq (Char  One One One Zero One One One Zero) c) (or (ceq (Char  Zero One One Zero One One One Zero) c) (or (ceq (Char  One Zero One Zero One One One Zero) c) (or (ceq (Char  Zero Zero One Zero One One One Zero) c) (or (ceq (Char  One One Zero Zero One One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (or (ceq (Char  One Zero Zero Zero One One One Zero) c) (or (ceq (Char  Zero Zero Zero Zero One One One Zero) c) (or (ceq (Char  One One One One Zero One One Zero) c) (or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  One Zero One One Zero One One Zero) c) (or (ceq (Char  Zero Zero One One Zero One One Zero) c) (or (ceq (Char  One One Zero One Zero One One Zero) c) (or (ceq (Char  Zero One Zero One Zero One One Zero) c) (or (ceq (Char  One Zero Zero One Zero One One Zero) c) (or (ceq (Char  Zero Zero Zero One Zero One One Zero) c) (or (ceq (Char  One One One Zero Zero One One Zero) c) (or (ceq (Char  Zero One One Zero Zero One One Zero) c) (or (ceq (Char  One Zero One Zero Zero One One Zero) c) (or (ceq (Char  Zero Zero One Zero Zero One One Zero) c) (or (ceq (Char  One One Zero Zero Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero Zero One One Zero) c) (or (ceq (Char  One Zero Zero Zero Zero One One Zero) c) (or (ceq (Char  Zero Zero Zero Zero Zero One One Zero) c) (or (ceq (Char  One One One One One Zero One Zero) c) (or (ceq (Char  Zero One One One One Zero One Zero) c) (or (ceq (Char  One Zero One One One Zero One Zero) c) (or (ceq (Char  Zero Zero One One One Zero One Zero) c) (or (ceq (Char  One One Zero One One Zero One Zero) c) (or (ceq (Char  Zero One Zero One One Zero One Zero) c) (or (ceq (Char  One Zero Zero One One Zero One Zero) c) (or (ceq (Char  Zero Zero Zero One One Zero One Zero) c) (or (ceq (Char  One One One Zero One Zero One Zero) c) (or (ceq (Char  Zero One One Zero One Zero One Zero) c) (or (ceq (Char  One Zero One Zero One Zero One Zero) c) (or (ceq (Char  Zero Zero One Zero One Zero One Zero) c) (or (ceq (Char  One One Zero Zero One Zero One Zero) c) (or (ceq (Char  Zero One Zero Zero One Zero One Zero) c) (or (ceq (Char  One Zero Zero Zero One Zero One Zero) c) (or (ceq (Char  Zero Zero Zero Zero One Zero One Zero) c) (or (ceq (Char  One One One One Zero Zero One Zero) c) (or (ceq (Char  Zero One One One Zero Zero One Zero) c) (or (ceq (Char  One Zero One One Zero Zero One Zero) c) (or (ceq (Char  Zero Zero One One Zero Zero One Zero) c) (or (ceq (Char  One One Zero One Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero One Zero Zero One Zero) c) (or (ceq (Char  One Zero Zero One Zero Zero One Zero) c) (or (ceq (Char  Zero Zero Zero One Zero Zero One Zero) c) (or (ceq (Char  One One One Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One One Zero Zero Zero One Zero) c) (or (ceq (Char  One Zero One Zero Zero Zero One Zero) c) (or (ceq (Char  Zero Zero One Zero Zero Zero One Zero) c) (or (ceq (Char  One One Zero Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero Zero Zero Zero One Zero) c) (or (ceq (Char  One Zero Zero Zero Zero Zero One Zero) c) (Zero))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) of
{
Zero  -> P Zero failstate
; One -> P Zero state701
}
 ; fail -> P Zero failstate
}
end
state701 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case (or (ceq (Char  One Zero Zero One One One Zero Zero) c) (or (ceq (Char  Zero Zero Zero One One One Zero Zero) c) (or (ceq (Char  One One One Zero One One Zero Zero) c) (or (ceq (Char  Zero One One Zero One One Zero Zero) c) (or (ceq (Char  One Zero One Zero One One Zero Zero) c) (or (ceq (Char  Zero Zero One Zero One One Zero Zero) c) (or (ceq (Char  One One Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero One Zero Zero One One Zero Zero) c) (or (ceq (Char  One Zero Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero Zero Zero Zero One One Zero Zero) c) (Zero))))))))))) of
{
Zero  -> P Zero failstate
; One -> P Zero state702
}
 ; fail -> P Zero failstate
}
end
state702 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state707 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero Zero One Zero) -> P Zero state710
 ; fail -> P Zero failstate
}
end
state710 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero Zero One Zero) -> P Zero state713
 ; fail -> P Zero failstate
}
end
state713 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero Zero One Zero) -> P Zero state716
 ; fail -> P Zero failstate
}
end
state716 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One Zero One Zero) -> P Zero state719
 ; fail -> P Zero failstate
}
end
state719 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero Zero One Zero) -> P Zero state722
 ; fail -> P Zero failstate
}
end
state722 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One Zero One Zero) -> P Zero state725
 ; fail -> P Zero failstate
}
end
state725 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero Zero One Zero) -> P Zero state728
 ; fail -> P Zero failstate
}
end
state728 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One One One Zero Zero) -> P Zero state731
 ; fail -> P Zero failstate
}
end
state731 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state735
}
 ; (Char  Zero One One One One One Zero Zero) -> P Zero state738
 ; fail -> P Zero failstate
}
end
state735 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state735
}
 ; (Char  Zero One One One One One Zero Zero) -> P Zero state738
 ; fail -> P Zero failstate
}
end
state738 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state741
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state741
 ; fail -> P Zero failstate
}
end
state741 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P One state743
 ; fail -> P One failstate
}
end
state743 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P One state743
 ; fail -> P One failstate
}
end
state738 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state741
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state741
 ; fail -> P Zero failstate
}
end
state748 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One Zero One Zero) -> P Zero state751
 ; fail -> P Zero failstate
}
end
state751 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One Zero One Zero) -> P Zero state754
 ; fail -> P Zero failstate
}
end
state754 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state759
 ; fail -> P Zero failstate
}
end
state759 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state761
 ; (Char  Zero One Zero Zero One Zero One Zero) -> P Zero state764
 ; fail -> P Zero failstate
}
end
state761 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state761
 ; (Char  Zero One Zero Zero One Zero One Zero) -> P Zero state764
 ; fail -> P Zero failstate
}
end
state764 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state767
 ; (Char  One Zero One Zero Zero One One Zero) -> P Zero state767
 ; fail -> P Zero failstate
}
end
state767 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state770
 ; fail -> P Zero failstate
}
end
state770 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state773
 ; fail -> P Zero failstate
}
end
state773 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state776
 ; fail -> P Zero failstate
}
end
state776 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state779
 ; fail -> P Zero failstate
}
end
state779 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state782
 ; fail -> P Zero failstate
}
end
state782 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state785
 ; fail -> P Zero failstate
}
end
state785 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state788
 ; fail -> P Zero failstate
}
end
state788 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state791
 ; fail -> P Zero failstate
}
end
state791 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state794
 ; fail -> P Zero failstate
}
end
state794 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state799
 ; fail -> P Zero failstate
}
end
state799 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state801
 ; (Char  One Zero Zero One Zero One One Zero) -> P Zero state804
 ; fail -> P Zero failstate
}
end
state801 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state801
 ; (Char  One Zero Zero One Zero One One Zero) -> P Zero state804
 ; fail -> P Zero failstate
}
end
state804 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state805
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state805
 ; fail -> P Zero failstate
}
end
state805 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state804 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state805
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state805
 ; fail -> P Zero failstate
}
end
state764 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state767
 ; (Char  One Zero One Zero Zero One One Zero) -> P Zero state767
 ; fail -> P Zero failstate
}
end
state810 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state813
 ; fail -> P Zero failstate
}
end
state813 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state816
 ; fail -> P Zero failstate
}
end
state816 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero Zero One Zero) -> P Zero state819
 ; fail -> P Zero failstate
}
end
state819 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state822
 ; fail -> P Zero failstate
}
end
state822 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state825
 ; fail -> P Zero failstate
}
end
state825 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero Zero One Zero) -> P Zero state828
 ; fail -> P Zero failstate
}
end
state828 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state831
 ; fail -> P Zero failstate
}
end
state831 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state834
 ; fail -> P Zero failstate
}
end
state834 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One Zero One Zero) -> P Zero state837
 ; fail -> P Zero failstate
}
end
state837 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state840
 ; fail -> P Zero failstate
}
end
state840 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero One One One Zero) -> P Zero state843
 ; fail -> P Zero failstate
}
end
state843 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero One Zero One Zero) -> P Zero state846
 ; fail -> P Zero failstate
}
end
state846 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state849
 ; fail -> P Zero failstate
}
end
state849 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state852
 ; fail -> P Zero failstate
}
end
state852 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state855
 ; fail -> P Zero failstate
}
end
state855 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One Zero One Zero) -> P Zero state860
 ; fail -> P Zero failstate
}
end
state860 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state863
 ; fail -> P Zero failstate
}
end
state863 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state868
 ; (Char  Zero One Zero Zero Zero Zero One Zero) -> P Zero state869
 ; fail -> P Zero failstate
}
end
state868 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state872
 ; fail -> P Zero failstate
}
end
state872 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state875
 ; fail -> P Zero failstate
}
end
state875 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state878
 ; fail -> P Zero failstate
}
end
state878 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state881
 ; fail -> P Zero failstate
}
end
state881 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state882
 ; fail -> P Zero failstate
}
end
state882 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state885
 ; fail -> P Zero failstate
}
end
state885 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state888
 ; fail -> P Zero failstate
}
end
state888 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One One One One Zero) -> P Zero state891
 ; fail -> P Zero failstate
}
end
state891 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state892
 ; fail -> P Zero failstate
}
end
state892 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state869 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state872
 ; fail -> P Zero failstate
}
end
state897 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state900
 ; fail -> P Zero failstate
}
end
state900 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state903
 ; fail -> P Zero failstate
}
end
state903 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state906
 ; fail -> P Zero failstate
}
end
state906 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state909
 ; fail -> P Zero failstate
}
end
state909 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state913
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state916
 ; fail -> P Zero failstate
}
end
state913 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state913
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state916
 ; fail -> P Zero failstate
}
end
state916 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state919
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state919
 ; fail -> P Zero failstate
}
end
state919 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state922
 ; fail -> P Zero failstate
}
end
state922 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state925
 ; fail -> P Zero failstate
}
end
state925 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state928
 ; fail -> P Zero failstate
}
end
state928 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero One One One Zero) -> P Zero state931
 ; fail -> P Zero failstate
}
end
state931 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state934
 ; fail -> P Zero failstate
}
end
state934 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state937
 ; fail -> P Zero failstate
}
end
state937 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state940
 ; fail -> P Zero failstate
}
end
state940 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero One One One Zero) -> P Zero state943
 ; fail -> P Zero failstate
}
end
state943 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state946
 ; fail -> P Zero failstate
}
end
state946 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state949
 ; fail -> P Zero failstate
}
end
state949 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state952
 ; fail -> P Zero failstate
}
end
state952 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One One Zero) -> P Zero state955
 ; fail -> P Zero failstate
}
end
state955 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state958
 ; fail -> P Zero failstate
}
end
state958 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state961
 ; fail -> P Zero failstate
}
end
state961 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state964
 ; fail -> P Zero failstate
}
end
state964 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state967
 ; fail -> P Zero failstate
}
end
state967 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state970
 ; fail -> P Zero failstate
}
end
state970 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state973
 ; fail -> P Zero failstate
}
end
state973 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state976
 ; fail -> P Zero failstate
}
end
state976 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state979
 ; fail -> P Zero failstate
}
end
state979 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state982
 ; fail -> P Zero failstate
}
end
state982 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state985
 ; fail -> P Zero failstate
}
end
state985 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
every -> P Zero state986
 ; (Char  One Zero One Zero One Zero One Zero) -> P Zero state992
 ; fail -> P Zero failstate
}
end
state986 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
every -> P Zero state986
 ; (Char  One Zero One Zero One Zero One Zero) -> P Zero state992
 ; fail -> P Zero failstate
}
end
state992 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state995
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state995
 ; fail -> P Zero failstate
}
end
state995 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state998
 ; fail -> P Zero failstate
}
end
state998 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state999
 ; fail -> P Zero failstate
}
end
state999 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state992 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state995
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state995
 ; fail -> P Zero failstate
}
end
state916 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state919
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state919
 ; fail -> P Zero failstate
}
end
state1004 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1007
 ; fail -> P Zero failstate
}
end
state1007 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1010
 ; fail -> P Zero failstate
}
end
state1010 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1013
 ; fail -> P Zero failstate
}
end
state1013 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1016
 ; fail -> P Zero failstate
}
end
state1016 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero One One One Zero) -> P Zero state1019
 ; fail -> P Zero failstate
}
end
state1019 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1022
 ; fail -> P Zero failstate
}
end
state1022 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1025
 ; fail -> P Zero failstate
}
end
state1025 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1028
 ; fail -> P Zero failstate
}
end
state1028 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1031
 ; fail -> P Zero failstate
}
end
state1031 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1034
 ; fail -> P Zero failstate
}
end
state1034 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1037
 ; fail -> P Zero failstate
}
end
state1037 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state1040
 ; fail -> P Zero failstate
}
end
state1040 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1043
 ; fail -> P Zero failstate
}
end
state1043 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1046
 ; fail -> P Zero failstate
}
end
state1046 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1049
 ; fail -> P Zero failstate
}
end
state1049 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1052
 ; fail -> P Zero failstate
}
end
state1052 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1055
 ; fail -> P Zero failstate
}
end
state1055 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1058
 ; fail -> P Zero failstate
}
end
state1058 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1063
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state1111
 ; fail -> P Zero failstate
}
end
state1063 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1066
 ; fail -> P Zero failstate
}
end
state1066 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state1069
 ; fail -> P Zero failstate
}
end
state1069 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1072
 ; fail -> P Zero failstate
}
end
state1072 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1075
 ; fail -> P Zero failstate
}
end
state1075 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1078
 ; fail -> P Zero failstate
}
end
state1078 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1081
 ; fail -> P Zero failstate
}
end
state1081 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1084
 ; fail -> P Zero failstate
}
end
state1084 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1088
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state1091
 ; fail -> P Zero failstate
}
end
state1088 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1088
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state1091
 ; fail -> P Zero failstate
}
end
state1091 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1094
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1094
 ; fail -> P Zero failstate
}
end
state1094 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1097
 ; fail -> P Zero failstate
}
end
state1097 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1100
 ; fail -> P Zero failstate
}
end
state1100 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1103
 ; fail -> P Zero failstate
}
end
state1103 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1106
 ; fail -> P Zero failstate
}
end
state1106 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P One state1108
 ; fail -> P One failstate
}
end
state1108 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P One state1108
 ; fail -> P One failstate
}
end
state1091 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1094
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1094
 ; fail -> P Zero failstate
}
end
state1111 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1114
 ; fail -> P Zero failstate
}
end
state1114 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state1117
 ; fail -> P Zero failstate
}
end
state1117 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1120
 ; fail -> P Zero failstate
}
end
state1120 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1123
 ; fail -> P Zero failstate
}
end
state1123 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1126
 ; fail -> P Zero failstate
}
end
state1126 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state1129
 ; fail -> P Zero failstate
}
end
state1129 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1132
 ; fail -> P Zero failstate
}
end
state1132 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1135
 ; fail -> P Zero failstate
}
end
state1135 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1138
 ; fail -> P Zero failstate
}
end
state1138 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state1141
 ; fail -> P Zero failstate
}
end
state1141 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1142
 ; fail -> P Zero failstate
}
end
state1142 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1147 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1150
 ; fail -> P Zero failstate
}
end
state1150 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1153
 ; fail -> P Zero failstate
}
end
state1153 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero One One Zero) -> P Zero state1156
 ; fail -> P Zero failstate
}
end
state1156 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1159
 ; fail -> P Zero failstate
}
end
state1159 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state1162
 ; fail -> P Zero failstate
}
end
state1162 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1165
 ; fail -> P Zero failstate
}
end
state1165 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1168
 ; fail -> P Zero failstate
}
end
state1168 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero One One Zero) -> P Zero state1171
 ; fail -> P Zero failstate
}
end
state1171 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1174
 ; fail -> P Zero failstate
}
end
state1174 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state1177
 ; fail -> P Zero failstate
}
end
state1177 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1180
 ; fail -> P Zero failstate
}
end
state1180 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1183
 ; fail -> P Zero failstate
}
end
state1183 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P One state1185
 ; fail -> P One failstate
}
end
state1185 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P One state1185
 ; fail -> P One failstate
}
end
state1190 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1193
 ; fail -> P Zero failstate
}
end
state1193 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1196
 ; fail -> P Zero failstate
}
end
state1196 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1199
 ; fail -> P Zero failstate
}
end
state1199 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1202
 ; fail -> P Zero failstate
}
end
state1202 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1205
 ; fail -> P Zero failstate
}
end
state1205 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1208
 ; fail -> P Zero failstate
}
end
state1208 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero Zero One One Zero) -> P Zero state1211
 ; fail -> P Zero failstate
}
end
state1211 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1214
 ; fail -> P Zero failstate
}
end
state1214 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1217
 ; fail -> P Zero failstate
}
end
state1217 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1220
 ; fail -> P Zero failstate
}
end
state1220 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1223
 ; fail -> P Zero failstate
}
end
state1223 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1228
 ; fail -> P Zero failstate
}
end
state1228 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1230
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1233
 ; fail -> P Zero failstate
}
end
state1230 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1230
 ; (Char  One One One One Zero One Zero Zero) -> P Zero state1233
 ; fail -> P Zero failstate
}
end
state1233 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1236
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state1236
 ; fail -> P Zero failstate
}
end
state1236 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1239
 ; fail -> P Zero failstate
}
end
state1239 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state1242
 ; fail -> P Zero failstate
}
end
state1242 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state1245
 ; fail -> P Zero failstate
}
end
state1245 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero One One One Zero) -> P Zero state1248
 ; fail -> P Zero failstate
}
end
state1248 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state1251
 ; fail -> P Zero failstate
}
end
state1251 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1254
 ; fail -> P Zero failstate
}
end
state1254 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1257
 ; fail -> P Zero failstate
}
end
state1257 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1260
 ; fail -> P Zero failstate
}
end
state1260 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1263
 ; fail -> P Zero failstate
}
end
state1263 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1266
 ; fail -> P Zero failstate
}
end
state1266 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1269
 ; fail -> P Zero failstate
}
end
state1269 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1272
 ; fail -> P Zero failstate
}
end
state1272 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1275
 ; fail -> P Zero failstate
}
end
state1275 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1280
 ; fail -> P Zero failstate
}
end
state1280 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1282
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state1285
 ; fail -> P Zero failstate
}
end
state1282 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1282
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state1285
 ; fail -> P Zero failstate
}
end
state1285 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1288
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state1288
 ; fail -> P Zero failstate
}
end
state1288 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero Zero One One Zero) -> P Zero state1291
 ; fail -> P Zero failstate
}
end
state1291 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero Zero One One Zero) -> P Zero state1292
 ; fail -> P Zero failstate
}
end
state1292 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1285 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1288
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state1288
 ; fail -> P Zero failstate
}
end
state1233 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1236
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state1236
 ; fail -> P Zero failstate
}
end
state1297 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1300
 ; fail -> P Zero failstate
}
end
state1300 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1306
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state1319
 ; fail -> P Zero failstate
}
end
state1306 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1309
 ; fail -> P Zero failstate
}
end
state1309 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1312
 ; fail -> P Zero failstate
}
end
state1312 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1315
 ; fail -> P Zero failstate
}
end
state1315 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1316
 ; fail -> P Zero failstate
}
end
state1316 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1319
 ; fail -> P Zero failstate
}
end
state1319 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1322
 ; (Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1322
 ; fail -> P Zero failstate
}
end
state1322 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1325
 ; fail -> P Zero failstate
}
end
state1325 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1328
 ; fail -> P Zero failstate
}
end
state1328 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One Zero Zero) -> P Zero state1332
 ; (Char  One One One One One One Zero Zero) -> P Zero state1335
 ; fail -> P Zero failstate
}
end
state1332 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One One One Zero Zero) -> P Zero state1335
 ; fail -> P Zero failstate
}
end
state1335 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1338
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state1338
 ; fail -> P Zero failstate
}
end
state1338 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1341
 ; fail -> P Zero failstate
}
end
state1341 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1344
 ; fail -> P Zero failstate
}
end
state1344 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1347
 ; fail -> P Zero failstate
}
end
state1347 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1350
 ; fail -> P Zero failstate
}
end
state1350 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1353
 ; fail -> P Zero failstate
}
end
state1353 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One One One Zero Zero) -> P Zero state1354
 ; fail -> P Zero failstate
}
end
state1354 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1335 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1338
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state1338
 ; fail -> P Zero failstate
}
end
state1319 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1322
 ; (Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1322
 ; fail -> P Zero failstate
}
end
state1359 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1362
 ; fail -> P Zero failstate
}
end
state1362 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1365
 ; fail -> P Zero failstate
}
end
state1365 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1368
 ; fail -> P Zero failstate
}
end
state1368 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1371
 ; fail -> P Zero failstate
}
end
state1371 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1374
 ; fail -> P Zero failstate
}
end
state1374 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1377
 ; fail -> P Zero failstate
}
end
state1377 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1380
 ; fail -> P Zero failstate
}
end
state1380 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1383
 ; fail -> P Zero failstate
}
end
state1383 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1386
 ; fail -> P Zero failstate
}
end
state1386 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1389
 ; fail -> P Zero failstate
}
end
state1389 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1392
 ; fail -> P Zero failstate
}
end
state1392 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1395
 ; fail -> P Zero failstate
}
end
state1395 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case (or (ceq (Char  One Zero One Zero Zero One Zero Zero) c) (or (ceq (Char  One One One One One Zero One Zero) c) (or (ceq (Char  One Zero Zero One One One Zero Zero) c) (or (ceq (Char  Zero Zero Zero One One One Zero Zero) c) (or (ceq (Char  One One One Zero One One Zero Zero) c) (or (ceq (Char  Zero One One Zero One One Zero Zero) c) (or (ceq (Char  One Zero One Zero One One Zero Zero) c) (or (ceq (Char  Zero Zero One Zero One One Zero Zero) c) (or (ceq (Char  One One Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero One Zero Zero One One Zero Zero) c) (or (ceq (Char  One Zero Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero Zero Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero One Zero One One Zero One Zero) c) (or (ceq (Char  One Zero Zero One One Zero One Zero) c) (or (ceq (Char  Zero Zero Zero One One Zero One Zero) c) (or (ceq (Char  One One One Zero One Zero One Zero) c) (or (ceq (Char  Zero One One Zero One Zero One Zero) c) (or (ceq (Char  One Zero One Zero One Zero One Zero) c) (or (ceq (Char  Zero Zero One Zero One Zero One Zero) c) (or (ceq (Char  One One Zero Zero One Zero One Zero) c) (or (ceq (Char  Zero One Zero Zero One Zero One Zero) c) (or (ceq (Char  One Zero Zero Zero One Zero One Zero) c) (or (ceq (Char  Zero Zero Zero Zero One Zero One Zero) c) (or (ceq (Char  One One One One Zero Zero One Zero) c) (or (ceq (Char  Zero One One One Zero Zero One Zero) c) (or (ceq (Char  One Zero One One Zero Zero One Zero) c) (or (ceq (Char  Zero Zero One One Zero Zero One Zero) c) (or (ceq (Char  One One Zero One Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero One Zero Zero One Zero) c) (or (ceq (Char  One Zero Zero One Zero Zero One Zero) c) (or (ceq (Char  Zero Zero Zero One Zero Zero One Zero) c) (or (ceq (Char  One One One Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One One Zero Zero Zero One Zero) c) (or (ceq (Char  One Zero One Zero Zero Zero One Zero) c) (or (ceq (Char  Zero Zero One Zero Zero Zero One Zero) c) (or (ceq (Char  One One Zero Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero Zero Zero Zero One Zero) c) (or (ceq (Char  One Zero Zero Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero One One One One Zero) c) (or (ceq (Char  One Zero Zero One One One One Zero) c) (or (ceq (Char  Zero Zero Zero One One One One Zero) c) (or (ceq (Char  One One One Zero One One One Zero) c) (or (ceq (Char  Zero One One Zero One One One Zero) c) (or (ceq (Char  One Zero One Zero One One One Zero) c) (or (ceq (Char  Zero Zero One Zero One One One Zero) c) (or (ceq (Char  One One Zero Zero One One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (or (ceq (Char  One Zero Zero Zero One One One Zero) c) (or (ceq (Char  Zero Zero Zero Zero One One One Zero) c) (or (ceq (Char  One One One One Zero One One Zero) c) (or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  One Zero One One Zero One One Zero) c) (or (ceq (Char  Zero Zero One One Zero One One Zero) c) (or (ceq (Char  One One Zero One Zero One One Zero) c) (or (ceq (Char  Zero One Zero One Zero One One Zero) c) (or (ceq (Char  One Zero Zero One Zero One One Zero) c) (or (ceq (Char  Zero Zero Zero One Zero One One Zero) c) (or (ceq (Char  One One One Zero Zero One One Zero) c) (or (ceq (Char  Zero One One Zero Zero One One Zero) c) (or (ceq (Char  One Zero One Zero Zero One One Zero) c) (or (ceq (Char  Zero Zero One Zero Zero One One Zero) c) (or (ceq (Char  One One Zero Zero Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero Zero One One Zero) c) (or (ceq (Char  One Zero Zero Zero Zero One One Zero) c) (Zero))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1399
}
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state1402
 ; fail -> P Zero failstate
}
end
state1399 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case (or (ceq (Char  One Zero One Zero Zero One Zero Zero) c) (or (ceq (Char  One One One One One Zero One Zero) c) (or (ceq (Char  One Zero Zero One One One Zero Zero) c) (or (ceq (Char  Zero Zero Zero One One One Zero Zero) c) (or (ceq (Char  One One One Zero One One Zero Zero) c) (or (ceq (Char  Zero One One Zero One One Zero Zero) c) (or (ceq (Char  One Zero One Zero One One Zero Zero) c) (or (ceq (Char  Zero Zero One Zero One One Zero Zero) c) (or (ceq (Char  One One Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero One Zero Zero One One Zero Zero) c) (or (ceq (Char  One Zero Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero Zero Zero Zero One One Zero Zero) c) (or (ceq (Char  Zero One Zero One One Zero One Zero) c) (or (ceq (Char  One Zero Zero One One Zero One Zero) c) (or (ceq (Char  Zero Zero Zero One One Zero One Zero) c) (or (ceq (Char  One One One Zero One Zero One Zero) c) (or (ceq (Char  Zero One One Zero One Zero One Zero) c) (or (ceq (Char  One Zero One Zero One Zero One Zero) c) (or (ceq (Char  Zero Zero One Zero One Zero One Zero) c) (or (ceq (Char  One One Zero Zero One Zero One Zero) c) (or (ceq (Char  Zero One Zero Zero One Zero One Zero) c) (or (ceq (Char  One Zero Zero Zero One Zero One Zero) c) (or (ceq (Char  Zero Zero Zero Zero One Zero One Zero) c) (or (ceq (Char  One One One One Zero Zero One Zero) c) (or (ceq (Char  Zero One One One Zero Zero One Zero) c) (or (ceq (Char  One Zero One One Zero Zero One Zero) c) (or (ceq (Char  Zero Zero One One Zero Zero One Zero) c) (or (ceq (Char  One One Zero One Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero One Zero Zero One Zero) c) (or (ceq (Char  One Zero Zero One Zero Zero One Zero) c) (or (ceq (Char  Zero Zero Zero One Zero Zero One Zero) c) (or (ceq (Char  One One One Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One One Zero Zero Zero One Zero) c) (or (ceq (Char  One Zero One Zero Zero Zero One Zero) c) (or (ceq (Char  Zero Zero One Zero Zero Zero One Zero) c) (or (ceq (Char  One One Zero Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero Zero Zero Zero One Zero) c) (or (ceq (Char  One Zero Zero Zero Zero Zero One Zero) c) (or (ceq (Char  Zero One Zero One One One One Zero) c) (or (ceq (Char  One Zero Zero One One One One Zero) c) (or (ceq (Char  Zero Zero Zero One One One One Zero) c) (or (ceq (Char  One One One Zero One One One Zero) c) (or (ceq (Char  Zero One One Zero One One One Zero) c) (or (ceq (Char  One Zero One Zero One One One Zero) c) (or (ceq (Char  Zero Zero One Zero One One One Zero) c) (or (ceq (Char  One One Zero Zero One One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (or (ceq (Char  One Zero Zero Zero One One One Zero) c) (or (ceq (Char  Zero Zero Zero Zero One One One Zero) c) (or (ceq (Char  One One One One Zero One One Zero) c) (or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  One Zero One One Zero One One Zero) c) (or (ceq (Char  Zero Zero One One Zero One One Zero) c) (or (ceq (Char  One One Zero One Zero One One Zero) c) (or (ceq (Char  Zero One Zero One Zero One One Zero) c) (or (ceq (Char  One Zero Zero One Zero One One Zero) c) (or (ceq (Char  Zero Zero Zero One Zero One One Zero) c) (or (ceq (Char  One One One Zero Zero One One Zero) c) (or (ceq (Char  Zero One One Zero Zero One One Zero) c) (or (ceq (Char  One Zero One Zero Zero One One Zero) c) (or (ceq (Char  Zero Zero One Zero Zero One One Zero) c) (or (ceq (Char  One One Zero Zero Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero Zero One One Zero) c) (or (ceq (Char  One Zero Zero Zero Zero One One Zero) c) (Zero))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1399
}
 ; (Char  Zero One One One Zero One Zero Zero) -> P Zero state1402
 ; fail -> P Zero failstate
}
end
state1402 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1405
 ; (Char  One Zero Zero One Zero One One Zero) -> P Zero state1405
 ; fail -> P Zero failstate
}
end
state1405 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1408
 ; fail -> P Zero failstate
}
end
state1408 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1409
 ; fail -> P Zero failstate
}
end
state1409 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1402 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1405
 ; (Char  One Zero Zero One Zero One One Zero) -> P Zero state1405
 ; fail -> P Zero failstate
}
end
state1414 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1417
 ; fail -> P Zero failstate
}
end
state1417 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1420
 ; fail -> P Zero failstate
}
end
state1420 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1423
 ; fail -> P Zero failstate
}
end
state1423 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1426
 ; fail -> P Zero failstate
}
end
state1426 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1429
 ; fail -> P Zero failstate
}
end
state1429 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1432
 ; fail -> P Zero failstate
}
end
state1432 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1435
 ; fail -> P Zero failstate
}
end
state1435 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1438
 ; fail -> P Zero failstate
}
end
state1438 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero One One One Zero) -> P Zero state1443
 ; (Char  Zero One One One Zero One One Zero) -> P Zero state1474
 ; fail -> P Zero failstate
}
end
state1443 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1446
 ; fail -> P Zero failstate
}
end
state1446 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1449
 ; fail -> P Zero failstate
}
end
state1449 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1452
 ; fail -> P Zero failstate
}
end
state1452 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1455
 ; fail -> P Zero failstate
}
end
state1455 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1458
 ; fail -> P Zero failstate
}
end
state1458 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state1461
 ; fail -> P Zero failstate
}
end
state1461 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1464
 ; fail -> P Zero failstate
}
end
state1464 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1467
 ; fail -> P Zero failstate
}
end
state1467 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One One One One Zero) -> P Zero state1470
 ; fail -> P Zero failstate
}
end
state1470 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1471
 ; fail -> P Zero failstate
}
end
state1471 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1474 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1477
 ; fail -> P Zero failstate
}
end
state1477 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1480
 ; fail -> P Zero failstate
}
end
state1480 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1483
 ; fail -> P Zero failstate
}
end
state1483 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero Zero One One Zero) -> P Zero state1486
 ; fail -> P Zero failstate
}
end
state1486 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One One One One Zero) -> P Zero state1489
 ; fail -> P Zero failstate
}
end
state1489 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1492
 ; fail -> P Zero failstate
}
end
state1492 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1495
 ; fail -> P Zero failstate
}
end
state1495 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1498
 ; fail -> P Zero failstate
}
end
state1498 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1501
 ; fail -> P Zero failstate
}
end
state1501 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1504
 ; fail -> P Zero failstate
}
end
state1504 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1507
 ; fail -> P Zero failstate
}
end
state1507 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1510
 ; fail -> P Zero failstate
}
end
state1510 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1513
 ; fail -> P Zero failstate
}
end
state1513 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1516
 ; fail -> P Zero failstate
}
end
state1516 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1519
 ; fail -> P Zero failstate
}
end
state1519 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state1522
 ; fail -> P Zero failstate
}
end
state1522 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1523
 ; fail -> P Zero failstate
}
end
state1523 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1528 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero One One One Zero) -> P Zero state1535
 ; (Char  Zero One One One Zero One One Zero) -> P Zero state1547
 ; (Char  One One One Zero One One One Zero) -> P Zero state1559
 ; (Char  Zero One Zero One Zero One One Zero) -> P Zero state1580
 ; (Char  Zero Zero One Zero One One One Zero) -> P Zero state1590
 ; fail -> P Zero failstate
}
end
state1535 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1538
 ; fail -> P Zero failstate
}
end
state1538 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1541
 ; fail -> P Zero failstate
}
end
state1541 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1542
 ; fail -> P Zero failstate
}
end
state1542 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1597
 ; fail -> P Zero failstate
}
end
state1597 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1600
 ; fail -> P Zero failstate
}
end
state1600 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1603
 ; fail -> P Zero failstate
}
end
state1603 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1606
 ; fail -> P Zero failstate
}
end
state1606 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One One One One Zero) -> P Zero state1609
 ; fail -> P Zero failstate
}
end
state1609 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One One One Zero Zero) -> P Zero state1610
 ; fail -> P Zero failstate
}
end
state1610 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1547 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1550
 ; fail -> P Zero failstate
}
end
state1550 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero One One One Zero) -> P Zero state1553
 ; fail -> P Zero failstate
}
end
state1553 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1554
 ; fail -> P Zero failstate
}
end
state1554 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1597
 ; fail -> P Zero failstate
}
end
state1559 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1562
 ; fail -> P Zero failstate
}
end
state1562 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1565
 ; fail -> P Zero failstate
}
end
state1565 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1568
 ; fail -> P Zero failstate
}
end
state1568 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1571
 ; fail -> P Zero failstate
}
end
state1571 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1574
 ; fail -> P Zero failstate
}
end
state1574 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1575
 ; fail -> P Zero failstate
}
end
state1575 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1597
 ; fail -> P Zero failstate
}
end
state1580 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1583
 ; fail -> P Zero failstate
}
end
state1583 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero One One Zero) -> P Zero state1586
 ; fail -> P Zero failstate
}
end
state1586 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1587
 ; fail -> P Zero failstate
}
end
state1587 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1597
 ; fail -> P Zero failstate
}
end
state1590 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1593
 ; fail -> P Zero failstate
}
end
state1593 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1594
 ; fail -> P Zero failstate
}
end
state1594 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1597
 ; fail -> P Zero failstate
}
end
state1615 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One One One One Zero) -> P Zero state1618
 ; fail -> P Zero failstate
}
end
state1618 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state1621
 ; fail -> P Zero failstate
}
end
state1621 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1624
 ; fail -> P Zero failstate
}
end
state1624 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1627
 ; fail -> P Zero failstate
}
end
state1627 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1630
 ; fail -> P Zero failstate
}
end
state1630 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1633
 ; fail -> P Zero failstate
}
end
state1633 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1636
 ; fail -> P Zero failstate
}
end
state1636 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1639
 ; fail -> P Zero failstate
}
end
state1639 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1642
 ; fail -> P Zero failstate
}
end
state1642 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1645
 ; fail -> P Zero failstate
}
end
state1645 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1648
 ; fail -> P Zero failstate
}
end
state1648 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One Zero Zero) -> P Zero state1651
 ; fail -> P Zero failstate
}
end
state1651 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1658
 ; (Char  Zero One One One Zero One One Zero) -> P Zero state1676
 ; (Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1688
 ; (Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1718
 ; (Char  One One One Zero One One One Zero) -> P Zero state1745
 ; (Char  One One One Zero One One One Zero) -> P Zero state1767
 ; fail -> P Zero failstate
}
end
state1658 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1661
 ; fail -> P Zero failstate
}
end
state1661 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1664
 ; fail -> P Zero failstate
}
end
state1664 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1667
 ; fail -> P Zero failstate
}
end
state1667 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1670
 ; fail -> P Zero failstate
}
end
state1670 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1671
 ; fail -> P Zero failstate
}
end
state1671 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1786
 ; fail -> P Zero failstate
}
end
state1786 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1789
 ; fail -> P Zero failstate
}
end
state1789 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1792
 ; fail -> P Zero failstate
}
end
state1792 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1793
 ; fail -> P Zero failstate
}
end
state1793 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1676 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1679
 ; fail -> P Zero failstate
}
end
state1679 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero One One One Zero) -> P Zero state1682
 ; fail -> P Zero failstate
}
end
state1682 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1683
 ; fail -> P Zero failstate
}
end
state1683 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1786
 ; fail -> P Zero failstate
}
end
state1688 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1691
 ; fail -> P Zero failstate
}
end
state1691 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1694
 ; fail -> P Zero failstate
}
end
state1694 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1697
 ; fail -> P Zero failstate
}
end
state1697 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1700
 ; fail -> P Zero failstate
}
end
state1700 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1703
 ; fail -> P Zero failstate
}
end
state1703 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1706
 ; fail -> P Zero failstate
}
end
state1706 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1709
 ; fail -> P Zero failstate
}
end
state1709 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1712
 ; fail -> P Zero failstate
}
end
state1712 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state1713
 ; fail -> P Zero failstate
}
end
state1713 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1786
 ; fail -> P Zero failstate
}
end
state1718 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1721
 ; fail -> P Zero failstate
}
end
state1721 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1724
 ; fail -> P Zero failstate
}
end
state1724 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1727
 ; fail -> P Zero failstate
}
end
state1727 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1730
 ; fail -> P Zero failstate
}
end
state1730 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1733
 ; fail -> P Zero failstate
}
end
state1733 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1736
 ; fail -> P Zero failstate
}
end
state1736 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero Zero One One One Zero) -> P Zero state1739
 ; fail -> P Zero failstate
}
end
state1739 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1740
 ; fail -> P Zero failstate
}
end
state1740 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1786
 ; fail -> P Zero failstate
}
end
state1745 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1748
 ; fail -> P Zero failstate
}
end
state1748 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1751
 ; fail -> P Zero failstate
}
end
state1751 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1754
 ; fail -> P Zero failstate
}
end
state1754 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1757
 ; fail -> P Zero failstate
}
end
state1757 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1760
 ; fail -> P Zero failstate
}
end
state1760 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1763
 ; fail -> P Zero failstate
}
end
state1763 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One Zero Zero) -> P Zero state1764
 ; fail -> P Zero failstate
}
end
state1764 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1786
 ; fail -> P Zero failstate
}
end
state1767 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1770
 ; fail -> P Zero failstate
}
end
state1770 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1773
 ; fail -> P Zero failstate
}
end
state1773 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1776
 ; fail -> P Zero failstate
}
end
state1776 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1779
 ; fail -> P Zero failstate
}
end
state1779 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1782
 ; fail -> P Zero failstate
}
end
state1782 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1783
 ; fail -> P Zero failstate
}
end
state1783 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1786
 ; fail -> P Zero failstate
}
end
state1798 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1801
 ; fail -> P Zero failstate
}
end
state1801 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1804
 ; fail -> P Zero failstate
}
end
state1804 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1807
 ; fail -> P Zero failstate
}
end
state1807 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state1810
 ; fail -> P Zero failstate
}
end
state1810 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1814
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1817
 ; fail -> P Zero failstate
}
end
state1814 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1814
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1817
 ; fail -> P Zero failstate
}
end
state1817 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1820
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state1820
 ; fail -> P Zero failstate
}
end
state1820 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1823
 ; fail -> P Zero failstate
}
end
state1823 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero One One One Zero) -> P Zero state1826
 ; fail -> P Zero failstate
}
end
state1826 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1829
 ; fail -> P Zero failstate
}
end
state1829 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1832
 ; fail -> P Zero failstate
}
end
state1832 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state1835
 ; fail -> P Zero failstate
}
end
state1835 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1838
 ; fail -> P Zero failstate
}
end
state1838 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state1841
 ; fail -> P Zero failstate
}
end
state1841 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1844
 ; fail -> P Zero failstate
}
end
state1844 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1847
 ; fail -> P Zero failstate
}
end
state1847 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1850
 ; fail -> P Zero failstate
}
end
state1850 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1853
 ; fail -> P Zero failstate
}
end
state1853 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1856
 ; fail -> P Zero failstate
}
end
state1856 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state1857
 ; fail -> P Zero failstate
}
end
state1857 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1817 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1820
 ; (Char  One One Zero Zero Zero One One Zero) -> P Zero state1820
 ; fail -> P Zero failstate
}
end
state1862 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1865
 ; fail -> P Zero failstate
}
end
state1865 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1868
 ; fail -> P Zero failstate
}
end
state1868 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1871
 ; fail -> P Zero failstate
}
end
state1871 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state1874
 ; fail -> P Zero failstate
}
end
state1874 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1878
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1881
 ; fail -> P Zero failstate
}
end
state1878 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1878
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1881
 ; fail -> P Zero failstate
}
end
state1881 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1884
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1884
 ; fail -> P Zero failstate
}
end
state1884 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero Zero One One Zero) -> P Zero state1887
 ; fail -> P Zero failstate
}
end
state1887 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1890
 ; fail -> P Zero failstate
}
end
state1890 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1893
 ; fail -> P Zero failstate
}
end
state1893 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1896
 ; fail -> P Zero failstate
}
end
state1896 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero One One Zero) -> P Zero state1899
 ; fail -> P Zero failstate
}
end
state1899 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1902
 ; fail -> P Zero failstate
}
end
state1902 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state1905
 ; fail -> P Zero failstate
}
end
state1905 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state1908
 ; fail -> P Zero failstate
}
end
state1908 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state1911
 ; fail -> P Zero failstate
}
end
state1911 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero One Zero One One Zero) -> P Zero state1914
 ; fail -> P Zero failstate
}
end
state1914 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One One Zero) -> P Zero state1917
 ; fail -> P Zero failstate
}
end
state1917 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1920
 ; fail -> P Zero failstate
}
end
state1920 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1923
 ; fail -> P Zero failstate
}
end
state1923 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1926
 ; fail -> P Zero failstate
}
end
state1926 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state1927
 ; fail -> P Zero failstate
}
end
state1927 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1881 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1884
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1884
 ; fail -> P Zero failstate
}
end
state1932 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state1935
 ; fail -> P Zero failstate
}
end
state1935 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1938
 ; fail -> P Zero failstate
}
end
state1938 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state1941
 ; fail -> P Zero failstate
}
end
state1941 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state1944
 ; fail -> P Zero failstate
}
end
state1944 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1948
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1951
 ; fail -> P Zero failstate
}
end
state1948 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state1948
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1951
 ; fail -> P Zero failstate
}
end
state1951 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1954
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1954
 ; fail -> P Zero failstate
}
end
state1954 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1957
 ; fail -> P Zero failstate
}
end
state1957 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1960
 ; fail -> P Zero failstate
}
end
state1960 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1963
 ; fail -> P Zero failstate
}
end
state1963 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state1966
 ; fail -> P Zero failstate
}
end
state1966 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1969
 ; fail -> P Zero failstate
}
end
state1969 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero Zero One Zero One One Zero) -> P Zero state1972
 ; fail -> P Zero failstate
}
end
state1972 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1975
 ; fail -> P Zero failstate
}
end
state1975 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero One One One Zero) -> P Zero state1978
 ; fail -> P Zero failstate
}
end
state1978 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1981
 ; fail -> P Zero failstate
}
end
state1981 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero Zero One One Zero) -> P Zero state1984
 ; fail -> P Zero failstate
}
end
state1984 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state1987
 ; fail -> P Zero failstate
}
end
state1987 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state1990
 ; fail -> P Zero failstate
}
end
state1990 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state1993
 ; fail -> P Zero failstate
}
end
state1993 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state1996
 ; fail -> P Zero failstate
}
end
state1996 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state1999
 ; fail -> P Zero failstate
}
end
state1999 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state2002
 ; fail -> P Zero failstate
}
end
state2002 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state2003
 ; fail -> P Zero failstate
}
end
state2003 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state1951 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1954
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state1954
 ; fail -> P Zero failstate
}
end
state2006 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state2009
 ; fail -> P Zero failstate
}
end
state2009 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state2012
 ; fail -> P Zero failstate
}
end
state2012 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state2015
 ; fail -> P Zero failstate
}
end
state2015 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One Zero Zero) -> P Zero state2018
 ; fail -> P Zero failstate
}
end
state2018 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state2022
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state2025
 ; fail -> P Zero failstate
}
end
state2022 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
c -> case not(or (ceq (Char  Zero One One One Zero One One Zero) c) (or (ceq (Char  Zero One Zero Zero One One One Zero) c) (Zero))) of
{
Zero  -> P Zero failstate
; One -> P Zero state2022
}
 ; (Char  One Zero Zero Zero Zero One One Zero) -> P Zero state2025
 ; fail -> P Zero failstate
}
end
state2025 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state2028
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state2028
 ; fail -> P Zero failstate
}
end
state2028 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state2031
 ; fail -> P Zero failstate
}
end
state2031 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state2034
 ; fail -> P Zero failstate
}
end
state2034 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state2037
 ; fail -> P Zero failstate
}
end
state2037 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One Zero One One One Zero) -> P Zero state2040
 ; fail -> P Zero failstate
}
end
state2040 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state2043
 ; fail -> P Zero failstate
}
end
state2043 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state2046
 ; fail -> P Zero failstate
}
end
state2046 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state2049
 ; fail -> P Zero failstate
}
end
state2049 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One Zero One One One Zero) -> P Zero state2052
 ; fail -> P Zero failstate
}
end
state2052 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state2055
 ; fail -> P Zero failstate
}
end
state2055 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state2058
 ; fail -> P Zero failstate
}
end
state2058 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state2061
 ; fail -> P Zero failstate
}
end
state2061 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero One One One One Zero) -> P Zero state2064
 ; fail -> P Zero failstate
}
end
state2064 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state2067
 ; fail -> P Zero failstate
}
end
state2067 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One One Zero One One Zero) -> P Zero state2070
 ; fail -> P Zero failstate
}
end
state2070 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero One Zero One One Zero) -> P Zero state2073
 ; fail -> P Zero failstate
}
end
state2073 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state2076
 ; fail -> P Zero failstate
}
end
state2076 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One One Zero) -> P Zero state2079
 ; fail -> P Zero failstate
}
end
state2079 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero One One One Zero) -> P Zero state2082
 ; fail -> P Zero failstate
}
end
state2082 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One One One Zero One Zero Zero) -> P Zero state2085
 ; fail -> P Zero failstate
}
end
state2085 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero Zero One One Zero) -> P Zero state2088
 ; fail -> P Zero failstate
}
end
state2088 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One One One Zero One One Zero) -> P Zero state2091
 ; fail -> P Zero failstate
}
end
state2091 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One One Zero One One Zero) -> P Zero state2094
 ; fail -> P Zero failstate
}
end
state2094 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
every -> P Zero state2095
 ; (Char  One Zero One Zero One Zero One Zero) -> P Zero state2101
 ; fail -> P Zero failstate
}
end
state2095 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
every -> P Zero state2095
 ; (Char  One Zero One Zero One Zero One Zero) -> P Zero state2101
 ; fail -> P Zero failstate
}
end
state2101 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state2104
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state2104
 ; fail -> P Zero failstate
}
end
state2104 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero One Zero Zero One One Zero) -> P Zero state2107
 ; fail -> P Zero failstate
}
end
state2107 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero One Zero Zero One One One Zero) -> P Zero state2108
 ; fail -> P Zero failstate
}
end
state2108 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One failstate
}
end
state2101 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One One Zero Zero One One One Zero) -> P Zero state2104
 ; (Char  One One Zero Zero One One One Zero) -> P Zero state2104
 ; fail -> P Zero failstate
}
end
state2025 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  Zero Zero One Zero Zero One One Zero) -> P Zero state2028
 ; (Char  Zero Zero One Zero Zero One One Zero) -> P Zero state2028
 ; fail -> P Zero failstate
}
end


main :: React Char Bit Nil
is
P Zero state0
end
