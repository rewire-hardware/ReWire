-- EXPECT-ERROR: Precedence parsing error
import ReWire
infix 4 ===
(===) :: Bool -> Bool -> Bool
(===) a b = a
f :: Bool -> Bool -> Bool -> Bool
f a b c = a === b === c
start :: Bool
start = f True True True
main = undefined
