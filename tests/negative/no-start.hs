-- EXPECT-ERROR: no definition for the start symbol (Main.start)
import ReWire

notStart :: Bool -> Bool
notStart x = x

main = undefined
