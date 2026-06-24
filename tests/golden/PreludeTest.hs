import ReWire

x :: Bool
x = True

y :: Bool
y = x && x

loop :: Bool -> ReacT Bool Bool Identity ()
loop a = do
  b <- signal ((zookus . zookus) a)
  loop (a && x && y)

zookus :: Bool -> Bool
zookus x = fst (x,x)

start :: ReacT Bool Bool Identity ()
start = loop False

main = undefined
