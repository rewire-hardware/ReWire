data Bit = Zero | One

bitAnd :: Bit -> Bit -> Bit
bitAnd Zero _ = Zero
bitAnd _ x    = x

gloop :: Either Bit (Either Bit (Bit,Bit)) -> Bit
gloop (Left x)              = x
gloop (Right (Left x))      = x
gloop (Right (Right (x,y))) = bitAnd x (bitAnd One y)

start :: Either Bit (Either Bit (Bit,Bit)) -> Bit
start x = gloop x
