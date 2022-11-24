data Bit = Zero | One

bitAnd :: Bit -> Bit -> Bit
bitAnd Zero _ = Zero
bitAnd _ x    = x

gloop :: Bit -> Either Bit (Either Bit (Bit,Bit)) -> Bit
gloop y z = case z of
	      Left x              -> x
	      Right (Left x)      -> y
              Right (Right (x,y)) -> bitAnd x (bitAnd One y)

start :: Either Bit (Either Bit (Bit,Bit)) -> Bit
start x = gloop One x
