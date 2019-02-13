-- higherKindedDatatypes.hs
module HigherKindedDatatypes where

-- identical to (a,b,c,d)
data Silly a b c d =
  MkSilly a b c d deriving Show

