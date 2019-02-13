-- trivial.hs
module TrivialExcersise where

data Trivial =
  Trivial'
instance Eq Trivial where
  Trivial' == Trivial' = True
