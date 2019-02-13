-- typeClassesExcersise.hs
module TypeClassesExcersise where

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

----------------------------------------
chk :: Eq b => (a -> b) -> a -> b -> Bool; chk = \a2b -> \a -> \b -> a2b a == b

----------------------------------------
arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith = \a2b -> \x -> \a -> (a2b a)
