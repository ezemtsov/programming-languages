-- moreFunctionalPatternsEx.hs
module MoreFunctionalPatternsEx where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' x = d
  where xLast = fst (x `divMod` 10)
        d = snd (xLast `divMod` 10)

hunsD x = d
  where xLast = x `div` 100
        d = xLast `mod` 10

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z = case z of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z == True   = x
  | z == False  = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g :: (a -> b) -> (a, c) -> (b, c)
g aTob (a, c) = ((aTob a), c)
