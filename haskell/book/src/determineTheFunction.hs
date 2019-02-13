-- determineTheFunction.hs
module DetermineTheFunction where

myFunc :: (x -> y)
  -> (y -> z)
  -> c
  -> (a, x)
  -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ $ xToY x)

i :: a -> a
i = \x -> x

c :: a -> b -> a
c = \a -> \b -> a

c'' :: b -> a -> b
c'' = \b -> \a -> b

c' :: a -> b -> b
c' = \a -> \b -> b

r :: [a] -> [a]
r = \a -> a ++ a

co :: (b -> c) -> (a -> b) -> a -> c
co =  \bToc -> \aTob -> \a -> bToc $ aTob a

a :: (a -> c) -> a -> a
a = \aToc -> \a -> a

a' :: (a -> b) -> a -> b
a' = \aTob -> \a -> aTob a


