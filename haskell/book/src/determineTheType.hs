{-# LANGUAGE NoMonomorphismRestriction  #-}

-- determineTheType.hs
module DetermineTheType where

-- simple example
example = 1

-- 54 :: Num => Num
a = (* 9) 6

-- (0,"doge") :: Tuple (Num, [Char])
b = head [(0, "doge"),(1,"kitteh")]

-- (0, "doge") :: Tuple (Integer, [Char])
c = head [(0 :: Integer, "doge"),(1,"kitteh")]

-- False :: Bool
d = if False then True else False

-- 5 :: Integer
e = length [1,2,3,4,5]

-- (ERROR or False?) Bool
-- f = (length [1,2,3,4])>(length "TACOCAT")

--

x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord x) => x -> x -> Bool
functionC x y =
  if (x > y) then True else False
