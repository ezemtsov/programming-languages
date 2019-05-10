{-# LANGUAGE BangPatterns #-}

import Debug.Trace (trace)

possiblyKaboom =
  \f -> f fst snd (0, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

possiblyKaboom' b =
  case b of
    True -> fst tup
    False -> snd tup
  where tup = (0, undefined)

--------------------------------------------------

data Test =
    A Test2
  | B Test2
  deriving (Show)

data Test2 =
    C Int
  | D Int
  deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (B (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (D i)) = i

--------------------------------------------------

inc = (+1)

twice = inc . inc

howManyTimes =
  inc (trace "I got eval'd" (1 + 1))

howManyTimes' =
  let onePlusOne =
        trace "I got eval'd" (1 + 1)
  in inc onePlusOne + twice onePlusOne

--------------------------------------------------

x = undefined
y = "blah"
main = do
  print (x `seq` snd (x, y))
