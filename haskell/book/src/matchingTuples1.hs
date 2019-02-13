-- matchingTuples1.hs
module TupleFunctions where

-- thesse have to be the same type because
-- (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- addEmup2 could also be written like so
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

--------------------------------------------------
-- exercises Variety Pack
k (x, y) = x              -- k :: (a, b) -> a
k1 = k ((4 - 1), 10)      -- k :: Integer
k2 = k ("three", (1 + 2)) -- k :: [Char]
k3 = k (3, True)          -- k :: Integer

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
