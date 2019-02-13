-- recursive.hs
module Recursive where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
  fibonacci (x - 1) + fibonacci (x - 2)

data DividedResult =
    Result (Integer, Integer)
  | DividedByZero
  
dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = Result (count, n)
         | otherwise =
             go (n - d) d (count + 1)
             
-- dividedBy 15 2 = go 15 2 0
-- 15 < 2 ? go 13 2 1
-- 13 < 2 ? go 11 2 2
-- 11 < 2 ? go  9 2 3
-- 9  < 2 ? go  7 2 4
-- 7  < 2 ? go  5 2 5
-- 5  < 2 ? go  3 2 6
-- 3  < 2 ? go  3 2 7
-- 1  < 2 ? (7, 1)

arithProg :: (Eq a, Num a) => a -> a
arithProg 1 = 1
arithProg n = n + arithProg (n - 1)

5 * 2 = 5 + 5

multipliedBy :: (Integral a) => a -> a -> a
multipliedBy _ 0 = 0
multipliedBy m b = m + multipliedBy m (b - 1)
