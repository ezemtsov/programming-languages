-- wordnumber.hs
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"

digits :: Int -> [Int]
digits num = go num []
  where go n r
         | (div n 10 == 0) = (mod n 10):r
         | otherwise = go (div n 10) ((mod n 10):r)

wordNumber :: Int -> String
wordNumber n = concat.
               intersperse "-".
               map digitToWord $
               digits n
