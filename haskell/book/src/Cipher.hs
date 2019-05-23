-- Cipher.hs
module Cipher where

import Data.Char
import Test.QuickCheck

basePos :: Char -> Int
basePos c
  | (isUpper c) = (ord 'A' - 1)
  | otherwise = (ord 'a' - 1)


abc = ['a'..'z'] ++ ['A'..'Z']
abcLen = (length abc) `div` 2


abcPos :: Char -> Int
abcPos c = ord c - basePos c


abcShift :: Char -> Int -> Char
abcShift c p = go c (p `mod` abcLen)
  where go c p
         | (elem c abc) == False = c
         | (abcPos c + p > abcLen) = chr (basePos c - abcLen + abcPos c + p)
         | otherwise = chr (basePos c + abcPos c + p)


caesar :: String -> Int -> String
caesar [] _ = []
caesar s p = go s p
  where go (x:xs) pos =
          (abcShift x pos):(caesar xs pos)


unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar s p = go s (-p)
  where go (x:xs) pos =
          (abcShift x pos):(caesar xs pos)

prop_Caesar a i =
  a == unCaesar (caesar a i) i

test = do
  quickCheck prop_Caesar

-- main' = do
--   putStrLn "Provide a phrase to cipher"
--   input <- getLine
--   putStrLn "Provide a key"
--   key <- getLine
--   putStrLn $ caesar input (digitToInt $ (key !! 0))
