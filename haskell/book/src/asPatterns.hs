-- asPatterns.hs
module AsPatterns where

import Data.Char
import Data.List

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf a@(x:_) b@(y:_) = and $ foldr (\x base -> elem x b : base) [True] a


capitalizeWords :: String -> [(String, String)]
capitalizeWords =
    map (\a@(x:xs) -> (a, (toUpper x):xs))
  . words

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
  | x == ' '  = capitalizeWord xs
  | otherwise = toUpper x : xs

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c xs = (takeWhile (/=c) xs) : splitOn c (drop 1 $ dropWhile (/='.') xs)

capitalizeParagraph :: String -> String
capitalizeParagraph xs = processed xs ++ "."
  where processed =
            concat
          . intersperse ". "
          . map capitalizeWord
          . splitOn '.'
