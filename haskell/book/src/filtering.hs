-- filtering.hs
module Filtering where

ex1a = [x | x <- [1..30], (mod x 3 == 0)]
ex1b = filter (\x -> (mod x 3 ==0)) [1..30]

ex2 = length ex1a

myWords :: String -> Char -> [String]
myWords s sc = go s sc []
  where go str sc result
         | str == "" = result
         | otherwise = go
                        (dropWhile (== sc) (dropWhile (/= sc) str))
                        sc
                        (result ++ [(takeWhile (/= sc) str)])
                        
myFilter :: String -> [String]
myFilter s = [x | x <- myWords s ' ',
                  not $ elem x ["a", "the", "an"]]

-- Write your own version of zip and ensure it behaves the same as original
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

-- Do what you did for zip, but now for zipWith:
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

-- Rewrite your zip in terms of the zipWith you wrote
zip'' :: [a] -> [b] -> [(a,b)]
zip'' x y = zipWith' (,) x y 
