module FizzBuzz where


--------------------------------------------------
-- Those are only needed for State version of FB

import Control.Monad
import Control.Monad.Trans.State

--------------------------------------------------
-- Normal FizzBuzz

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

--------------------------------------------------
-- FizzBuzz using State monad

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer
          -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

--------------------------------------------------
-- Exercise for fizzBuzz

fizzbuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzbuzzFromTo from to = fizzbuzzList [from..to]

--------------------------------------------------

main :: IO ()
main =
  mapM_ putStrLn $ reverse $ fizzbuzzFromTo 1 100
