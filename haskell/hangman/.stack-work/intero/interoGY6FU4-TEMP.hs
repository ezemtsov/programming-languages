module Main where

--import Test.Hspec
import Test.QuickCheck
--import Hangman

genPuzzle ::
  (Arbitrary a,
   Arbitrary b,
   Arbitrary c) =>
  Gen (a, b, c)
genPuzzle = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

main :: IO ()
main = do
  putStrLn "yo"
