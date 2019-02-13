module Main where

import Test.Hspec
import Test.QuickCheck
import Hangman

genPuzzle :: Gen Puzzle
genPuzzle = undefined

instance (Arbitrary Puzzle) where
  arbitrary = genPuzzle

main :: IO ()
main = do
  putStrLn "yo"
