module Main where

--import Test.Hspec
import Test.QuickCheck
import Hangman

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

genPuzzle :: Gen Puzzle
genPuzzle = do
  a <- genSafeString
  c <- genSafeString
  return (Puzzle a [] c)

instance Arbitrary Puzzle where
  arbitrary = genPuzzle

prop_fillinCharacter p@(Puzzle word _ _) c =
  check (fillInCharacter p c)
  where check (Puzzle w d g) =
          if (w == word)
             && (elem c g)
             && (if (elem c word) then (elem (Just c) d) else True)
          then True
          else False

main :: IO ()
main = do
  w <- randomWord'
  putStrLn $ "Picking random word: " ++ w
  let p = freshPuzzle w
  putStrLn "Test: fillinCharacter"
  quickCheck $ prop_fillinCharacter p
