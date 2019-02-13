module Hangman where

import Control.Monad (forever)
import Data.Char (isLetter)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type WordList = [String]
newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in    l >= minWordLength
             && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show puzzle@(Puzzle _ discovered guessed) =
    (  intersperse ' '
     $ fmap renderPuzzleChar discovered)
    ++ " | Guessed so far: " ++ guessed
    ++ " | Wrong guesses: " ++ (show $ wrongGuesses puzzle)

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle
  w
  [ Nothing | x <- w ]
  []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c =
  elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c =
  elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
         if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c)
           word filledInSoFar

wrongGuesses :: Puzzle -> Integer
wrongGuesses (Puzzle word _ guessed) =
    toInteger
  . length
  . filter ifWrong
  $ guessed
  where
    ifWrong c = not $ elem c word

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       ,alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that \
               \character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the \
               \word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in \
               \the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ _) =
  if (wrongGuesses puzzle) > 7 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case (filter isLetter guess) of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must \
               \be a single character"
