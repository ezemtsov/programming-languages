module Main where

import Test.QuickCheck
import Data.Char

import Control.Applicative
import System.Environment (getArgs)
import System.IO

testStr = "MEET AT DAWN"
keyword = "ALLY"

type Shift = Int
type ShiftPair = (Char, Shift)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

key :: [Char]
key = concat $ repeat keyword

shiftPairs :: String -> [ShiftPair]
shiftPairs xs = f <$> raw
  where
    raw = myZip xs key
    f (x, y) = (x, ord y `mod` 26)

codec :: (ShiftPair -> Char) -> String -> String
codec f xs = map f (shiftPairs xs)

ignore = flip elem " :\n-.,"

shift f (x,y)
  | ignore x = x
  | otherwise = chr $ f (ord x) y
    
encode :: String -> String
encode = codec $ shift (+)

decode :: String -> String
decode = codec $ shift (-)

output = encode testStr 

prop_vegenereCipher s =
  s == decode (encode s)

testV :: IO ()
testV = do
  quickCheck prop_vegenereCipher

main :: IO ()
main = do
  args <- getArgs
  file <- openFile (last args) ReadMode
  text <- hGetContents file
  let output
        | elem "-d" args = decode text
        | elem "-e" args = encode text
        | elem "-h" args = "Optins: -d -e"
  hPutStr stdout output
