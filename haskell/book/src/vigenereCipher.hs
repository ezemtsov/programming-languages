-- vigeneeCipher.hs
module VigenereCipher where

import Cipher
import Test.QuickCheck
import Data.Char

testStr = "MEET AT DAWN"
keyword = "ALLY"

type Shift = Int

myZip :: String -> String -> [(Char, Char)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys)
  | x == ' ' = (x,x) : myZip xs (y : ys)
  | otherwise = (x,y) : myZip xs ys

shift :: Char -> Shift
shift x
  | x == head keyword || x == ' ' = 0
  | otherwise = ord x - ord (head keyword)

shiftings :: (Char -> Shift) -> String -> [Shift]
shiftings f x = map (f .snd) $ myZip x keywordStream
  where keywordStream = concat $ repeat keyword

codec :: ((Char, Shift) -> Char) -> String -> String
codec f x = map f valueWithShifts
  where valueWithShifts = zip x $ shiftings shift x

encode :: String -> String
encode = codec (\(x,y) -> chr $ (ord x) + y)

decode :: String -> String
decode = codec (\(x,y) -> chr $ (ord x) - y)

output = encode testStr 

prop_vegenereCipher s =
  s == decode (encode s)

testV :: IO ()
testV = do
  quickCheck prop_vegenereCipher

main :: IO ()
main = do
  putStrLn "Provide a phrase to cipher"
  input <- getLine
  putStrLn "Provide a keyword"
  key <- getLine
  putStrLn $ encode input
