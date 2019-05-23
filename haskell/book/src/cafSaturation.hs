module Main where

incInts :: [Integer]
incInts = map (+1) [1..]

main :: IO ()
main = do
  print (incInts !! 1000)
  print (incInts !! 9001)
  print (incInts !! 90010)
  print (incInts !! 9001000)
  print (incInts !! 9501000)
  print (incInts !! 9901000)
