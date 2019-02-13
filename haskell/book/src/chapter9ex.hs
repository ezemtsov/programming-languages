-- import chapter9ex
module Charpter9ex where

import Data.Char

ex1 :: String -> String
ex1 s = [x | x <- s, isUpper x]

ex2 :: String -> String
ex2 (x:xs) = (toUpper x):xs

ex3 :: String -> String
ex3 "" = ""
ex3 (x:xs) = (toUpper x):(ex3 xs)
