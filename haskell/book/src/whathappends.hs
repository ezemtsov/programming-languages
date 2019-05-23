module WhatHappens where

import Debug.Trace
import Control.Concurrent
import System.IO.Unsafe

myData :: MVar Int
myData = unsafePerformIO newEmptyMVar

main' :: IO ()
main' = do
  putMVar myData 0
  zero <- takeMVar myData
  print zero

blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

main :: IO ()
main = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w
