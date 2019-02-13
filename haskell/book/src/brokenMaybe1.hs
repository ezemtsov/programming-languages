-- brokenMaybe1.hs
module BrokenMaybe1 where

f :: Bool -> Maybe Int
f False = 0 :: Int
f _ = Nothing
