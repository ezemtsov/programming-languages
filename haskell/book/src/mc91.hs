-- mc91.hs
module McCarthy where

mc91 :: Integral a => a -> a
mc91 n = go n
  where go n
          | n > 100 = n - 10
          | otherwise = mc91(mc91(n+11))
