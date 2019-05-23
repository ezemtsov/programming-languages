module Main where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  putStrLn ("Runing main caused an error!\
            \ It was: "
            ++ show e)
  writeFile "bbb" "hi"

main = do
  writeFile "zzz" "hi"
    `catch` handler
