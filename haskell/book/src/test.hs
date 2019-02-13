module Test where

sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")

triple x =
  x * 3

ex2 x =
  pi * (x * x)
