-- casePractice.hs
module CasePractice where

functionC x y = if (x > y) then x else y
caseFunctionC x y = case condition of
  True -> x
  False -> y
 where condition = (x > y)

ifEvenAdd2 n = if even n then (n+2) else n
caseIfEvenAdd2 n = case condition of
  True -> n+2
  False -> n
 where condition = even n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
