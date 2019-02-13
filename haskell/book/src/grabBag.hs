-- grabBag.hs
module GrabBag where

amTh x y z = x * y * z
bmTh x y = \z -> x * y * z
cmTh x = \y -> \z -> x * y * z
dmTh = \x -> \y -> \z -> x * y * z

addOne x = x +1
addOneAnon = \x -> x + 1

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1

addOneIfOddAnon n = case odd n of
  True -> n + 1
  False -> n

addFive x y = (if x > y then y else x) + 5
addFiveAnon = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
mflipanon f = \x -> \y -> f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
