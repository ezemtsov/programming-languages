-- maps.hs
module Maps where

import Data.Bool

-- Will the following expression return a value or be ⊥?
ex1 = take 1 $ map (+1) [undefined, 2, 3]
-- error

-- Will the following expression return a value?
ex2 = take 1 $ map (+1) [1, undefined, 3]
-- [2]

-- Will the following expression return a value?
ex3 = take 2 $ map (+1) [1, undefined, 3]
-- error

-- What does the following mystery function do? What is its type?
-- Describe it (to yourself or a loved one) in standard
-- English and then test it out in the REPL to make sure your
-- were correct.
itIsMystery xs =
  map (\x -> elem x "aeiou") xs
-- takes a string and returns a list of bools
-- where true is vowel and false is consonant

--What will be the result of the following functions:
ex5a = map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

ex5b = map minimum [[1..10], [10..20], [20..30]]
-- [1,10,20]

ex5c = map sum [[1..5], [1..5], [1..5]]
-- [15,15,15]


exp6' = map (\x -> if x == 3 then (-x) else (x)) [1..10]
exp6 = map (\x -> bool x (-x) (x==3)) [1..10]
