-- folds.hs
module Folds where

ex1 = foldr (*) 1 [1..5]
-- result = 1 * 5 * 4 * 3 * 2 * 1 = 120

ex2 = foldl (flip (*)) 1 [1..3]
-- result = 1 * 1 * 2 * 3 = 6

-- ex3: diffence between foldr and foldl is
-- result: a) foldr, but not foldl, traverses the spine of a list from right to left

-- ex4: Folds are catamorphisms, which means they are generally used to
-- result: d) generate infinite data structures ??

-- ex5: The following are simple folds very similar to wha you've already seen,
-- but each has at least one error. Please fix them and test in your REPL:
exA = foldr (++) "!" ["woot", "WOOT", "woot"]
exB = foldr max 'a' "fear is the little death"
exC = foldr (&&) True [False, True]
exD = foldr (||) False [False, True]
exE = foldl (flip ((++) . show)) "" [1..5]
exF = foldr const 0 [1..5]
exG = foldr const ' ' "tacos"
exH = foldl (flip const) ' ' "burritos"
exI = foldl (flip const) 0 [1..5]
