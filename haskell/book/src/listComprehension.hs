ex1 = [ x ^ y |
        x <- [1..10],
        y <- [2, 3],
        x ^ y < 200]

ex2 = [(x, y) |
       x <- [1, 2, 3],
       y <- [6, 7]]

ex3 = [(x, y) |
       x <- [1, 2, 3],
       y <- ['a', 'b']]

mySqr = [x^2 | x <- [1..10]]

ex4 = [(x, y) |
       x <- mySqr,
       y <- [1..3],
       x < 4]

ex5 = [x | x <- mySqr, rem x 2 == 0]
-- [4, 16, 64, 100]

ex6 = [(x, y) | x <- mySqr,
                y <- mySqr,
                x < 50, y > 50]
-- [(1,64), (4,64), (9,64), (16,64), (25,64) , (36,64), (49,64)] ++
-- [(1,81), (4,81), (9,81), (16,81), (25,81) , (36,81), (49,81)] ++
-- [(1,100), (4,100), (9,100), (16,100), (25,100) , (36,100), (49,100)]

ex7 = take 5 [ (x, y) | x <- mySqr,
                        y <- mySqr,
                        x < 50, y > 50]
-- (1,64),(1,81),(1,100),(4,64),(4,81)

ex8 = [x |
       x <- "Three Letter Acronym",
       elem x ['A'..'Z']]

acro xs = [x | x <- xs,
           elem x ['A'..'Z']]

myString xs = [x | x <- xs, elem x "aeiou"]
-- eoaieeaeeaiaau
-- aioaeoauiaaeiiaio

mySqr' = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

ex9 = [(x,y) | x <- mySqr',
               y <- myCube,
               x < 50, y < 50]

l = sum [1 | x <- ex9]
l' = length ex9

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

bEx1 = [x^y | x <- [1..5],
              y <- [2, undefined]]
-- won't work

bEx2 = take 1 $
       [x^y | x <- [1..5], y <- [2, undefined]]
-- works

bEx3 = sum [1, undefined, 3]
-- won't work

bEx4 = length [1, 2, undefined]
-- works

bEx5 = length $ [1, 2, 3] ++ undefined
-- won't work

bEx6 = length $ filter even [1, 2, 3, undefined]
-- won't work

bEx7 = take 1 $ filter even [1, 3, undefined]
-- won't work

bEx8 = take 1 $ filter odd [1, 3, undefined]
-- works

bEx9 = take 2 $ filter odd [1, 3, undefined]
-- works

bEx10 = take 3 $ filter odd [1, 3, undefined]
-- won't work
