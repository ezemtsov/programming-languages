-- algebraicTypes.hs
module AlgebraicTypes where

-- 1) Given the following datatype we can say:
--     a) Weekday is a type with five data constructors

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

-- 2) An with the same datatype definition in mind, what is
--     the type of the following funciton, f?
--     a) f :: [Char]

f Friday = "Miller Time"

-- 3) Types defined with the data keyword
--     b) must Begin with capital letter

-- 4) The funcion g xs = xs !! (length xs -1)
--     c) delivers the final elemet of xs
