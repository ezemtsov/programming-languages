-- quantumbool.hs
module QuantumBool where

data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)

-- data TwoQs =
--   MkTwoQs QuantumBool QuantumBool
--   deriving (Eq, Show)

type TwoQs = (QuantumBool, QuantumBool)

data Person =
  Person { name :: String
           , age :: Int }
  deriving (Eq, Show)

-- sample data
jm = Person "julie" 108
ca = Person "chris" 16

namae :: Person -> String
namae (Person s _) = s

-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show

-- data BookType = FictionBook Fiction
--               | NonfictionBook Nonfiction
--               deriving Show

type AuthorName = String

data Author =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

-- data Expr =
--     Number Int
--   | Add Expr Expr
--   | Minus Expr
--   | Mult Expr
--   | Divide Expr Expr

type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

type Expr =
  Either Number
   (Either Add
     (Either Minus
       (Either Mult Divide)))
