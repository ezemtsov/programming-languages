import Data.List (elemIndex)
import Control.Applicative (liftA3)

--------------------------------------------------
-- Exercises: Lookups
-- In the following exercises you will need to use
-- the following terms to make the expressions typecheck:

-- 1)
added :: Maybe Integer
added =
  (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2)
y1 :: Maybe Integer
y1 = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z1 :: Maybe Integer
z1 = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y1 <*> z1

-- 3)
x2 :: Maybe Int
x2 = elemIndex 3 [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x2 <*> y2

-- 4)
xs = [1, 2, 3]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x3 <*> y3

--------------------------------------------------
-- Write an Applicative instance for Identity

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

--------------------------------------------------
-- Write an Applicative instance for Constant

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a
      => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a) (Constant b)= Constant (a <> b)

--------------------------------------------------
-- Given the function and values provided, use (<$>)
-- from Functor, (<*>) and pure from the Applicative
-- typeclass to fill in missing bits of the broken code
-- to make it work.

ex1 = const <$> Just "Hello" <*> pure "World"

ex2 = (,,,) <$> Just 90
  <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

--------------------------------------------------
-- After examining the law, test each of the
-- expressions in the REPL

identityLaw v =
  (==)
  v
  (pure id <*> v)

compositionLaw u v w =
  (==)
  (pure (.) <*> u <*> v <*> w)
  (u <*> (v <*> w))

homomorphismLaw f x =
  (==)
  (pure f <*> pure x)
  (Just (f x)) -- that can be any Applicative

interchangeLaw u y =
  (==)
  (u <*> pure y)
  (pure ($ y) <*> u)


--------------------------------------------------
--------------------------------------------------
-- Given a type that has an instance of Applicative,
-- specialize the types of the methods. Test your
-- specialization in the REPL. One way to do this is
-- to bind aliases of the typeclass methods to more
-- concrete types that have the type we told you to
-- fill in

-- Type []
-- Methods
-- pure :: a -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- Type IO
-- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- Type (,) a
-- Methods
-- pure :: ??
-- (<*>) :: ??

-- Type (->) e
-- I'm too confused

--------------------------------------------------
--------------------------------------------------
-- Write instances for the following datatypes.
-- Confused? Write out what the type should be.
-- Use the checkers library to validate the instances.

data Pair a = Pair a a
  deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
  
instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair a b) =
    Pair (f a) (g b)

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (<*>) (Two f g) (Two x y) =
    Two (f <> x) (g y)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b)
      => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three f g h) (Three x y z) =
    Three (f <> x) (g <> y) (h z)

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a
      => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a b c) (Three' x y z) =
    Three' (a <> x) (b y) (c z)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c)
      => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c d) (Four x y z h) =
    Four (a <> x) (b <> y) (c <> z) (d h)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a)
      => Applicative (Four' a) where
  pure d = Four' mempty mempty mempty d
  (<*>) (Four' a b c d) (Four' x y z h) =
    Four' (a <> x) (b <> y) (c <> z) (d h)

--------------------------------------------------
-- Remember the vowels and stops exercise in the
-- folds chapter? Write the function to generate
-- the possible combinations of three input lists
-- using liftA3 from Control.Applicative

stops :: String
stops = "pbtgkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
