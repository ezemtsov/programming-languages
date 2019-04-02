import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err


-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the
-- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer",
-- that also has IO
makeIoOnlyObj :: [SomeObj]
              -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn' :: Query
           -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- after
pipelineFn :: Query
           -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn =
  (traverse makeIoOnlyObj
  . traverse decodeFn =<<) . fetchFn


--------------------------------------------------

data Either' a b =
    Left' a
  | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x)  = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
  pure           = Right'
  Left' e <*> _  = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y

  foldr _ z (Left' _) = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y

--------------------------------------------------

-- Traversable Laws

-- 1 Naturality
-- t . traverse f = traverse (t . f)

-- 2 Identity
-- traverse Identity = Identity

-- 3 Composition
 -- traverse (Compose . fmap g . f) =
  -- Compose . fmap (traverse g) . traverse f

-- SequenceA Laws

-- 1 Naturality
-- t. sequenceA = sequenceA . fmap t

-- 2 Identity
-- sequenceA . fmap Identity = Identity

-- 3 Composition
-- sequenceA . fmap Compose =
 -- Compose . fmap sequenceA . sequenceA

type TI = Bigger Int

main = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)


--------------------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ Nada)
              ,(5, return $ Yep a)]

instance EqProp a => EqProp (Optional a) where
  Nada =-= Nada = property True
  Yep a =-= Yep b = a =-= b
  _ =-= _ = property False


--------------------------------------------------

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a b) = f a <> (foldMap f b)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a b) =
    fmap Cons (f a) <*> traverse f b

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nil)
              ,(10, return $ Cons a b)]

instance Eq a => EqProp (List a) where
  Nil =-= Nil = property True
  Cons a b =-= Cons a' b' =
    if (a == a' && b == b')
    then property True
    else property False
  _ =-= _ = property False

--------------------------------------------------

data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> (f c)

instance (Arbitrary a
         ,Arbitrary b
         ,Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) =>
         EqProp (Three a b c) where
  (=-=) = eq

--------------------------------------------------

data Pair a b =
  Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a
         ,Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b)
       => EqProp (Pair a b) where
  (=-=) = eq

--------------------------------------------------

data Big a b =
  Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big a b c) = f b <> f c

instance Traversable (Big a) where
  traverse f (Big a b c) = Big a <$> f b <*> f c

instance ( Arbitrary a
         , Arbitrary b ) =>
         Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Big a b c

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

--------------------------------------------------

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

instance ( Arbitrary a
         , Arbitrary b ) =>
         Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Bigger a b c d

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq


--------------------------------------------------

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l1 a l2) =
    Node (fmap f l1) (f a) (fmap f l2)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l1 a l2) =
    foldMap f l1 <> f a <> foldMap f l2

  foldr f b Empty = b
  foldr f b (Leaf a) = f a b
  foldr f b (Node l1 a l2) =
    f a (foldr f (foldr f b l2) l1)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) =
    Node
    <$> traverse f l
    <*> f a
    <*> traverse f r

