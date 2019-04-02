data Constant a b =
  Constant b
  deriving (Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

data Two a b =
  Two a b
  deriving (Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c =
  Three a b c
  deriving (Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b =
  Three' a b b
  deriving (Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = mconcat . fmap f $ [b, c]

data Four' a b =
  Four' a b b b
  deriving (Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = mconcat . fmap f $ [b, c, d]

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
