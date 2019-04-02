import Data.Foldable
import Data.Monoid

data Identity a =
  Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a =
  Nada | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
  
sum' :: (Foldable t, Num a)
    => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a)
      => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (==a))

minimum' :: (Foldable t, Ord a)
        => t a -> Maybe a
minimum' a =
  foldr takeMin lastElement a
  where lastElement =
          getLast . foldMap (Last . Just) $ a
        takeMin =
          (\x b -> min (Just x) b)

maximum' :: (Foldable t, Ord a)
         => t a -> Maybe a
maximum' a =
  foldr takeMax lastElement a
  where lastElement =
          getLast . foldMap (Last . Just) $ a
        takeMax =
          (\x b -> max (Just x) b)

null' :: (Foldable t) => t a -> Bool
null' a = case firstElement of
  Nothing -> True
  Just _ -> False
  where firstElement =
            getFirst
          . foldMap (First . Just)
          $ a

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (Sum . (\x -> 1))

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (:[])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m)
         => (a -> m) -> t a -> m
foldMap' f a =
  foldr (mappend . f) mempty a
