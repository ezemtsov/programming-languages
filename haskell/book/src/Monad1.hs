import Control.Monad (join)
import Control.Applicative ((*>))

--------------------------------------------------
-- Write bind in terms of fmap and join

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join (fmap f m)

--------------------------------------------------

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

--------------------------------------------------

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "names pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

binbindingAndSequencing' :: IO ()
binbindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn "age pls:" >>
    getLine >>=
    \age ->
      putStrLn ("y helo thar: "
                ++ name ++ " who is: "
                ++ age ++ " years old.")

--------------------------------------------------

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [div x 2, div x 2]
    else [x]

--------------------------------------------------

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck
              (Cow nammy agey weighty)

mkSphericalCow' :: String
                -> Int
                -> Int
                -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String
                 -> Int
                 -> Int
                 -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty ->
        weightCheck (Cow nammy agey weighty)

------------------------------------------------------------

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

doSomething n =
  case f n of
    Nothing -> Nothing
    Just a ->
      case g a of
        Nothing -> Nothing
        Just b ->
          case h b of
            Nothing -> Nothing
            Just c ->
              pure (a, b, c)

--------------------------------------------------
mcomp :: Monad m =>
          (b -> m c)
       -> (a -> m b)
       -> a -> m c
mcomp f g a = join (f <$> (g a))

mcomp'' :: Monad m =>
           (b -> m c)
        -> (a -> m b)
        -> a -> m c
mcomp'' f g a = g a >>= f
