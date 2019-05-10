import Data.Word
import Data.Char
import Text.Trifecta
import Text.Parser.Combinators

import Control.Applicative

-- Write a parser for IPv4 addresses

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord)

example1 = [ "172.16.254.1"
           , "204.120.0.15" ]

-- IP: a:b:c:d = a(256)^3 + b(256)^2 + c(256)^1 + d
-- 192(256)^3 + 168(256)^2 + 0(256)^1 + 1 = 3232235512


ip4Parser :: Parser IPAddress
ip4Parser = do
  d <- some $ integer
       <* skipMany (char '.')
  return $ IPAddress $ from256 d

from256 :: [Integer]
        -> Word32
from256 [] = 0
from256 i@(x:xs) = from256 xs +
  fromInteger (x * 256 ^ (length i - 1))


-- Same as before, but IPv6

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)


example2 = [ "0:0:0:0:0:ffff:ac10:fe01"
           , "0:0:0:0:0:ffff:cc78:f"
           , "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
           , "fe80::0202:B3FF:FE1E:8329"
           , "2001:DB8::8:800:200C:417A" ]

hexL = ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']

hex :: Num a
    => Char -> a
hex c = case toUpper c of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  'A' -> 10
  'B' -> 11
  'C' -> 12
  'D' -> 13
  'E' -> 14
  'F' -> 15

ip6Parser :: Parser IPAddress6
ip6Parser = do
  d <- some $ choice
    [ some (oneOf hexL)
    , some (char ':') ]
  let fullForm =
        alpha 4 <$>
        ( addMissing $
          filter (/= ":") d )
      fstPart =
        fromHex $ concat $
        take 4 fullForm
      sndPart =
        fromHex $ concat $
        drop 4 fullForm
  return $ IPAddress6 fstPart sndPart

addMissing :: [String]
           -> [String]
addMissing l =
  let f x b = case x of
        "::" -> (take c $ repeat "0000") ++ b
        _    -> x:b
      c = 8 - length (filter (/= "::") l)
  in foldr f [] l

alpha :: Int
      -> [Char]
      -> [Char]
alpha base l
  | length l == base = l
  | otherwise = alpha base ('0':l)

fromHex :: String
        -> Word64
fromHex [] = 0
fromHex l@(x:xs) =
  fromHex xs +
  (hex x * 16 ^ (length l - 1))


-- Remove the derived Show instances from the IPAddress/IPAd-
-- dress6 types, and write your own Show instance for each
-- type that renders in the typical textual format appropriate
-- to each.

instance Show IPAddress where
  show (IPAddress a) = show a
  
instance Show IPAddress6 where
  show (IPAddress6 fstPart sndPart) = show $
    (fromIntegral fstPart * 2^64) + fromIntegral sndPart


-- Write a function that converts between IPAddress and
-- IPAddress6.

convertIp :: IPAddress
          -> IPAddress6
convertIp (IPAddress a) =
  IPAddress6 0 (fromIntegral a)
