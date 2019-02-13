-- dbProcessing.hs
module DbProcessing where

import Data.Time
import Data.Maybe

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1. Write a function that filters for DbDate values and returns
--    a list of the UTCTime values inside them.
isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

getDbDateValue :: DatabaseItem -> Maybe UTCTime
getDbDateValue (DbDate x) = Just x
getDbDateValue _ = Nothing

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbItems = mapMaybe getDbDateValue (filter isDbDate dbItems)

-- 2. Write function that filters for DbNumber values and returns
--    a list of the Integer values inside them.
isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _ = False

getDbNumberValue :: DatabaseItem -> Maybe Integer
getDbNumberValue (DbNumber x) = Just x
getDbNumberValue _ = Nothing

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dbItems = mapMaybe getDbNumberValue (filter isDbNumber dbItems)

-- 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dbItems = maximum $ filterDbDate dbItems

-- 4. Write a function that sums all of the DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb dbItems = sum $ filterDbNumber dbItems

-- 5. Write a function that gets the average of the DbNumber values.
avgDb :: [DatabaseItem] -> Double
avgDb dbItems = (fromIntegral (sumDb dbItems)) /
                (fromIntegral (length $ filterDbNumber dbItems))
