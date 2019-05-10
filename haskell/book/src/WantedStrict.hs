{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module WantedStrict where

import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Text.RawString.QQ

sectionJson :: BS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

main = do
  let blah :: Maybe Value
      blah = decodeStrict sectionJson
  print blah
