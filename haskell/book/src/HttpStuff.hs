module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

-- replace with other websites
-- if desired or needed
urls :: [String]
urls = [ "http://worldclockapi.com/api/jsonp/cet/now?callback=mycallback"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
