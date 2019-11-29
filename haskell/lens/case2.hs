{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TemplateHaskell   #-}
import           Control.Lens
import           Data.Text    (Text)
import           Test.Hspec

data Address = Address
  { _street :: !Text
  , _city   :: !Text
  }
  deriving (Show)
--makeLenses ''Address
street :: Lens' Address Text
street f addr = (\new -> addr { _street = new }) <$> f (_street addr)
--street = _street
city = _city

data Person = Person
  { _name    :: !Text
  , _address :: !Address
  , _age     :: !Int
  }
  deriving (Show)
--makeLenses ''Person
address :: Lens' Person Address
--address = lens _address $ \obj new -> obj { _address = new }
address f pers = (\new -> pers { _address = new }) <$> f (_address pers)
age :: Lens' Person Int
--age = lens _age $ \obj new -> obj { _age = new }
age f pers = (\new -> pers { _age = new }) <$> f (_age pers)

hollywood :: Text
hollywood = "Hollywood Blvd"

alice :: Person
alice = Person
  { _name = "Alice"
  , _address = Address
      { _street = hollywood
      , _city = "Los Angeles"
      }
  , _age = 30
  }

wilshire :: Text
wilshire = "Wilshire Blvd"

-- FIXME set Alice's street to Wilshire
aliceWilshire :: Person
aliceWilshire = address.street.~wilshire $ alice

getStreet :: Person -> Text
getStreet = view $ address.street

-- | Increase age by 1
birthday :: Person -> Person
birthday = age %~ (+1)

getAge :: Person -> Int
getAge = view age

main :: IO ()
main = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
