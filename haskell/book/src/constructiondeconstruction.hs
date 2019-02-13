-- constructiondeconstruction.hs
module ConstructionDeconstruction where

-- data GuessWhat =
--   Chickenbutt deriving (Eq, Show)

-- data Id a =
--   MkId a deriving (Eq, Show)

-- data Product a b =
--   Product a b deriving (Eq, Show)

-- data Sum a b =
--     First a
--   | Second b
--   deriving (Eq, Show)

-- data RecordProduct a b =
--   RecordProduct { pfirst :: a
--                 , psecond :: b}
--   deriving (Eq, Show)

-- newtype NumCow =
--   NumCow Int
--   deriving (Eq, Show)

-- newtype NumPig =
--   NumPug Int
--   deriving (Eq, Show)

-- data Farmhouse =
--   Farmhouse NumCow NumPig
--   deriving (Eq, Show)

-- type Farmhouse' = Product NumCow NumPig

-- newtype NumSheep =
--   NumSheep Int
--   deriving (Eq, Show)

-- data BigFarmhouse =
--   BigFarmhouse NumCow NumPig NumSheep
--   deriving (Eq, Show)

-- type BigFarmhouse' =
--   Product NumCow (Product NumPig NumSheep)

-- type Name = String
-- type Age = Int
-- type LovesMud = Bool
-- type PoundsOfWool = Int

-- data CowInfo =
--   CowInfo Name Age
--   deriving (Eq, Show)

-- data PigInfo =
--   PigInfo Name Age LovesMud
--   deriving (Eq, Show)

-- data SheepInfo =
--   SheepInfo Name Age PoundsOfWool
--   deriving (Eq, Show)

-- data Animal =
--     Cow CowInfo
--   | Pig PigInfo
--   | Sheep SheepInfo
--   deriving (Eq, Show)

-- -- Alternately

-- type Animal' =
--   Sum CowInfo (Sum PigInfo SheepInfo)

-- ------------------------------------------------------------

-- data OperatingSystem =
--     GnuPlusLinux
--   | OpenBSDPlusNevermindJustBSDStill
--   | Mac
--   | Windows
--   deriving (Eq, Show)

-- data ProgLang =
--     Haskell
--   | Agda
--   | Idris
--   | PureScript
--   deriving (Eq, Show)

-- data Programmer =
--   Programmer { os :: OperatingSystem
--              , lang :: ProgLang }
--   deriving (Eq, Show)

-- nineToFive :: Programmer
-- nineToFive = Programmer { os = Mac
--                         , lang = Haskell }

-- -- We can reorder stuff
-- -- when we use record syntax
-- feelingWizardly :: Programmer
-- feelingWizardly =
--   Programmer { lang = Agda
--              , os = GnuPlusLinux }

-- allOperatingSystems :: [OperatingSystem]
-- allOperatingSystems =
--   [ GnuPlusLinux
--   , OpenBSDPlusNevermindJustBSDStill
--   , Mac
--   , Windows
--   ]

-- allLanguages :: [ProgLang]
-- allLanguages =
--   [Haskell, Agda, Idris, PureScript]

-- allProgrammers :: [Programmer]
-- allProgrammers = [Programmer os lang | os <- allOperatingSystems,
--                                        lang <- allLanguages]

--------------------------------------------------

-- data ThereYet =
--   There Float Int Bool
--   deriving (Eq, Show)

-- notYet :: Int -> Bool -> ThereYet
-- notYet = nope 25.5

-- notQuite :: Bool -> ThereYet
-- notQuite = notYet 10

-- yusssss :: ThereYet
-- yusssss = notQuite False

--------------------------------------------------

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

-- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) =
  True
isDairyFarmer _ =
  False

data FarmerRec =
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType }
  deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

--------------------------------------------------

data Automobile = Null
                | Car { make :: String
                      , model :: String
                      , year :: Integer }
                  deriving (Eq, Show)


--------------------------------------------------

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- 3 ^ 3

quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes   = Yes
quantFlip1 No    = Yes
quantFlip1 Both  = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes   = Yes
quantFlip2 No    = Yes
quantFlip2 Both  = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes   = Yes
quantFlip3 No    = Yes
quantFlip3 Both  = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes   = Yes
quantFlip4 No    = No
quantFlip4 Both  = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes   = Yes
quantFlip5 No    = No
quantFlip5 Both  = No

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes   = Yes
quantFlip6 No    = No
quantFlip6 Both  = Both

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes   = Yes
quantFlip7 No    = Both
quantFlip7 Both  = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes   = Yes
quantFlip8 No    = Both
quantFlip8 Both  = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes   = Yes
quantFlip9 No    = Both
quantFlip9 Both  = Both

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes   = No
quantFlip10 No    = Yes
quantFlip10 Both  = Yes

quantFlip11 :: Quantum -> Quantum
quantFlip11 Yes   = No
quantFlip11 No    = Yes
quantFlip11 Both  = No

quantFlip12 :: Quantum -> Quantum
quantFlip12 Yes   = No
quantFlip12 No    = Yes
quantFlip12 Both  = Both

quantFlip13 :: Quantum -> Quantum
quantFlip13 Yes   = No
quantFlip13 No    = No
quantFlip13 Both  = Yes

quantFlip14 :: Quantum -> Quantum
quantFlip14 Yes   = No
quantFlip14 No    = No
quantFlip14 Both  = No

quantFlip15 :: Quantum -> Quantum
quantFlip15 Yes   = No
quantFlip15 No    = No
quantFlip15 Both  = Both

quantFlip16 :: Quantum -> Quantum
quantFlip16 Yes   = No
quantFlip16 No    = Both
quantFlip16 Both  = Yes

quantFlip17 :: Quantum -> Quantum
quantFlip17 Yes   = No
quantFlip17 No    = Both
quantFlip17 Both  = No

quantFlip18 :: Quantum -> Quantum
quantFlip18 Yes   = No
quantFlip18 No    = Both
quantFlip18 Both  = Both

quantFlip19 :: Quantum -> Quantum
quantFlip19 Yes   = Both
quantFlip19 No    = Yes
quantFlip19 Both  = Yes

quantFlip20 :: Quantum -> Quantum
quantFlip20 Yes   = Both
quantFlip20 No    = Yes
quantFlip20 Both  = No

quantFlip21 :: Quantum -> Quantum
quantFlip21 Yes   = Both
quantFlip21 No    = Yes
quantFlip21 Both  = Both

quantFlip22 :: Quantum -> Quantum
quantFlip22 Yes   = Both
quantFlip22 No    = No
quantFlip22 Both  = Yes

quantFlip23 :: Quantum -> Quantum
quantFlip23 Yes   = Both
quantFlip23 No    = No
quantFlip23 Both  = No

quantFlip24 :: Quantum -> Quantum
quantFlip24 Yes   = Both
quantFlip24 No    = No
quantFlip24 Both  = Both

quantFlip25 :: Quantum -> Quantum
quantFlip25 Yes   = Both
quantFlip25 No    = Both
quantFlip25 Both  = Yes

quantFlip26 :: Quantum -> Quantum
quantFlip26 Yes   = Both
quantFlip26 No    = Both
quantFlip26 Both  = No

quantFlip27 :: Quantum -> Quantum
quantFlip27 Yes   = Both
quantFlip27 No    = Both
quantFlip27 Both  = Both

--------------------------------------------------

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

--------------------------------------------------

data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- eQuad cardinality = 4

eQuad1 :: Either Quad Quad
eQuad1 = Left One

eQuad2 :: Either Quad Quad
eQuad2 = Left Two

eQuad3 :: Either Quad Quad
eQuad3 = Left Three

eQuad4 :: Either Quad Quad
eQuad4 = Left Four

-- prodQuad cardinality = 4 * 4 = 16

prodQuad1 :: (Quad, Quad)
prodQuad1 = (One, One)

prodQuad2 :: (Quad, Quad)
prodQuad2 = (One, Two)

prodQuad3 :: (Quad, Quad)
prodQuad3 = (One, Three)

prodQuad4 :: (Quad, Quad)
prodQuad4 = (One, Four)

prodQuad5 :: (Quad, Quad)
prodQuad5 = (Two, One)

prodQuad6 :: (Quad, Quad)
prodQuad6 = (Two, Two)

prodQuad7 :: (Quad, Quad)
prodQuad7 = (Two, Three)

prodQuad8 :: (Quad, Quad)
prodQuad8 = (Two, Four)

prodQuad9 :: (Quad, Quad)
prodQuad9 = (Three, One)

prodQuad10 :: (Quad, Quad)
prodQuad10 = (Three, Two)

prodQuad11 :: (Quad, Quad)
prodQuad11 = (Three, Three)

prodQuad12 :: (Quad, Quad)
prodQuad12 = (Three, Four)

prodQuad13 :: (Quad, Quad)
prodQuad13 = (Four, One)

prodQuad14 :: (Quad, Quad)
prodQuad14 = (Four, Two)

prodQuad15 :: (Quad, Quad)
prodQuad15 = (Four, Three)

prodQuad16 :: (Quad, Quad)
prodQuad16 = (Four, Four)

-- funcQuad cardinality = 4 ** 4 = 256

funcQuad1 :: Quad -> Quad
funcQuad1 One = One
funcQuad1 Two = One
funcQuad1 Three = One
funcQuad1 Four = One

-- prodTBool cardinality = 2 * 2 * 2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = (True, True, True)

-- gTwo cardinality = 2 ^ 2 ^ 2 = 2 ^ 4 = 16
gTwo :: Bool -> Bool -> Bool
gTwo True True = True
gTwo True False = True
gTwo False True = True
gTwo False False = True

-- fTwo cardinality = 2 ^ 4 ^ 4 = 2 ^ 16 = 65536
fTwo :: Bool -> Quad -> Quad
fTwo True One = One
fTwo False One = One
fTwo True Two = One
fTwo False Two = One
fTwo True Three = One
fTwo False Three = One
fTwo True Four = One
fTwo False Four = One

