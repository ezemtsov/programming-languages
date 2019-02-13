-- vehicles.hs
module Vehicles where

data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer =
  Mini |
  Mazda |
  Tata
  deriving (Eq, Show)

data Size =
  Size Integer
  deriving (Eq, Show)

data Airline =
  PapuAir |
  CatapultsR'Us |
  TakeYourChanceUnited
  deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price |
  Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 123)

-- ex1: What is the type of myCar?
--      myCar :: Vehicle

-- ex2: Given the following, define the functions:
isCar :: Vehicle -> Bool
isCar (Car _ _)  = True
isCar _          = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- ex3: Now we're going to write a function to tell us
--      the manufacturer of a piece of data:

getManu :: Vehicle -> Manufacturer
getManu (Car a _) = a
getManu _         = error "Has no manufacturer"

-- nullary
data Example0 =
  Example0
  deriving (Eq, Show)

-- unary
data Example1 =
  Example1 Int
  deriving (Eq, Show)

-- product of Int and String
data Example2 =
  Example2 Int String
  deriving (Eq, Show)


