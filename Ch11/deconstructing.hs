module Deconstructing where

newtype Name  = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec { name :: Name
                           , acres :: Acres
                           , farmerType :: FarmerType }
                           deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
-- we could do the below if we derived Eq
--isDairyFarmerRec farmer = farmerType farmer == DairyFarmer
isDairyFarmerRec farmer = case farmerType farmer of
                            DairyFarmer -> True
                            _ -> False

isDairyFarmerRec' :: FarmerRec -> Bool
isDairyFarmerRec' (FarmerRec _ _ DairyFarmer) = True
isDairyFarmerRec' _ = False

-- example of what NOT to do!
--data Automobile = Null
--                | Car { make :: String
--                      , model :: String
--                      , year :: Integer } 
--                deriving (Eq, Show)
-- using a record accessor on the null case would result in bottom:
-- year Null
--
-- book's advice: when using a product type that has record accessors,
-- keep it separate from any sum type that wraps it

-- this is, per the book, how it should be done instead
data Car = Car { make :: String
               , model :: String
               , year :: Integer } 
               deriving (Eq, Show)

-- using Null at all is still suboptimal, but this is a better structure
-- than the commented-out one above because we broke out the product
-- so it has its own type constructor
data Automobile = Null
                | Automobile Car
                deriving (Eq, Show)

-- now, rather than throwing a runtime error, `make Null` would cause
-- a compile-time error
