{-
    Data Types similar to ones from AlgebraicDataTypes,
    but these ones use record syntax.
-}
module ADT_Records where

data Client = GovOrg { name :: String }
            | Company { name :: String,
                        id :: Integer,
                        contact :: Person,
                        contactPosition :: String }
            | Individual { person :: Person, sendAds :: Bool }
            deriving Show

data Person = Person {  fName :: String,
                        lName :: String,
                        gender :: Gender }
              deriving Show

data Gender = Male | Female | Unknown deriving Show

data Manufacturer = Manufacturer { mName :: String }
                    deriving Show

data TimeTravelDirection = Future | Past | Both deriving Show

data TimeMachine = TimeMachine {
    manufacturer    :: Manufacturer,
    modelNumber     :: Integer,
    modelName       :: String,
    direction       :: TimeTravelDirection,
    price           :: Float
} deriving Show


