module AlgebraicDataTypes where

data Client = GovOrg String
            | Company String Integer String String
            | Individual Person Bool
              deriving Show

data Person = Person String String Gender
              deriving Show

data Gender = Male | Female | Unknown deriving Show

data Manufacturer = Manufacturer String

data TimeTravelDirection = Future | Past | Both

data TimeMachine = TimeMachine Manufacturer Integer String TimeTravelDirection Float
