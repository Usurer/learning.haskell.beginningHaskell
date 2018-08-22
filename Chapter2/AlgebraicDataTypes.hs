module AlgebraicDataTypes where

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
              deriving Show

data Person = Person String String Gender
              deriving Show

data Gender = Male | Female | Unknown deriving Show

data Manufacturer = Manufacturer String deriving Show

data TimeTravelDirection = Future | Past | Both deriving Show

data TimeMachine = TimeMachine Manufacturer Integer String TimeTravelDirection Float deriving Show
