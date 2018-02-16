data Client = GovOrg String
    | Company String Integer String String
    | Individual Person Bool
    deriving Show

data Person = Person String String
    deriving Show

data Gender = Male | Female | Unknown
    deriving Show

{- 
    This is a multiline comment.
-}

data TimeMachine = TimeMachine Manufacturer Int String TimeTravelDirection Float
    deriving Show

data Manufacturer = Manufacturer String
    deriving Show

data TimeTravelDirection = Past | Future | Both
    deriving Show