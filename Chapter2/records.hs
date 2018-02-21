data Client = GovOrg { name :: String }
    | Company { name :: String, companyCode :: Integer, contactPoint :: String, contactPointPosition :: String} 
    | Individual { person :: Person, isInMailist :: Bool }
    deriving Show

data Person = Person { firstName :: String, lastName :: String, gender :: Gender}
    deriving Show

data Gender = Male | Female | Unknown
    deriving Show

data TimeMachine = TimeMachine { manufacturer :: Manufacturer, modelNo :: Int, tmName :: String, travelDirection :: TimeTravelDirection, price :: Float }
    deriving Show

data Manufacturer = Manufacturer { mName :: String }
    deriving Show

data TimeTravelDirection = Past | Future | Both
    deriving Show

-- an example of Record usage

discount :: Float -> [TimeMachine] -> [TimeMachine]
discount percent lst = let 
    discounted p = p * (1 - percent / 100) 
    recurse = discount percent (tail lst)
    in
    if null lst
    then lst
    else case head lst of
        tm@(TimeMachine { price = p }) -> tm { price =  discounted p } : recurse

--  See how I use tm@ - this will bind TimeMachine instance to tm variable - using tm { price =  new price } syntax
--  mind that if I'll simply use TimeMachine { price = p } instead of tm@(...), I won't be able to update it as
--  TimeMachine { price = new Price }

tm_1 = TimeMachine (Manufacturer "Bob") 1 "xxx" Both 10
tm_2 = TimeMachine (Manufacturer "Bob") 1 "xxx" Both 15
tm_3 = TimeMachine (Manufacturer "Bob") 1 "xxx" Both 20

result_1 = discount 10 [tm_1, tm_2, tm_3]