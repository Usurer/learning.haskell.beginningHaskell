module PatternMatching where
import AlgebraicDataTypes

clientName :: Client -> String
clientName client = case client of
    GovOrg name -> name
    Company name id person responsibility -> name
    Individual person showAds -> 
        case person of
        Person fname lname gender -> fname ++ " " ++ lname

clientName2 :: Client -> String
clientName2 client = case client of
    GovOrg name -> name
    Company name id person responsibility -> name
    Individual (Person fname lname gender) _ -> fname ++ " " ++ lname

companyName :: Client -> Maybe String
companyName client = case client of
    Company name _ _ _  -> Just name
    _                   -> Nothing

fibonacci :: Integer -> Integer
fibonacci n = case n of
    0 -> 0
    1 -> 1
    _ -> fibonacci(n-1) + fibonacci(n-2)

