module Ex2_5 where
import AlgebraicDataTypes
import PatternMatching

clientGender :: Client -> Maybe Gender
clientGender client = case client of
    Individual (Person _ _ gender) _ -> Just gender
    _                                -> Nothing 

clientsGender :: [Client] -> (Integer, Integer)
clientsGender lst = let h = head lst in 
    if null (tail lst)
    then case clientGender h of 
        Just n -> case n of
                    Male -> (1, 0)
                    Female -> (0, 1)
                    _   -> (0, 0)
    else case clientGender h of
        Just n -> case n of
                    Male -> (males + 1, females)
                    Female -> (males, females + 1)
                    _       -> (males, females)
    where recurse = clientsGender (tail lst)
          males = fst recurse
          females = snd recurse
