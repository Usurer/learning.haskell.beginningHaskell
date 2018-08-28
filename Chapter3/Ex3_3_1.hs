{-
    I'm gonna leave this file as a memo of my stupidity.
    I've spent a lot of time thinking about proper initial element
    for a foldl/foldr of a Client list, but I've completely
    forgotten about foldl1/folrd1 which work without initial
    element.
    Anyway, keeping this code in a separate file gonna make
    Ex3_3 less crowded.
    And, also, I've changed minumumClient, so it won't accept an 
    empty list.
-}

module Ex3_3_1 where

import ADT_Records

clientName :: Client -> String
clientName client = case client of
    c@(GovOrg {})   ->  name c
    c@(Company {})  ->  name c
    Individual { person = Person { fName = f, lName = l } } 
        -> f ++ " " ++ l

compareClients :: Client -> Client -> (Client, Client)
compareClients x y = 
    if (length (clientName x)) >= (length (clientName y))
    then (x, y)
    else (y, x)

minimumClient :: [Client] -> Client
minimumClient [] = error "Empty list"
minimumClient [x] = x
minimumClient lst = foldl1 (\x y ->  snd $ compareClients x y) lst

client1 = GovOrg "NASA"
client2 = GovOrg "RosCosmos"
client3 = Individual (Person "Bob" "Bobson" Male) False
testData = [client1, client2, client3]

