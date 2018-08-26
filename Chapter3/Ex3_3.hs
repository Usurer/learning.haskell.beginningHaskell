module Ex3_3 where

import ADT_Records

product1 :: [Integer] -> Integer
product1 [] = 0
product1 (x:xs)
    | null xs = x
    | otherwise = x * (product1 xs)

all1 :: [Bool] -> Bool
all1 [] = error "List can't be empty"
all1 (x:xs)
    | null xs = x
    | otherwise = x && (all1 xs)


clientName :: Maybe Client -> String
clientName client = case client of
    Nothing     -> ""
    Just c@(GovOrg {})   ->  name c
    Just c@(Company {})  ->  name c
    Just Individual { person = Person { fName = f, lName = l } } 
        -> f ++ " " ++ l

minimumClient :: [Client] -> Maybe Client
minimumClient [] = Nothing
minimumClient [x] = Just x
minimumClient (x:xs) = let 
    current = clientName (Just x)
    others = clientName $ minimumClient xs
    in
    if (length current < length others)
    then Just x
    else minimumClient xs

client1 = GovOrg "NASA"
client2 = GovOrg "RosCosmos"
client3 = Individual (Person "Bob" "Bobson" Male) False
testData = [client1, client2, client3]

