module Ex2_5 where
import AlgebraicDataTypes
import PatternMatching

clientGender :: Client -> Maybe Gender
clientGender client = case client of
    Individual (Person _ _ gender) _ -> Just gender
    _                                -> Nothing 

clientsGender :: [Client] -> (Integer, Integer)

clientsGender [] = (0, 0)

clientsGender [a] = let res = clientGender a in
    case res of
        Nothing -> (0, 0)
        Just val -> case val of
            Male -> (1, 0)
            Female -> (0, 1)
            _      -> (0, 0)

clientsGender lst = let h = clientsGender [(head lst)] in
    (fst h + males, snd h + females)
    where males = fst (clientsGender (tail lst))
          females = snd (clientsGender (tail lst))


testClient_1 = Individual (Person "Bob" "Bobson" Male) False
testClient_2 = Individual (Person "John" "Jonhsson" Male) False
testClient_3 = Individual (Person "Alice" "Alicedottir" Female) False
testClient_4 = Individual (Person "Jane" "Smith" Female) False
testClient_5 = Individual (Person "Lou" "Black" Unknown) False
testClient_6 = Individual (Person "Jack" "Smith" Male) False
testClient_7 = GovOrg "NASA"
testList = [testClient_1,testClient_2,testClient_3,testClient_4,testClient_5,testClient_6,testClient_7]

