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


{-
I wanted second param to be Integer, but it causes error messages
It seems that direct cast would be required.
So I'd better keep it as Float
-}
decreasePrice :: TimeMachine -> Float -> TimeMachine
decreasePrice tm percent = case tm of
    TimeMachine a b c d price -> TimeMachine a b c d (price * (1 - percent/100))

decreasePriceList :: [TimeMachine] -> Float -> [TimeMachine]

decreasePriceList [] p = []
decreasePriceList [a] p = [decreasePrice a p]
decreasePriceList lst p = (decreasePrice (head lst) p) : (decreasePriceList (tail lst) p)

testMachine_1 = TimeMachine (Manufacturer "NASA") 1 "dsf" Future 9.99
testMachine_2 = TimeMachine (Manufacturer "NASA") 1 "dsf" Future 10
testMachine_3 = TimeMachine (Manufacturer "NASA") 1 "dsf" Future 12.3
