module SimpleFunctions where

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

data Client 	= GovOrg String
		| Company String Integer String String
		| Individual Person
		deriving Show

data Person = Person String String Gender deriving Show

data Gender = Male | Female | Unknown deriving Show

data Manufacturer = Manufacturer String deriving Show
data TravelDirection = Past | Future | Both deriving Show
data TimeMachine = TimeMachine { manufacturer :: Manufacturer
	, model :: Integer
	, name :: String
	, direction :: TravelDirection
	, price :: Float }
	deriving Show

clientName :: Client -> Maybe String
clientName client = case client of 
	GovOrg name -> Nothing
	Company name _ _ _ -> Nothing
	Individual (Person fName lName _) -> Just (fName ++ " " ++ lName)

companyName client = case client of 
	Company name _ _ _ -> Just name
	_ -> Nothing

genderDiversion :: [Client] -> (Integer, Integer)
genderDiversion lst  = if null lst
	then (0, 0)
	else if null (tail lst)
		then d
		else (fst d + males, snd d + females)
	where 	males = fst (genderDiversion (tail lst))
		females = snd (genderDiversion (tail lst))
		d = case (head (lst)) of
			Individual (Person _ _ gender) -> case gender of
				Male -> (1, 0 )
				Female -> (0, 1)
				_ -> (0, 0)
			_ -> (0, 0)

salePrices :: [TimeMachine] -> [TimeMachine]
salePrices [x@(TimeMachine { price = p })] = [(x { price = (p / 2) })]
salePrices lst = case lst of
	[x@(TimeMachine { price = p })] -> [(x { price = (p / 2) })]
	x:xs -> (salePrices [x]) ++ (salePrices xs)
	[] -> []

