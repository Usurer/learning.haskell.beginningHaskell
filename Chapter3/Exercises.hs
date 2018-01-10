{-# LANGUAGE LambdaCase #-}

data Client = GovOrg { clientName :: String }
	| Company { clientName :: String
		, companyId :: Integer
		, person :: Person
		, duty :: String }
	| Individual { person :: Person }
	deriving Show

data Person = Person { firstName :: String
		, lastName :: String }
        deriving Show
        

isGovOrg :: Client -> Bool
isGovOrg = (\case GovOrg  _     -> True
                  _             -> False)

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs lst = filter isGovOrg lst

product1 :: [Integer] -> Integer
product1 [] = 0
product1 (x:xs) = case xs of
	(a:as)	-> x * product1 xs
	[]		-> x

product2 :: [Integer] -> Integer
product2 [] = 0
product2 lst = foldr (*) 1 lst -- similar to fordr1 (*) lst

getClientName :: Client -> String
getClientName x = case x of
	GovOrg 	{ clientName = name } -> name
	Company { clientName = name } -> name
	Individual { person = Person { firstName = a, lastName = b } }  	-> a ++ " " ++ b

minimumClient :: [Client] -> Client
minimumClient [] = error "Empty list"
minimumClient (x:xs) = let min1 = minimumClient xs
	in	if (null xs || length (getClientName x) < length (getClientName min1))
			then x
			else min1

minimumClient1 :: [Client] -> Client
minimumClient1 [] = error "Empty list"
