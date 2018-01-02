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