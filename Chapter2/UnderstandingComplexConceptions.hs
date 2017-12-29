import Data.Char

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


greet GovOrg { clientName = n } = "Hi " ++ n

greet2 client = case client of
	GovOrg { clientName = n } -> n
	Company { clientName = n } -> n
	Individual { person = Person { lastName = l } } -> l

nameInCapitals :: Person -> Person
nameInCapitals p@(Person { firstName = initial:rest }) =
	let newName = (toUpper initial):rest
	in p { firstName = newName }
-- Empty name case
nameInCapitals p@(Person { firstName = "" }) = p

capitalizeString :: String -> String
capitalizeString (x:xs) = (toUpper x) : xs
capitalizeString []  = ""


nameInCapitals2 :: Person -> Person
nameInCapitals2 p@(Person { firstName = x}) =
	let 	newName1 = capitalizeString (firstName p)
		newName2 = capitalizeString (lastName p)
	in p { firstName = newName1, lastName = newName2 }
-- Empty name case
--nameInCapitals2 p@(Person { firstName = "", lastName = "" }) = p
