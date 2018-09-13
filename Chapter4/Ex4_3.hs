module Ex4_3 where

{-
    The excercise assumes the use of polymorphic Client i data.
    I don't want to add it to ADT_Records for now, so I'm
    gonna just declare it here.
    It also seems that the excercise also assumes the use of
    different features (like deriving Ord or Eq)
    that weren't introduced yet.
-}

--import ADT_Records
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

data Client i = 
      GovOrg { clientId :: i, clientName :: String}
    | Company { 
        clientId :: i, 
        clientName :: String, 
        person :: Person, 
        duty :: String }
    | Individual { clientId :: i, person :: Person }
    deriving (Show, Eq, Ord)

data Person = Person { fName :: String, lName :: String }
    deriving (Show, Eq, Ord)

{-
    According to 'Learn you a Haskell', it should be safe
    to use Deriving for Eq and Ord here:
    - Eq will compare ctors first and fields second (although
    there are no fields in this case), thus it will require
    fields to be Eq (build-in types like Integer or String are ok)
    - Ord will use a ctors oder, which is good in this case
-}
data ClientKind = 
      GovOrgKind
    | CompanyKind
    | IndividualKind
    deriving (Show, Eq, Ord)


getClientKind :: Client i -> ClientKind
getClientKind x = case x of
    GovOrg {}       ->  GovOrgKind
    Company {}      ->  CompanyKind
    Individual {}   ->  IndividualKind

{-
    Probable solution for the first scenario:
    - get an empty Map
    - for each element in list:
      - get it's ClientKind
      - alter the Map value for the current ClientKind:
        - add new element to the Set
    In this case we'll have only one run along the initial list.
-}
{-
classifyClients1 :: 
    [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients1 lst = 
    let m = M.empty in
    map 
-}

-- This is a test function - I'm trying to understand how to turn
-- a list into Map. Learn You a Haskell assumes using Folds
-- to create your own Map.fromList function, so I'll try it.
testFold :: [String] -> M.Map Integer String
testFold lst = 
    L.foldl (\accMap str -> 
        M.insertWith (\init new -> 
            init ++ ", " ++ new) 
        (toInteger $ length str) str accMap
    ) M.empty lst

