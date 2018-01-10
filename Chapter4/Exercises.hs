{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as M
import qualified Data.Set as S

insert' :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insert' key val map = M.alter (\x -> Just val) key map

delete' :: (Ord k) => k -> M.Map k a -> M.Map k a
delete' key map = M.alter (\x -> Nothing) key map

adjust' :: (Ord k) => k -> (a -> a) -> M.Map k a -> M.Map k a
adjust' key f map = M.alter (\case 
    Just v  -> Just $ f v
    Nothing -> Nothing) key map


data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String, person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
            deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String } deriving (Show, Eq, Ord)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Show, Eq, Ord)

mapper1 :: [Client Integer] -> [(ClientKind, Client Integer)]
mapper1 lst = map (\x -> case x of
    GovOrg _ _          -> (GovOrgKind, x)
    Company _ _ _ _     -> (CompanyKind, x)
    Individual _ _      -> (IndividualKind, x)) lst

-- mapper2 :: [Client Integer] -> M.Map (ClientKind, S.Set (Client Integer))
-- mapper2 lst = S.fromList lst

-- mapper2 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
-- mapper2 lst = S.fromList lst

partitioner :: [(ClientKind, Client Integer)] -> [[(ClientKind, Client Integer)]]
partitioner [] = []
partitioner [x] = [[x]]
partitioner (x:y:[]) = if (fst x /= fst y)
        then [[x], [y]]
        else [[x, y]]
partitioner (x:y:ys) = partitioner [x, y] ++ partitioner ys

folder :: [(ClientKind, Client Integer)] -> (ClientKind, [Client Integer])
folder lst = (fst (head lst), foldr (\x y -> (snd x) : y) [] lst)

-- test :: [(Integer, String)] -> [(Integer, String)]
-- test ((x, y):xs) = (x, y) : xs

c1 = snd (GovOrgKind, GovOrg 1 "NASA")
c2 = snd (GovOrgKind, GovOrg 2 "RosCosmos")
c3 = snd (CompanyKind, Company 3 "Bob" (Person "Bob" "Boboson") "SEO")
a = [c1, c2, c3]

mapperResult = mapper1 a
partitionerResult = partitioner mapperResult
--folderResult = folder (head partitionerResult)
mapResult = M.fromList $ map (\x -> folder x) partitionerResult