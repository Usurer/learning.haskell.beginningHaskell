module Ex3_2 where
import ADT_Records

{-
    I don't want to copypaste the ADT_Records.hs from Chapter2.
    Thus I've added Chapter2 to Cabal config as a source folder.
    It leads to build warning, but no errors for now.
    Sadly that doesn't help while loading current file in GHCI,
    so to help it locate ADT_Records, use the following:
        ghci -i../Chapter2
    Mind, that there is no space nor any other symbol between an
    -i option and ../Chapter2 value.
-}

filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes lst 
    | null lst = []
    | otherwise = filter (\x -> x == 1)  lst

filterANumber :: (Num a, Eq a) => [a] -> a -> [a]
filterANumber lst compareTo
    | null lst  = []
    | otherwise = filter (\x -> x == compareTo) lst

-- $ is a low-priority operator to combine functions without ()
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = filter (\x -> not $ f x) lst

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs lst = filter (\x -> case x of 
    GovOrg _ -> True
    _        -> False) lst

