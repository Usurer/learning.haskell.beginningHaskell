module Ex2_6 where

{-
    An attempt to execute the current implementation of the
    Ackermann function will lead to terrible memory consumption
    and unfinished process (never gonna finish, should be killed).
    According to SO 
        https://stackoverflow.com/questions/16115815/ackermann-very-inefficient-with-haskell-ghc
    this is an issue of GHC.
-}

ackermann :: Integer -> Integer -> Integer
ackermann m n | m == 0          = n + 1
ackermann m n | m > 0 && n == 0 = ackermann (m - 1) 1
ackermann m n | m > 0 && n > 0  = ackermann (m - 1) (ackermann m (n - 1))


unzip2 :: [(x, y)] -> ([x], [y])
unzip2 [] = ([], [])
unzip2 ((a, b):xs) 
    | null xs   = ([a], [b])
    | otherwise = let recurse = unzip2 xs in
            (a : (fst recurse),
             b : (snd recurse))
