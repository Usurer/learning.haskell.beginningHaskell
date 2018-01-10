{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as M

insert' :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insert' key val map = M.alter (\x -> Just val) key map

delete' :: (Ord k) => k -> M.Map k a -> M.Map k a
delete' key map = M.alter (\x -> Nothing) key map

adjust' :: (Ord k) => k -> (a -> a) -> M.Map k a -> M.Map k a
adjust' key f map = M.alter (\case 
    Just v  -> Just $ f v
    Nothing -> Nothing) key map