module Ex4_2 where

import qualified Data.Map as M

{-
    Replace old or insert a new value for the key.
-}
insert1 :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert1 key val lst = 
    M.alter (\x -> Just val) key lst

{-
    Remove the key-value pair if the key is found
-}
delete1 :: Ord k => k -> M.Map k a -> M.Map k a
delete1 key lst =   M.alter (\x -> Nothing) key lst

{-
    If key is found in the list - apply function transform to the value.
    If key nof found - do nothing.
-}
adjust1 :: Ord k => k -> (a -> a) -> M.Map k a -> M.Map k a
adjust1 key transform  lst =
    M.alter (\x -> 
        case x of
            Just y  -> Just (transform y)
            _       -> x)
    key lst


testData = M.fromList [(1, "a"), (2, "b"), (3, "c")]

