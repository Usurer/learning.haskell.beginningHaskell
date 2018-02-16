maxmin :: Ord a => [a] -> (a, a)
maxmin lst = let listHead = head lst
        in if null (tail lst)
        then (listHead, listHead)
        else if listHead > max
            then (listHead, min)
            else if listHead < min
                then (max, listHead)
                else maxmin (tail lst)
        where   recurse = maxmin (tail lst)
                max = fst recurse
                min = snd recurse