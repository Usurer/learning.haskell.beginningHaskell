module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1
		then lst2
		else (head lst1) : (tail lst1 +++ lst2)

reverse2 :: [a] -> [a]
reverse2 lst = if null lst
	then lst
	else reverse2 (tail lst) +++ [head lst]

maxmin :: Ord a => [a] -> (a, a)
maxmin lst = if null (tail lst)
	then (head lst, head lst)
	else (	if fst (maxmin (tail lst)) > head lst
		then fst (maxmin (tail lst) )
		else head lst,
		if snd (maxmin (tail lst)) < head lst
		then snd (maxmin (tail lst))
		else head lst
	     )

		
