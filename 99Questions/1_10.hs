myLast  :: [a] -> a
myLast  [] = error "Empty list"
myLast  lst = if (null (tail lst))
            then  head lst
            else myLast (tail lst)

myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "Not enough elements"
myButLast (x:(_:[])) = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Integer -> a
elementAt [] _ = error "Empty list" 
elementAt lst 1 = head lst
elementAt lst x = elementAt (tail lst) (x - 1)

myLength :: [a] -> Integer
myLength [] = 0
myLength lst = 1 + myLength (tail lst)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == head (reversed' xs) && isPalindrome (tail (reversed' xs))
    where reversed' lst = myReverse lst

data NestedList a = Elem a | List [NestedList a] deriving Show

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List (x:xs)) = myFlatten x ++ (myFlatten (List xs))
myFlatten (List []) = []