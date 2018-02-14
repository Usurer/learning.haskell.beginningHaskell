firstOrEmpty listOfStrings = if null listOfStrings then "empty" else head listOfStrings

(+++) :: [x] -> [x] -> [x]
lst1 +++ lst2 = if null lst1
    then lst2
    else (head lst1) : ((tail lst1) +++ lst2)

reverse2 :: [x] -> [x]
reverse2 lst = if null lst
    then []
    else reverse2 (tail lst) +++ (head lst]