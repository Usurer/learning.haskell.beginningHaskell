data Person = Person String String Gender
    deriving Show

data Gender = Male | Female | Unknown
    deriving Show

-- GenderDistribution Num_of_Males Num_of_Females Num_of_Unknowns
data GenderDistribution = GenderDistribution Int Int Int deriving Show

clientsByGender :: [Person] -> GenderDistribution
clientsByGender lst = let 
        male1 m f u = GenderDistribution (m + 1) f u
        female1 m f u = GenderDistribution m (f + 1) u
        unknown1 m f u = GenderDistribution m f (u + 1)
    in
    if null lst
    then GenderDistribution 0 0 0
    else case head lst of
        Person _ _ Male -> case recurse of
            GenderDistribution m f u -> male1 m f u
        Person _ _ Female -> case recurse of
            GenderDistribution m f u -> female1 m f u
        Person _ _ Unknown -> case recurse of
            GenderDistribution m f u -> unknown1 m f u
    where   recurse = clientsByGender (tail lst)

bob = Person "Bob" "Bobson" Male
john = Person "John" "Johnsson" Male
mary = Person "Mary" "Marydotter" Female
liz = Person "Liz" "Lizzer" Female
anna = Person "Anna" "Marie" Unknown
sunny = Person "Sunny" "Sunny" Unknown

example_1 = [bob, mary, anna] -- 1 of each
example_2 = [bob, john, mary] -- 2 m 1 f
example_3 = [liz, mary, anna, sunny] -- 2 f 2 u

result_1 = clientsByGender example_1 -- expected 1 1 1
result_2 = clientsByGender example_2 -- expected 2 1 0
result_3 = clientsByGender example_3 -- expected 0 2 2 

--------------------------------------------------------

data TimeMachine = TimeMachine Manufacturer Int String TimeTravelDirection Float
    deriving Show

data Manufacturer = Manufacturer String
    deriving Show

data TimeTravelDirection = Past | Future | Both
    deriving Show

discount :: Float -> [TimeMachine] -> [TimeMachine]
discount percent lst = let 
    discounted p = p * (1 - percent / 100) 
    recurse = discount percent (tail lst)
    in
    if null lst
    then lst
    else case head lst of
        TimeMachine a b c d price -> TimeMachine a b c d (discounted price) : recurse

tm_1 = TimeMachine (Manufacturer "Bob") 1 "xxx" Both 10
tm_2 = TimeMachine (Manufacturer "Bob") 1 "xxx" Both 15
tm_3 = TimeMachine (Manufacturer "Bob") 1 "xxx" Both 20

result_4 = discount 10 [tm_1, tm_2, tm_3]