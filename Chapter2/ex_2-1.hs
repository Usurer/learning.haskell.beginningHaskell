-- expected:
-- returns [['a', 'b', 'c'], ['d', 'e']]
firstList = ('a' : 'b' : 'c' : []) : ('d' : 'e' : []) : []

-- expected:
-- [] returns true, [x] returns false
-- [[], ["abc"]] returns true, [[x], ["abc"]] returns false
input_1 = []
input_2 = ['x']
input_3 = [[], ['a', 'b', 'c']]
input_4 = [['x'], ['a', 'b', 'c']]

-- Mind that an attempt to use input_1 or input_2 with this code will cause an error during file load in GHCI
-- I assume it may be related to the fact that compiler does some optimizations and knows that inputs 1 and 2 doesn't contain lists
result_1 = if null input_4 then True else (if (null (head input_4)) then True else False)

-- expected:
-- ['a'], [1] etc returns True
-- [], [1, 2, 3], ['a', 'b'] returns False
input_5 = [1]
input_6 = [1, 2, 3]

result_2 = if ((null input_6) || (not (null (tail input_6)))) then False else True

-- expected:
-- ["abc", "de"] returns "abcde"
-- [[1, 2, 3], [4, 5]] returns [1, 2, 3, 4, 5]

input_7 = [[1, 2, 3], [4, 5]]
result_3 = head input_7 ++ (head (tail input_7))
