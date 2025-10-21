-- The number N1 will be drawn from the special numbers that have three non-zero digits, none of which are repeated.
specials :: [[Int]]
specials
  = [ [a,b,c]
    -- the digits are non-zero, so we select a from the range 1-9
    | a <- [1..9]
    -- b and c must be different from a, so we filter them out of the list --
    , b <- [1..9], b `notElem` [a]
    , c <- [1..9], c `notElem` [a,b]
    ]

-- The generator produces all possible combinations of the five numbers, given the constraints.
generator :: [([Int],[Int],[Int],[Int],[Int])]
generator
  = [ (n1,n2,n3,n4,n5)
    -- n1 is generated using the specials function above
    | n1 <- specials
    -- n2 is a permutation of two of the digits of n1
    , n2 <- perms2of3 n1
    -- n3 is a permutation of all three digits of n1
    , n3 <- perms3of3 n1
    -- n4 is another permutation of two of the digits of n1
    , n4 <- perms2of3 n1
    -- n5 is another permutation of all three digits of n1
    , n5 <- perms3of3 n1
    -- finally, the first digit of n1 must be different from the first digit of n2
    , head n1 /= head n2
    ]

-- perms3of3 generates all permutations of a list of three elements 
-- perms3of3 means 3 out of 3 digits are selected and rearranged
-- Haskell does have a permutations function in Data.List, however here we are writing our own 
-- There are 6 permutations of 3 elements
perms3of3 :: [Int] -> [[Int]]
perms3of3 [a,b,c]
  = [[a,b,c],[b,a,c],[c,b,a],[b,c,a],[c,a,b],[a,c,b]]

-- perms2of3 generates all permutations of a list of three elements taken two at a time
-- perms2of3 means 2 out of 3 digits are selected and rearranged
-- There are 6 permutations of 2 elements from a set of 3
perms2of3 :: [Int] -> [[Int]]
perms2of3 [a,b,c]
  = [[a,b],[b,a],[b,c],[c,b],[a,c],[c,a]]

-- The selector function checks if the generated numbers satisfy the given conditions.
-- Each filtered item is a tuple of numbers, such that constraints 1, 2, and 3 are satisfied.
selector :: ([Int],[Int],[Int],[Int],[Int]) -> Bool
selector (n1,n2,n3,n4,n5)
-- checks the three conditions:
-- 1. N1 - N2 = N3
-- 2. N3 - N4 = N5
-- 3. N1 + N3 + N5 < 2000
  =  i1 - i2 == i3
  && i3 - i4 == i5
  && i1 + i3 + i5 < 2000
  where
  i1 = number n1
  i2 = number n2
  i3 = number n3
  i4 = number n4
  i5 = number n5

-- Converts a list of digits into the corresponding integer.
-- As before, a sequence of digits may be converted to a number 
-- by adding the head of the sequence to the result of recursively 
-- converting the tail of the sequence and multiplying it by 10,
-- until the sequence is empty and the result is 0.
number :: [Int] -> Int
number xs
-- we reverse the list so that we can process it from least significant digit to most 
-- totalize is a helper function that does the actual conversion
  = totalize (reverse xs)
    where
    totalize (x:xs)
      = x + 10 * totalize xs
    totalize []
      = 0

main :: IO ()
main
  = print (head (filter selector generator))