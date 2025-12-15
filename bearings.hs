-- teaser from lecture 3

-- Sunday Times Teaser 3125 of Sunday 14th September 2022,
-- The Bearings’ Trait by Stephen Hogg, was as follows.
-- At Teaser Tor trig. point I found a geocaching box. The
-- three-figure compass bearings (bearing 000 = north, 090
-- = east, etc.) from there to the church spires at Ayton,
-- Beeton and Seaton were needed to decode the clue to
-- the next location.
-- Each spire lay in a different compass quadrant (eg. 000 to
-- 090 [sic] is the North-East quadrant). Curiously, each of
-- the numerals 1 to 9 occurred in these bearings and none
-- of the bearings were prime values.

-- Given the above, if you chose one village at random to
-- be told only its church spire’s bearing, it might be that you
-- could not calculate the other two bearings with certainty,
-- but it would be more likely that you could.
-- Give the three bearings in ascending order.

-- The solution given by the newspaper was 159, 267, 348.

-- Understanding the problem:
-- This teaser may be seen as the search for 3 bearings, A, B, and C, 
-- satisfying the following constraints:
-- A, B, and C must be in the range 0-359 inclusive (we are only considering integers here).
-- They must be made up of the digits 1 to 9.
-- Since they must be made up of the digits 1 to 9,
-- This limits the range even further to 100-359.
-- This means, none of the points is in the first quadrant.
-- So we have one in each of these quadrants:
-- Second quadrant: 90-179 (limited further to 100-179 by the constraints above).
-- Third quadrant: 180-269
-- Fourth quadrant: 270-359
-- Since each digit must be unique, and the point in the second quadrant will 
-- begin with 1, the range of the point in the third quadrant will be limited even further,
-- to 200-269.
-- Similarly, for the point in the fourth quadrant, the range will be 300-359.
-- Final ranges:
-- A: 100-179
-- B: 200-269
-- C: 300-359
-- We can represent this as follows:
-- A must be a 3-digit muber of the form [1, X2, X1].
-- B must be a 3-digit number of the form [2, Y2, Y1].
-- C must be a 3-digit number of the form [3, Z2, Z1].
-- Since A, B, and C must be made up of the digits 1 to 9:
-- X1, Y1, Z1 will all be in the range 4-9 inclusive.
-- This will limit the ranges to:
-- A: 145-179
-- B: 245-269
-- C: 345-359
-- Since A, B, and C are made up of the digits 1 to 9,
-- X1, X2, Y1, Y2, Z1 znd Z2 must all be distinct.
-- None of A, B and C are prime numbers.

-- As a warmup, it will be useful to have a function that converts a
-- list of digits to a number.
number :: [Int] -> Int
number xs
    = totalize (reverse xs)
        where 
        totalize [] = 0
        totalize (x:xs)
            = x + 10 * totalize xs
-- Example of how number works:
-- Let's say we have the list [1,2,3].
-- number [1,2,3] = totalize (reverse [1,2,3])
-- number [1,2,3] = totalize ([3,2,1])
-- Applying totalize:
-- totalize(3,[2,1]) = 3 + 10 * totalize(2,[1])
-- totalize(2,[1]) = 2 + 10 * totalize(1,[0])
-- totalize(1,[0]) = 1 + 10 * totalize [0]
-- totalize [] = 0 (base case)
-- substituting back:
-- totalise(1,[0]) = 1 + 10 * 0 = 1
-- totalise(2,[1]) = 2 + 10 * 1 = 12
-- totalise(3,[2,1]) = 3 + 10 * 12 = 123 
-- so we have number[1,2,3] = 123

-- A generator
-- A generator constructs a list of items that might be solutions
-- to the problem. Each item is a list of 3 bearings [A, B, C], 
-- such that the constraints above are satisfied (except the "not prime" constraint).
generator :: [[[Int]]]
generator
= [ [[1 , x2 , x1 ] ,[2 , y2 , y1 ] ,[3 , z2 , z1 ]]
-- Explaining the constraints/ranges:
-- X2 must be 4-7 because if it was greater than 7, this would cause the bearing to be > 180,
-- which would place it in the third quadrant.
-- X2 must be 4-7 as A must be in the second quadrant.
-- Similarly, B must be in the third quadrant so Y2 must be 4-6.
-- Z2 must be 4-5, as the largest possible bearing is 359.
| x2 <- [4..7]
, x1 <- [4..9] , x1 `notElem` [ x2 ]
, y2 <- [4..6] , y2 `notElem` [ x2 , x1 ]
, y1 <- [4..9] , y1 `notElem` [ x2 , x1 , y2 ]
, z2 <- [4..5] , z2 `notElem` [ x2 , x1 , y2 , y1 ]
, z1 <- [4..9] , z1 `notElem` [ x2 , x1 , y2 , y1 , z2
    ]
]

-- A selector:
-- A selector may be used to filter items that are solutions to the problem.
-- Each filtered item is a list of 3 bearings, such that the numbers are not prime.
selector :: [[ Int ]] -> Bool
selector [ as , bs , cs ]
= not ( prime a || prime b || prime c )
where
    a = number as
    b = number bs
    c = number cs

-- An auxiliary function is needed to test if a number is prime.
prime :: Int -> Bool
prime n = factors n == [1 , n ]
where
    factors n = [ f | f <- [1.. n ] , n `mod` f == 0]

-- Putting it together:
-- The final part of our program for this teaser puts the generator
-- and selector together, filtering the list from the generator with the selector.
main :: IO ()
main = print ( head ( filter selector generator ) )

-- The result of this program is [[1,5,9],[2,6,7],[3,4,8]].
-- By pure luck!

-- Check the answer:
-- The right way to solve this problem is to solve the given riddle.
-- Given the above, if you chose one village at random to
-- be told only its church spire’s bearing, it might be that you
-- could not calculate the other two bearings with certainty,
-- but it would be more likely that you could.

-- The full list of filtered items is as follows.
-- [[[1,5,9],[2,6,7],[3,4,8]]
-- ,[[1,6,8],[2,4,9],[3,5,7]]
-- ,[[1,6,9],[2,4,7],[3,5,8]]
-- ,[[1,6,9],[2,4,8],[3,5,7]]
-- ,[[1,7,6],[2,4,9],[3,5,8]]
-- ,[[1,7,8],[2,4,9],[3,5,6]]
-- ,[[1,7,6],[2,5,9],[3,4,8]]
-- ,[[1,7,8],[2,5,9],[3,4,6]]
-- ]
-- Now the solution really must be 159, 267, 348 because this is
-- the only solution where if you chose one bearing at random, it
-- might be that you could not calculate the other two with
-- certainty (348), but it would be more likely that you could (159
-- or 267).
