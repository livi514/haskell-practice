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

