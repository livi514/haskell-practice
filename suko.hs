-- This is an example problem we went through in a lecture about design patterns.

-- A design pattern:
-- There are a few design patterns for functional programs,
-- but we shall need only generate-and-select, hinted at by Hughes.
-- Allows you to break down a problem into smaller pieces - "separation of concerns".

-- Generate: Construct a list of all possible candidate solutions.
-- Select: Filter out those candidates that are actual solutions.

-- Example: Suko
-- The Times Suko problem challenges readers to arrange the digits 1-9
-- in a 3x3 grid, so that numbers in four light grey circles 
-- are the totals of the digits in the cells surrounding them,
-- and the numbers in the 3 coloured circles are the totals of the 
-- digits in the cells of the same colour.

import Data.List

-- Constructs a list of all of the values that might provide solutions to the Suko problem.
-- Each value is a permutation of the numbers 1 to 9.
generator :: [[Int]]
generator
  = permutations [1..9]

-- The selector can be used to filter our those  values that  are solutions to the Suko problem.
-- The first circle (top-left) contains the number 15. 
-- So the sum of the four surrounding cells (a,b,d,e) must equal 15.
-- The second circle (top-right) contains the number 14.
-- So the sum of the four surrounding cells (b,c,e,f) must equal 14
-- The other circles provide similar constraints (they include the numbers 26 and 23, and the surrounding cells must sum to this).
-- Finally, the three coloured circles provide further constraints (they include the numbers 16, 17 and 12).
-- Cells a, b, c, d must sum to 16.
-- Cells e, g, h must sum to 17.
-- Cells f, i must sum to 12.
selector :: [Int] -> Bool
selector [a,b,c,d,e,f,g,h,i] 
  =  a + b + d + e == 15
  && b + c + e + f == 14
  && d + e + g + h == 26
  && e + f + h + i == 23
  && a + b + c + d == 16
  && e + g + h     == 17
  && f + i         == 12

-- The generate-and-select design pattern can be used to describe a good solution to the Suko problem.
main :: IO ()
main
  = print (head (filter selector generator)) 