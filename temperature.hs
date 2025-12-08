-- Example from lecture on polymorphism
import Data.List

-- Teaser:
-- I have written down an above-freezing temperature, a whole number of degrees Celsius,
-- in which the digits are all different and are in decreasing order.
-- I have then calculated the Fahrenheit equivalent.
-- It is also a whole number whose digits are all different, 
-- but here the digits are in increasing order.
-- If I told you the first digit of the Celsius temperature, 
-- then you would not be able to calculate the temperature.
-- However, bearing that in mind, if I now told you the final digit 
-- of the Celsius temperature, then it would be possible to calculate it.
-- You should now be able to work out the Celsius and Fahrenheit temperatures.
-- What are they?
-- The solution given by the newspaper was 75C and 167F.

-- Understanding the problem:
-- The teaser may be seen as the search for a temperature, T, satisfying the following constraints:
-- T is a whole number of degrees Celsius, greater than 0.
-- The digits of T are all different and in decreasing order.
-- T is a whole number of degrees Fahrenheit, with all different digits in increasing order.

-- A list of items is in increasing order if there is an ordering defined for the items, 
-- and every item in the list is less than the following one.

-- polymorphic function: works for any type that has an ordering defined
-- ord - any "a" you like as long there is an ordering constraint 
-- given two of them, you could say which is bigger
-- Types that have an ordering defined include Int, Char, and String.
-- However, it would not work for a type like functions.
increasing :: Ord a => [a] -> Bool
-- checks if a list is in increasing order
-- (x1:x2:xs) means a list with at least two elements
-- you take the first two elements to compare, the rest are xs
-- this function uses recursion to check the rest of the list (xs)
increasing (x1:x2:xs)
  | x1 < x2   = increasing (x2:xs)
  | otherwise = False
increasing [x]
  = True

-- For decreasing:
-- You can reverse the list and see if the reverse of the list is increasing 
-- decreasing xs = increasing (reverse xs)
-- Why is this better than just switching the equality sign?
-- Reusing code 
-- More concise
decreasing :: Ord a => [a] -> Bool
decreasing xs
  = increasing (reverse xs)

-- The first part of our program is a generator, 
-- which constructs a list of items that might be solutions to the problem.
-- Each item is an integer temperature in Celsius.
generator :: [Int]
generator
-- limiting the range to above-freezing temperatures
-- could be made more efficient by further limiting the range 
-- but this is sufficient for our purposes
-- we choose an upper bound that seems reasonable
-- The teaser does not specify an upper bound
-- we can always adjust the range later if needed
  = [1..10000]


selector :: Int -> Bool
selector c
  =  exact f
  && different cs && decreasing cs
  && different fs && increasing fs
  where
  f  = ctof (fromIntegral c)
  cs = digits c
  fs = digits (round f)

exact :: RealFrac a => a -> Bool
exact x
  = floor x == ceiling x

ctof :: Float -> Float
ctof c
  = 9/5 * c + 32

digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

different :: Eq a => [a] -> Bool
different xs 
  = xs == nub xs

main :: IO ()
main
  = print (head (filter selector generator))