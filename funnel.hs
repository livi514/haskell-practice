digits2 :: Int -> [Int]

-- ok so we provide the number at the top of the funnel in decimal -- 
digits2 n
-- if this number is < 2, then the funnel will only have this row, because in binary, the number will be represented by a single digit -- 
-- we return a list with this single digit --
 | n < 2     = [n]
 -- otherwise, we calculate the binary representation of n recursively --
 | otherwise = digits2 (n `div` 2) ++[n `mod` 2]

-- we generate a list of numbers from 1 to 510 --
generator :: [Int]
generator
 = [1..510] -- a list comprehension --
-- we will check each of these numbers to see if they satisfy the condition defined in the selector function --

-- we define a selector function that checks if the funnel of the padded binary digits of n, multiplied by 9 and divided by 2, equals n --
selector :: Int -> Bool
selector n
-- the selector function checks if the number at the top of the funnel, when processed as described, equals the original number n --
 = n == funnel (pad 9 0 (digits2 n)) * 9 `div` 2

-- once we have the binary digits of n -- 
-- we define a function to pad the list of digits with leading zeros until it reaches a length of n --
pad :: Int -> Int -> [Int] -> [Int]
pad n x xs
 | length xs < n = pad n x (x:xs)
 | otherwise     = xs

-- we define a function to calculate the next row in the funnel by summing adjacent pairs of numbers --
next :: [Int] -> [Int]
next (n1:n2:n3:ns)
 = n1+n2 : next (n2:n3:ns)
next [n1,n2]
 = [n1+n2]

-- if there's only one number left, we return it as the only element in the list --
funnel :: [Int] -> Int
funnel (n1:n2:ns)
 = funnel (next (n1:n2:ns))
funnel [n]
 = n

-- the main function filters the generated list using the selector function and prints the first number that satisfies the condition --
-- this will be the smallest number whose funnel of padded binary digits, when processed as described, equals the original number --
-- SO it will be the number at the top of the funnel -- 
-- but to get the number at the bottom of the funnel, just divide by 4.5 -- 
main :: IO ()
main
 = print (head (filter selector generator))