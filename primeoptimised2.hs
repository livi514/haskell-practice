-- ok after 2, we can skip even numbers --
-- so we check if n is even, if it is and n > 2, then it's not prime --
-- then we only check odd factors from 3 up to sqrt(n) --

prime :: Int -> Bool
prime n 
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = not (factorisable n 3) -- we've already handled the case if n is 2, so we start at 3 -- 
    where 
        factorisable n f
            | n < f * f = False 
            | otherwise = mod n f == 0
                || factorisable n (f+2) -- we already checked even numbers, so we can skip them --

main :: IO()
main = print (prime 29)