-- We are defining a function called prime that takes an Int and returns a Bool --
-- It will check whether the given number is prime or not. --
prime :: Int -> Bool
prime n 
-- if n < 2, it is not prime --
-- 2 is the smallest prime number --
    | n < 2 = False
    -- for all other n >= 2, check if the number is NOT factorisable -- 
    -- if it's not factorisable, then it is prime, so we return True --
    -- we are using a local helper function factorisable defined in a where clause --
    -- if factorisable returns True, then n is not prime, so we return False, and vice-versa --
    | otherwise = not (factorisable n 2)
    where 
        -- n is the number we are checking for primality --
        -- f is the current factor we are testing --
        -- we start testing from f = 2 (the smallest prime) --
        factorisable n f
            -- base case: if f is greater than or equal to n, then n is not factorisable --
            | n <= f = False 
            -- recursive case: check if f divides n without a remainder -- 
            -- if it does, n is factorisable, return True --
            -- otherwise, increment f and check again --
            | otherwise = mod n f == 0
                || factorisable n (f+1)

main :: IO()
main = print (prime 29)