-- OK i've discovered we only need to check for factors up to the square root of n --
-- this is because if n = a * b, then at least one of those factors must be <= sqrt(n) --
-- so if we don't find any factors up to floor(sqrt(n)), we can be sure there are none at all --

prime :: Int -> Bool
prime n 
    | n < 2 = False
    | otherwise = not (factorisable n 2)
    where 
        factorisable n f
            | n < f * f = False -- edited this line to use f*f instead of n <= f --
            | otherwise = mod n f == 0
                || factorisable n (f+1)

main :: IO()
main = print (prime 29)