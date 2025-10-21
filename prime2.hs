-- prime and prime2 are functionally identical --
-- the only difference is that prime2 uses a let..in construct instead of a where clause --
-- both are valid ways to define local helper functions in Haskell --
-- I just wanted to demonstrate both methods here --

-- where is used to define local helper functions or variables after the main expression -- 
-- it's tied to the guards or pattern matching above it --
-- let..in is used to define local bindings within an expression --
-- it can be used anywhere an expression is valid --

prime :: Int -> Bool
prime n 
    | n < 2 = False
    | otherwise = 
        let 
            factorisable n f
                | n <= f = False
                | otherwise = mod n f == 0
                    || factorisable n (f+1)
        in not (factorisable n 2)

main :: IO()
main = print (prime 29)
