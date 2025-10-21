-- elem1 included a function that checks if an integer is in a list of integers.
-- elem2: a polymorphic version of elem1

-- elem2 : checks if an element is in a list of elements of the same type
elem2 :: Eq a => a -> [a] -> Bool
elem2 e [] = False
elem2 e (x:xs)
    | e == x    = True
    | otherwise = elem2 e xs

-- We use the type constraint "Eq a" to indicate that 
-- the type of "a" must be a type for which equality testing is defined.
-- Examples of data types this works for include Int, Char, and String.
-- However, it would not work for a type like functions,
-- since equality testing is not defined for functions.

main :: IO()
main = do
    print (elem2 3 [1,2,3,4])                     -- True
    print (elem2 'a' "hello")                     -- False
    print (elem2 "cat" ["dog", "cat", "mouse"])  -- True
    print (elem2 5.0 [1.0, 2.0, 3.0])            -- False