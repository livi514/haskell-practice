-- sets of integers
-- example from lecture on polymorphism

-- elem1: checks if an integer is in a list of integers
-- Next exercise: write a polymorphic version of elem1 (see elem2.hs)
elem1 :: Int -> [Int] -> Bool
elem1 e [] = False
elem1 e (x:xs) 
    | e == x    = True
    | otherwise = elem1 e xs

