-- A variation of the merge function that works with any datatype.
merge :: [a] -> [b] -> [(a,b)]
merge (a:as) (b:bs) 
    = (a,b) : merge as bs
merge [] bs = []
merge as [] = []

main :: IO()
main = print (merge ['a','b','c'] [1,2,3])