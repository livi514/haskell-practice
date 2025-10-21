merge :: [Int] -> [Int] -> [(Int,Int)]
merge (a:as) (b:bs) 
    = (a,b) : merge as bs
merge [] bs = []
merge as [] = []

main :: IO()
main = print (merge [1,3,5] [2,4,6])