-- practice using lists -- 
count :: [Int] -> Int
count [] = 0
count (x:xs) = 1 + count xs

main :: IO()
main = print (count [1,2,3,4,5])