lose :: Int -> [Int] -> [Int]
lose n [] = []
lose n (x:xs)
    | n == 0 = x:xs
    | otherwise = lose (n-1) xs

main :: IO()
main = print (lose 3 [1,2,3,4,5])

