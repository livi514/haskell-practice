append :: [Int] -> [Int] -> [Int]
append (x:xs) ys
    = x : append xs ys
append [] ys = ys

main :: IO()
main = print (append [1,2,3] [4,5,6])