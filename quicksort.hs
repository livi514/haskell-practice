quicksort :: [Int] -> [Int]
quicksort (x:xs) 
    = quicksort [u | u <- xs, u < x]
    ++ [x] ++
    quicksort [u | u <- xs, u >= x]
quicksort [] = []

main ::IO()
main = print (quicksort [1,5,3,4,2])