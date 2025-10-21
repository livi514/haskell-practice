-- add together the elements of a list --
addListElements :: [Int] -> Int
addListElements [] = 0
addListElements (x:xs) = x + addListElements xs

main :: IO()
main = print (addListElements [1,2,3,4,5])