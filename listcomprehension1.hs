main :: IO()
main = print[x | x <- [1,4,2,3], odd x]