-- Exercise:
-- Define an unmerge function that converts a list of pairs into two lists.
unmerge :: [(a,b)] -> ([a],[b])
unmerge [] = ([],[])
unmerge ((a,b):ps) = (a:as, b:bs)
    where (as, bs) = unmerge ps

main :: IO()
main = print (unmerge [('a',1),('b',2),('c',3)])