-- Turning the digits of a number into a number
number :: [Int] -> Int
number xs
  = totalize (reverse xs)
    where
    totalize []
       = 0
    totalize (x:xs)
      = x + 10 * totalize xs

generator :: [[[Int]]]
generator
  = [ [[1,x2,x1],[2,y2,y1],[3,z2,z1]]
    | x2 <- [4..7]  
    , x1 <- [4..9], x1 `notElem` [x2]
    , y2 <- [4..6], y2 `notElem` [x2,x1] 
    , y1 <- [4..9], y1 `notElem` [x2,x1,y2]
    , z2 <- [4..5], z2 `notElem` [x2,x1,y2,y1] 
    , z1 <- [4..9], z1 `notElem` [x2,x1,y2,y1,z2]
    ] 

selector :: [[Int]] -> Bool
selector [as,bs,cs]
  = not (prime a || prime b || prime c)
    where
    a = number as
    b = number bs
    c = number cs

prime :: Int -> Bool
prime n
  = factors n == [1,n]
    where
    factors n
      = [f | f <- [1..n], n `mod` f == 0]

main :: IO ()
main
  = print (head (filter selector generator))