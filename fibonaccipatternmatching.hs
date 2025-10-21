fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

main :: IO()
main = print (fibonacci 6)