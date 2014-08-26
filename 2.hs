fib :: Integer -> Integer
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
fibs = map fib [1..]

answer = sum $ filter even $ takeWhile (<=4000000) fibs
