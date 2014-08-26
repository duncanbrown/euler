factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n = length (factors n) == 2

answer = maximum $ filter isPrime $ factors 600851475143
