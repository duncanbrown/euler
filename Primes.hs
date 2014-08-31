module Primes (isPrime, primes) where

isPrime :: Integer -> Bool
isPrime n = let sqrtN = floor $ sqrt $ fromIntegral n
                testFactors = 2 : [3,5..sqrtN]
            in null [x | x <- testFactors, x `isFactorOf` n]

isFactorOf :: Integer -> Integer -> Bool
x `isFactorOf` y = y `mod` x == 0

primes = 2: filter isPrime [2..]
