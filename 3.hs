isPrime :: Integer -> Bool
isPrime n = let sqrtN = floor $ sqrt $ fromIntegral n
                testFactors = 2 : [3,5..sqrtN]
            in null [x | x <- testFactors, x `isFactorOf` n]

isFactorOf :: Integer -> Integer -> Bool
x `isFactorOf` y = y `mod` x == 0

n = 600851475143

answer = head $ filter isPrime [n `div` x | x <- [2..], x `isFactorOf` n]
