isPrime :: Integer -> Bool
isPrime n = let sqrtN = floor $ sqrt $ fromIntegral n
                testFactors = 2 : [3,5..sqrtN]
            in null [x | x <- testFactors, x `isFactorOf` n]

isFactorOf :: Integer -> Integer -> Bool
x `isFactorOf` y = y `mod` x == 0

answer = let n = 600851475143
             start = floor (fromIntegral n/3)
         in head [x | x <- [start,start-2..1], x `isFactorOf` n && isPrime x]
