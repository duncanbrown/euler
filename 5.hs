isEvenlyDivisibleBy :: Integer -> Integer -> Bool
x `isEvenlyDivisibleBy` y = x `mod` y == 0

answer = head $ filter passesTest [40,60..]
          where passesTest n = all (n `isEvenlyDivisibleBy`) [20,19..1]
