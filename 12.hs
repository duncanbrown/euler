triangles :: [Integer]
triangles = 1 : zipWith (+) [2..] triangles

divisors :: Integer -> [Integer]
divisors n = let stop = ceiling $ sqrt $ fromInteger n
             in concat [[x,n `div` x] | x <- [1..stop], n `mod` x == 0]


answer = head $ filter ((>500) . length . divisors) triangles
