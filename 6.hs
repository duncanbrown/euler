sumSquares :: [Integer] -> Integer
sumSquares = sum . map (^2)

squareSum :: [Integer] -> Integer
squareSum = (^2) . sum

sqSumDiff :: [Integer] -> Integer
sqSumDiff n = squareSum n - sumSquares n
