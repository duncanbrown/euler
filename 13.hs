import Control.Applicative

main :: IO Integer
main = sums <$> readFile "13.txt"

sums :: String -> Integer
sums = sum . map read . lines
