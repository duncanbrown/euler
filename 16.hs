digits :: Integer -> [Int]
digits = map (read . (:[])) . show

answer = sum . digits $ 2 ^ 1000
