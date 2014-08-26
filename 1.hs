answer = sum $ filter multipleOf3Or5 [1..999]
          where multipleOf3Or5 n = (n`multipleOf`3) || (n`multipleOf`5)
                x `multipleOf` y = x `mod` y == 0
