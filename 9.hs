answer = head [ a*b*c | a <- [1..332],
                           b <- [a+1..499],
                           let c = 1000 - b - a,
                           a^2 + b^2 == c^2]
