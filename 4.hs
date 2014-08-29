isPalindromic :: Integer -> Bool
isPalindromic n = let s = show n in reverse s == s

answer = let ns = [999,998..1]
         in maximum [p | x <- ns, y <- ns, let p = x*y, isPalindromic p]
