import Data.Set (fromList, size, member)

collatz :: Integer -> Integer
collatz n
  | even n    = n `div` 2
  | otherwise = (3*n)+1

collatzChain :: Integer -> [Integer]
collatzChain 1 = []
collatzChain n = let next = collatz n
                 in next : collatzChain next

pairMap :: (a -> b) -> [a] -> [(a,b)]
pairMap f xs = zip xs $ map f xs

-- pairFoldl :: (a -> b -> a) -> a -> [b] -> (b,a)
-- pairFoldl f seed xs = foldl () seed xs

answer :: Integer
-- answer = fst . maxPair . chainLengthsPairs . filter candidate $ [1,3..1000000]
--           where maxPair = foldl (\acc@(n,length) nxt@(n',length') -> if length' > length then nxt else acc) (0,0)
--                 chainLengthsPairs = pairMap (length . collatzChain)
--                 candidate n = True

answer = fst $ foldl fold (0, fromList []) [1,3..1000000]
          where fold acc@(n,c) n'
                  | n' `member` c           = acc
                  | size c' < size c        = acc
                  | otherwise               = (n',c')
                    where c' = fromList $ collatzChain n'
