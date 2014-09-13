import qualified Data.Map as Map

type Point = (Integer, Integer)
type GridSize = Integer
type Cache = Map.Map Point Integer


countPaths :: Cache -> GridSize -> Point -> Integer
countPaths c n p@(x,y) = Map.findWithDefault paths p c
                          where paths
                                  | x >= n || y >= n = 1
                                  | otherwise        = sum $ map (countPaths c n) [(x+1,y),(x,y+1)]

buildCache :: GridSize -> Cache
buildCache n = foldl f Map.empty [(n-x,n-y) | x <- [1..n], y <- [1..n]]
                where f c p = Map.insert p (countPaths c n p) c

count :: GridSize -> Integer
count n = buildCache n Map.! (0,0)

answer = count 20
