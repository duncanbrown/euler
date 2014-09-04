import qualified Data.Map as Map
import Data.List

main :: IO Integer
main = do
  txt <- readFile "11.txt"
  let grid = buildGrid txt
  let answer = greatestProductOf 4 grid
  return answer

type GridPosition = (Integer,Integer)

type Grid = Map.Map GridPosition Integer

buildGrid :: String -> Grid
buildGrid = Map.fromList .
            concat .
            map convertRow .
            numberedRows
              where numberedRows = zip [0..] . lines
                    numberedCols = zip [0..] . map read . words
                    convertRow (row,line) = map (\(col, number) -> ((col,row), number)) . numberedCols $ line

getPositions :: Grid -> GridPosition -> ([Integer],[Integer]) -> [GridPosition]
getPositions g (col,row) (cOffsets,rOffsets)
 = map (\(c,r) -> (col+c,row+r)) $ zip cOffsets rOffsets


adjacents :: Integer -> Grid -> GridPosition -> [[GridPosition]]
adjacents n g pos = filter allInGrid $ map (getPositions g pos) [
                            (zeros, up),
                            (zeros, down),
                            (up, zeros),
                            (down, zeros),
                            (up,up),
                            (up,down),
                            (down,up),
                            (down,down)]
                            where up = [1..n]
                                  down = [-1,-2..(-n)]
                                  zeros = genericReplicate n 0
                                  allInGrid = all (\pos -> Map.member pos g)


greatestProductOf :: Integer -> Grid -> Integer
greatestProductOf n g = maximum . concatMap adjacentValueProducts $ Map.keys g
                          where adjacentValueProducts = map (product . map (g Map.!) ) . adjacents n g
