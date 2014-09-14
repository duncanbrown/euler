import qualified Data.Map as Map

digits :: Int -> [Int]
digits = map (read . (:[])) . show



names :: Map.Map Int String
names = Map.fromList $ zip ([1..19] ++ [20,30..90]) ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

str :: Int -> String
str n
  | n `Map.member` names   = names Map.! n
  | otherwise              = f $ digits n
    where f ns
            | length ns == 2 = str ((head ns) * 10) ++ str (last ns)
            | length ns == 3 = str (head ns) ++ "hundred" ++ str (read . concat $ tail ns)
