import Control.Monad
import Data.Ord (comparing)

data Part = Part1 | Part2 deriving Show

minimaBy :: (a -> a -> Ordering) -> [a] -> [a]
minimaBy cmp = foldr aux [] where
  aux x []     = [x]
  aux x (m:ms) = case cmp x m
                   of LT -> [x]
                      EQ -> x:m:ms
                      _  ->   m:ms

possibleCombinations :: Int -> [Int] -> [[Int]]
possibleCombinations n _ | n < 0  = []
possibleCombinations n _ | n == 0 = [[]]
possibleCombinations _ []         = []
possibleCombinations n (x:xs)     = map (x:) (possibleCombinations (n-x) xs)
                                 ++ possibleCombinations n xs

eggnog :: Int
eggnog = 150

main :: IO()
main = do
  containers <- liftM (map read . lines) $ readFile "input.txt"
  forM_ [Part1, Part2] $ \part -> do
    print part
    print $ length $ select part $ possibleCombinations eggnog containers

select :: Part -> [[Int]] -> [[Int]]
select Part1 = id
select Part2 = minimaBy (comparing length)
