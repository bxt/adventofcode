import Data.Ord (comparing)

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
  containers <- map read . lines <$> readFile "input.txt"
  let pC = possibleCombinations eggnog containers
  print $ length pC
  print $ length $ minimaBy (comparing length) $ pC
