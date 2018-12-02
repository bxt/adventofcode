import Data.Map (fromListWith, toList)
import Data.Function (on)

buildFrequencyLists :: [String] -> [[(Char, Integer)]]
buildFrequencyLists = map (toList . fromListWith (+) . map (\x -> (x,1)))

countFor :: Integer -> [[(Char, Integer)]] -> Int
countFor n = length . filter (any ((== n) . snd))

diff :: Eq a => [a] -> [a] -> [a]
diff (x:xs) (y:ys) = if x == y then x:rest else rest
  where rest = diff xs ys
diff []     []     = []
diff _      _      = error "unequal list length"

main :: IO()
main = do
  idList <- lines <$> readFile "input.txt"
  let freqencyLists = buildFrequencyLists idList
  print $ (countFor 3 freqencyLists) * (countFor 2 freqencyLists) -- 5658
  let combos = [(x, y) | x <- idList, y <- idList]
  print $ snd $ head $ filter (\((x, _), y) -> length x == length y + 1) $ zip combos $ map (uncurry diff) $ combos
