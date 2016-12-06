import Data.List (group, sort, sortOn, transpose)

columnValuesByFrequencyAscending :: (Ord a, Eq a) => [[a]] -> [[a]]
columnValuesByFrequencyAscending = map (map head . sortOn length . group . sort) . transpose

main :: IO()
main = do
  result <- columnValuesByFrequencyAscending . lines <$> readFile "input.txt"
  putStr "Part One: " >> putStrLn (map last result)
  putStr "Part Two: " >> putStrLn (map head result)
