import Data.List (group, sort, sortOn, transpose)
import Control.Monad (forM_)

columnValuesByFrequencyAscending :: (Ord a, Eq a) => [[a]] -> [[a]]
columnValuesByFrequencyAscending =
  map ( map head
      . sortOn length
      . group
      . sort
      )
  . transpose

main :: IO()
main = do
  result <- columnValuesByFrequencyAscending . lines <$> readFile "input.txt"
  forM_ [("One", last), ("Two", head)] $ \(part, aggregator) ->
    putStrLn $ "Part " ++ part ++ ": " ++ map aggregator result
