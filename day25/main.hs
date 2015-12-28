import Data.Char (isPunctuation)
import Control.Arrow ((&&&))

parseInput :: String -> (Int, Int)
parseInput s = (get 15 &&& get 17) $ words $ filter (not . isPunctuation) s
  where get n = pred . read . (!! n)

cantor (x, y) = y + (x + y)*(x + y + 1) `quot` 2

codes = iterate ((`rem` 33554393) . (* 252533)) 20151125

main :: IO()
main = print =<< ((codes !!) . cantor . parseInput) <$> readFile "input.txt"
