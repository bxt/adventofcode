import Text.Regex.Posix
import Data.List

type Recipe = [Int]
type Ingreidents = [[Int]]
type Cookie = [Int]

recipes :: Int      -- ^ Number of ingreidents
        -> Int      -- ^ Number of teaspoons
        -> [Recipe] -- ^ Lists of possible quantities for each ingreident
recipes 1 tsp = [[tsp]]
recipes n tsp = concat [map (i:) $ recipes (n-1) (tsp-i) | i <- [0 .. tsp] ]

numbers :: String -> [Int]
numbers = map read . getAllTextMatches . (=~ "-?[0-9]+")

parseIngreidents :: String -> Ingreidents
parseIngreidents = map numbers . lines

experiments :: Int -> Ingreidents -> [Cookie]
experiments tsp is = map (bake is) $ recipes (length is) tsp

score :: Cookie -> Int
score = product . map (max 0) . init

calories :: Cookie -> Int
calories = last

isDiet :: Cookie -> Bool
isDiet = (== 500) . calories

bake :: Ingreidents -> Recipe -> Cookie
bake is p = map (sum . zipWith (*) p) (transpose is)

main :: IO()
main = part2 where
  part1 = main' id
  part2 = main' $ filter isDiet
  main' extraStep
          = print
          . maximum
          . map score
          . extraStep
          . experiments 100
          . parseIngreidents
        =<< readFile "input.txt"
