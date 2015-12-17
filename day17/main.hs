import Control.Monad
import Data.Ord (comparing)
import Data.Maybe (fromJust)

data Part = Part1 | Part2 deriving Show

minimaBy :: (a -> a -> Ordering) -> [a] -> [a]
minimaBy cmp = foldr aux [] where
  aux x []     = [x]
  aux x (m:ms) = case cmp x m
                   of LT -> [x]
                      EQ -> x:m:ms
                      _  ->   m:ms

possibleCombinationCount :: Int -> [Int] -> Int
possibleCombinationCount n _ | n < 0  = 0
possibleCombinationCount n _ | n == 0 = 1
possibleCombinationCount _ []         = 0
possibleCombinationCount n (x:xs)     = possibleCombinationCount (n-x) xs
                                      + possibleCombinationCount n xs

possibleCombinations :: Int -> [Int] -> Maybe [[Int]]
possibleCombinations 0 _      = Just [[]]
possibleCombinations _ []     = Nothing
possibleCombinations n (x:xs) | n < x     = without
                              | otherwise = let with = liftM (map (x:)) (possibleCombinations (n-x) xs)
                                             in cmb (++) without with
                     where without = possibleCombinations n xs

cmb :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
cmb f (Just a) (Just b) = Just $ f a b
cmb _ (Just a) _        = Just a
cmb _ _        (Just b) = Just b
cmb _ _        _        = Nothing

eggnog :: Int
eggnog = 150

main :: IO()
main = do
  containers <- liftM (map read . lines) $ readFile "input.txt"
  forM_ [Part1, Part2] $ \part -> do
    print part
    print $ calc part containers

calc :: Part -> [Int] -> Int
calc Part1 = possibleCombinationCount eggnog
calc Part2 = length . minimaBy (comparing length) . fromJust . possibleCombinations eggnog
