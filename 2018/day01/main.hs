import Data.Set (member, fromList, insert)

withoutLeadingPlus :: String -> String
withoutLeadingPlus ('+':xs) = xs
withoutLeadingPlus xs       = xs

parseFrequencyList :: String -> [Int]
parseFrequencyList = map (read . withoutLeadingPlus) . lines

findDuplicate :: (Ord a, Eq a) => [a] -> a
findDuplicate (x:xs) = aux (fromList [x]) xs where
  aux seen (x:xs) | x `member` seen = x
                  | otherwise       = aux (x `insert` seen) xs

cumsum :: [Int] -> [Int]
cumsum = scanl (+) 0

main :: IO()
main = do
  freqencyList <- parseFrequencyList <$> readFile "input.txt"
  print $ sum freqencyList -- 536
  print $ findDuplicate $ cumsum $ cycle freqencyList -- 75108
