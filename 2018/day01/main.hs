
withoutLeadingPlus :: String -> String
withoutLeadingPlus ('+':xs) = xs
withoutLeadingPlus xs       = xs

parseFrequencyList :: String -> [Int]
parseFrequencyList = map read . map withoutLeadingPlus . lines

-- taken from 2016/1
findDuplicate :: Eq a => [a] -> a
findDuplicate (x:xs) = aux [x] xs where
  aux ys (x:xs) | x `elem` tail ys = x
                | otherwise        = aux (x:ys) xs

cumsum :: [Int] -> [Int]
cumsum = scanl (+) 0

main :: IO()
main = do
  freqencyList <- parseFrequencyList <$> readFile "input.txt"
  print $ sum freqencyList -- 536
  print $ findDuplicate $ cumsum $ cycle freqencyList
