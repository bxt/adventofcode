import Data.Set (member, fromList, insert)

-- | Remove leading '+', if any
-- >>> withoutLeadingPlus "+12"
-- "12"
-- >>> withoutLeadingPlus "no plus here"
-- "no plus here"
withoutLeadingPlus :: String -> String
withoutLeadingPlus ('+':xs) = xs
withoutLeadingPlus xs       = xs

-- | Parses a newline separated list of numbers with sign
-- >>> parseFrequencyList "+42\n-1337"
-- [42,-1337]
parseFrequencyList :: String -> [Int]
parseFrequencyList = map (read . withoutLeadingPlus) . lines

-- | Find first element to be twice in list
-- >>> findDuplicate [1,2,3,4,5,4,1]
-- 4
findDuplicate :: (Ord a, Eq a) => [a] -> a
findDuplicate (x:xs) = aux (fromList [x]) xs where
  aux seen (x:xs) | x `member` seen = x
                  | otherwise       = aux (x `insert` seen) xs

-- | Build cummulative sums
-- >>> accumulate [1,2,3,1000]
-- [0,1,3,6,1006]
accumulate :: [Int] -> [Int]
accumulate = scanl (+) 0

printProcessedInput :: Show a => ([Int] -> a) -> IO()
printProcessedInput processor = readFile "input.txt"
                            >>= print . processor . parseFrequencyList

-- | First puzzle, find sum of frequencies
-- >>> part1
-- 536
part1 :: IO()
part1 = printProcessedInput sum

-- | Second puzzle, find first repeated frequency when summing up
-- >>> part2
-- 75108
part2 :: IO()
part2 = printProcessedInput (findDuplicate . accumulate . cycle)

main :: IO()
main = do
  part1
  part2
