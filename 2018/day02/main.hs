import Data.Map (fromListWith, toList)
import Data.List (tails)

-- | Find how often each element occurs in list
-- >>> frequencies [1,2,3,1,1,3,1]
-- [(1,4),(2,1),(3,2)]
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = toList . fromListWith (+) . map (\x -> (x,1))

-- | Multiply number of IDs with a letter exactly 2 times and 3 times
-- >>> checksum ["abcdef","bababc","abbcde","abcccd","aabcdd","abcdee","ababab" ]
-- 12
checksum :: [String] -> Int
checksum idList = product $ map (`countWhereAnySndIs` freqencyLists) [2,3]
  where
    freqencyLists = map frequencies idList
    countWhereAnySndIs :: Eq a => a -> [[(b, a)]] -> Int
    countWhereAnySndIs n = length . filter (any ((== n) . snd))

-- | Keeps the items which are the same in two lists
-- >>> keepCommon "abcde" "axcye"
-- "ace"
-- >>> keepCommon "fghij" "fguij"
-- "fgij"
keepCommon :: Eq a => [a] -> [a] -> [a]
keepCommon xs ys = map fst $ filter (uncurry (==)) $ zip xs ys

-- | Finds a pair with only one letter diffent between them
-- >>> findCommonsOfPairWithOneDifference ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"]
-- "fgij"
findCommonsOfPairWithOneDifference :: Eq a => [[a]] -> [a]
findCommonsOfPairWithOneDifference = head . (>>= aux) . tails where
  aux (x:xs) = filter ((== length x - 1) . length) $ map (keepCommon x) xs

-- | First part of the puzzle, find the checksum
-- >>> part1
-- 5658
part1 :: IO()
part1 = readFile "input.txt"
    >>= print . checksum . lines

-- | Second part of the puzzle, find the closest pair
-- >>> part2
-- nmgyjkpruszlbaqwficavxneo
part2 :: IO()
part2 = readFile "input.txt"
    >>= putStrLn . findCommonsOfPairWithOneDifference . lines

main :: IO()
main = do
  part1
  part2
