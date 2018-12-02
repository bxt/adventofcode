import Data.Map (fromListWith, toList)
import Data.List (tails)

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = toList . fromListWith (+) . map (\x -> (x,1))

checksum :: [String] -> Int
checksum idList = product $ map (`countWhereAnySndIs` freqencyLists) [2,3]
  where
    freqencyLists = map frequencies idList
    countWhereAnySndIs :: Eq a => a -> [[(b, a)]] -> Int
    countWhereAnySndIs n = length . filter (any ((== n) . snd))

keepCommon :: Eq a => [a] -> [a] -> [a]
keepCommon xs ys = map fst $ filter (uncurry (==)) $ zip xs ys

findCommonsOfPairWithOneDifference :: Eq a => [[a]] -> [a]
findCommonsOfPairWithOneDifference = head . (>>= aux) . tails where
  aux (x:xs) = filter ((== length x - 1) . length) $ map (keepCommon x) xs

main :: IO()
main = do
  idList <- lines <$> readFile "input.txt"
  print $ checksum idList -- 5658
  print $ findCommonsOfPairWithOneDifference idList -- nmgyjkpruszlbaqwficavxneo
