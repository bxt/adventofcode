import Data.List
import Control.Monad

loadPackages :: IO [Int]
loadPackages = (sortBy (flip compare) . map read . lines) <$> readFile "input.txt"

possibleCombinationsUsing :: Int -> Int -> [Int] -> [[Int]]
possibleCombinationsUsing _ n _ | n < 0 = []
possibleCombinationsUsing m _ _ | m < 0 = []
possibleCombinationsUsing m n _ | m == 0 && n == 0 = [[]]
possibleCombinationsUsing _ _ [] = []
possibleCombinationsUsing m n (x:xs) = map (x:) (possibleCombinationsUsing (m-1) (n-x) xs)
                                    ++ possibleCombinationsUsing m n xs

possibleCombinations :: Int -> [Int] -> [[Int]]
possibleCombinations n _ | n < 0  = []
possibleCombinations n _ | n == 0 = [[]]
possibleCombinations _ []         = []
possibleCombinations n (x:xs)     = map (x:) (possibleCombinations (n-x) xs)
                                 ++ possibleCombinations n xs

-- | Subtract sorted lists
minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] _  = []
minus (x:xs) (y:ys) = case compare x y of
  EQ ->   minus    xs (y:ys)
  GT -> x:minus    xs (y:ys)
  LT ->   minus (x:xs)   ys

main, mainP1, mainP2 :: IO()
main = mainP2

mainP1 = do
  p <- loadPackages
  let s = sum p `quot` 3
  let maxLegroom = head . filter (not . null) $ map (\i -> possibleCombinationsUsing i s p) [1 .. ]
  let balancable = filter (\pCi -> not $ null $ possibleCombinations s (p `minus` pCi)) maxLegroom
  print $ minimum $ map product balancable

mainP2 = do
  p <- loadPackages
  let s = sum p `quot` 4
  let maxLegroom = head . filter (not . null) $ map (\i -> possibleCombinationsUsing i s p) [1 .. ]
  let balancable = filter (\pCi -> not . null $ do
      s1 <- possibleCombinations s (p `minus` pCi)
      s2 <- possibleCombinations s ((p `minus` pCi) `minus` s1)
      return () ) maxLegroom
  print $ minimum $ map product balancable
