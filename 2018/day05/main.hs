import Data.Char (toLower, isSpace)
import Data.List (dropWhileEnd)
import Data.Function (on)

-- instead of x-mas bells and whistles we have some faaaaancy operators XD

(~=) :: Char -> Char -> Bool
(~=) = (==) `on` toLower

(*=-=*) :: Char -> Char -> Bool -- ^ aka reacts with
x *=-=* y = x /= y && x ~= y

zipOwnTail :: [a] -> [(a, a)]
zipOwnTail xs = zip xs (tail xs)

react :: String -> String
react (x:y:xs) | x *=-=* y = react xs
               | otherwise = x : react (y:xs)
react xs                    = xs

reactAll :: String -> String
reactAll = fst . head . dropWhile (uncurry (/=)) . zipOwnTail . iterate react

reactedLength :: String -> Int
reactedLength = length . reactAll

filterOtherChars :: Char -> String -> String
filterOtherChars needle = filter (not . (~= needle))

removeProblematicUnits :: String -> [String]
removeProblematicUnits input = map (`filterOtherChars` input) ['A'..'Z']

main :: IO()
main = do
  input <- filter (not . isSpace) <$> readFile "input.txt"
  print $ reactedLength input -- 11546
  print $ minimum $ map reactedLength $ removeProblematicUnits input -- 5124
