import Data.Char (toLower, isSpace)
import Data.Function (on)

-- instead of x-mas bells and whistles we have some faaaaancy operators XD

-- | Case-insensitive equals
-- >>> 'A' ~= 'A'
-- True
-- >>> 'A' ~= 'a'
-- True
-- >>> 'A' ~= 'B'
-- False
-- >>> 'A' ~= 'b'
-- False
(~=) :: Char -> Char -> Bool
(~=) = (==) `on` toLower

-- | Wther two letters might react
-- >>> 'A' *=-=* 'A'
-- False
-- >>> 'A' *=-=* 'a'
-- True
-- >>> 'A' *=-=* 'B'
-- False
-- >>> 'A' *=-=* 'b'
-- False
(*=-=*) :: Char -> Char -> Bool
x *=-=* y = x /= y && x ~= y

-- | React all possible elements
-- >>> reactAll "dabAcCaCBAcCcaDA"
-- "dabCBAcaDA"
reactAll :: String -> String
reactAll = foldr react "" where -- reads string from back, "" is like an empty stack
  react x (y:xs) | x *=-=* y = xs -- when element matches, discard it and pop the match
  react x xs                 = x : xs -- otherwise push to stack

reactedLength :: String -> Int
reactedLength = length . reactAll

-- | Remove all chars case-insensitively equal to needle
-- >>> filterOtherChars 'A' "dabAcCaCBAcCcaDA"
-- "dbcCCBcCcD"
-- >>> filterOtherChars 'B' "dabAcCaCBAcCcaDA"
-- "daAcCaCAcCcaDA"
-- >>> filterOtherChars 'C' "dabAcCaCBAcCcaDA"
-- "dabAaBAaDA"
-- >>> filterOtherChars 'D' "dabAcCaCBAcCcaDA"
-- "abAcCaCBAcCcaA"
filterOtherChars :: Char -> String -> String
filterOtherChars needle = filter (not . (~= needle))

-- | Generate all possible input with one char removed
-- >>> take 4 $ removeProblematicUnits "dabAcCaCBAcCcaDA"
-- ["dbcCCBcCcD","daAcCaCAcCcaDA","dabAaBAaDA","abAcCaCBAcCcaA"]
removeProblematicUnits :: String -> [String]
removeProblematicUnits input = map (`filterOtherChars` input) ['A'..'Z']

main :: IO()
main = do
  input <- filter (not . isSpace) <$> readFile "input.txt"
  print $ reactedLength input -- 11546
  print $ minimum $ map reactedLength $ removeProblematicUnits input -- 5124
