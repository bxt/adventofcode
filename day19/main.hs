import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Data.List
import Data.Maybe (fromJust)
import qualified Control.Applicative as A

type Element = String
type Molecule = [String]
type Replacement = (Element, Molecule)
type Replacements = [Replacement]

parseInput :: Parsec String u (Replacements, Molecule)
parseInput = (,) <$> replacements <* endOfLine <*> molecule <* eof where
  replacements = many replacement
  replacement  = (,) <$> element <* string " => " <*> molecule
  molecule     = many1 element' <* endOfLine
  element      = string "e" <|> element'
  element'     = (:) <$> upper <*> many lower

creations :: (Replacements, Molecule) -> [[Element]]
creations (replacements, m) = zipthough replace m
  where
    replace :: Molecule -> Element -> Molecule -> [Molecule]
    replace m1 e m2 = map (\(_, x) -> m1 ++ x ++ m2) $ filter ((==e).fst) replacements

zipthough :: ([a] -> a -> [a] -> [b]) -> [a] -> [b]
zipthough f l = aux f l [] where
  aux f (x:xs) ys = f ys x xs ++ aux f xs (ys ++ [x])
  aux _  _     _  = []

meltdown :: (Replacements, Molecule) -> Int
meltdown (rs, m) = fromJust $ tryMelt ("x",["x"]) m $ cycle rs

tryMelt :: Replacement -> Molecule -> Replacements -> Maybe Int
tryMelt x ["e"] _      = Just 0
tryMelt x m     (r:rs) = tryMelt1 x m (r:rs) A.<|> tryMelt2 x m rs

tryMelt1 :: Replacement -> Molecule -> Replacements -> Maybe Int
tryMelt1 x m rs@(r@(e',m'):_) = do
  m'' <- rep m' e' m
  n <- tryMelt r m'' rs
  return $ succ n

tryMelt2 :: Replacement -> Molecule -> Replacements -> Maybe Int
tryMelt2 x m (r:rs) | x == r    = Nothing
                    | otherwise = tryMelt x m (r:rs)

rep :: Eq a => [a] -> a -> [a] -> Maybe [a]
rep a b s@(x:xs) | a `isPrefixOf` s = Just $ b : drop (length a) s
                 | otherwise        = fmap (x:) $ rep a b xs
rep _ _ [] = Nothing

-- Based on this solution:
-- https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
calculateReplacements :: Molecule -> Int
calculateReplacements = do
  total <- length
  braces <- length <$> filter (=="Rn")
  commas <- length <$> filter (=="Y")
  return $ total - 2 * (commas + braces) - 1

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = do
  input <- fromRight <$> parseFromFile parseInput "input.txt"
  print . length . nub . sort . creations $ input
  print . calculateReplacements . snd $ input
