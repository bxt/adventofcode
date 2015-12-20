import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Data.List (sort, nub)

type Element = String
type Molecule = [String]
type Replacement = (Element, Molecule)
type Replacements = [Replacement]

parseInput :: Parsec String u (Replacements, Molecule)
parseInput = (,) <$> replacements <* endOfLine <*> molecule <* eof where
  replacements = many replacement
  replacement  = (,) <$> elementOrE <* string " => " <*> molecule
  molecule     = many1 element <* endOfLine
  elementOrE   = string "e" <|> element
  element      = (:) <$> upper <*> many lower

creations :: (Replacements, Molecule) -> [[Element]]
creations (replacements, m) = zipthough replace m
  where
    replace :: Molecule -> Element -> Molecule -> [Molecule]
    replace m1 e m2 = map (\(_, x) -> m1 ++ x ++ m2) $ filter ((==e).fst) replacements

zipthough :: ([a] -> a -> [a] -> [b]) -> [a] -> [b]
zipthough f l = aux f l [] where
  aux f (x:xs) ys = f ys x xs ++ aux f xs (ys ++ [x])
  aux _  _     _  = []

-- Based on this solution:
-- https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
calculateReplacements :: Molecule -> Int
calculateReplacements = do
  total  <- length
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
