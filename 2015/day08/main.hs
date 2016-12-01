import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Numeric
import Data.List
import Data.List.Split (splitOn)

stringList :: Parsec String u [String]
stringList = many (escapedString <* endOfLine) <* eof where
  escapedString  = quotes $ many escapedChar
  quotes         = between (char '"') (char '"')
  escapedChar    = escapeSequence <|> alphaNum
  escapeSequence = hexEscape <|> simpleEscape
  hexEscape      = toEnum . fst . head . readHex <$> (try (string "\\x") *> count 2 hexDigit)
  simpleEscape   = char '\\' *> oneOf "\\\""

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

escapeString :: String -> String
escapeString s = "\"" ++ f s ++ "\"" where
  f = replace "\"" "\\\"" . replace "\\" "\\\\"

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

countAll :: [[a]] -> Int
countAll = sum . map length

countUnescapedChars, countEscapedChars, countReescapedChars :: IO Int
countUnescapedChars = countAll . fromRight <$> parseFromFile stringList "input.txt"
countEscapedChars = countAll . lines <$> readFile "input.txt"
countReescapedChars = countAll . map escapeString . lines <$> readFile "input.txt"

main :: IO()
main = mainP2 where
  mainP1 = main' countEscapedChars countUnescapedChars
  mainP2 = main' countReescapedChars countEscapedChars
  main' f g = (-) <$> f <*> g >>= print
