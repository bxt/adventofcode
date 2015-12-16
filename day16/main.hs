import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad
import Data.Function (on)

type CompoundTable = Map.Map String Int
data Aunt = Aunt { name :: String, compounds :: CompoundTable } deriving Show

-- Sue 3: trees: 6, cars: 6, children: 4

parseAunts :: Parsec String u [Aunt]
parseAunts = many (aunt <* endOfLine) <* eof
  where aunt      = Aunt <$> name <* string ": " <*> compounds
        compounds = Map.fromList <$> compound `sepBy` string ", "
        compound  = (,) <$> name <* string ": " <*> value
        value     = read <$> many1 digit
        name      = many1 $ noneOf ":"

matches :: Aunt -> Aunt -> Bool
matches a b = and $ Map.elems $ ((Map.intersectionWith (==)) `on` compounds) a b

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

loadAunts :: String -> IO [Aunt]
loadAunts = liftM fromRight . parseFromFile parseAunts

main :: IO()
main = do
  as <- loadAunts "input.txt"
  a <- liftM head $ loadAunts "sue.txt"
  print $ filter (matches a) as
