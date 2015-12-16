import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Control.Monad
import Data.Function (on)

data Part = Part1 | Part2 deriving Show

type CompoundTable = Map.Map String Int
data Aunt = Aunt { name :: String, compounds :: CompoundTable } deriving Show

parseAunts :: Parsec String u [Aunt]
parseAunts = many (aunt <* endOfLine) <* eof
  where aunt      = Aunt <$> name <* string ": " <*> compounds
        compounds = Map.fromList <$> compound `sepBy` string ", "
        compound  = (,) <$> name <* string ": " <*> value
        value     = read <$> many1 digit
        name      = many1 $ noneOf ":"

matches :: Part -> Aunt -> Aunt -> Bool
matches p a b = and $ Map.elems $ (intersection p `on` compounds) a b where
  intersection Part1 = Map.intersectionWith (==)
  intersection Part2 = Map.intersectionWithKey cmp where
   cmp "cats"        = (<)
   cmp "trees"       = (<)
   cmp "pomeranians" = (>)
   cmp "goldfish"    = (>)
   cmp _             = (==)

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

loadAunts :: String -> IO [Aunt]
loadAunts = liftM fromRight . parseFromFile parseAunts

main :: IO()
main = do
  as <- loadAunts "input.txt"
  a <- liftM head $ loadAunts "sue.txt"
  forM_ [Part1, Part2] $ \p -> do
    print p
    print $ name $ head $ filter (matches p a) as
