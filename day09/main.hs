import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad.Reader

type City = String
type DistanceTable = Map.Map (String, String) Int

distanceTable :: Parsec String u DistanceTable
distanceTable = Map.fromList <$> many (distance <* endOfLine) <* eof
  where distance    = (,) <$> cities <*> (string " = " *> value)
        cities      = (,) <$> city <*> (string " to " *> city)
        value       = read <$> many1 digit
        city        = (:) <$> upper <*> many letter

pairs :: [a] -> [(a, a)]
pairs x = zip x $ tail x

distance :: (City, City) -> DistanceTable -> Int
distance (f, t) = fmap fromJust $ mplus <$> Map.lookup (t, f) <*> Map.lookup (f, t)

routeLength :: [City] -> DistanceTable -> Int
routeLength = liftM sum . mapM distance . pairs

cities :: DistanceTable -> [City]
cities = nub . ap [fst, snd] . Map.keys

routeLengths :: DistanceTable -> [Int]
routeLengths = mapM routeLength . permutations =<< cities

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = mainP2 where
  mainP1 = main' minimum
  mainP2 = main' maximum
  main' f = parseFromFile distanceTable "input.txt" >>= print . fmap f routeLengths . fromRight
