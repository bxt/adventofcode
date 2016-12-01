import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad

type Person = String
type Neighbors = (Person, Person)
type Invitations = Map.Map Neighbors Int

neighbors :: (Person, Person) -> Neighbors
neighbors (a, b) = if b < a then (b, a) else (a, b)

persons :: Neighbors -> [Person]
persons (a, b) = [a, b]

invitations :: String -> Invitations
invitations = Map.fromListWith (+) . map parseLine . lines
  where parseLine = parseWords . words . init
        parseWords [a, _, gainLose, v, _, _, _, _, _, _, b] =
          (neighbors (a, b), read v * sign gainLose)
        sign "gain" = 1
        sign "lose" = (-1)

neighborings :: [Person] -> [Neighbors]
neighborings x = map neighbors $ zip x $ tail $ cycle x

happiness :: (Person, Person) -> Invitations -> Int
happiness = Map.findWithDefault 0

seatingHappiness :: [Person] -> Invitations -> Int
seatingHappiness = liftM sum . mapM happiness . neighborings

allPersons :: Invitations -> [Person]
allPersons = nub . concatMap persons . Map.keys

possibleHappinesses :: Invitations -> [Int]
possibleHappinesses = mapM seatingHappiness . permutations =<< allPersons

addMyself :: Invitations -> Invitations
addMyself = Map.insert (me, me) 0

me :: Person
me = "Bernhard"

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = mainP2 where
  mainP1  = main' id
  mainP2  = main' addMyself
  main' f = readFile "input.txt"
        >>= print . maximum . possibleHappinesses . f . invitations
