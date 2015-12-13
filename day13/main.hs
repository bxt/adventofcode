import Text.Parsec
import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad

type Person = String
type Invitations = Map.Map (Person, Person) Int

invitations :: Parsec String u Invitations
invitations = Map.fromList <$> many (invitation <* endOfLine) <* eof
  where invitation  = aux <$> person <* s1 <*> value <* s2 <*> person <* char '.'
        s1          = string " would "
        s2          = string " happiness units by sitting next to "
        value       = (*) <$> sign <* space <*> digits
        sign        = 1 <$ string "gain" <|> (-1) <$ string "lose"
        digits      = read <$> many1 digit
        person      = (:) <$> upper <*> many letter
        aux p1 v p2 = ((p1, p2), v)

neighbourings :: [a] -> [(a, a)]
neighbourings x = pairs x ++ pairs (reverse x)
  where pairs x = zip x $ tail $ cycle x

happiness :: (Person, Person) -> Invitations -> Int
happiness = flip (Map.!)

seatingHappiness :: [Person] -> Invitations -> Int
seatingHappiness = liftM sum . mapM happiness . neighbourings

persons :: Invitations -> [Person]
persons = nub . map fst . Map.keys

possibleHappinesses :: Invitations -> [Int]
possibleHappinesses = mapM seatingHappiness . permutations =<< persons

addMyself :: Invitations -> Invitations
addMyself = foldr inserts <*> persons
  where inserts p = Map.insert (p, me) 0 . Map.insert (me, p) 0

me :: Person
me = "Bernhard"

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = mainP2 where
  mainP1  = main' id
  mainP2  = main' addMyself
  main' f = parseFromFile invitations "input.txt"
        >>= print . maximum . possibleHappinesses . f . fromRight
