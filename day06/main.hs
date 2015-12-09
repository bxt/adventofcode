import Text.Parsec hiding (State)
import Text.Parsec.String (parseFromFile)
import qualified Data.Map as Map
import Data.List
import Control.Monad
import Data.Ix (inRange)

type Brightness = Int
type Light = (Int, Int)
data Operation = Operation Command Light Light deriving (Show, Eq)
data Command = On | Off | Toggle deriving (Show, Eq)
type Interpreter = Command -> Brightness -> Brightness

operations :: Parsec String u [Operation]
operations = many (operation <* endOfLine) <* eof
  where operation = Operation <$> command <* space <*> tuple <* string " through " <*> tuple
        command   = readCommand <$> (choice $ map (try . string) ["turn on", "turn off", "toggle"])
        tuple     = (,) <$> int <* char ',' <*> int
        int       = read <$> many1 digit

readCommand :: String -> Command
readCommand "turn on"  = On
readCommand "turn off" = Off
readCommand "toggle"   = Toggle

affects :: Light -> Light -> Light -> Bool
affects (f1, f2) (t1, t2) (l1, l2) = inRange (f1, t1) l1 && inRange (f2, t2) l2

runOperation :: Interpreter -> Light -> Brightness -> Operation -> Brightness
runOperation i l n (Operation c f t) = if affects f t l then i c n else n

runOperations :: Interpreter -> [Operation] -> Light -> Brightness
runOperations i os l = foldl' (runOperation i l) 0 os

calculateLights :: Interpreter -> [Light] -> [Operation] -> [Brightness]
calculateLights i ls os = map (runOperations i os) ls

lights :: Int -> [Light]
lights to = [(a,b) | a <- [0 .. to],  b <- [0 .. to]]

part1, part2 :: Interpreter

part1 On     a = 1
part1 Off    a = 0
part1 Toggle a = if a == 1 then 0 else 1

part2 On     = succ
part2 Off    = max 0 . pred
part2 Toggle = succ . succ

main :: IO()
main = mainP2 where
  mainP1 = main' part1
  mainP2 = main' part2
  main' i = parseFromFile operations "input.txt" >>= print . fmap (sum . calculateLights i (lights 999))
