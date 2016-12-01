import Data.List.Split (splitOn)
import Data.Function (on)

data Direction = N | E | S | W deriving (Show, Enum)

data Turn = R | L deriving (Show, Read)

turn :: Turn -> Direction -> Direction
turn R W = N
turn R d = succ d
turn L N = W
turn L d = pred d

type Location = (Integer, Integer)

distance :: Location -> Integer
distance = uncurry $ on (+) abs

step :: Direction -> Location -> Location
step N (x, y) = (x, pred y)
step E (x, y) = (succ x, y)
step S (x, y) = (x, succ y)
step W (x, y) = (pred x, y)

data Instruction = Turn Turn | Step deriving Show

parseInstructions :: String -> [Instruction]
parseInstructions = concatMap parseInstruction . splitOn ", " where
  parseInstruction (turn:distance) = (Turn $ read $ turn:"") : take (read distance) steps
  steps = repeat Step

runInstructions :: (Direction, Location) -> [Instruction] -> [(Direction, Location)]
runInstructions = scanl runInstruction where
  runInstruction (d, l) (Turn t) = (turn t d, l)
  runInstruction (d, l)  Step    = (d, step d l)

findDuplicate :: Eq a => [a] -> a
findDuplicate (x:xs) = aux [x] xs where
  aux ys (x:xs) | x `elem` tail ys = x
                | otherwise        = aux (x:ys) xs

main :: IO()
main = mainP2 where
  mainP1  = print . distance . snd . last =<< main'
  mainP2  = print . distance . findDuplicate . map snd =<< main'
  main'   = runInstructions (N, (0,0)) . parseInstructions <$> readFile "input.txt"
