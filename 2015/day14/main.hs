import Data.List
import Control.Monad (liftM2)
import Data.Ord (comparing)

type Time = Int
data Reindeer = Reindeer { name     :: String
                         , speed    :: Int
                         , flyTime  :: Time
                         , restTime :: Time
                         } deriving Show

maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp = foldr aux [] where
  aux x []     = [x]
  aux x (m:ms) = case cmp x m
                   of GT -> [x]
                      EQ -> x:m:ms
                      _  ->   m:ms

parseReindeer :: String -> [Reindeer]
parseReindeer = map parseLine . lines
  where parseLine = parseWords . words . init
        parseWords [n, _, _, s, _, _, fT, _, _, _, _, _, _, rT, _] =
          Reindeer n (read s) (read fT) (read rT)

distanceAfter ::  Time -> Reindeer -> Int
distanceAfter s r = cycles * cycleDistance r + min rest (flyTime r) * speed r
  where (cycles, rest) = quotRem s (cycleDuration r)
        cycleDuration  = liftM2 (+) flyTime restTime
        cycleDistance  = liftM2 (*) speed flyTime

leadersAfter :: Time -> [Reindeer] -> [Reindeer]
leadersAfter = maximaBy . comparing . distanceAfter

leaders :: [Reindeer] -> [[Reindeer]]
leaders = mapM leadersAfter [1 .. raceDuration]

scores :: [Reindeer] -> [Int]
scores = map length . group . sort . map name . concat . leaders

raceDuration :: Time
raceDuration = 2503

main :: IO()
main = mainP2 where
  mainP1  = main' $ maximum . map (distanceAfter raceDuration)
  mainP2  = main' $ maximum . scores
  main' f = print . f . parseReindeer =<< readFile "input.txt"
