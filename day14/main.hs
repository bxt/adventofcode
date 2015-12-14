import Data.List
import Control.Monad (liftM2)
import Data.Ord (comparing)

type Time = Int
data Reindeer = Reindeer { name     :: String
                         , speed    :: Int
                         , flyTime  :: Time
                         , restTime :: Time
                         } deriving Show

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

leaderAfter :: Time -> [Reindeer] -> Reindeer
leaderAfter = maximumBy . comparing . distanceAfter

leaders :: [Reindeer] -> [Reindeer]
leaders = mapM leaderAfter [1 .. raceDuration]

raceDuration :: Time
raceDuration = 2503

main :: IO()
main = mainP2 where
  mainP1  = main' $ map (distanceAfter raceDuration)
  mainP2  = main' $ map length . group . sort . map name . leaders
  main' f = print . maximum . f . parseReindeer =<< readFile "input.txt"
