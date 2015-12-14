import Data.List
import Control.Monad

data Reindeer = Reindeer { name     :: String
                         , speed    :: Int
                         , flyTime  :: Int
                         , restTime :: Int
                         } deriving Show

parseReindeer :: String -> [Reindeer]
parseReindeer = map parseLine . lines
  where parseLine = parseWords . words . init
        parseWords [n, _, _, s, _, _, fT, _, _, _, _, _, _, rT, _] =
          Reindeer n (read s) (read fT) (read rT)

distanceAfter ::  Int -> Reindeer -> Int
distanceAfter s r = cycles * cycleDistance r + (min rest (flyTime r)) * speed r
  where (cycles, rest) = quotRem s (cycleDuration r)
        cycleDuration  = liftM2 (+) flyTime restTime
        cycleDistance  = liftM2 (*) speed flyTime

raceDuration :: Int
raceDuration = 2503

main :: IO()
main = mainP1 where
  mainP1  = main' $ maximum . map (distanceAfter raceDuration)
  mainP2  = main' id
  main' f = print . f . parseReindeer =<< readFile "input.txt"
