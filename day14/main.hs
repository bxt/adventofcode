import Data.List
import Control.Monad

data Activity = Flying | Resting deriving Show
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

race :: Int -> [Reindeer] -> [Int]
race s rs = map (raceOne Flying s) rs

raceOne :: Activity -> Int -> Reindeer -> Int
raceOne a' s' r = aux a' s' where
  aux _ 0 = 0
  aux a s = let t = min s $ time a r
             in t * speedWhen a r + aux (after a) (s - t)

time :: Activity -> Reindeer -> Int
time Resting = restTime
time Flying  = flyTime

speedWhen :: Activity -> Reindeer -> Int
speedWhen Resting = const 0
speedWhen Flying  = speed

after :: Activity -> Activity
after Resting = Flying
after Flying  = Resting

main :: IO()
main = mainP1 where
  mainP1  = main' id
  mainP2  = main' undefined
  main' f = readFile "input.txt"
        >>= print . maximum . race 2503 . f . parseReindeer
