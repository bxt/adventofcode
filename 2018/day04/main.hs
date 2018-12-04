import Text.Parsec hiding (State)
import Text.Parsec.String (parseFromFile)
import Data.Array.IArray
import Data.Array.Base (UArray)
import Data.Map as Map hiding (elems, assocs)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.List (sort, maximumBy)


type GID = Int
type Minute = Int
type SleepPattern = Array Minute Int
type Timestamp = (Int, Int, Int, Int, Minute) -- year, month, day, hour, minute

data Event = Event { timestamp :: Timestamp
                   , payload   :: Payload
                   } deriving (Eq, Show)

data Payload = Guard GID | Sleep | Wake deriving (Eq, Show, Ord)


minute :: Timestamp -> Minute
minute (_, _, _, _, m) = m

eventList :: Parsec String u [Event]
eventList = many (event <* endOfLine) <* eof
  where event = Event <$> timestamp <* char ' ' <*> payload
        timestamp = (,,,,)
                <$> (char '[' *> int ) <* char '-'
                <*> int <* char '-'
                <*> int <* char ' '
                <*> int <* char ':'
                <*> int <* char ']'
        payload = try guard <|> try sleep <|> wake
        guard = Guard <$> (string "Guard #" *> int) <* string " begins shift"
        sleep = string "falls asleep" *> pure Sleep
        wake = string "wakes up" *> pure Wake
        int  = read <$> many1 digit

instance Ord Event where
  -- order by date, at ties put guards first for grouping later
  e1 `compare` e2 = comparing timestamp e1 e2 `mappend` comparing payload e1 e2

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

gatherGuards :: [Event] -> Map GID SleepPattern
gatherGuards events = accumSleeps $ guards $ sort events where
  guards :: [Event] -> [(GID, [(Minute, Minute)])]
  guards (Event { payload = (Guard gid) }:xs) = intervals gid xs where
    -- finds pairs of intervals as long as possible, then goes back to gard
    intervals gid ( Event { timestamp = t1, payload = Sleep }
                  : Event { timestamp = t2, payload = Wake  }
                  : xs
                  )  = (gid, [(minute t1, minute t2 - 1)]) : intervals gid xs
    intervals _   xs = guards xs
  guards [] = []
  guards _ = error "Expecto patronum!"

  accumSleeps :: [(GID, [(Minute, Minute)])] -> Map GID SleepPattern
  accumSleeps = Map.map formSleepPattern . Map.fromListWith (++) where
    hourRange = (0, 59) :: (Minute, Minute)
    formSleepPattern :: [(Minute, Minute)] -> SleepPattern
    formSleepPattern = accumArray (+) 0 hourRange . (flip zip (repeat 1) . range =<<)


extremeGuardBy :: Ord a => (SleepPattern -> a) -> Map GID SleepPattern -> (GID, SleepPattern)
extremeGuardBy sleepPatternProp = maximumBy (comparing (sleepPatternProp . snd)) . Map.toList


part1 :: Map GID SleepPattern -> Int
part1 guards = gid * mostSleepyMinute sleepPattern where
  (gid, sleepPattern) = globallyMostSleepingGuard guards

  globallyMostSleepingGuard :: Map GID SleepPattern -> (GID, SleepPattern)
  globallyMostSleepingGuard = extremeGuardBy totalSleepTime where
    totalSleepTime :: SleepPattern -> Int
    totalSleepTime = sum . elems

  mostSleepyMinute :: SleepPattern -> Minute
  mostSleepyMinute = fst . maximumBy (comparing snd) . assocs


part2 :: Map GID SleepPattern -> Int
part2 guards = gid * maximumSleepAmount sleepPattern where
  (gid, sleepPattern) = locallyMostSleepingGuard guards

  locallyMostSleepingGuard :: Map GID SleepPattern -> (GID, SleepPattern)
  locallyMostSleepingGuard = extremeGuardBy maximumSleepAmount

  maximumSleepAmount :: SleepPattern -> Int
  maximumSleepAmount = snd . maximumBy (comparing snd) . assocs


main :: IO()
main = do
  events <- fromRight <$> parseFromFile eventList "input.txt"
  let guards = gatherGuards events
  print $ part1 guards -- 118599
  print $ part2 guards -- 33949
