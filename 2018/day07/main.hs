
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)

data Constraint = Constraint { requirement :: Char
                             , target :: Char
                             } deriving (Show)

-- | Parse a line of the input
-- >>> parse "Step B must be finished before step D can begin."
-- Constraint {requirement = 'B', target = 'D'}
parse :: String -> Constraint
parse ['S', 't', 'e', 'p', ' ', requirement, ' ', 'm', 'u', 's', 't', ' ', 'b', 'e', ' ', 'f', 'i', 'n', 'i', 's', 'h', 'e', 'd', ' ', 'b', 'e', 'f', 'o', 'r', 'e', ' ', 's', 't', 'e', 'p', ' ', target, ' ', 'c', 'a', 'n', ' ', 'b', 'e', 'g', 'i', 'n', '.'] = Constraint requirement target

(-->) = Constraint

-- | Wether or not a step would be runnable contraint wise
-- >>> runnable ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] Set.empty 'A'
-- True
-- >>> runnable ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] Set.empty 'B'
-- False
-- >>> runnable ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] Set.empty 'D'
-- False
-- >>> runnable ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] (Set.fromList "A") 'B'
-- True
-- >>> runnable ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] (Set.fromList "A") 'D'
-- False
-- >>> runnable ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] (Set.fromList "ABC") 'D'
-- True
runnable :: [Constraint] -> Set Char -> Char -> Bool
runnable cs done c = all ((`Set.member` done) . requirement)
                   $ filter ((== c) . target) cs

-- | What letters exist
-- >>> available ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D']
-- fromList "ABCD"
available :: [Constraint] -> Set Char
available cs = Set.fromList $ (map target cs) ++ (map requirement cs)

-- | Next step resolving ties alphabetically
-- >>> nextStep ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] Set.empty
-- Just 'A'
-- >>> nextStep ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] (Set.fromList "A")
-- Just 'B'
-- >>> nextStep ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] (Set.fromList "AB")
-- Just 'C'
-- >>> nextStep ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] (Set.fromList "ABC")
-- Just 'D'
-- >>> nextStep ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D'] (Set.fromList "ABCD")
-- Nothing
-- >>> nextStep ['A' --> 'B', 'B' --> 'A'] Set.empty
-- Nothing
nextStep :: [Constraint] -> Set Char -> Maybe Char
nextStep cs done = listToMaybe
                 $ Set.toAscList
                 $ Set.filter (runnable cs done)
                 $ (`Set.difference` done)
                 $ available cs

-- | List order of steps to run
-- >>> runSequence ['A' --> 'B', 'A' --> 'C', 'C' --> 'D', 'B' --> 'D']
-- "ABCD"
runSequence :: [Constraint] -> [Char]
runSequence cs = foo cs Set.empty (nextStep cs Set.empty) where
  foo cs done (Just x) = x : foo cs (Set.insert x done) (nextStep cs (Set.insert x done))
  foo _  _    Nothing  = []

main :: IO()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  print $ runSequence input
