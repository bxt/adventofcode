import Text.Parsec hiding (State)
import Text.Parsec.String (parseFromFile)
import Data.Array.IArray
import Data.Array.Base (UArray)

type Coords = (Int, Int)
type Extend = (Coords, Coords)
type ClaimMap = Array Coords Int
data Rect = Rect { x :: Int , y :: Int , w :: Int , h :: Int } deriving (Show)
data Claim = Claim { identifier :: Int , area :: Rect } deriving (Show)

claimList :: Parsec String u [Claim]
claimList = many (claim <* endOfLine) <* eof
  where claim = Claim <$> (char '#' *> int) <* string " @ " <*> area
        area = Rect <$> int <* char ',' <*> int <* string ": " <*> int <* char 'x' <*> int
        int  = read <$> many1 digit

-- | Caluclate the corner coordinates of a claim
-- >>> extend Claim { identifier = 0, area = Rect { x = 1, y = 3, w = 2, h = 4 } }
-- ((1,3),(2,6))
extend :: Claim -> Extend
extend Claim {area = a} = ((x a,y a),(x a + w a - 1, y a + h a - 1))

-- | Caluclate the bounding extend of two extends
-- >>> mergeExtends ((1,3),(2,6)) ((3,2),(4,3))
-- ((1,2),(4,6))
mergeExtends :: Extend -> Extend -> Extend
mergeExtends ((fromX1, fromY1), (toX1, toY1)) ((fromX2, fromY2), (toX2, toY2)) =
  ((min fromX1 fromX2, min fromY1 fromY2), (max toX1 toX2, max toY1 toY2))

-- | Caluclate the positions occupied by a claim
-- >>> positions Claim { identifier = 0, area = Rect { x = 1, y = 3, w = 2, h = 4 } }
-- [(1,3),(1,4),(1,5),(1,6),(2,3),(2,4),(2,5),(2,6)]
positions :: Claim -> [Coords]
positions = range . extend

-- | Use a right, crash when left
-- prop> fromRight (Right a) == a
-- >>> fromRight (Left "Ouch!")
-- *** Exception: "Ouch!"
fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

buildClaimMap :: [Claim] -> ClaimMap
buildClaimMap input = accumArray (+) 0 bounds values where
  bounds = foldl1 mergeExtends $ map extend input
  values = input >>= flip zip (repeat 1) . positions

countOverclaimed :: ClaimMap -> Int
countOverclaimed = length . filter (>1) . elems

notOverclaimed :: ClaimMap -> Claim -> Bool
notOverclaimed claimMap = all (== 1) . map (claimMap !) . positions

main :: IO()
main = do
  claims <- fromRight <$> parseFromFile claimList "input.txt"
  let claimMap = buildClaimMap claims
  print $ countOverclaimed claimMap -- 100595
  print $ identifier $ head $ filter (notOverclaimed claimMap) claims -- 415
