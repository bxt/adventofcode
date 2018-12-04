import Text.Parsec hiding (State)
import Text.Parsec.String (parseFromFile)
import Data.Array.IArray
import Data.Array.Base (UArray)

type Coords = (Int, Int)

data Rect = Rect { x :: Int
                 , y :: Int
                 , w :: Int
                 , h :: Int
                 } deriving (Show)

data Claim = Claim { identifier :: Int
                   , area :: Rect
                   } deriving (Show)

claimList :: Parsec String u [Claim]
claimList = many (claim <* endOfLine) <* eof
  where claim = Claim <$> (char '#' *> int) <* string " @ " <*> area
        area = Rect <$> int <* char ',' <*> int <* string ": " <*> int <* char 'x' <*> int
        int  = read <$> many1 digit

positions :: Claim -> [Coords]
positions Claim {area = a} = range ((x a,y a),(x a + w a - 1, y a + h a - 1))

minMax :: Ord a => [a] -> (a, a)
minMax = (,) <$> minimum <*> maximum

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = do
  input <- fromRight <$> parseFromFile claimList "input.txt"
  let xs = [x . area, (+) <$> x . area <*> w . area] <*> input
  let ys = [y . area, (+) <$> y . area <*> h . area] <*> input
  let (xFrom, xTo) = minMax xs :: Coords
  let (yFrom, yTo) = minMax ys :: Coords
  let values = (flip zip (repeat 1) . positions) =<< input :: [(Coords, Int)]
  let array = accumArray (+) 0 ((xFrom, yFrom), (xTo, yTo)) values :: Array Coords Int

  print $ length $ filter (>1) $ elems array -- 100595

  let nonOverlapping = filter (all (== 1) . map (array !) . positions) input
  print $ identifier $ head nonOverlapping
