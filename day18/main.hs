import Data.Array.IArray
import Control.Monad
import Data.List (transpose, delete)

type Coords = (Int, Int)
type Light = Int

updateArray :: Ix i => (i -> Array i a -> b) -> Array i a -> Array i b
updateArray f a =  array (bounds a) [(i, f i a) | i <- indices a]

findWithDefault :: Ix i => a -> i -> Array i a -> a
findWithDefault def i a | inRange (bounds a) i = a!i
                        | otherwise            = def

listsArray :: [[a]] -> Array Coords a
listsArray aas = array bounds list
  where xMax   = pred $ length $ head aas
        yMax   = pred $ length aas
        bounds = ((0, 0), (xMax, yMax))
        list   = zip (range bounds) $ concat $ transpose aas

gol :: Coords -> Array Coords Light -> Light
gol (x,y) a | a!(x,y) == 1 && n `elem` [2,3] = 1
            | a!(x,y) == 0 && n == 3         = 1
            | otherwise                      = 0
  where n   = sum $ map (`get` a) $ neighbors (x,y)
        get = findWithDefault 0

neighbors :: Coords -> [Coords]
neighbors (x,y) = delete (x,y) $ range ((x-1, y-1), (x+1, y+1))

activateCorners :: Ix i => Array (i, i) Light -> Array (i, i) Light
activateCorners a = a // zip [(x1,y1),(x1,y2),(x2,y1),(x2,y2)] (repeat 1)
  where ((x1, y1), (x2, y2)) = bounds a

readLight :: Char -> Light
readLight '.' = 0
readLight '#' = 1

main :: IO()
main = mainP2 where
  mainP1 = main' id
  mainP2 = main' activateCorners
  main' f = do
    lights <- listsArray . map (map readLight) . lines <$> readFile "input.txt"
    let result = f $ iterate (updateArray gol . f) lights !! 100
    print $ sum $ elems result
