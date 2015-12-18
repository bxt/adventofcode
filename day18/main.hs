import Data.Array.IArray
import Control.Monad

updateArray :: Ix i => (i -> Array i a -> b) -> Array i a -> Array i b
updateArray f a =  array (bounds a) (zip is (mapM f is a))  where
  is = indices a

findWithDefault :: Ix i => a -> i -> Array i a -> a
findWithDefault def i a | inRange (bounds a) i = a!i
                        | otherwise            = def

listsArray :: [[a]] -> Array (Int, Int) a
listsArray aas = array bounds assList
  where xMax    = pred $ length $ head aas
        yMax    = pred $ length aas
        bounds  = ((0, 0), (xMax, yMax))
        assList = concatMap (\(y, xs) -> zipWith (\x v -> ((x,y),v)) [0..xMax] xs) $ zip [0..yMax] aas

gol :: (Int, Int) -> Array (Int, Int) Int -> Int
gol (x,y) a | a!(x,y) == 1 && n `elem` [2,3] = 1
            | a!(x,y) == 0 && n == 3         = 1
            | otherwise                      = 0
  where n   = sum $ map (`get` a) [(x,y-1),(x+1,y-1),(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1)]
        get = findWithDefault 0

activateCorners :: Ix i => Array (i, i) Int -> Array (i, i) Int
activateCorners a = a // zip [(x1,y1),(x1,y2),(x2,y1),(x2,y2)] (repeat 1)
  where ((x1, y1), (x2, y2)) = bounds a

readLight :: Char -> Int
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
