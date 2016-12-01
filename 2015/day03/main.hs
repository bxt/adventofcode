import Data.List

type House = (Integer, Integer)

walk, walkBoth :: String -> [House]
walk = scanl move (0,0)
walkBoth = dualScanl move (0,0) (0,0)
  where
    dualScanl f s1 s2 ls = s1 : s2 : (case ls of
                               []     -> []
                               x:y:xs -> dualScanl f (f s1 x) (f s2 y) xs)

move :: House -> Char -> House
move (x,y) 'v' = (x,y+1)
move (x,y) '<' = (x-1,y)
move (x,y) '^' = (x,y-1)
move (x,y) '>' = (x+1,y)

main :: IO()
main = main_p2 where
  main_p1 = main' walk
  main_p2 = main' walkBoth
  main' f = readFile "input.txt" >>= print . length . group . sort . f
