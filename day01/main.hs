
walk :: String -> Int
walk = sum . map move

basementAfter :: String -> Int
basementAfter = (+1) . length . takeWhile (/= -1) . scanl1 (+) . map move

move :: Char -> Int
move '(' = 1
move ')' = -1

main :: IO()
main = main_p2 where
  main_p1 = main' walk
  main_p2 = main' basementAfter
  main' f = readFile "input.txt" >>= print . f
