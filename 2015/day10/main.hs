import Data.List

rle :: String -> String
rle xs = (\x -> [head $ show $ length x, head x]) =<< group xs

start :: String
start = "3113322113"

main :: IO()
main = mainP2 where
  mainP1 = main' 40
  mainP2 = main' 50
  main' n = print $ length $ iterate rle start !! n
