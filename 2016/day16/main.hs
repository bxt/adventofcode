import Control.Monad (forM_)

find :: (a -> Bool) -> [a] -> a
find p (x:xs) | p x       = x
              | otherwise = find p xs

generate :: Int -> String -> String
generate l = take l . find ((>= l) . length) . iterate expand where
  expand s = s ++ '0' : map tr (reverse s)
  tr '0' = '1'
  tr '1' = '0'

checksum :: String -> String
checksum = find (odd . length) . iterate shrink where
  shrink (x:y:xs) | x == y = '1' : shrink xs
                  | x /= y = '0' : shrink xs
  shrink []                = []

input :: String
input = "10001110011110000"

main = forM_ [("One", 272), ("Two", 35651584)] $ \(part, l) ->
  putStrLn $ (("Part " ++ part ++ ": ") ++) $ checksum $ generate l input
