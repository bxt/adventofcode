import Data.List

type Password = String

validPassword :: Password -> Bool
validPassword p = hasStraight p && (countPairs p >= 2)

hasStraight :: Password -> Bool
hasStraight (a:b:c:xs) = (succ a == b && succ b == c) || hasStraight (b:c:xs)
hasStraight _          = False

countPairs :: Password -> Int
countPairs = length
           . nub
           . sort
           . map head
           . filter ((>=2) . length)
           . group

next :: Password -> Password
next = reverse . aux . reverse
  where aux (x:xs) | x == 'z'      =     'a' : aux xs
                   | elem x' "oil" = succ x' : xs
                   | otherwise     =      x' : xs
          where x' = succ x

nextPassword :: Password -> Password
nextPassword = head
             . filter validPassword
             . tail
             . iterate next

start :: Password
start = "hepxcrrq"

main :: IO()
main = print part2 where
  part2 = nextPassword part1
  part1 = nextPassword start
