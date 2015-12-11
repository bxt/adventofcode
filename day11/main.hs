import Data.List

type Password = String

validPassword, mistakeChars, straight, twoPairs :: Password -> Bool

validPassword p = all ($p) [straight, mistakeChars, twoPairs]

mistakeChars = and . mapM notElem "oil"

straight (a:b:c:xs) = (succ a == b && succ b == c) || straight (b:c:xs)
straight _          = False

twoPairs = (>=2)
         . length
         . nub
         . sort
         . map head
         . filter ((>=2) . length)
         . group

next :: Password -> Password
next = reverse . aux . reverse
  where aux (x:xs) | x == 'z'  = 'a' : aux xs
                   | otherwise = succ x : xs

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
