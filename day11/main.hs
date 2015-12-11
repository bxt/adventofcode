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
next = reverse
     . tail
     . map fst
     . scanl aux (sentinel, succ)
     . reverse
  where aux (_, b) c = let c' = b c
                        in (if c' == succ last then (first, succ) else (c', id))
        sentinel = '-'
        first    = 'a'
        last     = 'z'

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
