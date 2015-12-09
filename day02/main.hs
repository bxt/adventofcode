import Text.Parsec
import Text.Parsec.String (parseFromFile)

data Gift = Gift Int Int Int

giftList :: Parsec String u [Gift]
giftList = many (gift <* endOfLine) <* eof
  where gift = Gift <$> int <* char 'x' <*> int <* char 'x' <*> int
        int  = read <$> many1 digit

paper :: Gift -> Int
paper (Gift l w h) = wrapping + slack
  where wrapping = 2 * sum faces
        slack    = minimum faces
        faces    = [l*w, w*h, h*l]

ribbon :: Gift -> Int
ribbon (Gift l w h) = wrap + bow
  where wrap       = 2 * minimum perimeters
        perimeters = [l+w, w+h, h+l]
        bow        = l*w*h

main :: IO()
main = main_p2 where
  main_p1 = main' paper
  main_p2 = main' ribbon
  main' f = parseFromFile giftList "input.txt" >>= print . fmap (sum . map f)
