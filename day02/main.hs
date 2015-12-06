import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

data Gift = Gift Int Int Int

giftList :: Parsec String u [Gift]
giftList = many (gift <* endOfLine) <* eof
  where gift = Gift <$> (int <* char 'x') <*> (int <* char 'x') <*> int
        int  = read <$> many1 digit

paper :: Gift -> Int
paper (Gift l w h) = 2 * sum sides + minimum sides
  where sides = [l*w, w*h, h*l]

ribbon :: Gift -> Int
ribbon (Gift l w h) = l*w*h + 2 * minimum perimeters
  where perimeters = [l+w, w+h, h+l]

main :: IO()
main = main_p2 where
  main_p1 = main' paper
  main_p2 = main' ribbon
  main' f = parseFromFile giftList "input.txt" >>= print . fmap (sum . map f)
