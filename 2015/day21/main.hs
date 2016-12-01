import Text.Parsec
import Text.Parsec.String (parseFromFile)
import Data.Maybe (fromJust)
import Data.List (tails)
import Control.Applicative (liftA2)

data Character = Character { nick :: String, baseStats :: Stats } deriving (Show, Eq)
data Item = Item { name :: String, costs :: Int, stats :: Stats } deriving (Show, Eq)
type Equip = [Item]
data Stats = Stats { hp, damage, armor :: Int } deriving (Show, Eq)
data Shop = Shop { unShop :: [(Department, [Item])] } deriving (Show, Eq)
data Department = Weapons | Armor | Rings deriving (Show, Read, Eq)

departments :: [Department]
departments = [Weapons, Armor, Rings]

(<++>) :: (Applicative f, Monoid m) => f m -> f m -> f m
(<++>) = liftA2 mappend

instance Monoid Stats where
  mempty = Stats { hp = 0, damage = 0, armor = 0 }
  s1 `mappend` s2 = Stats { hp     = hp     s1 + hp     s2
                          , damage = damage s1 + damage s2
                          , armor  = armor  s1 + armor  s2
                          }

parseNumber :: Parsec String u Int
parseNumber = read <$> many1 digit

parseInput :: String -> Parsec String u Character
parseInput n = Character n <$> parseStats <* eof where
  parseStats = Stats <$> the "Hit Points" <*> the "Damage" <*> the "Armor"
  the s      = string (s ++ ": ") *> parseNumber <* endOfLine

parseShop :: Parsec String u Shop
parseShop = Shop <$> many department <* eof where
  department  = (,) <$> dptHeadline <*> items <* endOfLine
  dptHeadline = dptName <* string ": " <* many (noneOf "\n") <* endOfLine
  dptName     = read <$> foldl1 (<|>) (map (string . show) departments)
  items       = many1 item <?> "items"
  item        = Item <$> parseName <*> numberSpace <*> parseStats <* endOfLine
  parseName   = init . concat <$> many1 (many1 (alphaNum <|> char '+') <++> fmap (:[]) space) <* skipMany1 space
  parseStats  = Stats 0 <$> numberSpace <*> parseNumber
  numberSpace = parseNumber <* skipMany1 space <?> "table entry"

equippedStats :: Character -> Equip -> Stats
equippedStats c is = mconcat $ baseStats c : map stats is

beats :: Stats -> Stats -> Bool
s1 `beats` s2 = s1 `hitsAgainst` s2 <= s2 `hitsAgainst` s1

hitsAgainst :: Stats -> Stats -> Int
s1 `hitsAgainst` s2 = hp s2 `quotCeil` effectiveDamage where
  effectiveDamage = max 1 $ damage s1 - armor s2

quotCeil :: Integral a => a -> a -> a
quotCeil a b | r == 0    = q
             | otherwise = q + 1
  where (q,r) = quotRem a b

totalCosts :: Equip -> Int
totalCosts = sum . map costs

validEquips :: Shop -> [Equip]
validEquips = foldl1 (<++>) . mapM validEquipsFrom departments

validEquipsFrom :: Department -> Shop -> [Equip]
validEquipsFrom d = aux d . itemsFrom d where
  aux Weapons =              singletons
  aux Armor   = nothing <++> singletons
  aux Rings   = nothing <++> singletons <++> pairs
  nothing _   = [[]]
  singletons  = map (:[])
  pairs is    = [ [x,y] | (x:xs) <- tails is , y <- xs ]

itemsFrom :: Department -> Shop -> [Item]
itemsFrom d = fromJust . lookup d . unShop

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = do
  boss       <- fromRight <$> parseFromFile (parseInput "Boss") "input.txt"
  let player =  Character { nick = "Henry Case", baseStats = mempty { hp = 100 } }
  equips     <- validEquips . fromRight <$> parseFromFile parseShop "shop.txt"

  let beatsBossWith = (`beats` baseStats boss) . equippedStats player
  print $ minimum $ map totalCosts $ filter        beatsBossWith  equips
  print $ maximum $ map totalCosts $ filter (not . beatsBossWith) equips
