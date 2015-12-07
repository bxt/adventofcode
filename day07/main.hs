import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Data.Maybe
import Data.List

type Value = Word16
type Values = Map.Map String Value

data Operation = AND | OR | RSHIFT | LSHIFT deriving (Show, Eq, Read)
data Assignment = Assignment {gate :: Gate, to :: String} deriving (Show, Eq)
data Gate = Constant Value
          | Wire String
          | Not Gate
          | Gate2 Gate Operation Gate
          deriving (Show, Eq)

gateList :: Parsec String u [Assignment]
gateList = many (assignment <* endOfLine) <* eof
  where assignment  = Assignment <$> gate <*> (string " -> " *> wireName)
        gate        = try gate2 <|> not <|> constant <|> wire
        gate2       = Gate2 <$> (value <* space) <*> (biOperation <* space) <*> value
        biOperation = read <$> biOpName where
          biOpName  = choice $ map string ["AND", "OR", "LSHIFT", "RSHIFT"]
        not         = string "NOT " *> (Not <$> value)
        value       = wire <|> constant
        wire        = Wire <$> wireName
        constant    = Constant . read <$> many1 digit
        wireName    = many1 lower

loopAssignments :: [Assignment] -> Values
loopAssignments as = snd $ fromJust $ find (null.fst) (iterate runAssignments (as, Map.empty))

runAssignments :: ([Assignment], Values) -> ([Assignment], Values)
runAssignments ([]  , vs) = ([], vs)
runAssignments (a:as, vs) = ((if run then (a:) else id) as', vs'')
  where (vs', run)  = runAssignment a vs
        (as', vs'') = runAssignments (as, vs')

runAssignment :: Assignment -> Values -> (Values, Bool)
runAssignment a vs = fromMaybe (vs, True) (do
  v <- runGate (gate a) vs
  return (Map.insert (to a) v vs, False))

runGate :: Gate -> Values -> Maybe Value
runGate (Constant w) _  = Just w
runGate (Wire name)  vs = Map.lookup name vs
runGate (Not gate)   vs = complement <$> runGate gate vs
runGate (Gate2 gate1 op gate2) vs = runOperation op <$> runGate gate1 vs <*> runGate gate2 vs

runOperation :: Operation -> Value -> Value -> Value
runOperation AND    a b = a .&. b
runOperation OR     a b = a .|. b
runOperation LSHIFT a b = shiftL a (fromIntegral b)
runOperation RSHIFT a b = shiftR a (fromIntegral b)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

main :: IO()
main = mainP2 where
  mainP1 = main' id
  mainP2 = main' (replace (h 14146) (h 956)) where
    h x = Assignment (Constant x) "b"
  main' f = parseFromFile gateList "input.txt" >>= print . fmap ((Map.!"a").loopAssignments.f)
