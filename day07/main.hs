import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.String
import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad.State.Lazy

type Value = Word16
type Values = Map.Map String Gate

data Operation = AND | OR | RSHIFT | LSHIFT deriving (Show, Eq, Read)
data Gate = Constant Value
          | Wire String
          | Not Gate
          | Gate2 Gate Operation Gate
          deriving (Show, Eq)

valueMap :: Parsec String u Values
valueMap = Map.fromList <$> many (assignment <* endOfLine) <* eof
  where assignment  = flip (,) <$> gate <*> (string " -> " *> wireName)
        gate        = try gate2 <|> not <|> constant <|> wire
        gate2       = Gate2 <$> (value <* space) <*> (biOperation <* space) <*> value
        biOperation = read <$> biOpName where
          biOpName  = choice $ map string ["AND", "OR", "LSHIFT", "RSHIFT"]
        not         = string "NOT " *> (Not <$> value)
        value       = wire <|> constant
        wire        = Wire <$> wireName
        constant    = Constant . read <$> many1 digit
        wireName    = many1 lower

calculate :: String -> Values -> Value
calculate v = evalState (getGate v >>= runGate)

getGate :: String -> State Values Gate
getGate name = gets (Map.! name)

setGate :: String -> Gate -> State Values ()
setGate name gate = modify (Map.insert name gate)

runGate :: Gate -> State Values Value
runGate (Constant v) = return v
runGate (Wire name)  = do
  v <- getGate name >>= runGate
  setGate name . Constant $ v
  return v
runGate (Not gate)   = complement <$> runGate gate
runGate (Gate2 gate1 op gate2) = runOperation op <$> runGate gate1 <*> runGate gate2

runOperation :: Operation -> Value -> Value -> Value
runOperation AND    a b = a .&. b
runOperation OR     a b = a .|. b
runOperation LSHIFT a b = shiftL a (fromIntegral b)
runOperation RSHIFT a b = shiftR a (fromIntegral b)

main :: IO()
main = mainP1 where
  mainP1 = main' id
  mainP2 = main' (Map.insert "b" (Constant 956))
  main' f = parseFromFile valueMap "input.txt" >>= print . fmap (calculate "a" . f)
