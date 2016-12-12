import Text.Parsec hiding (State)
import Text.Parsec.String (parseFromFile)
import Data.Char (toUpper)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe

type Value = Int
data Operand    = Register Register | Immediate Value deriving (Show)
data Register    = A | B | C | D deriving (Show, Read, Eq, Ord)
data Instruction = Inc Register
                 | Dec Register
                 | Jnz Operand Int
                 | Cpy Operand Register
                 deriving Show

data MachineState = MachineState { registers :: Map.Map Register Int
                                 , nexts :: [Instruction]
                                 , done :: [Instruction]
                                 } deriving (Show)

type MachineAction = MaybeT (State MachineState)

parseInput :: Parsec String u [Instruction]
parseInput = many (instruction <* endOfLine) <* eof where
  instruction = inc <|> dec <|> jnz <|> cpy
  inc         = Inc <$> (try (string "inc ") *> register)
  dec         = Dec <$> (try (string "dec ") *> register)
  jnz         = Jnz <$> (try (string "jnz ") *> operand) <* string " " <*> immediate
  cpy         = Cpy <$> (try (string "cpy ") *> operand) <* string " " <*> register
  operand     = (Register <$> register) <|> (Immediate <$> immediate)
  register    = (read . map toUpper) <$> (string "a" <|> string "b" <|> string "c" <|> string "d")
  immediate   = read <$> many1 (digit <|> char '-')

runInstruction :: Instruction -> MachineAction()
runInstruction i = case i of
  (Inc reg)    -> modifyReg reg succ >> jump 1
  (Dec reg)    -> modifyReg reg pred >> jump 1
  (Cpy op reg) -> evalOperand op >>= modifyReg reg . const >> jump 1
  (Jnz op off) -> condJump off =<< (/= 0) <$> evalOperand op

evalOperand :: Operand -> MachineAction Value
evalOperand (Register reg) = getReg reg
evalOperand (Immediate val) = return val

modifyReg :: Register -> (Value -> Value) -> MachineAction()
modifyReg reg f = modify (\s -> s { registers = Map.adjust f reg (registers s) } )

getReg :: Register -> MachineAction Value
getReg reg = gets $ (Map.! reg) . registers

jump :: Int -> MachineAction()
jump 0 = return ()
jump n | n > 0 = jumpPred n
       | n < 0 = jumpSucc n
  where
    jumpPred n = do
      (x:xs) <- gets nexts
      lift $ modify (\s -> s { nexts = xs, done = x:done s })
      jump $ pred n
    jumpSucc n = do
      (x:xs) <- gets done
      lift $ modify (\s -> s { done = xs, nexts = x:nexts s })
      jump $ succ n

condJump :: Int -> Bool -> MachineAction()
condJump off b = jump $ if b then off else 1

runMachine :: MachineAction()
runMachine = do
  (x:_) <- gets nexts
  runInstruction x
  runMachine

initialState :: [Instruction] -> Value -> MachineState
initialState is c = MachineState { registers = Map.fromList [(A, 0), (B, 0), (C, c), (D, 0)]
                                 , nexts     = is
                                 , done      = []
                                 }

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = aux 0 >> aux 1 where
  aux n = do
    input <- fromRight <$> parseFromFile parseInput "input.txt"
    print $ (Map.! A) $ registers $ execState (runMaybeT runMachine) (initialState input n)
