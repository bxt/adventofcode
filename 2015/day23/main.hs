import Text.Parsec hiding (State)
import Text.Parsec.String (parseFromFile)
import Data.Char (toUpper)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe

data Register    = A | B deriving (Show, Read, Eq, Ord)
data Instruction = Hlf Register
                 | Tpl Register
                 | Inc Register
                 | Jmp Int
                 | Jie Register Int
                 | Jio Register Int
                 deriving Show

data MachineState = MachineState { registers :: Map.Map Register Int
                                 , nexts :: [Instruction]
                                 , done :: [Instruction]
                                 } deriving (Show)

type MachineAction = MaybeT (State MachineState)
type Value = Int

parseInput :: Parsec String u [Instruction]
parseInput = many (instruction <* endOfLine) <* eof where
  instruction = hlf <|> tpl <|> inc <|> jmp <|> jie <|> jio
  hlf         = Hlf <$> (try (string "hlf ") *> register)
  tpl         = Tpl <$> (try (string "tpl ") *> register)
  inc         = Inc <$> (try (string "inc ") *> register)
  jmp         = Jmp <$> (try (string "jmp ") *> offset)
  jie         = Jie <$> (try (string "jie ") *> register) <* string ", " <*> offset
  jio         = Jio <$> (try (string "jio ") *> register) <* string ", " <*> offset
  register    = (read . map toUpper) <$> (string "a" <|> string "b")
  offset      = read <$> (optional (string "+") *> many1 (digit <|> char '-'))

runInstruction :: Instruction -> MachineAction()
runInstruction i = case i of
  (Hlf reg)     -> modifyReg reg (`quot` 2) >> jump 1
  (Tpl reg)     -> modifyReg reg (* 3)      >> jump 1
  (Inc reg)     -> modifyReg reg succ       >> jump 1
  (Jmp off)     -> jump off
  (Jie reg off) -> condJump off =<< even  <$> getReg reg
  (Jio reg off) -> condJump off =<< (==1) <$> getReg reg

modifyReg :: Register -> (Value -> Value) -> MachineAction()
modifyReg reg f = modify (\s -> s { registers = Map.adjust f reg (registers s) } )

getReg :: Register -> MachineAction Value
getReg reg = gets $ (Map.! reg) . registers

jump :: Int -> MachineAction()
jump 0 = return ()
jump n | n < 0 = jumpSucc n
       | n > 0 = jumpPred n
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
initialState is a = MachineState { registers = Map.fromList [(A, a), (B, 0)]
                                 , nexts     = is
                                 , done      = []
                                 }

fromRight :: Show a => Either a b -> b
fromRight = either (error . show) id

main :: IO()
main = aux 0 >> aux 1 where
  aux n = do
    input <- fromRight <$> parseFromFile parseInput "input.txt"
    print $ (Map.! B) $ registers $ execState (runMaybeT runMachine) (initialState input n)
