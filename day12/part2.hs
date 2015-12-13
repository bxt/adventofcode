import qualified Data.ByteString.Lazy as B
import qualified Data.Scientific as S
import Data.Text (pack)
import Data.HashMap.Strict (elems)
import Data.Aeson
import Data.Maybe (fromJust)

-- need to run `cabal install aeson` for JSON support

nonRedSum :: B.ByteString -> S.Scientific
nonRedSum = sum . extract . fromJust . decode

extract :: Value -> [S.Scientific]
extract (Number n) = [n]
extract (Array  a) = concatMap extract a
extract (Object o) | elem needle es = []
                   | otherwise      = concatMap extract es
                   where es     = elems o
                         needle = (String (pack "red"))
extract _          = []

main = print . nonRedSum =<< B.readFile "input.txt"
