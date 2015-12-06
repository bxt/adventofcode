import Data.List

type Rule = String -> Bool

niceBy :: [Rule] -> Rule
niceBy rules s = all ($ s) rules

threeVowels, doubleLetter, blacklist, doublePair, withBetween :: Rule

threeVowels = (==3) . length . take 3 . filter (`elem` vowels)
  where vowels = "aeiou"

doubleLetter (x:xs@(y:ys)) = x == y || doubleLetter xs
doubleLetter _            = False

blacklist s = any (`isInfixOf` s) forbiddenStrings
  where forbiddenStrings = ["ab", "cd", "pq", "xy"]

doublePair (x:xs@(y:ys)) = [x, y] `isInfixOf` ys || doublePair xs
doublePair _             = False

withBetween (x:xs@(y1:y2:ys)) = x == y2 || withBetween xs
withBetween _                 = False

main :: IO()
main = mainP2 where
  mainP1 = main' [threeVowels, doubleLetter, not.blacklist]
  mainP2 = main' [doublePair, withBetween]
  main' rules = readFile "input.txt" >>= print . length . filter (niceBy rules) . lines
