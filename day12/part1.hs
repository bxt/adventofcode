import Text.Regex.Posix

numbers :: String -> [Int]
numbers = map read . getAllTextMatches . (=~ "-?[0-9]+")

main = print . sum . numbers =<< readFile "input.txt"
