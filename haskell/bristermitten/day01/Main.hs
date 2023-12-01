

module Main where
import qualified Data.Char as Char
import Data.Maybe (maybeToList)

getInput :: IO String
getInput = readFile "input.txt"

firstAndLast [] = error "No digits found"
firstAndLast [x] = (x, x)
firstAndLast xs = (head xs, last xs)

process :: String -> Int
process = uncurry ((+) . (10 *)) . firstAndLast . map (read . pure) . filter Char.isDigit


process2 :: String -> Int
process2 = uncurry ((+) . (10 *)) . firstAndLast . runParseParts
    where
        runParseParts :: String -> [Int]
        runParseParts [] = []
        runParseParts xs = let (n, rest) = parseParts xs in maybeToList n ++ runParseParts rest

        parseParts :: String -> (Maybe Int, String)
        parseParts [] = (Nothing, [])
        parseParts ('z':'e':'r':'o':xs) = (Just 0, 'e':'r':'o':xs)
        parseParts ('o':'n':'e':xs) = (Just 1, 'n':'e':xs)
        parseParts ('t':'w':'o':xs) = (Just 2, 'w':'o':xs)
        parseParts ('t':'h':'r':'e':'e':xs) = (Just 3, 'h':'r':'e':'e':xs)
        parseParts ('f':'o':'u':'r':xs) = (Just 4, 'o':'u':'r':xs)
        parseParts ('f':'i':'v':'e':xs) = (Just 5, 'i':'v':'e':xs)
        parseParts ('s':'i':'x':xs) = (Just 6, 'i':'x':xs)
        parseParts ('s':'e':'v':'e':'n':xs) = (Just 7, 'e':'v':'e':'n':xs)
        parseParts ('e':'i':'g':'h':'t':xs) = (Just 8, 'i':'g':'h':'t':xs)
        parseParts ('n':'i':'n':'e':xs) = (Just 9, 'i':'n':'e':xs)
        parseParts (x:xs) = if Char.isDigit x then (Just (read [x]), xs) else (Nothing, xs)

main :: IO ()
main = do
    input <- lines <$> getInput
    print (sum (map process input))
    print (sum (map process2 input))