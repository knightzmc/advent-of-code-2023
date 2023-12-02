{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import Control.Applicative (Alternative (some, (<|>)))
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Parsing (Parser, char, mustParse, satisfy, sepBy, string)

data Game = Game Int [Map String Int] deriving (Show)

gameId :: Game -> Int
gameId (Game n _) = n

parseGame :: Parser Game
parseGame = do
    _ <- string "Game "
    n <- some (satisfy Char.isDigit)
    _ <- string ": "
    Game (read n) <$> parseScoreGroups

parseScoreGroups :: Parser [Map String Int]
parseScoreGroups = parseScoreGroup `sepBy` string "; "

parseScoreGroup :: Parser (Map String Int)
parseScoreGroup = Map.fromList <$> (parseNumCol `sepBy` string ", ")

parseNumCol :: Parser (String, Int)
parseNumCol = do
    n <- read <$> some (satisfy Char.isDigit)
    _ <- char ' '
    col <- "red" <|> "blue" <|> "green"
    pure (col, n)

gameSatisfies :: Game -> [(String, Int)] -> Bool
gameSatisfies (Game _ groups) nums =
    let gameIsOk game = all (\(k, num) -> maybe True (<= num) (Map.lookup k game)) nums
     in all gameIsOk groups

gameMin :: Game -> Map String Int
gameMin (Game _ groups) =
    Map.unionsWith max groups

part1 :: [String] -> Int
part1 input =
    let games = mustParse parseGame <$> input
        satisfying = filter (\game -> gameSatisfies game [("red", 12), ("blue", 14), ("green", 13)]) games
     in sum (gameId <$> satisfying)

part2 :: [String] -> Int
part2 input =
    let games = mustParse parseGame <$> input
        mins = gameMin <$> games
        powers = Map.foldr (*) 1 <$> mins
     in sum powers

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print (part1 input)
    print (part2 input)
