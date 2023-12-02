{-# LANGUAGE GHC2021 #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.String
import Parsing

data Game = Game Int [[(Int, String)]] deriving (Show)

parseGame :: Parser Game
parseGame = do
    string "Game "
    n <- anyChar
    string ": "
    Game (read [n]) <$> parseScoreGroups

parseScoreGroups :: Parser [[(Int, String)]]
parseScoreGroups = do
    groups <- parseScoreGroup `sepBy` ("; ")
    pure groups

parseScoreGroup :: Parser [(Int, String)]
parseScoreGroup = parseNumCol `sepBy` (", ")

parseNumCol :: Parser (Int, String)
parseNumCol = do
    n <- read <$> some (satisfy isDigit)
    string " - "
    col <- some (satisfy isAlpha)
    pure (n, col)

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let game = parseGame <$> input
    print game
    pure ()