{-# LANGUAGE LambdaCase #-}

module Parsing where

import Control.Applicative
import Data.String

-- Microscopic parser combinator library
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
        (a, s') <- p s
        pure (f a, s')

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (Parser p1) <*> (Parser p2) = Parser $ \s -> do
        (f, s') <- p1 s
        (a, s'') <- p2 s'
        pure (f a, s'')

instance Monad Parser where
    (Parser p1) >>= f = Parser $ \s -> do
        (a, s') <- p1 s
        runParser (f a) s'

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance IsString (Parser String) where
    fromString = string

anyChar :: Parser Char
anyChar = Parser $ \case
    (x : xs) -> Just (x, xs)
    _ -> Nothing

char :: Char -> Parser Char
char c = Parser $ \case
    (x : xs) | x == c -> Just (c, xs)
    _ -> Nothing

string :: String -> Parser String
string = traverse char

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)
