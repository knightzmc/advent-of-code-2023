{-# LANGUAGE LambdaCase #-}

module Parsing where

import Control.Applicative
import Data.String

-- Microscopic parser combinator library
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

parse :: Parser a -> String -> Maybe a
parse (Parser p) = fmap fst . p

mustParse :: Parser a -> String -> a
mustParse p s = case parse p s of
    Just a -> a
    Nothing -> error "parse error"

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

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
    (x : xs) | f x -> Just (x, xs)
    _ -> Nothing

anyChar :: Parser Char
anyChar = satisfy (const True)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)
