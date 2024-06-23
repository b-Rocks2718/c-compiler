{-# LANGUAGE FlexibleContexts #-}

module AParser (Parser, runParser, satisfy, char, posInt,
                spaces, ident, parseRegex) where

import           Control.Applicative
import           Data.Char
import           Text.Regex.Posix

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Int
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap f (Parser p) = Parser $ (fmap $ first f) . p

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\s -> p1 s <|> p2 s)

spaces :: Parser String
spaces = many (satisfy isSpace)

ident :: Parser String
ident = (++) <$> some (satisfy isAlpha) <*> many (satisfy isAlphaNum)

-- assumes you'll only try to match the beginning of the string
parseRegex :: String -> Parser String
parseRegex regex = Parser f
  where
    f s
      | s =~ regex = Just (match, drop (length match) s)
      | otherwise = Nothing
      where match = (s =~ regex) :: String
