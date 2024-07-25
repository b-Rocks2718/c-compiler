{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Lexer where

import Control.Applicative
import Data.Char
import Text.Regex.Posix

newtype Parser a b =
  Parser { runParser :: [a] -> Either String (b, [a]) }

satisfy :: Show a => (a -> Bool) -> Parser a a
satisfy p = Parser f
  where
    f [] = Left "empty input"
    f (x:xs)
        | p x       = Right (x, xs)
        | otherwise = Left $ show x ++ " failed satisfy"

match :: (Show a, Eq a) => a -> Parser a a
match c = satisfy (==c)

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor (Parser a) where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative (Parser a) where
  pure a = Parser (\s -> Right (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Left s       -> Left s
      Right (f,s') -> runParser (f <$> xp) s'

instance Alternative (Either String) where
  empty = Left ""
  (Left _) <|> r = r
  l <|> _ = l

instance Alternative (Parser a) where
  empty = Parser (\_ -> Left "")
  (Parser p1) <|> (Parser p2) = Parser (\s -> p1 s <|> p2 s)

data Token = IntLit Int
           | Ident String
           | Void
           | IntTok
           | ReturnTok
           | OpenP
           | CloseP
           | OpenB
           | CloseB
           | Semi
           | Tilde
           | IncTok
           | DecTok
           | Plus
           | Asterisk
           | Slash
           | Percent
           | Minus
           | None -- only used for error detection
           deriving (Show, Eq)

spaces :: Parser Char String
spaces = many (satisfy isSpace)

-- assumes you'll only try to match the beginning of the string
lexRegex :: String -> Parser Char String
lexRegex regex = Parser f
  where
    f s
      | s =~ regex = Right (match, drop (length match) s)
      | otherwise = Left $ "did not match regex: " ++ regex ++
        " at " ++ head (lines s)
      where match = (s =~ regex) :: String

-- takes token and matching regex as input
lexConstToken :: Token -> String -> Parser Char Token
lexConstToken t r = t <$ lexRegex r

-- to ensure the entire file was lexed
lexEOF :: Parser Char Token
lexEOF = spaces *> Parser f
  where
    f s
      | null s = Right (None, s)
      | otherwise = Left $ "Could not lex: " ++ head (lines s)

-- hard to generalize because token values have different types
lexIntLit :: Parser Char Token
lexIntLit = IntLit . read <$> lexRegex "^[0-9]+\\b"

lexIdent :: Parser Char Token
lexIdent = Ident <$> lexRegex "^[a-zA-Z_]\\w*\\b"

lexToken :: Parser Char Token
lexToken = lexConstToken Void "^void\\b" <|>
           lexConstToken ReturnTok "^return\\b" <|>
           lexConstToken IntTok "^int\\b" <|>
           lexConstToken OpenP "^\\(" <|>
           lexConstToken CloseP "^\\)" <|>
           lexConstToken OpenB "^\\{" <|>
           lexConstToken CloseB "^\\}" <|>
           lexConstToken Semi "^;" <|>
           lexConstToken Tilde "^~" <|>
           lexConstToken IncTok "^\\+\\+" <|>
           lexConstToken Asterisk "^\\*" <|>
           lexConstToken Slash "^/" <|>
           lexConstToken Percent "^\\%" <|>
           lexConstToken DecTok "^--" <|>
           lexConstToken Plus "^\\+" <|>
           lexConstToken Minus "^-" <|>
           lexIntLit <|>
           lexIdent

-- for testing
repeatParse :: Integer -> Parser Char a -> Parser Char [a]
repeatParse 0 _ = pure []
repeatParse n p = (:) <$> p <*> repeatParse (n - 1) p

-- main function
lexer :: Parser Char [Token]
lexer = many (spaces *> lexToken) <* lexEOF

lexerEval :: String -> Either String ([Token], String)
lexerEval = runParser lexer

removeComments :: String -> String -> String
removeComments regex s
  | match = preprocess (a ++ c)
  | otherwise = s
  where
    match = (s =~ regex) :: Bool
    (a, b, c) = (s =~ regex) :: (String, String, String)

-- regex library is in multiline mode
-- I coudln't figure out how to fix it
-- so newlines are removed here

-- inline comments (//) are removed first while newlines are still there
-- then newlines are removed
-- then multi line (/* */) comments can be removed
preprocess :: String -> String
preprocess = removeComments "/\\*([^*]|\\*+[^/])*\\*+/" .
             unwords . lines . removeComments "//.*$"

-- useful for testing
showEither :: (Show a, Show b) => Either a b -> String
showEither (Left s) = show s
showEither (Right s) = show s

showEitherStr :: Either String String -> String
showEitherStr (Left s) = s
showEitherStr (Right s) = s
