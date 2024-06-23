{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Lexer where

import Control.Applicative
import Data.Char
import System.Environment
import Data.Maybe
import Text.Regex.Posix

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

-- for error messages
errorParse :: String -> Parser Token
errorParse s = Parser $ const $ Left s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Left "empty input"
    f (x:xs)
        | p x       = Right (x, xs)
        | otherwise = Left "failed satisfy"

char :: Char -> Parser Char
char c = satisfy (== c)

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap f (Parser p) = Parser $ (fmap $ first f) . p

instance Applicative Parser where
  pure a = Parser (\s -> Right (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Left s       -> Left s
      Right (f,s') -> runParser (f <$> xp) s'

instance Alternative (Either String) where
  empty = Left ""
  (Left _) <|> r = r
  l <|> _ = l

instance Alternative Parser where
  empty = Parser (const Left "")
  (Parser p1) <|> (Parser p2) = Parser (\s -> p1 s <|> p2 s)


data Token = IntLit Int
           | Ident String
           | Void
           | Int_
           | Return
           | OpenP
           | CloseP
           | OpenB
           | CloseB
           | Semi
           deriving (Show)

spaces :: Parser String
spaces = many (satisfy isSpace)

-- assumes you'll only try to match the beginning of the string
parseRegex :: String -> Parser String
parseRegex regex = Parser f
  where
    f s
      | s =~ regex = Right (match, drop (length match) s)
      | otherwise = Left $ "did not match regex: " ++ regex ++ " at " ++ s
      where match = (s =~ regex) :: String

-- takes token and matching regex as input
parseConstToken :: Token -> String -> Parser Token
parseConstToken t r = (const t) <$> parseRegex r

-- regex package was being weird for {} characters
parseCharToken :: Token -> Char -> Parser Token
parseCharToken t c = (const t) <$> char c


-- hard to generalize because token values have different types
parseIntLit :: Parser Token
parseIntLit = (IntLit . read) <$> parseRegex "^[0-9]+\\b"

parseIdent :: Parser Token
parseIdent = Ident <$> parseRegex "^[a-zA-Z_]\\w*\\b"

parseToken :: Parser Token
parseToken = parseConstToken Void "^void\\b" <|> parseConstToken Return "^return\\b" <|>
              parseConstToken Int_ "^int\\b" <|> parseCharToken OpenP '(' <|>
              parseCharToken CloseP ')' <|> parseCharToken OpenB '{' <|>
              parseCharToken CloseB '}' <|> parseCharToken Semi ';' <|>
              parseIntLit <|> parseIdent <|> errorParse "did not match any tokens"

-- for testing
repeatParse :: Integer -> Parser a -> Parser [a]
repeatParse 1 p = (:[]) <$> p
repeatParse n p = (:) <$> p <*> repeatParse (n - 1) p

testStr :: String
testStr = "\nint main(void) {\n  return 2;\n}"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  args <- getArgs
  let path = fromMaybe "" $ safeHead args
  content <- readFile path
  let result = runParser (many $ spaces *> parseToken) content
  putStrLn (show result)
