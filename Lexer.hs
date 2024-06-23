{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Lexer where

import Control.Applicative
import Data.Char
import System.Environment
import Data.Maybe
import Text.Regex.Posix

-- 'Parser a' will take [Token] as input instead of String
newtype Lexer a =
  Lexer { runLexer :: String -> Either String (a, String) }

satisfy :: (Char -> Bool) -> Lexer Char
satisfy p = Lexer f
  where
    f [] = Left "empty input"
    f (x:xs)
        | p x       = Right (x, xs)
        | otherwise = Left "failed satisfy"

char :: Char -> Lexer Char
char c = satisfy (== c)

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Lexer where
  fmap f (Lexer p) = Lexer $ (fmap $ first f) . p

instance Applicative Lexer where
  pure a = Lexer (\s -> Right (a, s))
  (Lexer fp) <*> xp = Lexer $ \s ->
    case fp s of
      Left s       -> Left s
      Right (f,s') -> runLexer (f <$> xp) s'

instance Alternative (Either String) where
  empty = Left ""
  (Left _) <|> r = r
  l <|> _ = l

instance Alternative Lexer where
  empty = Lexer (const Left "")
  (Lexer p1) <|> (Lexer p2) = Lexer (\s -> p1 s <|> p2 s)

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
           | None -- only used for error detection
           deriving (Show)

spaces :: Lexer String
spaces = many (satisfy isSpace)

-- assumes you'll only try to match the beginning of the string
lexRegex :: String -> Lexer String
lexRegex regex = Lexer f
  where
    f s
      | s =~ regex = Right (match, drop (length match) s)
      | otherwise = Left $ "did not match regex: " ++ regex ++
        " at " ++ (head $ lines s)
      where match = (s =~ regex) :: String

-- takes token and matching regex as input
lexConstToken :: Token -> String -> Lexer Token
lexConstToken t r = (const t) <$> lexRegex r

-- to ensure the entire file was parser
lexEOF :: Lexer Token
lexEOF = spaces *> Lexer f
  where
    f s
      | null s = Right (None, s)
      | otherwise = Left $ "Could not lex: " ++ (head $ lines s)

-- hard to generalize because token values have different types
lexIntLit :: Lexer Token
lexIntLit = (IntLit . read) <$> lexRegex "^[0-9]+\\b"

lexIdent :: Lexer Token
lexIdent = Ident <$> lexRegex "^[a-zA-Z_]\\w*\\b"

lexToken :: Lexer Token
lexToken = lexConstToken Void "^void\\b" <|>
           lexConstToken Return "^return\\b" <|>
           lexConstToken Int_ "^int\\b" <|>
           lexConstToken OpenP "^\\(" <|>
           lexConstToken CloseP "^\\)" <|>
           lexConstToken OpenB "^\\{" <|>
           lexConstToken CloseB "^\\}" <|>
           lexConstToken Semi "^;" <|>
           lexIntLit <|>
           lexIdent

-- for testing
repeatLex :: Integer -> Lexer a -> Lexer [a]
repeatLex 0 _ = pure []
repeatLex n p = (:) <$> p <*> repeatLex (n - 1) p

-- main function
lexer :: Lexer [Token]
lexer = (many $ spaces *> lexToken) <* lexEOF

lexerEval :: String -> Either String ([Token], String)
lexerEval = runLexer lexer

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
preprocess = (removeComments "/\\*([^*]|\\*+[^/])*\\*+/") .
             unwords . lines . (removeComments "//.*$")

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  content <- readFile path
  let result = preprocess content
  let tokens = lexerEval result
  putStrLn (show tokens)
