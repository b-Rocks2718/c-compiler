
module Main where

import System.Environment
import Data.Maybe
import AParser
import Control.Applicative
import Text.Regex.Posix

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

firstCharIsClosingBrace :: String -> Bool
firstCharIsClosingBrace str = case str =~ "\\A\\}" :: Bool of
    True -> True
    False -> False

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- takes token and matching regex as input
parseConstToken :: Token -> String -> Parser Token
parseConstToken t r = (const t) <$> parseRegex r

-- regex package was being weird for {} characters
parseCharToken :: Token -> Char -> Parser Token
parseCharToken t c = (const t) <$> satisfy (==c)

parseIntLit :: Parser Token
parseIntLit = (IntLit . read) <$> parseRegex "^[0-9]+\\b"

parseIdent :: Parser Token
parseIdent = Ident <$> parseRegex "^[a-zA-Z_]\\w*\\b"

parseToken :: Parser Token
parseToken = parseConstToken Void "^void\\b" <|> parseConstToken Return "^return\\b" <|>
              parseConstToken Int_ "^int\\b" <|> parseCharToken OpenP '(' <|>
              parseCharToken CloseP ')' <|> parseCharToken OpenB '{' <|>
              parseCharToken CloseB '}' <|> parseCharToken Semi ';' <|>
              parseIntLit <|> parseIdent

-- for testing
repeatParse :: Integer -> Parser a -> Parser [a]
repeatParse 1 p = (:[]) <$> p
repeatParse n p = (:) <$> p <*> repeatParse (n - 1) p

testStr :: String
testStr = "\nint main(void) {\n  return 2;\n}"

main :: IO ()
main = do
  args <- getArgs
  let path = fromMaybe "" $ safeHead args
  content <- readFile path
  let result = runParser (many $ spaces *> parseToken) content
  putStrLn (show result)
