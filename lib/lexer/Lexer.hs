{-# LANGUAGE FlexibleContexts #-}
module Lexer where

import Utils
import Numeric (readHex)

data Token = IntLit {intLitVal :: Int}
           | LongLit {intLitVal :: Int}
           | UIntLit {intLitVal :: Int}
           | ULongLit {intLitVal :: Int}
           | Ident {identName :: String}
           | Void
           | IntTok
           | ReturnTok
           | OpenP
           | CloseP
           | OpenB
           | CloseB
           | OpenS
           | CloseS
           | Semi
           | Tilde
           | IncTok
           | DecTok
           | Plus
           | Asterisk
           | Slash
           | Percent
           | Minus
           | Ampersand
           | Pipe
           | Carat
           | ShiftLTok
           | ShiftRTok
           | Exclamation
           | DoubleAmpersand
           | DoublePipe
           | DoubleEquals
           | Equals
           | NotEqual
           | LessThan
           | GreaterThan
           | LessThanEq
           | GreaterThanEq
           | PlusEq
           | MinusEq
           | TimesEq
           | DivEq
           | ModEq
           | AndEq
           | OrEq
           | XorEq
           | ShlEq
           | ShrEq
           | IfTok
           | ElseTok
           | Question
           | Colon
           | GoToTok
           | DoTok
           | WhileTok
           | ForTok
           | BreakTok
           | ContinueTok
           | Comma
           | StaticTok
           | ExternTok
           | SwitchTok
           | CaseTok
           | DefaultTok
           | UnsignedTok
           | SignedTok
           | LongTok
           deriving (Show, Eq)

spaces :: Parser Char String
spaces = many (satisfy isSpace)

-- only matches the beginning of the string
lexRegex :: String -> Parser Char String
lexRegex regex = Parser f
  where
    f s
      | s =~ ('^' : regex) = Ok (match, drop (length match) s)
      | otherwise = Fail
      where match = (s =~ regex) :: String

-- takes token and matching regex as input
lexConstToken :: Token -> String -> Parser Char Token
lexConstToken t r = t <$ lexRegex r

-- to ensure the entire file was lexed
lexEOF :: Parser Char ()
lexEOF = spaces *> Parser f
  where
    f s
      | null s = Ok ((), s)
      | otherwise = Err $ "Syntax Error: Unrecognized token: " ++ [head s]

lexIntLit :: Parser Char Token
lexIntLit = lexDecInt <|> lexHexInt

lexDecInt :: Parser Char Token
lexDecInt = IntLit . read <$> lexRegex "[0-9]+\\b"

lexHexInt :: Parser Char Token
lexHexInt = IntLit . fst . head . readHex . drop 2 <$> lexRegex "0[xX][0-9a-fA-F]+\\b"

lexLongLit :: Parser Char Token
lexLongLit = LongLit . read <$> lexRegex "[0-9]+[lL]\\b"

lexUIntLit :: Parser Char Token
lexUIntLit = UIntLit . read . init <$> lexRegex "[0-9]+[uU]\\b"

lexULongLit :: Parser Char Token
lexULongLit = ULongLit . read <$> lexRegex "[0-9]+([lL][uU]|[uU][lL])\\b"

lexIdent :: Parser Char Token
lexIdent = Ident <$> lexRegex "[a-zA-Z_]\\w*\\b"

-- remember: for tokens that start the same, put the longer one first
lexToken :: Parser Char Token
lexToken = -- keywords
           lexConstToken Void "void\\b" <|>
           lexConstToken ReturnTok "return\\b" <|>
           lexConstToken IntTok "int\\b" <|>
           lexConstToken IfTok "if\\b" <|>
           lexConstToken ElseTok "else\\b" <|>
           lexConstToken GoToTok "goto\\b" <|>
           lexConstToken DoTok "do\\b" <|>
           lexConstToken WhileTok "while\\b" <|>
           lexConstToken ForTok "for\\b" <|>
           lexConstToken BreakTok "break\\b" <|>
           lexConstToken ContinueTok "continue\\b" <|>
           lexConstToken StaticTok "static\\b" <|>
           lexConstToken ExternTok "extern\\b" <|>
           lexConstToken SwitchTok "switch\\b" <|>
           lexConstToken CaseTok "case\\b" <|>
           lexConstToken DefaultTok "default\\b" <|>
           lexConstToken UnsignedTok "unsigned\\b" <|>
           lexConstToken SignedTok "signed\\b" <|>
           lexConstToken LongTok "long\\b" <|>

           -- symbols and operators
           lexConstToken Comma "," <|>
           lexConstToken Question "\\?" <|>
           lexConstToken Colon ":" <|>
           lexConstToken OpenP "\\(" <|>
           lexConstToken CloseP "\\)" <|>
           lexConstToken OpenB "\\{" <|>
           lexConstToken CloseB "\\}" <|>
           lexConstToken Semi ";" <|>
           lexConstToken Tilde "~" <|>
           lexConstToken IncTok "\\+\\+" <|>
           lexConstToken PlusEq "\\+=" <|>
           lexConstToken TimesEq "\\*=" <|>
           lexConstToken Asterisk "\\*" <|>
           lexConstToken DivEq "/=" <|>
           lexConstToken Slash "/" <|>
           lexConstToken ModEq "\\%=" <|>
           lexConstToken Percent "\\%" <|>
           lexConstToken DecTok "--" <|>
           lexConstToken MinusEq "-=" <|>
           lexConstToken Plus "\\+" <|>
           lexConstToken Minus "-" <|>
           lexConstToken DoubleAmpersand "&&" <|>
           lexConstToken DoublePipe "\\|\\|" <|>
           lexConstToken AndEq "&=" <|>
           lexConstToken Ampersand "&" <|>
           lexConstToken OrEq "\\|=" <|>
           lexConstToken Pipe "\\|" <|>
           lexConstToken XorEq "\\^=" <|>
           lexConstToken Carat "\\^" <|>
           lexConstToken ShrEq ">>=" <|>
           lexConstToken ShlEq "<<=" <|>
           lexConstToken ShiftRTok ">>" <|>
           lexConstToken ShiftLTok "<<" <|>
           lexConstToken NotEqual "!=" <|>
           lexConstToken DoubleEquals "==" <|>
           lexConstToken Exclamation "!" <|>
           lexConstToken Equals "=" <|>
           lexConstToken GreaterThanEq ">=" <|>
           lexConstToken LessThanEq "<=" <|>
           lexConstToken GreaterThan ">" <|>
           lexConstToken LessThan "<" <|>
           lexConstToken OpenS "\\[" <|>
           lexConstToken CloseS "\\]" <|>

           -- numbers and identifiers
           lexUIntLit <|>
           lexIntLit <|>
           lexIdent

showTokens :: Result ([Token], String) -> String
showTokens (Ok (ts, _)) = show ts
showTokens (Err s) = s
showTokens Fail = "Fail"

lexer :: Parser Char [Token]
lexer = many (spaces *> lexToken) <* lexEOF

lexProg :: String -> Result ([Token], String)
lexProg = runParser lexer