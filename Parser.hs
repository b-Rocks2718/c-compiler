
module Parser where

import Lexer
import Control.Applicative

-- AST data structure
data ASTProg = ASTProg ASTFunc
data ASTFunc = ASTFunc String ASTStmt
data ASTStmt = ASTReturn ASTExpr
data ASTExpr = Factor Factor
             | ASTBinary ASTExpr BinOp ASTExpr
data Factor = ASTLit Int
             | ASTUnary UnaryOp Factor
             | FactorExp ASTExpr

data UnaryOp = Complement
             | Negate
             deriving (Show)

data BinOp = SubOp | AddOp | MulOp | DivOp | ModOp
            deriving (Show)

instance Show ASTProg where
  show (ASTProg f) = "Program(\n" ++ showFunc f 1 ++ "\n)"

showFunc :: ASTFunc -> Int -> String
showFunc (ASTFunc name body) n =
  tabs ++ "Function(\n" ++ tabs ++ "    name=\"" ++ name ++
  "\",\n" ++ tabs ++ "    body=" ++ showStatement body (n + 1) ++
  "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show ASTFunc where
  show = flip showFunc 0

-- only return statements for now
showStatement :: ASTStmt -> Int -> String
showStatement (ASTReturn expr) n =
  "Return(\n" ++ tabs ++ "    " ++ show expr ++ "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show ASTStmt where
  show = flip showStatement 0

instance Show ASTExpr where
  show (Factor x) = show x
  show (ASTBinary expl op expr) = 
    show op ++ "(" ++ show expl ++ ", " ++ show expr ++ ")"

instance Show Factor where
  show (ASTLit n) = "Constant(" ++ show n ++ ")"
  show (ASTUnary op expr) = show op ++ "(" ++ show expr ++ ")"
  show (FactorExp expr) = show expr

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

isIntLit :: Token -> Bool
isIntLit (IntLit _) = True
isIntLit _ = False

isUnaryOp :: Token -> Bool
isUnaryOp Minus = True
isUnaryOp Tilde = True
isUnaryOp _ = False

isBinOp :: Token -> Bool
isBinOp Minus = True
isBinOp Plus = True
isBinOp Asterisk = True
isBinOp Slash = True
isBinOp Percent = True
isBinOp _ = False

identName :: Token -> String
identName (Ident s) = s
identName x = error $ show x ++ " is not an identifier"

intLitVal :: Token -> Int
intLitVal (IntLit n) = n
intLitVal x = error $ show x ++ " is not an int lit"

getUnaryOp :: Token -> UnaryOp
getUnaryOp Tilde = Complement
getUnaryOp Minus = Negate
getUnaryOp x = error $ show x ++ " is not an unary operator"

getBinOp :: Token -> BinOp
getBinOp Plus = AddOp
getBinOp Minus = SubOp
getBinOp Asterisk = MulOp
getBinOp Slash = DivOp
getBinOp Percent = ModOp
getBinOp x = error $ show x ++ " is not a binary operator"

-- Program is just a function
parseProgram :: Parser Token ASTProg
parseProgram = ASTProg <$> parseFunction <* parseEOF

-- function looks like ident(void){stmt}
parseFunction :: Parser Token ASTFunc
parseFunction = liftA2 ASTFunc
  (identName <$> (match IntTok *> satisfy isIdent <* match OpenP <*
                 tryParse Void <* match CloseP <* match OpenB))
  parseStatement <* match CloseB

-- stmt is just return exp;
parseStatement :: Parser Token ASTStmt
parseStatement =
  ASTReturn <$> (match ReturnTok *> parseExpr <* match Semi)

parseLit :: Parser Token Factor
parseLit = ASTLit . intLitVal <$> satisfy isIntLit

parseUnary :: Parser Token Factor
parseUnary = ASTUnary . getUnaryOp <$> satisfy isUnaryOp <*>
             parseFactor

parseParens :: Parser Token Factor
parseParens = match OpenP *> parseFactor <* match CloseP

-- exp is just an integer literal or unary op on an exp
parseFactor :: Parser Token Factor
parseFactor = parseLit <|>
            parseUnary <|>
            parseParens

parseExpr :: Parser Token ASTExpr
parseExpr = (Factor <$> parseFactor) <|> 
            liftA3 ASTBinary parseExpr (getBinOp <$> satisfy isBinOp) parseExpr

-- to ensure the entire file was parsed
parseEOF :: Parser Token [a]
parseEOF = Parser f
  where
    f ts
      | null ts = Right ([], ts)
      | otherwise = Left $ "Could not lex: " ++ show ts

tryParse :: (Show a, Eq a) => a -> Parser a [a]
tryParse a = (:[]) <$> match a <|> pure []

-- print Eithers without 'Left'/'Right'
showAST :: Either String (ASTProg, [Token]) -> String
showAST (Right (p, ts)) = show p
showAST (Left s) = s

showTokens :: Either String ([Token], String) -> String
showTokens (Right (ts, s)) = show ts
showTokens (Left s) = s
